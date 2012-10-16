basta.default <- function(object, studyStart, studyEnd, minAge = 0, 
    model = "GO", shape = "simple", covarsStruct = "fused", niter = 20000, 
    burnin = 5001, thinning = 20, recaptTrans = studyStart, 
    thetaStart = NULL, thetaJumps = NULL, thetaPriorMean = NULL, 
    thetaPriorSd = NULL, gammaStart = NULL, gammaJumps = NULL, 
    gammaPriorMean = NULL, gammaPriorSd = NULL, 
    nsim = 1, parallel = FALSE, ncpus = 2, lifeTable = TRUE, 
    progrPlots = FALSE, updateJumps = TRUE, ...) {
  
  # This function estimates age-specific mortality from capture-recapture/
  # recovery (CRR) data when a large proportion of (or all) the records have
  # unknown times of birth and death. It uses the framework described by
  # Colchero & Clark (2012) J Anim Ecol.
  bastaIntVars <- c(".algObj", ".defTheta", ".CalcMort", ".CalcSurv", 
      ".dataObj", ".covObj", ".userPars", ".fullParObj", ".agesIni", 
      ".parsIni", ".priorAgeDist", ".parsCovIni", ".postIni", 
      ".Random.seed",  ".jumps", ".jumpObjIni")
  idDel <- which(bastaIntVars %in% ls(all.names = TRUE))
  if (length(idDel) > 0) {
    rm(list = bastaIntVars[idDel], envir = .GlobalEnv)
  }
  .algObj <- .CreateAlgObj(model, shape, studyStart, studyEnd, minAge, 
      covarsStruct, recaptTrans, niter, burnin, thinning, updateJumps, nsim)
  .FindErrors(object, .algObj)
  .dataObj <- .PrepDataObj(object, .algObj)
  .defTheta <- .SetDefaultTheta(.algObj)
  .CalcMort <- .DefineMort(.algObj)
  .CalcSurv <- .DefineSurv(.algObj)
  .covObj <- .CreateCovObj(object, .dataObj, .algObj)
  .algObj$covStruc <- class(.covObj)[1]
  .userPars <- .CreateUserPar(.covObj, thetaStart, thetaJumps,thetaPriorMean,
      thetaPriorSd, gammaStart, gammaJumps, gammaPriorMean, gammaPriorSd)
  .fullParObj <- .BuildFullParObj(.covObj, .defTheta, .algObj, .userPars, 
      .dataObj)
  .jumps <- list()
  .jumps$theta <- .fullParObj$theta$jump
  if (.fullParObj$class[1] == "theGam") {
    .jumps$gamma <- .fullParObj$gamma$jump
  }
  .agesIni <- .PrepAgeObj(.dataObj, .algObj)
  .parsIni <- .DefineIniParObj(.fullParObj)
  .parsCovIni <- .BuildParCovObj(.covObj, .parsIni)
  objList <- list(.algObj = .algObj, .dataObj = .dataObj, .covObj = .covObj, 
      .defTheta = .defTheta, .userPars = .userPars, .CalcMort = .CalcMort, 
      .CalcSurv = .CalcSurv, .parsIni = .parsIni, .parsCovIni = .parsCovIni, 
      .agesIni = .agesIni, .fullParObj = .fullParObj, .priorAgeDist = NA, 
      .postIni = NA, .jumps = .jumps, .jumpObjIni = NA)
  .AssignVarsToEnv(objList)
  cat("Calculating prior age distribution... ")
  .priorAgeDist <- .SetPriorAgeDist(.agesIni)
  assign(".priorAgeDist", .priorAgeDist, envir = .GlobalEnv)
  cat(" done.\n\n")
  .postIni <- .BuildPostObj(.agesIni, .parsIni, .parsCovIni)
  assign(".postIni", .postIni, envir = .GlobalEnv)
  Start <- Sys.time()
  if(updateJumps) {
    .jumpObjIni <- .PrepJumpObj(.fullParObj, .covObj)
    assign(".jumpObjIni", .jumpObjIni, envir = .GlobalEnv)
    updatedJumps <- .RunIniUpdJump()
    .jumps <- updatedJumps$updJumps
    assign(".jumps", .jumps, envir = .GlobalEnv)
  } 
  if (nsim > 1) {
    cat("Multiple simulations started...\n\n") 
    if (parallel) {
      availPkgs <- installed.packages()
      if (!is.element("snowfall", availPkgs)) {
        warning("\nPackage 'snowfall' is not installed.\nSimulations ",
            "will not be ran in parallel (computing time will ",
            "be longer...)\n")
        bastaOut <- lapply(1:nsim, .RunBastaMCMC)
      } else {
        opp <- options()
        options(warn = -1)
        require(snowfall)
        sfInit(parallel = TRUE, cpus = ncpus)
        sfExport(list = bastaIntVars)
        sfLibrary("BaSTA", character.only = TRUE, warn.conflicts = FALSE)
        sfLibrary(msm, warn.conflicts = FALSE)
        bastaOut <- sfClusterApplyLB(1:nsim, .RunBastaMCMC)
        sfRemoveAll(hidden = TRUE)
        sfStop()
        options(opp)
      }
    } else {
      bastaOut <- lapply(1:nsim, .RunBastaMCMC)
    }
  } else {
    cat("Simulation started...\n\n")
    bastaOut <- lapply(1:nsim, .RunBastaMCMC)
  }
  End <- Sys.time()
  names(bastaOut) <- paste("sim.", 1:nsim, sep = "")
  compTime <- round(as.numeric(End-Start, units = units(End - Start)), 2)
  cat(sprintf("Total MCMC computing time: %.2f %s.\n\n", compTime, 
          units(End - Start)))
  bastaResults <- .CalcDiagnost(bastaOut)
  bastaResults$settings <- c(niter, burnin, thinning, nsim)
  names(bastaResults$settings) <- c("niter", "burnin", "thinning", "nsim")
  bastaResults$modelSpecs <- 
      c(unlist(.algObj)[c("model", "shape", "covStruc", "minAge")],
          paste(names(.covObj$cat), collapse = ", "), 
          paste(names(.covObj$cont), collapse = ", "))
  names(bastaResults$modelSpecs) <- c("model", "shape", "Covar. structure", 
      "minAge", "Categorical", "Continuous")
  bastaResults <- .CalcQuants(bastaOut, bastaResults)
  bastaResults$jumpPriors <- 
      cbind(c(.jumps$theta, .jumps$gamma), 
          c(.fullParObj$theta$priorMean, .fullParObj$gamma$priorMean), 
          c(.fullParObj$theta$priorSd, .fullParObj$gamma$priorSd))
  dimnames(bastaResults$jumpPriors) <- 
      list(.fullParObj$allNames[substr(.fullParObj$allNames, 1, 2) != "pi"],
          c("Jump.sds", "Prior.means", "Prior.sds"))
  bastaResults$parsForPlot <- list()
  for (pp in names(bastaOut)) {
    bastaResults$parsForPlot[[pp]] <- 
        bastaOut[[pp]]$par[seq(1, niter, thinning), ]
  }
  bastaResults$lifeTable <- .CalcLifeTable(bastaResults, lifeTable, object)
  rm(list = bastaIntVars, envir = .GlobalEnv)
  # Define class for output object:
  class(bastaResults) <- "basta"
  return(bastaResults)
}

