basta <-
    function(object, ... ) UseMethod("basta")

# BaSTA Internal functions:
.AssignVarsToEnv <- function(objList) {
  for (obj in 1:length(objList)) {
    assign(names(objList)[obj], objList[[obj]], envir = .GlobalEnv, 
        inherits = TRUE)
  }
}

# Create initial objects.
.CreateAlgObj <- function(model, shape, studyStart, studyEnd, 
    minAge, covarsStruct, recaptTrans, niter, burnin, thinning, updateJumps,
    nsim) {
  return(list(model = model, shape = shape, start = studyStart, 
          end = studyEnd, minAge = minAge, covStruc = covarsStruct, 
          recap = recaptTrans, niter = niter, burnin = burnin, 
          thinning = thinning, updJump = updateJumps, nsim = nsim))
}

.CreateUserPar <- function(.covObj, thetaStart, thetaJumps,thetaPriorMean,
    thetaPriorSd, gammaStart, gammaJumps, gammaPriorMean, gammaPriorSd) {
  .userPars <- list()
  .userPars$theta <- list(start = thetaStart, jump = thetaJumps, 
      priorMean = thetaPriorMean, priorSd = thetaPriorSd)
  if (class(.covObj)[1] %in% c("fused", "propHaz")) {
    .userPars$gamma <- list(start = gammaStart, jump = gammaJumps, 
        priorMean = gammaPriorMean, priorSd = gammaPriorSd)
  }
  return(.userPars)
}


.FindErrors <- function(object, .algObj) {
  data.check <- DataCheck(object, .algObj$start, .algObj$end, silent = TRUE)
  if (!data.check[[1]]) {
    stop("You have an error in Dataframe 'object',\nplease use function ", 
        "'DataCheck'\n", call. = FALSE)
  }
  
# 2.2 Check that niter, burnin, and thinning are compatible.
  if (.algObj$burnin > .algObj$niter) {
    stop("Object '.algObj$burnin' larger than '.algObj$niter'.", call. = FALSE)
  }
  if (.algObj$thinning > .algObj$niter) {
    stop("Object '.algObj$thinning' larger than '.algObj$niter'.", call. = FALSE)
  }
  
# 2.3 Model type, .algObj$shape and covariate structure:
  if (!is.element(.algObj$model, c("EX", "GO", "WE", "LO"))) {
    stop("Model misspecification: specify available models", 
        " (i.e. 'EX', 'GO', 'WE' or 'LO')\n", call. = FALSE)
  }
  if (!is.element(.algObj$shape, c("simple", "Makeham", "bathtub"))) {
    stop(".algObj$shape misspecification. Appropriate arguments are:", 
        " 'simple', 'Makeham' or 'bathtub'.\n", call. = FALSE)
  }
  if (!is.element(.algObj$covStruc, c("fused", "prop.haz", "all.in.mort"))) {
    stop("Covariate structure misspecification. Appropriate arguments are:", 
        " 'fused', 'prop.haz' or 'all.in.mort'.\n", call. = FALSE)
  }
  if (.algObj$model == "EX" & .algObj$shape != "simple") {
    stop("Model misspecification: EX .algObj$model can only be fitted with a", 
        " simple .algObj$shape", call. = FALSE)
  }
  if (.algObj$model == "EX" & .algObj$covStruc != "fused") {
    stop("Model misspecification: EX .algObj$model can only be fitted with a", 
        " fused covariate structure", call. = FALSE)
  }
  if (.algObj$covStruc == "all.in.mort" & sum(.algObj$model == "GO", 
      .algObj$shape == "simple") < 2) {
    stop("Model misspecification: all.in.mort is only available with", 
        " Gompertz (GO) models and simple .algObj$shape.", call. = FALSE)
  }
}


.PrepDataObj <- function(object, .algObj) {
  .dataObj <- list()
  .dataObj$study <- .algObj$start:.algObj$end
  .dataObj$studyLen <- length(.dataObj$study)
  .dataObj$n <- nrow(object)
  .dataObj$Y <- as.matrix(object[, 1:.dataObj$studyLen + 3])
  colnames(.dataObj$Y) <- .dataObj$study
  bd <- as.matrix(object[, 2:3])
  .dataObj$bi <- bd[, 1]
  .dataObj$di <- bd[, 2]
  bi0 <- which(.dataObj$bi == 0)
  if (length(bi0) > 0) {
    .dataObj$idNoB <- bi0
    .dataObj$updB <- TRUE
  } else {
    .dataObj$updB <- FALSE
  }
  di0 <- which(.dataObj$di == 0)
  if (length(di0) > 0) {
    .dataObj$idNoD <- di0
    .dataObj$updD <- TRUE
  } else {
    .dataObj$updD <- FALSE
  }
  
  if (!.dataObj$updB & !.dataObj$updD) {
    class(.dataObj) <- "noAgeUpd"
  } else {
    .dataObj$idNoA <- sort(unique(c(.dataObj$idNoB, .dataObj$idNoD)))
    # 4.1.2 Calculate first and last time observed 
    #       and total number of times observed:
    ytemp <- t(t(.dataObj$Y) * .dataObj$study)
    .dataObj$lastObs <- c(apply(ytemp, 1, max))
    ytemp[ytemp == 0] <- 10000
    .dataObj$firstObs <- c(apply(ytemp, 1, min))
    .dataObj$firstObs[.dataObj$firstObs == 10000] <- 0
    .dataObj$oi <- .dataObj$Y %*% rep(1, .dataObj$studyLen)
    
    # 4.1.3 Define study duration:
    .dataObj$Dx <- 1 #(study.years[2] - study.years[1])
    .dataObj$Tm <- matrix(.dataObj$study, .dataObj$n, 
        .dataObj$studyLen, byrow = TRUE)
    fii <- .dataObj$firstObs
    fii[.dataObj$bi > 0 & .dataObj$bi >= .algObj$start] <- 
        .dataObj$bi[.dataObj$bi > 0 & .dataObj$bi >= .algObj$start] + 1
    fii[.dataObj$bi > 0 & .dataObj$bi < .algObj$start]  <- .algObj$start
    lii <- .dataObj$lastObs
    lii[.dataObj$di > 0 & .dataObj$di <= .algObj$end] <- 
        .dataObj$di[.dataObj$di > 0 & .dataObj$di <= .algObj$end] - 1
    lii[.dataObj$di > 0 & .dataObj$di > .algObj$end] <- .algObj$end
    .dataObj$obsMat <- .BuildAliveMatrix(fii, lii, .dataObj)
    .dataObj$obsMat[lii == 0 | fii == 0, ] <- 0
    class(.dataObj) <- "ageUpd"
  }  
  return(.dataObj)
}


.PrepAgeObj <- function(.dataObj, .algObj) {
  ageObj <- list()
  birth  <- .dataObj$bi
  if (.dataObj$updB) {
    idBi0Fi1 <- which(.dataObj$bi == 0 & .dataObj$firstObs > 0)
    birth[idBi0Fi1] <- .dataObj$firstObs[idBi0Fi1] - 
        sample(1:6, length(idBi0Fi1), replace = TRUE)
    idBi0Fi0 <- which(.dataObj$bi == 0 & .dataObj$firstObs == 0 & .dataObj$di > 0)
    birth[idBi0Fi0] <- .dataObj$di[idBi0Fi0] - 
        sample(0:6, length(idBi0Fi0), replace = TRUE)
  }
  death <- .dataObj$di
  if (.dataObj$updD) {
    idDi0Li1 <- which(.dataObj$di == 0 & .dataObj$lastObs > 0)
    death[idDi0Li1] <- .dataObj$lastObs[idDi0Li1] + 
        sample(1:6, length(idDi0Li1), replace = TRUE)
    idDi0Li0 <- which(.dataObj$di == 0 & .dataObj$lastObs == 0) 
    death[idDi0Li0] <- .dataObj$bi[idDi0Li0] + 
        sample(0:6, length(idDi0Li0), replace = TRUE)
    idDiNeg <- which(death < .algObj$start)
    death[idDiNeg]<- .algObj$start + 
        sample(1:6, length(idDiNeg), replace = TRUE)
  }
  age <- death - birth
  ageObj$ages <- cbind(birth, death, age)
  
  # 5.5 Full observation matrix:
  firstObs <- c(apply(cbind(.algObj$start, birth + 1), 1, max))
  lastObs <- c(apply(cbind(.algObj$end, death - 1), 1, min))
  alive <- .BuildAliveMatrix(firstObs, lastObs, .dataObj)
  ageObj$alive <- alive
  class(ageObj) <- c(class(.dataObj), "noMinAge")
  
  if (.algObj$minAge > 0) {
    # Juvenile and adult ages:
    indAd <- rep(0, .dataObj$n)
    indJu <- indAd
    indAd[age >= .algObj$minAge] <- 1
    indJu[age < .algObj$minAge] <- 1
    ageJu <- age
    ageJu[age > .algObj$minAge] <- .algObj$minAge
    ageAd <- age - .algObj$minAge
    ageAd[age < .algObj$minAge] <- 0
    ageJuTr <- age * 0
    idtr <- which(birth < .algObj$start & 
            .algObj$start - birth < .algObj$minAge)
    ageJuTr[idtr] <- .algObj$start - birth[idtr]
    ageAdTr <- age * 0
    idtr <- which(birth + .algObj$minAge < .algObj$start)
    ageAdTr[idtr] <- .algObj$start - (birth[idtr] + .algObj$minAge)
    ageObj$ages <- cbind(ageObj$ages, ageAd, ageAdTr, indAd, 
        ageJu, ageJuTr, indJu)
    class(ageObj)[2] <- "minAge"
  } else {
    idtr <- which(birth < .algObj$start)
    ageTr <- age * 0
    ageTr[idtr] <- .algObj$start - birth[idtr]
    ageObj$ages <- cbind(ageObj$ages, ageTr)
  }
  return(ageObj)
}


.CreateCovObj <- function(object, .dataObj, .algObj) {
  covObj <- list()
  covClass <- c("noCov", "noCovType")
  if (ncol(object) > .dataObj$studyLen + 3) {
    covMat <- as.matrix(object[, 
            (.dataObj$studyLen + 4):ncol(object)])
    covType <- .FindCovType(covMat)
    if (.algObj$covStruc == "fused") {
      covClass[1] <- "fused"
      if (!is.null(covType$cat)) {
        covObj$inMort <- covMat[, covType$cat]
        covObj$imLen <- ncol(covObj$inMort)
      } else {
        covClass[1] <- "propHaz"
      }
      if (!is.null(covType$cont)) {
        covObj$propHaz <- matrix(covMat[, c(covType$int, covType$cont)], 
            ncol = length(c(covType$int, covType$cont)), dimnames = 
                list(NULL, c(names(covType$int), names(covType$cont))))
        covObj$phLen <- ncol(covObj$propHaz)
      } else {
        covClass[1] <- "inMort"
      }
    } else if (.algObj$covStruc == "all.in.mort") {
      covObj$inMort <- covMat
      covObj$imLen <- ncol(covObj$inMort)
      covClass[1] <- "inMort"
    } else {
      if (!is.null(covType$int)) {
        covObj$propHaz <- matrix(covMat[, -covType$int], .dataObj$n,
            ncol(covMat) -1, dimnames = list(NULL, 
                colnames(covMat)[-covType$int]))
      } else if (!is.null(covType$cat)) {
        covObj$propHaz <- matrix(covMat[, -covType$cat[1]], .dataObj$n,
            ncol(covMat) -1, dimnames = list(NULL, 
                colnames(covMat)[-covType$cat[1]]))
      } else {
        covObj$propHaz <- covMat
      }
      covObj$phLen <- ncol(covObj$propHaz)
      covClass[1] <- "propHaz"
    }
    if (!is.null(covType$cat) & !is.null(covType$cont)) {
      covClass[2] <- "bothCov"
      covObj$cat <- covType$cat
      covObj$cont <- covType$cont
    } else if (!is.null(covType$cat)) {
      covClass[2] <- "cateCov"
      covObj$cat <- covType$cat
    } else if (!is.null(covType$cont)) {
      covClass[2] <- "contCov"
      covObj$cont <- covType$cont
    }
  } else {
    covObj$covs <- NULL
  }
  class(covObj) <- covClass
  return(covObj)
}


.FindCovType <- function(covMat) {
  # This functions finds and returns if an intercecpt was included 
  # and which covariates are categorical or continuous.
  if (!is.null(covMat)) {
    lu <- apply(covMat, 2, function(x) length(unique(x)))
    ru <- apply(covMat, 2, range)
    idcat <- which(lu == 2 & apply(ru, 2, sum) == 1)
    if (length(idcat) == 0) {
      idcat <- NULL
    }
    idint <- which(lu == 1)
    if (length(idint) == 0) {
      idint <- NULL
    }
    idcon <- which(lu > 2)
    if (length(idcon) == 0) {
      idcon <- NULL
    }
  }
  else {
    idcat <- NULL
    idint <- NULL
    idcon <- NULL
  }
  return(list(int = idint, cat = idcat, cont = idcon))
}


# Define model functions and parameter objects:
.SetDefaultTheta  <- function(.algObj) {
  if (.algObj$model == "EX") {
    nTh <- 1
    startTh <- 0.2 
    jumpTh <- 0.1
    priorMean <- 0.01
    priorSd <- 0.5
    nameTh <- "b0"
    lowTh <- 0
    jitter <- 0.05
  } else if (.algObj$model == "GO") {
    nTh <- 2 
    startTh <- c(-2, 0.01) 
    jumpTh <- c(0.1, 0.1)
    priorMean <- c(-6, 0.001)
    priorSd <- c(1, 0.25)
    nameTh <- c("b0", "b1")
    lowTh <- c(-Inf, -Inf)
    jitter <- c(0.1, 0.05) 
    if (.algObj$shape == "bathtub") {
      lowTh <- c(-Inf, 0)
    }
  } else if (.algObj$model == "WE") {
    nTh <- 2
    startTh <- c(1.5, 0.2) 
    jumpTh <- c(0.1, 0.1)
    priorMean <- c(1, 0.05)
    priorSd <- c(0.5, 0.1)
    nameTh <- c("b0", "b1")
    lowTh <- c(0, 0)
    jitter <- c(0.1, 0.1) 
  } else if (.algObj$model == "LO") {
    nTh <- 3 
    startTh <- c(-2, 0.01, 1e-04) 
    jumpTh <- c(0.1, 0.1, 0.1) 
    priorMean <- c(-6, 0.001, 1e-10)
    priorSd <- c(1, 0.1, 0.1)
    nameTh <- c("b0", "b1", "b2")
    lowTh <- c(-Inf, 0, 0)
    jitter <- c(0.1, 0.1, 0.1) 
  }
  if (.algObj$shape == "Makeham") {
    nTh <- nTh + 1 
    startTh <- c(0, startTh) 
    jumpTh <- c(0.1, jumpTh) 
    priorMean <- c(0, priorMean)
    priorSd <- c(0.1, priorSd)
    nameTh <- c("c", nameTh)
    lowTh <- c(-Inf, lowTh)
    jitter <- c(0.1, jitter) 
  } else if (.algObj$shape == "bathtub") {
    nTh <- nTh + 3 
    startTh <- c(-0.1, 0.6, 0, startTh)
    jumpTh <- c(0.1, 0.1, 0.1, jumpTh) 
    priorMean <- c(-2, 0.001, 0, priorMean)
    priorSd <- c(0.5, 0.1, 0.1, priorSd)
    nameTh <- c("a0", "a1", "c", nameTh)
    lowTh <- c(-Inf, 0, -Inf, lowTh)
    jitter <- c(0.1, 0.1, 0.1) 
  }
  defaultTheta  <- list(length = nTh, start = startTh, jump = jumpTh, 
      priorMean = priorMean, priorSd = priorSd, name = nameTh, 
      low = lowTh, jitter = jitter)
  attr(defaultTheta, ".algObj$model") = .algObj$model
  attr(defaultTheta, ".algObj$shape") = .algObj$shape
  return(defaultTheta)
}


.DefineMort <- function(.algObj) {
  if (.algObj$model == "EX") {
    .CalcMort <- function(x, theta) c(theta) * rep(1, length(x))
  } else if (.algObj$model == "GO") {
    if (.algObj$shape == "simple") {
      .CalcMort <- function(x, theta) {
        exp(theta[ ,"b0"] + theta[, "b1"] * x)
      }
    } else if (.algObj$shape == "Makeham") {
      .CalcMort <- function(x, theta) {
        theta[, "c"] + exp(theta[, "b0"] + theta[, "b1"] * x)
      }
    } else {
      .CalcMort <- function(x, theta) {
        exp(theta[, "a0"] - theta[, "a1"] * x) + theta[, "c"] + 
            exp(theta[, "b0"] + theta[, "b1"] * x)
      }
    }
  } else if (.algObj$model == "WE") {
    if (.algObj$shape == "simple") {
      .CalcMort <- function(x, theta) {
        theta[, "b0"] * theta[, "b1"]^theta[, "b0"] * 
            x^(theta[, "b0"] - 1)
      }
    } else if (.algObj$shape == "Makeham") {
      .CalcMort <- function(x, theta) {
        theta[, "c"] + theta[, "b0"] * theta[, "b1"]^theta[, "b0"] * 
            x^(theta[, "b0"] - 1)
      }
    } else {
      .CalcMort <- function(x, theta) {
        exp(theta[, "a0"] - theta[, "a1"] * x) + theta[, "c"] + 
            theta[, "b0"] * theta[, "b1"]^theta[, "b0"] * 
            x^(theta[, "b0"] - 1)
      }
    }
  } else if (.algObj$model == "LO") {
    if (.algObj$shape == "simple") {
      .CalcMort <- function(x, theta) {
        exp(theta[, "b0"] + theta[, "b1"] * x) / 
            (1 + theta[, "b2"] * exp(theta[, "b0"]) / 
              theta[, "b1"] * (exp(theta[, "b1"] * x) - 1))
      }
    } else if (.algObj$shape == "Makeham") {
      .CalcMort <- function(x, theta) {
        theta[, "c"] + exp(theta[, "b0"] + theta[, "b1"] * x) / 
            (1 + theta[, "b2"] * exp(theta[, "b0"]) / 
              theta[, "b1"] * (exp(theta[, "b1"] * x) - 1))
      }
    } else {
      .CalcMort <- function(x, theta) {
        exp(theta[, "a0"] - theta[, "a1"] * x) + theta[, "c"] + 
            exp(theta[, "b0"] + theta[, "b1"] * x) / 
            (1 + theta[, "b2"] * exp(theta[, "b0"]) / 
              theta[, "b1"] * (exp(theta[, "b1"] * x) - 1))
      }
    }
  }
  return(.CalcMort)
}


.DefineSurv <- function(.algObj) {
  if (.algObj$model == "EX") {
    .CalcSurv <- function(x, theta) exp(- c(theta) * x)
  } else if (.algObj$model == "GO") {
    if (.algObj$shape == "simple") {
      .CalcSurv <- function(x, theta) {
        exp(exp(theta[, "b0"]) / theta[, "b1"] * 
                (1 - exp(theta[, "b1"] * x)))
      }
    } else if (.algObj$shape == "Makeham") {
      .CalcSurv <- function(x, theta) {
        exp(-theta[, "c"] * x + exp(theta[, "b0"]) / theta[, "b1"] * 
                (1 - exp(theta[, "b1"] * x)))
      }
    } else {
      .CalcSurv <- function(x, theta) {
        exp(exp(theta[, "a0"]) / theta[, "a1"] * (exp(-theta[, "a1"] * x) - 1) - 
                theta[, "c"] * x + exp(theta[, "b0"]) / theta[, "b1"] * 
                (1 - exp(theta[, "b1"] * x)))
      }
    }
  } else if (.algObj$model == "WE") {
    if (.algObj$shape == "simple") {
      .CalcSurv <- function(x, theta) {
        exp(-(theta[, "b1"] * x)^theta[, "b0"])
      }      
    } else if (.algObj$shape == "Makeham") {
      .CalcSurv <- function(x, theta) {
        exp(-theta[, "c"] * x - (theta[, "b1"] * x)^theta[, "b0"])
      }
    } else {
      .CalcSurv <- function(x, theta) {
        exp(exp(theta[, "a0"]) / theta[, "a1"] * (exp(-theta[, "a1"] * x) - 1) -
                theta[, "c"] * x - (theta[, "b1"] * x)^theta[, "b0"])
      }
    }
  } else if (.algObj$model == "LO") {
    if (.algObj$shape == "simple") {
      .CalcSurv <- function(x, theta) {
        (1 + theta[, "b2"] * exp(theta[, "b0"]) / theta[, "b1"] * 
              (exp(theta[, "b1"] * x) - 1))^(-1 / theta[, "b2"])
      }
    } else if (.algObj$shape == "Makeham") {
      .CalcSurv <- function(x, theta) {
        exp(-theta[, "c"] * x) * (1 + theta[, "b2"] * exp(theta[, "b0"]) / 
              theta[, "b1"] * (exp(theta[, "b1"] * x) - 1))^(-1 / theta[, "b2"])
      }
    } else {
      .CalcSurv <- function(x, theta) {
        exp(exp(theta[, "a0"]) / theta[, "a1"] * (exp(-theta[, "a1"] * x) - 1) -
                    theta[, "c"] * x) * (1 + theta[, "b2"] * 
              exp(theta[, "b0"]) / theta[, "b1"] * 
              (exp(theta[, "b1"] * x) - 1))^(-1 / theta[, "b2"])
      }
    }
  }
  return(.CalcSurv)
}


.CalcLowC <- function(theta, ages) {
  theta[, "c"] <- 0
  mort <- .CalcMort(ages, theta)
  cLow <- -min(mort)[1]
  return(cLow)
}


.BuildFullParObj <- function(.covObj, .defTheta, .algObj, 
    .userPars, .dataObj) {
  .fullParObj <- list()
  .fullParObj$theta <- list()
  parNames <- c("start", "priorMean", "priorSd", "jump")
  for (i in 1:4) {
    if (class(.covObj)[1] %in% c("inMort", "fused")) {
      if (is.null(.userPars$theta[[parNames[i]]])) {
        .fullParObj$theta[[parNames[[i]]]] <- 
            matrix(.defTheta[[parNames[i]]], .covObj$imLen, 
                .defTheta$length, byrow = TRUE, 
                dimnames = list(colnames(.covObj$inMort), .defTheta$name))
      } else {
        if (is.element(length(.userPars$theta[[parNames[i]]]), 
            c(.defTheta$length, .defTheta$length * .covObj$imLen))) {
          if (length(.userPars$theta[[parNames[i]]]) == .defTheta$length) {
            .fullParObj$theta[[parNames[[i]]]] <- 
                matrix(.userPars$theta[[parNames[i]]], .covObj$imLen, 
                    .defTheta$length, byrow = TRUE, 
                    dimnames = list(colnames(.covObj$inMort), .defTheta$name))
          } else {
            .fullParObj$theta[[parNames[[i]]]] <- .userPars$theta[[parNames[i]]]
            dimnames(.fullParObj$theta[[parNames[[i]]]]) <- 
                list(colnames(.covObj$inMort), .defTheta$name)
          }
        } else {
          stop(paste("\nDimensions of theta ", parNames[i], 
                  " matrix are incorrect.\n",
                  "Provide a single vector of length ", .defTheta$length,
                  "\nor a matrix of dimensions ", .covObj$imLen ," times ", 
                  .defTheta$length, 
                  ".\n(i.e. number of covariates times number", 
                  " of\n parameters for model ", 
                  .algObj$model," with ", .algObj$shape, " shape).", sep = ""), 
              call. = FALSE)
        }
      }
      allParNames <- paste(rep(.defTheta$name, 
              each = ncol(.covObj$inMort)), 
          rep(colnames(.covObj$inMort), .defTheta$len), sep = ".")
    } else {
      if (is.null(.userPars$theta[[parNames[i]]])) {
        .fullParObj$theta[[parNames[[i]]]] <- 
            matrix(.defTheta[[parNames[i]]], 1, .defTheta$length, 
                dimnames = list(NULL, .defTheta$name))
      } else {
        if (length(.userPars$theta[[parNames[i]]]) == .defTheta$length) {
          .fullParObj$theta[[parNames[[i]]]] <- 
              matrix(.userPars$theta[[parNames[i]]], 1, .defTheta$length, 
                  dimnames = list(NULL, .defTheta$name))
        } else {
          stop(paste("\nLength of theta ", parNames[i], " is incorrect.\n",
                  "Provide a single vector of length ", .defTheta$length,
                  ".\n(i.e. number of parameters for model ", 
                  .algObj$model," with ", .algObj$shape, " shape).", sep = ""), 
              call. = FALSE)
        }
      }
      allParNames <- .defTheta$name
    }
  }
  .fullParObj$theta$low <- t(t(.fullParObj$theta$start) * 0 + .defTheta$low)
  .fullParObj$theta$len <- length(.fullParObj$theta$start)
  if (class(.covObj)[1] %in% c("propHaz", "fused")) {
    .fullParObj$gamma <- list()
    for (i in 1:4) {
      if (is.null(.userPars$gamma[[parNames[i]]])) {
        .fullParObj$gamma[[parNames[i]]] <- rep(c(0.01, 0.01, 1, 0.1)[i],
            .covObj$phLen)
        names(.fullParObj$gamma[[parNames[i]]]) <- colnames(.covObj$propHaz)
      } else {
        if (length(.userPars$gamma[[parNames[i]]]) == .covObj$phLen) {
          .fullParObj$gamma[[parNames[i]]] <- .userPars$gamma[[parNames[i]]]
          names(.fullParObj$gamma[[parNames[i]]]) <- colnames(.covObj$propHaz)
        } else {
          stop(paste("\nLength of gamma parameters is incorrect.\n",
                  "Provide a single vector of length ", .covObj$phLen,
                  ".\n(i.e. number of proportional hazards covariates).", 
                  sep = ""), call. = FALSE)
        }
      }
    }
    .fullParObj$gamma$len <- length(.fullParObj$gamma$start)
    allParNames <- c(allParNames, paste("gamma", colnames(.covObj$propHaz), 
            sep = "."))
  }
  
  # d) Detection probability:
  .fullParObj$pi <- list()
  idpi <- findInterval(.algObj$start:.algObj$end, .algObj$recap)
  names(idpi) <- .algObj$start:.algObj$end
  npi <- length(unique(idpi))
  .fullParObj$pi$start <- rep(0.5, npi)
  names(.fullParObj$pi$start) <- .algObj$recap
  .fullParObj$pi$idpi <- idpi
  .fullParObj$pi$n <- npi
  .fullParObj$pi$prior2 <- 0.1
  .fullParObj$pi$Prior1 <- tapply(0.1 + t(t(.dataObj$Y) %*% rep(1, .dataObj$n)),
      idpi, sum)
  
  .fullParObj$pi$len <- length(.fullParObj$pi$start)
  .fullParObj$allNames <- c(allParNames, paste("pi", .algObj$recap, sep = "."))
  Classes <- ifelse(class(.covObj)[1] %in% c("inMort", "noCov"), "theta",  
      "theGam")
  if (.algObj$minAge > 0) {
    .fullParObj$lambda <- list(start = 0.01, priorMean = 0.01, priorSd = 1, 
        jump = 0.01)
    Classes <- c(Classes, "lambda")
  } else {
    Classes <- c(Classes, "noLambda")
  }
  Classes <- c(Classes, "pi", "noEta")
  .fullParObj$class <- Classes
  return(.fullParObj)
}


.DefineIniParObj <- function(.fullParObj) {
  iniParObj <- list()
  iniParObj$theta <- .fullParObj$theta$start
  if (.fullParObj$class[1] %in% c("theGam")) {
    iniParObj$gamma <- .fullParObj$gamma$start
  }
  if (.fullParObj$class[2] == "lambda") {
    iniParObj$lambda <- .fullParObj$lambda$start
  }
  if (.fullParObj$class[3] %in% c("pi", "piEta")) {
    iniParObj$pi <- .fullParObj$pi$start
  }
  class(iniParObj) <- .fullParObj$class
  return(iniParObj)
}


.BuildPostObj <- function(ageObj, parObj, parCovObj) {
  postObj <- list()
  postObj$mat <- matrix(0, .dataObj$n, 6, 
      dimnames = list(NULL, c("fx", "Sx", "vx", "lx", "px", "postX")))
  postObj <- .CalcPostX(ageObj, parObj, postObj, parCovObj, 1:.dataObj$n)  
  return(postObj)
}


.BuildAliveMatrix <- function(f, l, .dataObj) {
  Fm <- .dataObj$Tm - f
  Fm[Fm >= 0] <- 1
  Fm[Fm < 0] <- 0
  Lm <- .dataObj$Tm - l
  Lm[Lm <= 0] <- -1
  Lm[Lm > 0] <- 0
  return(Fm * (-Lm))
}


# Sample ages:
.ProposeAges <- function(ageObj) {
  ageObjNew <- ageObj
  if (.dataObj$updB) {
    ageObjNew$ages[.dataObj$idNoB, 'birth'] <- 
        ageObj$ages[.dataObj$idNoB, 'birth'] + 
        sample(-1:1, length(.dataObj$idNoB), replace = TRUE)
    idOi <- .dataObj$idNoB[.dataObj$oi[.dataObj$idNoB] > 0]
    ageObjNew$ages[idOi, 'birth'] <- 
        apply(cbind(ageObjNew$ages[idOi, 'birth'],
                .dataObj$firstObs[idOi] - 1), 1, min)
    idOi0 <- .dataObj$idNoB[.dataObj$oi[.dataObj$idNoB] == 0]
    ageObjNew$ages[idOi0, 'birth'] <- 
        apply(cbind(ageObjNew$ages[idOi0, 'birth'],
                ageObj$ages[idOi0, 'death']), 1, min)
  }
  if (.dataObj$updD) {
    ageObjNew$ages[.dataObj$idNoD, 'death'] <- 
        ageObj$ages[.dataObj$idNoD, 'death'] +
        sample(-1:1, length(.dataObj$idNoD), replace = TRUE) 
    ageObjNew$ages[.dataObj$idNoD, 'death'] <- 
        apply(cbind(ageObjNew$ages[.dataObj$idNoD, 'death'],
                ageObjNew$ages[.dataObj$idNoD, 'birth'], 
                .dataObj$lastObs[.dataObj$idNoD] + 1), 1, max) 
    
  }
  ageObjNew$ages[.dataObj$idNoA, "age"] <- 
      ageObjNew$ages[.dataObj$idNoA, "death"] - 
      ageObjNew$ages[.dataObj$idNoA, "birth"]
  firstAlive <- c(apply(cbind(.algObj$start, 
              ageObjNew$ages[, "birth"] + 1), 1, max))
  lastAlive <- c(apply(cbind(.algObj$end, 
              ageObjNew$ages[, "death"] - 1), 1, min))
  alive <- .BuildAliveMatrix(firstAlive, lastAlive, .dataObj)
  ageObjNew$alive <- alive
  return(ageObjNew)
}


.SampleAges <- function(ageObj, ...) UseMethod(".SampleAges")

.SampleAges.noMinAge <- function(ageObj) {
  ageObjNew <- .ProposeAges(ageObj)
  idtr <- which(ageObjNew$ages[.dataObj$idNoA, "birth"] < .algObj$start)
  ageObjNew$ages[.dataObj$idNoA[idtr], "ageTr"] <- 
      .algObj$start - ageObjNew$ages[.dataObj$idNoA[idtr], "birth"]
  return(ageObjNew)
}

.SampleAges.minAge <- function(ageObj) {
  ageObjNew <- .ProposeAges(ageObj)
  ageObjNew$ages[.dataObj$idNoA, ] <- 
      .SplitByMinAge(ageObjNew$ages[.dataObj$idNoA, ])
  return(ageObjNew)
}

.SplitByMinAge <- function(ages) {
  ages[, "indAd"] <- 0
  ages[, "indJu"] <- 0
  ages[ages[, "age"] >= .algObj$minAge, "indAd"] <- 1
  ages[ages[, "age"] < .algObj$minAge, "indJu"] <- 1
  ages[ages[, "age"] > .algObj$minAge, "ageJu"] <- .algObj$minAge
  ages[, "ageAd"] <- ages[, "age"] - .algObj$minAge
  ages[, "ageAd"] <- ages[, "ageAd"] * ages[, "indAd"]
  idtr <- which(ages[, "birth"] < .algObj$start & 
          .algObj$start - ages[, "birth"] < .algObj$minAge)
  ages[idtr, "ageJuTr"] <- .algObj$start - ages[idtr, "birth"]
  idtr <- which(ages[, "birth"] + .algObj$minAge < .algObj$start)
  ages[idtr, "ageAdTr"] <- .algObj$start - (ages[idtr, "birth"] + 
        .algObj$minAge)
  return(ages)
}

.AcceptAges <- function(postObjNow, postObjNew, ind) {
  acceptCrit <- exp(postObjNew$mat[ind, "postX"] - 
          postObjNow$mat[ind, "postX"])
  indNoNa <- which(!is.na(acceptCrit))
  acceptCrit <- acceptCrit[indNoNa]
  ind2 <- ind[indNoNa]
  acceptProb <- runif(length(ind2))
  indAccept <- ind2[which(acceptCrit > acceptProb)]
  return(indAccept)
}

# Sample parameters:
.JitterPars <- function(parObj, ageObj, postObj, parsCovObj) {
  parObjNew <- parObj
  negMort <- TRUE
  while(negMort) {
    parObjNew$theta <- matrix(rtnorm(length(parObj$theta), parObj$theta, 
            .5, lower = .fullParObj$theta$low), 
        nrow(parObj$theta), ncol(parObj$theta), 
        dimnames = dimnames(parObj$theta))
    xRange <- ceiling(0:max(ageObj$ages[, "age"]) * 1.1)
    if (.algObj$shape != "simple") {
      lowC <-  sapply(1:nrow(parObj$theta), 
          function(cc) .CalcLowC(t(parObjNew$theta[cc, ]),
                xRange))
      parObjNew$theta[, 'c'] <- rtnorm(1:nrow(parObj$theta), 
          parObjNew$theta[, 'c'], .05, lower = lowC)
    }
    if (class(parObj)[1] == "theGam") {
      parObjNew$gamma <- rnorm(length(parObj$gamma), parObj$gamma, 0.5)
    }
    parsCovNew <- .CalcParCovObj(.covObj, parObjNew, parsCovObj)
    postNew <- .CalcLike(ageObj, parObjNew, postObj, parsCovNew, 1:.dataObj$n)
    negMort <- ifelse(postNew$mortPars == -Inf | is.na(postNew$mortPars), 
        TRUE, FALSE)
  }
  return(parObjNew)
}

.ProposeThetaPars <- function(parObj, ageObj, jumps, idPar) {
  parObjNew <- parObj
  negMort <- TRUE
  nRow <- idPar - floor((idPar - 1) / nrow(parObj$theta)) * 
      nrow(parObj$theta)
  while(negMort) {
    xRange <- 0:100
    if (.algObj$shape != "simple" & 
        substr(.fullParObj$allNames[idPar], 1, 1) == "c") {
      lowC <-  .CalcLowC(t(parObjNew$theta[nRow, ]), xRange)
      parObjNew$theta[idPar] <- rtnorm(1, parObj$theta[idPar], 
          jumps$theta[idPar], lower = lowC)
    } else {
      parObjNew$theta[idPar] <- rtnorm(1, parObj$theta[idPar], 
          jumps$theta[idPar], lower = .fullParObj$theta$low[idPar])
    }
    mortTest <- .CalcMort(xRange, t(parObjNew$theta[nRow, ]))
    negMort <- ifelse(all(mortTest >= 0), FALSE, TRUE)
  }
  return(parObjNew)
}

.ProposeGammaPars <- function(parObj, jumps, idPar) {
  parObjNew <- parObj
  parObjNew$gamma[idPar] <- rnorm(1, parObj$gamma[idPar], 
      jumps$gamma[idPar])
  return(parObjNew)
}


# Propose all mortality parameters:
.ProposeMortPars <- function(parObj, ...) UseMethod(".ProposeMortPars")

.ProposeMortPars.theta <- function(parObj, ageObj, jumps, idPar) {
  parObjNew <- .ProposeThetaPars(parObj, ageObj, jumps, idPar)
  return(parObjNew)
}


.ProposeMortPars.theGam <- function(parObj, ageObj, jumps, idPar) {
  if (idPar <= .fullParObj$theta$len) {
    parObjNew <- .ProposeThetaPars(parObj, ageObj, jumps, idPar)
  } else {
    parObjNew <- .ProposeGammaPars(parObj, jumps, idPar - 
            .fullParObj$theta$len)
  }
  return(parObjNew)
}

# Propose pi's:
.SamplePiPars <- function(parObj, ...) UseMethod(".SamplePiPars")

.SamplePiPars.pi <- function(parObj, ageObj) {
  rho2 <- .fullParObj$pi$prior2 + 
      t(t(ageObj$alive - .dataObj$Y) %*% rep(1, .dataObj$n))
  Rho2 <- tapply(rho2, .fullParObj$pi$idpi, sum)
  piNew <- rbeta(.fullParObj$pi$n, .fullParObj$pi$Prior1, Rho2)
  if (1 %in% piNew) {
    piNew[piNew == 1] <- 1-1e-5
    warning("Some recapture probabilities are equal to 1.",
        "\nThey have been constraint to be fractionally less than 1 ",
        "for computational reasons\n", call. = FALSE)
  }
  parObj$pi <- piNew
  return(parObj)
}

.SamplePiPars.noPi <- function(parObj, ...) {
  return(parObj)
}

.ProposeLambda <- function(parObj) UseMethod(".ProposeLambda")

.ProposeLambda.lambda <- function(parObj) {
  parObj$lambda <- rtnorm(n = 1, mean = parObj$lambda, 
      sd = .fullParObj$lambda$jump, lower = 0)
  return(parObj)
}

.ProposeLambda.noLambda <- function(parObj) {
  return(parObj)
}

# Calculate link function only for theta parameters:
.BuildParCovObj <- function(.covObj, ...) UseMethod(".BuildParCovObj")

.BuildParCovObj.noCov <- function(.covObj, parObj) {
  parCovObj <- list()
  parCovObj$theta <- parObj$theta
  class(parCovObj) <- 'noParCov'
  return(parCovObj)
}

.BuildParCovObj.inMort <- function(.covObj, parObj) {
  parCovObj <- list()
  parCovObj$theta <- .covObj$inMort %*% parObj$theta
  class(parCovObj) <- "inMortParCov"
  return(parCovObj)
}

.BuildParCovObj.propHaz <- function(.covObj, parObj) {
  parCovObj <- list()
  parCovObj$theta <- parObj$theta
  parCovObj$gamma <- .covObj$propHaz %*% parObj$gamma
  class(parCovObj) <- "propHazParCov"
  return(parCovObj)
}

.BuildParCovObj.fused <- function(.covObj, parObj) {
  parCovObj <- list()
  parCovObj$theta <- .covObj$inMort %*% parObj$theta
  parCovObj$gamma <- .covObj$propHaz %*% parObj$gamma
  class(parCovObj) <- "fusedParCov"
  return(parCovObj)
}

.CalcParCovObj <- function(.covObj, ...) UseMethod(".CalcParCovObj")

.CalcParCovObj.noCov <- function(.covObj, parObj, parCovObj) {
  parCovObj$theta <- parObj$theta 
  return(parCovObj)
}

.CalcParCovObj.inMort <- function(.covObj, parObj, parCovObj) {
  parCovObj$theta <- .covObj$inMort %*% parObj$theta
  return(parCovObj)
}

.CalcParCovObj.propHaz <- function(.covObj, parObj, parCovObj) {
  parCovObj$theta <- parObj$theta 
  parCovObj$gamma <- .covObj$propHaz %*% parObj$gamma
  return(parCovObj)
}

.CalcParCovObj.fused <- function(.covObj, parObj, parCovObj) {
  parCovObj$theta <- .covObj$inMort %*% parObj$theta
  parCovObj$gamma <- .covObj$propHaz %*% parObj$gamma
  return(parCovObj)
}

# Calculate pdf of ages at death:
.CalcPdf <- function(parCovObj, ...) UseMethod(".CalcPdf")

.CalcPdf.noParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta) - .CalcSurv(x[ind] + .dataObj$Dx, 
      parCovObj$theta)
}

.CalcPdf.inMortParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta[ind, ]) - 
      .CalcSurv(x[ind] + .dataObj$Dx, parCovObj$theta[ind, ])
}

.CalcPdf.propHazParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta)^exp(parCovObj$gamma[ind]) - 
      .CalcSurv(x[ind] + .dataObj$Dx, parCovObj$theta)^exp(parCovObj$gamma[ind])
}

.CalcPdf.fusedParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta[ind, ])^exp(parCovObj$gamma[ind]) - 
      .CalcSurv(x[ind] + .dataObj$Dx, 
          parCovObj$theta[ind, ])^exp(parCovObj$gamma[ind])
}

# Survival at age at truncation:
.CalcTruSurv <- function(parCovObj, ...) UseMethod(".CalcTruSurv")

.CalcTruSurv.noParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta)
}

.CalcTruSurv.inMortParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta[ind, ])
}

.CalcTruSurv.propHazParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta)^exp(parCovObj$gamma[ind])
}

.CalcTruSurv.fusedParCov <- function(parCovObj, x, ind) {
  .CalcSurv(x[ind], parCovObj$theta[ind, ])^exp(parCovObj$gamma[ind])
}



# Calculate likelihood:
.CalcLike <- function(ageObj, ...) UseMethod(".CalcLike")


.CalcLike.minAge <- function(ageObj, parObj, postObj, parCovObj, ind) {
  postObj$mat[ind, "fx"] <- log(.CalcPdf(parCovObj, ageObj$ages[, "ageAd"], 
          ind)) * ageObj$ages[ind, "indAd"]
  postObj$mat[ind, "Sx"] <- log(.CalcTruSurv(parCovObj, 
          ageObj$ages[, "ageAdTr"], ind)) * ageObj$ages[ind, "indAd"]
  postObj$mortPars <- .CalcPostMortPars(parObj, postObj)
  return(postObj)
}

.CalcLike.noMinAge <- function(ageObj, parObj, postObj, parCovObj, ind) {
  postObj$mat[ind, "fx"] <- log(.CalcPdf(parCovObj, ageObj$ages[, "age"], 
          ind))
  postObj$mat[ind, "Sx"] <- log(.CalcTruSurv(parCovObj, 
          ageObj$ages[, "ageTr"], ind))
  postObj$mortPars <- .CalcPostMortPars(parObj, postObj)
  return(postObj)
}

.CalcPostMortPars <- function(parObj, ...) UseMethod(".CalcPostMortPars")

.CalcPostMortPars.theta <- function(parObj, postObj) {
  parPost <- sum(postObj$mat[, "fx"] -
              postObj$mat[, "Sx"]) + 
      sum(dtnorm(c(parObj$theta), c(.fullParObj$theta$priorMean), 
              c(.fullParObj$theta$priorSd), 
              lower = c(.fullParObj$theta$low), log = TRUE)) 
  return(parPost)
}

.CalcPostMortPars.theGam <- function(parObj, postObj) {
  parPost <- sum(postObj$mat[, "fx"] -
              postObj$mat[, "Sx"]) + 
      sum(dtnorm(c(parObj$theta), c(.fullParObj$theta$priorMean), 
              c(.fullParObj$theta$priorSd), 
              lower = c(.fullParObj$theta$low), log = TRUE)) +
      sum(dnorm(parObj$gamma, .fullParObj$gamma$priorMean, 
              .fullParObj$gamma$priorSd, log = TRUE))
  return(parPost)
}

.CalcPostLambda <- function(parObj, ...) UseMethod(".CalcPostLambda") 

.CalcPostLambda.lambda <- function(parObj, postObj, ageObj, ind) {
  postObj$mat[ind, "lx"] <- 
      log(parObj$lambda) * ageObj$ages[ind, "indJu"] - 
      parObj$lambda * ageObj$ages[ind, "ageJu"] + 
      parObj$lambda * ageObj$ages[ind, "ageJuTr"]
  postObj$lambda <- sum(postObj$mat[, "lx"]) + 
      dtnorm(parObj$lambda, mean = .fullParObj$lambda$priorMean, 
          sd = .fullParObj$lambda$priorSd, lower = 0)
  return(postObj)
}

.CalcPostLambda.noLambda <- function(parObj, postObj, ...) {
  return(postObj)
}


.CalcPostPi <- function(parObj, ...) UseMethod(".CalcPostPi")

.CalcPostPi.pi <- function(parObj, postObj, ageObj, ind) {
  postObj$mat[ind, "px"] <- 
      (ageObj$alive - .dataObj$obsMat)[ind, ] %*% 
      log(1 - parObj$pi[.fullParObj$pi$idpi])
  return(postObj)
}

.CalcPostPi.noPi <- function(parObj, postObj, ...) {
  return(postObj)
}

.SumPosts <- function(postObj, ind) {
  postObj$mat[ind, "postX"] <- 
      postObj$mat[ind, c("fx", "vx", "lx", "px")] %*% rep(1, 4)
  return(postObj)
}

.CalcPostX <- function(ageObj, parObj, postObj, parCovObj, ind) {
  postObj <- .CalcLike(ageObj, parObj, postObj, parCovObj, ind)
  postObj <- .CalcPostPi(parObj, postObj, ageObj, ind)
  postObj <- .CalcPostLambda(parObj, postObj, ageObj, ind)  
  postObj$mat[ind, "vx"] <- 
      .CalcPriorAgeDist(ageObj, ind)
  postObj <- .SumPosts(postObj, ind)
  return(postObj)
}

# Prior age distribution
.SetPriorAgeDist <- function(ageObj) {
  priorMean <- .fullParObj$theta$priorMean[1, ]
  priorSd <- .fullParObj$theta$priorSd[1, ]
  low <- .fullParObj$theta$low[1, ]
  maxAge <- max(ageObj$ages[, "age"]) * 5
  xvdisc <- 0:maxAge
  nPriors <- ncol(.fullParObj$theta$priorMean)
  priorRange <- lapply(1:nPriors, function(th) 
        seq(qtnorm(0.0001, mean = priorMean[th], sd = priorSd[th], 
                lower = low[th]),
            qtnorm(0.9999, mean = priorMean[th], sd = priorSd[th], 
                lower = low[th]), length = 10))
  names(priorRange) <- names(priorMean)
  priorDens <- lapply(1:nPriors, function(th) dtnorm(priorRange[[th]], 
            mean = priorMean[th], sd = priorSd[th], lower = low[th]))
  Dprior <- sapply(1:nPriors, function(th) priorRange[[th]][2] -
            priorRange[[th]][1])
  names(Dprior) <- names(priorMean)
  names(priorDens) <- names(priorMean)
  priorCombs <- as.matrix(expand.grid(priorRange))
  priorDens <- expand.grid(priorDens)
  xMat <- matrix(xvdisc, nrow(priorCombs), length(xvdisc), byrow = TRUE)
  if (.algObj$shape != "simple") {
    nPriorComb <- priorCombs
    nPriorComb[, "c"] <- 0
    lowsC <- - apply(.CalcMort(xMat, nPriorComb), 1, min)
    idC <- which(priorCombs[, "c"] < lowsC)
    priorCombs[idC, "c"] <- lowsC[idC]
  }
  
  xv   <- seq(0, maxAge, length = 200)
  dx <- xv[2] - xv[1]
  lifeExpec <- c(sapply(xv, function(xx) .CalcSurv(xx, priorCombs) * dx) %*% 
          rep(1, length(xv)))
  .priorAgeDist <- apply(.CalcSurv(xMat, priorCombs) / lifeExpec * 
          apply(priorDens, 1, prod) * prod(Dprior), 2, sum) 
  .priorAgeDist <- log(.priorAgeDist / sum(.priorAgeDist))
  names(.priorAgeDist) <- xvdisc
  return(.priorAgeDist)
}

.CalcPriorAgeDist <- function(ageObj, ...) UseMethod(".CalcPriorAgeDist")

.CalcPriorAgeDist.minAge <- function(ageObj, ind) {
  .priorAgeDist[ageObj$ages[ind, "ageAd"] + 1] * ageObj$ages[ind, "indAd"]
}

.CalcPriorAgeDist.noMinAge <- function(ageObj, ind) {
  .priorAgeDist[ageObj$ages[ind, "age"] + 1]
}

# Fill in outObj$par:
.FillParMat <- function(parObj) {
  parVec <- c(parObj$theta)
  if (class(parObj)[1] == "theGam") {
    parVec <- c(parVec, parObj$gamma)
  }
  if (class(parObj)[3] == "pi") {
    parVec <- c(parVec, parObj$pi)
  }
  return(parVec)
}

# MCMC function:
.RunBastaMCMC <- function(sim) {
  rm(".Random.seed", envir = .GlobalEnv)
  runif(1); assign(".Random.seed", .Random.seed, envir = .GlobalEnv)
  agesNow <- .agesIni
  parsNow <- .JitterPars(.parsIni, .agesIni, .postIni, .parsCovIni)
  parsCovNow <- .CalcParCovObj(.covObj, parsNow, .parsCovIni)
  postNow <- .CalcLike(agesNow, parsNow, .postIni, parsCovNow, 1:.dataObj$n)
  niter <- .algObj$niter
  burnin <- .algObj$burnin
  thinning <- .algObj$thinning
  outObj <- list()
  outObj$par <- matrix(0, niter, length(.fullParObj$allNames), 
      dimnames = list(NULL, .fullParObj$allNames))
  outObj$par[1, ] <- .FillParMat(parsNow)
  outObj$post <- rep(0, niter)
  outObj$post[1] <- sum(postNow$mat[, 'fx'] - postNow$mat[, 'Sx'] + 
          postNow$mat[, 'px'])
  thinSeq <- seq(burnin, niter, thinning)
  lenThin <- length(thinSeq)
  if (class(.dataObj) == "ageUpd") {
    if (length(.dataObj$idNoB) > 0) {
      outObj$birth <- matrix(NA, 0, length(.dataObj$idNoB))
    }
    if (length(.dataObj$idNoD) > 0) {
      outObj$death <- matrix(NA, 0, length(.dataObj$idNoD))
    }
  }
  idMp <- which(substr(.fullParObj$allNames, 1, 2) != "pi")
  nMp <- length(idMp)
  op <- options()
  options(warn = -1)
  for (m in 2:niter) {
    # Sample theta (and gamma) parameters:
    for (mp in 1:nMp) {
      parsNew <- .ProposeMortPars(parsNow, agesNow, .jumps, mp)
      parsCovNew <- .CalcParCovObj(.covObj, parsNew, parsCovNow)
      postNew <- .CalcLike(agesNow, parsNew, postNow, parsCovNew, 1:.dataObj$n)
      if (!is.na(postNew$mortPars)) {
        acceptCrit <- exp(postNew$mortPars - postNow$mortPars)
        acceptProb <- runif(1)
        if (acceptCrit > acceptProb) {
          parsNow <- parsNew
          parsCovNow <- parsCovNew
          postNow <- postNew
        }
      }
    }
    outObj$par[m, ] <- .FillParMat(parsNow)
    
    # Sample recapture parameters:
    parsNow <- .SamplePiPars(parsNow, agesNow)
    postNow <- .CalcPostPi(parsNow, postNow, agesNow, 1:.dataObj$n)
    
    # Sample early age parameter:
    if (class(parsNow)[2] == "lambda") {
      parsNew <- .ProposeLambda(parsNow)
      postNew <- .CalcPostLambda(parsNew, postNow, agesNow, 1:.dataObj$n)
      acceptCrit <- exp(postNew$lambda - postNow$lambda)
      acceptProb <- runif(1)
      if (acceptCrit > acceptProb) {
        parsNow <- parsNew
        postNow <- postNew
      }
    }
    
    # Sum up columns:
    postNow <- .SumPosts(postNow, 1:.dataObj$n)
    
    # Sample Missing ages at death:
    if (class(.dataObj) == "ageUpd") {
      agesNew <- .SampleAges(agesNow)
      idNew <- which(agesNew$ages[, 'birth'] != agesNow$ages[, 'birth'] | 
              agesNew$ages[, 'death'] != agesNow$ages[, 'death'])
      postNew <- .CalcPostX(agesNew, parsNow, postNow, parsCovNow, 
          .dataObj$idNoA) 
      idAccept <- .AcceptAges(postNow, postNew, idNew)
      postNow$mat[idAccept, ] <- postNew$mat[idAccept, ]
      agesNow$ages[idAccept, ] <- agesNew$ages[idAccept, ]
      agesNow$alive[idAccept, ] <- agesNew$alive[idAccept, ]
      postNow$mortPars <- .CalcPostMortPars(parsNow, postNow)
      if (class(parsNow)[2] == "lambda") {
        postNow$lambda <- sum(postNow$mat[, "lx"]) + 
            dtnorm(parsNow$lambda, mean = .fullParObj$lambda$priorMean, 
                sd = .fullParObj$lambda$priorSd, lower = 0)
      }
      if (m %in% thinSeq) {
        if (length(.dataObj$idNoB) > 0) {
          outObj$birth <- rbind(outObj$birth, 
              agesNow$ages[.dataObj$idNoB, "birth"])
        }
        if (length(.dataObj$idNoD) > 0) {
          outObj$death <- rbind(outObj$death, 
              agesNow$ages[.dataObj$idNoD, "death"])
        }
      }
    }
    outObj$post[m] <- sum(postNow$mat[, 'fx'] - postNow$mat[, 'Sx'] + 
            postNow$mat[, 'px'])
  }
  options(op)
  return(outObj)
}

# Jump udate routine.
# 3.6 Function to update jumps:

.UpdateJumps <- function(jumpObj, updObj, step) {
  # Based on Roberts & Rosenthal (2009).
  pNum <- (step - updObj$int[1]) / updObj$len
  jumpObj$updateRate <- 
      apply(matrix(updObj$updVec[step + c(-(updObj$len - 1):0), ], 
              ncol = length(jumpObj$jump)), 2, function(upd) sum(upd)) / 
      updObj$len
#  jumpObj$updateRate[jumpObj$updateRate == 0] <- 1e-2
  delt <- min(0.01, pNum^(-1/2))
  idLow <- which(jumpObj$updateRate < updObj$targ)
  jumpObj$jump[idLow] <- jumpObj$jump[idLow] * exp(-delt)
  jumpObj$jump[-idLow] <- jumpObj$jump[-idLow] * exp(delt)
  jumpObj$jump <- jumpObj$jump * exp(jumpObj$updateRate - updObj$targ)
#  jumpObj$jump <- jumpObj$jump * jumpObj$updateRate / updObj$targ
  jumpObj$jumpsMat <- rbind(jumpObj$jumpsMat, jumpObj$jump)
  if (all(jumpObj$updateRate > updObj$targ * 0.9) &
      all(jumpObj$updateRate < updObj$targ * 1.1)) {
    jumpObj$update <- FALSE
  }
  return(jumpObj)
}


.PrepJumpObj <- function(.fullParObj, .covObj) {
  jump <- c(.fullParObj$theta$jump)
  if (.fullParObj$class[1] == "theGam") {
    jump <- c(jump, .fullParObj$gamma$jump)
  }
  nPar <- length(jump)
  return(list(jump = jump, jumpsMat = matrix(NA, 0, nPar), 
          update = TRUE, updateRate = rep(0, nPar)))
}


.RunIniUpdJump <- function() {
  cat("Starting simulation to find jump sd's... ")
  jumpObj <- .jumpObjIni
  parsNow <- .parsIni
  agesNow <- .agesIni
  parsCovNow <- .parsCovIni
  postNow <- .postIni
  newJumps <- list()
  newJumps$theta <- .fullParObj$theta$jump
  if (class(.parsIni)[1] == "theGam") {
    newJumps$gamma <- .fullParObj$gamma$jump
  }
  idMp <- which(substr(.fullParObj$allNames, 1, 2) != "pi")
  idGam <- which(substr(.fullParObj$allNames, 1, 2) == "ga")
  nMp <- length(idMp)
  updObj <- list(len = 50, targ = 0.25)
  niter <- updObj$len * 125
  updObj$int <- seq(updObj$len, niter, updObj$len) 
  updObj$updVec <- matrix(0, niter, nMp)
  op <- options()
  options(warn = -1)
  for (m in 1:niter) {
    # Sample theta (and gamma) parameters:
    for (mp in 1:nMp) {
      parsNew <- .ProposeMortPars(parsNow, agesNow, newJumps, mp)
      parsCovNew <- .CalcParCovObj(.covObj, parsNew, parsCovNow)
      postNew <- .CalcLike(agesNow, parsNew, postNow, parsCovNew, 1:.dataObj$n)
      if (!is.na(postNew$mortPars)) {
        acceptCrit <- exp(postNew$mortPars - postNow$mortPars)
        acceptProb <- runif(1)
        if (acceptCrit > acceptProb) {
          parsNow <- parsNew
          parsCovNow <- parsCovNew
          postNow <- postNew
          updObj$updVec[m, mp] <- 1
        }
      }
    }
    
    # Sample recapture parameters:
    parsNow <- .SamplePiPars(parsNow, agesNow)
    postNow <- .CalcPostPi(parsNow, postNow, agesNow, 1:.dataObj$n)
    
    # Sample early age parameter:
    if (class(parsNow)[2] == "lambda") {
      parsNew <- .ProposeLambda(parsNow)
      postNew <- .CalcPostLambda(parsNew, postNow, agesNow, 1:.dataObj$n)
      acceptCrit <- exp(postNew$lambda - postNow$lambda)
      acceptProb <- runif(1)
      if (acceptCrit > acceptProb) {
        parsNow <- parsNew
        postNow <- postNew
      }
    }
    
    # Sum up columns:
    postNow <- .SumPosts(postNow, 1:.dataObj$n)
    
    # Sample Missing ages at death:
    if (class(.dataObj) == "ageUpd") {
      agesNew <- .SampleAges(agesNow)
      idNew <- which(agesNew$ages[, 'birth'] != agesNow$ages[, 'birth'] | 
              agesNew$ages[, 'death'] != agesNow$ages[, 'death'])
      postNew <- .CalcPostX(agesNew, parsNow, postNow, parsCovNow, 
          .dataObj$idNoA) 
      idAccept <- .AcceptAges(postNow, postNew, idNew)
      postNow$mat[idAccept, ] <- postNew$mat[idAccept, ]
      agesNow$ages[idAccept, ] <- agesNew$ages[idAccept, ]
      agesNow$alive[idAccept, ] <- agesNew$alive[idAccept, ]
      postNow$mortPars <- .CalcPostMortPars(parsNow, postNow)
      if (class(parsNow)[2] == "lambda") {
        postNow$lambda <- sum(postNow$mat[, "lx"]) + 
            dtnorm(parsNow$lambda, mean = .fullParObj$lambda$priorMean, 
                sd = .fullParObj$lambda$priorSd, lower = 0)
      }
    }
    # Update jumps:
    if (m %in% updObj$int) {
      jumpObj <- .UpdateJumps(jumpObj, updObj, m)
      newJumps$theta[1:.fullParObj$theta$len] <- 
          jumpObj$jump[1:.fullParObj$theta$len]
      if (class(parsNow)[1] == "theGam") {
        newJumps$gamma <- jumpObj$jump[-c(1:.fullParObj$theta$len)]
      }
    }
  }
  options(op)
  aveJumps <- apply(matrix(jumpObj$jumpsMat[nrow(jumpObj$jumpsMat) -
                  c(100:0), ], ncol = ncol(jumpObj$jumpsMat)), 2, mean)
  newJumps$theta[1:.fullParObj$theta$len] <- aveJumps[1:.fullParObj$theta$len]
  if (class(.parsIni)[1] == "thegam") {
    newJumps$gamma <- avejumps[idGam]
  }
  cat(" done.\n\n")
  return(list(updJumps = newJumps, jObject = jumpObj, m = m))
}


# Diagnostics:
.CalcDiagnost <- function(bastaOut) {
  thinned <- seq(.algObj$burnin, .algObj$niter, .algObj$thinning)
  nthin <- length(thinned)
  parMat <- bastaOut[[1]]$par[thinned, ]
  fullParMat <- bastaOut[[1]]$par[.algObj$burnin:.algObj$niter, ]
  posterior <- bastaOut[[1]]$post[thinned]
  if (.algObj$nsim > 1) {
    for (ii in 2:.algObj$nsim) {
      parMat <- rbind(parMat, bastaOut[[ii]]$par[thinned, ])
      fullParMat <- rbind(fullParMat, 
          bastaOut[[ii]]$par[.algObj$burnin:.algObj$niter, ])
      posterior <- c(posterior, bastaOut[[ii]]$post[thinned])
    }
  }
  coef <- cbind(apply(parMat, 2, mean, na.rm=TRUE), apply(parMat, 2, 
          sd, na.rm=TRUE), t(apply(parMat, 2, quantile, c(0.025, 0.975), 
              na.rm = TRUE)), 
      apply(parMat, 2, 
          function(x) cor(x[-1], x[-length(x)], use = "complete.obs")), 
      apply(fullParMat, 2, function(x) 
            length(which(diff(x) != 0)) / (length(x) -1)))
  colnames(coef) <- c("Estimate", "StdErr", "Lower95%CI", "Upper95%CI", 
      "SerAutocor", "UpdateRate")
  
  # Calculate convergence if more than 1 simulations were run:
  if (.algObj$nsim > 1) {
    idSims <- rep(1:.algObj$nsim, each = nthin)
    Means <- apply(parMat, 2, function(x) 
          tapply(x, idSims, mean))
    Vars <- apply(parMat, 2, function(x) 
          tapply(x, idSims, var))
    meanall <- apply(Means, 2, mean)
    B <- nthin / (.algObj$nsim - 1) * apply(t((t(Means) - meanall)^2), 2, sum)
    W <- 1 / .algObj$nsim * apply(Vars, 2, sum)
    Varpl <- (nthin - 1) / nthin * W + 1 / nthin * B
    Rhat <- sqrt(Varpl / W)
    Rhat[Varpl==0] <- 1
    conv <- cbind(B, W, Varpl, Rhat)
    rownames(conv) <- colnames(parMat)
    coef <- cbind(coef, conv[, 'Rhat'])
    colnames(coef) <- c(colnames(coef)[-ncol(coef)], "PotScaleReduc")
    idnconv <- which(conv[, 'Rhat'] > 1.1)
    
    # Calculate DIC if all parameters converged.
    if (length(idnconv) == 0) {
      L <- length(posterior)
      Dm <- -2 * posterior
      Dmode <- -2 * posterior[which(posterior ==  max(posterior))[1]]
      Dave <- mean(Dm)
      pD <- Dave - Dmode
      k <- ncol(parMat)
      DIC <- 2 * Dave - Dmode
      modSel <- c(Dave, Dmode, pD, k, DIC)
      names(modSel) <- c("D.ave", "D.mode", "pD", "k", "DIC")
      cat("Survival parameters converged appropriately.",
          "\nDIC was calculated.\n")
      kulLeib <- .CalcKulbackLeibler(coef)
    } else {
      warning("Convergence not reached for some survival parameters.",
          "\nDIC could not be calculated.\n", call. = FALSE)
      modSel <- "Not calculated"
      kulLeib <- "Not calculated"
    }
  } else {
    conv <- "Not calculated"
    modSel <- "Not calculated"
    kulLeib <- "Not calculated"
  }
  diagObj <- list(coefficients = coef, DIC = modSel, convergence = conv, 
      KullbackLeibler = kulLeib, params = parMat)
  return(diagObj)
}

.CalcKulbackLeibler <- function(coef) {
  if (!is.null(.covObj$cat) & 
      !(length(.covObj$cat) == 2 & class(.covObj)[1] == "propHaz")) {
    if (length(.covObj$cat) > 1) {
      if (class(.covObj)[1] %in% c("fused", "inMort")) {
        parNames <- .defTheta$name
        nPar <- .defTheta$length
        low <- .defTheta$low
        nCat <- length(.covObj$cat)
        namesCat <- names(.covObj$cat)
      } else {
        parNames <- "gamma"
        nCat <- length(.covObj$cat) - 1
        namesCat <- names(.covObj$cat)[-1]
        nPar <- 1
        low <- -Inf
      }
      nComb <- (nCat - 1)^2 - ((nCat - 1)^2 - (nCat - 1)) / 2
      covComb1 <- c()
      covComb2 <- c()
      klMat1 <- matrix(0, nPar, nComb, dimnames = list(parNames, NULL))
      klMat2 <- klMat1
      comb <- 0
      for (i in 1:nCat) {
        for (j in 1:nCat) {
          if (i > j) {
            comb <- comb + 1
            covComb1 <- c(covComb1, 
                sprintf("%s - %s", namesCat[i], namesCat[j]))
            covComb2 <- c(covComb2, 
                sprintf("%s - %s", namesCat[j], namesCat[i]))
            for (p in 1:nPar) {
              idP <- sapply(c(i, j), function(ij) 
                    which(.fullParObj$allNames == 
                            sprintf("%s.%s", parNames[p], namesCat[ij])))
              parRan <- range(sapply(1:2, function(pp) qtnorm(c(0.001, 0.999), 
                            coef[idP[pp], 1], coef[idP[pp], 2], 
                            lower = low[p])))
              parVec <- seq(parRan[1], parRan[2], length = 100)
              dp <- parVec[2] - parVec[1]
              parDens <- sapply(1:2, function(pp) 
                    dtnorm(seq(parRan[1], parRan[2], length = 100), 
                        coef[idP[pp], 1], coef[idP[pp], 2], lower = low[p]))
              klMat1[p, comb] <- sum(parDens[, 1] * 
                      log(parDens[, 1] / parDens[, 2]) * dp)
              klMat2[p, comb] <- sum(parDens[, 2] * 
                      log(parDens[, 2] / parDens[, 1]) * dp)
            }
          }
        }
      }
      colnames(klMat1) <- covComb1
      colnames(klMat2) <- covComb2
      qKlMat1 <- (1 + (1 - exp(-2 * klMat1)^(1 / 2))) / 2
      qKlMat2 <- (1 + (1 - exp(-2 * klMat2)^(1 / 2))) / 2
      mqKl <- (qKlMat1 + qKlMat2) / 2
      outList <- list(kl1 = klMat1, kl2 = klMat2, qkl1 = qKlMat1, 
          qkl2 = qKlMat2, mqKl = mqKl)
    } else {
      outList <- "Not calculated"
    }
  } else {
    outList <- "Not calculated"
  }
  return(outList)
}

.CalcQuants <- function(bastaOut, bastaResults) {
  if (class(.agesIni)[1] == "ageUpd") {
    nthin <- ceiling((.algObj$niter - .algObj$burnin + 1) / .algObj$thinning)
    bMat <- array(matrix(.dataObj$bi, nthin, .dataObj$n, byrow = TRUE), 
        dim = list(nthin, .dataObj$n, .algObj$nsim))
    if (.dataObj$updB) {
      for (sim in 1:.algObj$nsim) {
        bMat[, .dataObj$idNoB, sim] <- bastaOut[[sim]]$birth
      }
    }
    dMat <- array(matrix(.dataObj$di, nthin, .dataObj$n, byrow = TRUE), 
        dim = list(nthin, .dataObj$n, .algObj$nsim))
    if (.dataObj$updD) {
      for (sim in 1:.algObj$nsim) {
        dMat[, .dataObj$idNoD, sim] <- bastaOut[[sim]]$death
      }
    }
    qdMat <- apply(dMat, 2, quantile, c(0.5, 0.025, 0.975))
    qbMat <- apply(bMat, 2, quantile, c(0.5, 0.025, 0.975))
    qxMat <- apply(dMat - bMat, 2, quantile, c(0.5, 0.025, 0.975))
  } else {
    qbMat <- matrix(.dataObj$bi, nrow = 1)
    qdMat <- matrix(.dataObj$di, nrow = 1)
    qxMat <- qdMat - qbMat
  }
  ageRan <- range(qxMat[1, qxMat[1, ] >= .algObj$minAge]) - .algObj$minAge
  ageVec <- seq(0, ageRan[2], length = 100)
  mxq <- Sxq <- list()
  if (is.null(.covObj$cat)) {
    covNames <- c("noCov")
  } else {
    covNames <- paste(".", names(.covObj$cat), sep = "")
  }
  for (cov in covNames) {
    # Set theta parameters:
    if (class(.covObj)[1] %in% c('noCov', 'propHaz') | 
        is.null(.covObj$cat)) {
      thPars <- matrix(bastaResults$params[, .defTheta$name], 
          ncol = .defTheta$length)
    } else if (class(.covObj)[1] == "fused") {
      idTh <- grep(cov, .fullParObj$allNames, fixed = TRUE)
      thPars <- matrix(bastaResults$params[, idTh], 
          ncol = length(idTh))
    } else {
      idTh <- grep(cov, .fullParObj$allNames, fixed = TRUE)
      thPars <- matrix(bastaResults$params[, idTh], 
          ncol = length(idTh))
      if (!is.null(.covObj$cont)) {
        for (pp in .covObj$cont) {
          idCon <- which(substr(.fullParObj$allNames, 
                  nchar(.fullParObj$allNames) - (nchar(pp) + 1), 
                  nchar(.fullParObj$allNames)) == pp)
          thPars <- thPars + 
              matrix(bastaResults$params[, idCon], ncol = length(idCon)) *
              mean(.covObj$inMort[, .covObj$cont[pp]])
        }
      }
    }
    colnames(thPars) <- .defTheta$name
    # Set gamma parameters:
    if (class(.covObj)[1] %in% c('noCov', 'inMort')) {
      gamPar <- rep(0, nrow(bastaResults$params))
    } else if (class(.covObj)[1] == "fused") {
      idGam <- grep("gamma", .fullParObj$allNames)
      gamPar <- matrix(bastaResults$params[, idGam], ncol = length(idGam)) %*% 
          c(apply(.covObj$propHaz, 2, mean, na.rm = TRUE))
    } else {
      if (cov == sprintf(".%s", names(.covObj$cat)[1]) | 
          is.null(.covObj$cat)) {
        gamPar <- rep(0, nrow(bastaResults$params))
      } else {
        gamPar <- bastaResults$params[, grep(cov, .fullParObj$allNames, 
                fixed = TRUE)]
      }
      if (!is.null(.covObj$cont)) {
        idGam <- grep("gamma", .fullParObj$allNames)
        idCon <- idGam[which(substr(.fullParObj$allNames[idGam], 7, 
                        nchar(.fullParObj$allNames[idGam])) %in% 
                    names(.covObj$cont))]
        gamPar <- gamPar + t(t(as.matrix(bastaResults$params[, idCon])) * 
                apply(as.matrix(.covObj$propHaz[, names(.covObj$cont)]), 2, 
                    mean, na.rm = TRUE))
      }
    }
    mxq[[cov]] <- apply(sapply(1:nrow(thPars), function(pp) 
              .CalcMort(ageVec, t(thPars[pp, ])) * exp(gamPar[pp])), 1, 
        quantile, c(0.5, 0.025, 0.975))
    colnames(mxq[[cov]]) <- ageVec + .algObj$minAge
    Sxq[[cov]] <- apply(sapply(1:nrow(thPars), function(pp) 
              .CalcSurv(ageVec, t(thPars[pp, ]))^exp(gamPar[pp])), 1, 
        quantile, c(0.5, 0.025, 0.975))
    colnames(Sxq[[cov]]) <- ageVec + .algObj$minAge
  }
  bastaResults$birthQuant <- qbMat
  bastaResults$deathQuant <- qdMat
  bastaResults$agesQuant <- qxMat
  bastaResults$mortQuant <- mxq
  bastaResults$survQuant <- Sxq
  return(bastaResults)
}

.CalcLifeTable <- function(bastaResults, lifeTable, object) {
  if (lifeTable) {
    LT  <- list()
    if (is.null(.covObj$cat)) {
      covNames <- c("noCov")
    } else {
      covNames <- names(.covObj$cat)
    }
    for (cov in covNames) {
      if (cov == "noCov") {
        x <- bastaResults$agesQuant[1, 
            bastaResults$birthQuant[1, ] >= .algObj$start]
      } else {
        x <- bastaResults$agesQuant[1, object[, cov] == 1 & 
                bastaResults$birthQuant[1, ] >= .algObj$start]
      }
      tempLT <- MakeLifeTable(x, ax = 0.5, n = 1)
      tempLT <- subset(tempLT, tempLT$StartAge >= .algObj$minAge)
      rownames(tempLT) <- NULL
      LT[[cov]] <- tempLT
    }
  } else {
    LT <- NULL
  }
  return(LT)
}

