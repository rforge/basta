 citHeader("To cite package 'BaSTA' in publications use:")
     
     ## R >= 2.8.0 passes package metadata to citation().
     if(!exists("meta") || is.null(meta)) meta <- packageDescription("BaSTA")
     year <- sub(".*(2[[:digit:]]{3})-.*", "\\1", meta$Date)
     vers <- paste("R package version", meta$Version)
     journ<- "Methods in Ecology and Evolution"
     
     citEntry(entry="Article",
              title = "BaSTA: an R package for Bayesian estimation of age-specific mortality from incomplete mark-recapture/recovery data with covariates",
              author = personList(as.person("Fernando Colchero"),
                                  as.person("Owen R. Jones"),
                                  as.person("Maren Rebke")),
              year = 2012,
              note = vers,
              journal = journ,

              textVersion =
              paste("Colchero, F., O.R. Jones and M. Rebke (",
                    year,
                    "). BaSTA: an R package for Bayesian estimation of age-specific mortality from incomplete mark-recapture/recovery data with covariates. ", journ, 
                    vers, ".", sep=""))