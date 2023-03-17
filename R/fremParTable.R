#' fremParTable
#'
#' @description Create a parameter table for frem models
#' @inheritParams calcFFEM
#' @param runno The run number of the FREM model.
#' @param modDevDir The path to the directory where the NONMEM run files are.
#' @param thetaNum The numbers of the thetas that should appear in the parameter table.
#' @param omegaNum The numbers of the omegas that should appear in the parameter table.
#' @param sigmaNum The numbers of the sogmas that should appear in the parameter table.
#' @param extFile The path to the ext file of the FREM model.
#' @param model.name The name the model should have in the parameter table. (see model.name in PhRame::NM.parameter.table)
#' @param ignore.list A list of parameters that should not appear in the parameter table. (see ignore in PhRame::NM.parameter.table)
#' @param parnames The latex names of the parameters as they should appear in the table. (see print.names PhRame::NM.parameter.table)
#' @param unitnames The latex units as the should appear in the parameter table. (see unit.names PhRame::NM.parameter.table).
#' @param includeRSE Should RSEs be included in the parameter table (TRUE/FALSE).(The reverse of no.rse in PhRame::NM.parameter.table.
#' @param rseFile The path to a PsN bootstrap raw_results file or the .cov file from the FREM run. The former is u
#' @param n The number of samples from the variance-covariance matrix derived from the .cov or bootstrap raw_results file to base the
#' estimate of standard error on.
#' @param returnParTable Should the function return the data.frame with parameter estimate information instead of printing the latex code for the
#' table.
#' @param parTable A data.frame to be used as input to PhRame::NM.parameter.table.latex.
#'
#' @details The function uses the NM.Parameter.table and NM.Parameter.table.latex to parse the NONMEM run information and to print the latex code for the table.
#' The typical use is to take the output from a FREM model, which contains parameter estimates for all the covariates and the variances and covariances of the OMEGA BLOCK of
#' parameter etas and covariate etas, and reduce it down to the THETA estimates of the parameters in the base model and the diagonal omegas related to these. The omegas can either
#' be part of the FREM model specification or not. The omegas reported in the parameter table are either, in case the omega is not part of the FREM model specificaiton,
#' the omegas as reported in the NONMEM output or, in the case the omega is part of the NONMEM specification, adjusted for the covariates that are part of the model ("omega prim").
#'
#' The RSEs are derived numerically by sampling from the variance-covariance matrix that is either as specified in the .cov file from NONMEM or the values in a PsN bootstrap raw_results file. The reason for
#' the numerical approach is that the SEs reported by NONMEM is for the omegas in the model and not for the omega prims.
#'
#' @return Either printed latex code for the parameter table or a data frame suitable for PhRame::NM.Parameter.table.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' parnames <- c("OFV","Condition number","\\glstext{CL}", "\\glstext{V}",
#'               "\\glstext{MAT}","\\glstext{MATFR}", "\\Food on \\glstext{FREL}","\\Food on \\glstext{MAT}",
#'               "\\glstext{IIV} \\glstext{RUV}", "\\glstext{IIV} \\glstext{FREL}", "\\glstext{IIV} \\glstext{CL}",
#'               "\\glstext{IIV} \\glstext{V}", "\\glstext{IIV} \\glstext{MAT}","\\glstext{RUV}")
#'
#' unitnames  <- c("","","(L/h)","(L)","(h)", "","","","(CV)","(CV)","(CV)","(CV)","(CV)","(CV)")
#'
#' thetaNum <- 4:9              # Thetas to appear in parameter table
#' omegaNum <- c(1,3:6)         # Omegas to appear in the parameter table
#' sigmaNum <- 1                # Sigmas to appear in the parameter table
#'
#' myParTableFinal <- fremParTable(finalRun,
#'                                 modDevDir,
#'                                 thetaNum         = thetaNum,
#'                                 omegaNum         = omegaNum,
#'                                 sigmaNum         = sigmaNum,
#'                                 numNonFREMThetas = EPtwoInfo$numNonFREMThetas,
#'                                 numSkipOm        = EPtwoInfo$numSkipOm,
#'                                 extFile          = extFile,
#'                                 model.name       = "Final model",
#'                                 ignore.list      = ("Condition number"),
#'                                 parnames         = parnames2,
#'                                 unitnames        = unitnames2,
#'                                 includeRSE       = FALSE,
#'                                 rseFile          = bsFile,
#'                                 returnParTable   = TRUE
#' )
#' }
fremParTable <- function(runno,
                         modDevDir,
                         thetaNum,
                         omegaNum,
                         sigmaNum,
                         numNonFREMThetas,
                         numSkipOm,
                         extFile,
                         model.name  = NULL,
                         ignore.list = NULL,
                         parnames    = NULL,
                         unitnames   = NULL,
                         includeRSE  = FALSE,
                         rseFile     = NULL,
                         n           = 175,
                         returnParTable = FALSE,
                         parTable = NULL) {

  ## runno: The number of the FREM run
  ## parTable: data.frame with data that is suitable for NM.Parameter.table.latex
  ## modeDevDir: The directory where the runfiles are
  ## thetaNum: The numbers of the thetas to appear in the table
  ## omegaNum: The number of diagonal omegas to appear in the table
  ## sigmaNum: The number of diagonal sigmas to appear in the table
  ## numNonFREMThetas: The number of non FREM tehtas in the model
  ## numSkipOm: The number of skipped omegas in the FREM model
  ## extFile: The ext fiel for the FREM model
  ## model.name: The name of the model (NM.parameter.table)
  ## ignore.list: The parameters to ignore (NM.parameter table argument)
  ## parnames: Vector of parameter names (latex syntax) as long as the number of entries in the eventual parameter table
  ## unitnames: Vector of unit names (latex syntax) as long as the number of entries in the eventual parameter table
  ## includeRESE: Should RSEs be included in the parameter table
  ## rseFile: Either the .cov file from the FREM run or a bootstrap raw results file
  ## n: The number of samples two draw from the rseFile
  ## returnParTable: Should the parTable data.frame be returnd instead of the latex table code

  if(!is.null(parTable)) {
    if(!is.data.frame(parTable)) stop("parTable has to be a data.frame")
  } else {

    ## Create the "template parameter table
    finMod <- NM.Parameter.table(runno,
                                 ModelDir   = paste0(modDevDir,"/"),
                                 as.latex   = FALSE,
                                 no.shr     = TRUE,
                                 no.rse     = !includeRSE,
                                 model.name = model.name,
                                 ignore     = ignore.list)#list("Condition number"))

    ## Select the OFV, Condition number, structural model thetas, structural model diagonal omegas and the structural model sigma.
    parTable <- finMod %>%
      filter( parnames == "OFV" |
                parnames =="Condition number" |
                parnames %in% paste0("TH",thetaNum) |
                parnames %in% paste0("OM",omegaNum,":",omegaNum) | parnames == paste0("SI",sigmaNum,":",sigmaNum)
      )

    # The number of rows in the parameter table
    numTabRows <- length(thetaNum) + length(omegaNum) + length(sigmaNum) +
      2 -                 # Standard entries OFV and Condition number
      length(ignore.list) # Minus any parameters that should be ignores

    ## Process the parameter names
    if(!is.null(parnames)) {
      if(length(parnames)!=numTabRows) stop("The number of parameter names should match the number of entries in the parameter table")
      parTable$parnames <- parnames
    }
    parTable$parnames <- factor(parTable$parnames,levels=parTable$parnames)

    ## Process the unit names
    if(!is.null(unitnames)) {
      if(length(unitnames)!=numTabRows) stop("The number of unit names should match the number of entries in the parameter table")
      parTable$unit.names <- unitnames
    }

    calcSummary <- function(parVector,thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm,quiet = T) {

      ## parVector: The row in the ext file with the final parameter estimates
      ## thetaNum: The numbers of the thetas to appear in the table

      #Find the thetas
      thetaEsts <- parVector %>% select(one_of(paste0("THETA",thetaNum)))

      ## Figure out the number of FREM omegas
      fremOmegas <- diag(calcFFEM(parVector,
                                  numNonFREMThetas = numNonFREMThetas,
                                  numSkipOm = numSkipOm,
                                  quiet        = T)$Vars)

      ## Figure out the non-FREM omegas to extract from the ext information
      extOmegaNum <- head(omegaNum,-length(fremOmegas))
      extOmegas   <- paste0("OMEGA.",extOmegaNum,".",extOmegaNum,".")

      ## Figure out the sigmas to extract from the ext info
      extSigmas   <- paste0("SIGMA.",sigmaNum,".",sigmaNum,".")

      res <- c(
        as.numeric(thetaEsts),
        as.numeric(parVector %>% select(one_of(extOmegas))),
        fremOmegas,
        as.numeric(parVector %>% select(one_of(extSigmas)))
        )

      return(res)
    }

    ## Compute the omega prims and assemble the vector of NM parameter estimates and the omega prims and sigma
    extRes      <- getExt(extFile = extFile,set = NULL) %>% filter(ITERATION==-1000000000)
    fremParEsts <- calcSummary(extRes,thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm)


    ## Replace the parameter estimates in parTable with the fremParEsts
    parTable[parTable$type=="Thetas" |
               parTable$type=="Omegas"|
               parTable$type=="Epsilon","mypar"] <- fremParEsts

    ## Add the RSEs is requested
    if(includeRSE) {

      if(is.null(rseFile)) stop("You need to set rseFile to either a .cov file or a bootstrap raw results file")

      ## Get the uncertainty in the parameters including omega prims. Use a numerical derivation from a bootstrap
      dfSamplesBS <- getSamples(rseFile,extFile=extFile,n=n)
      dfSamplesBS <- cbind(ITER=1,dfSamplesBS)

      # create empty matrix that will contain rses of all parameters to appear in the parameter table
      fremParRses <- data.frame(matrix(rep(NA,nrow(dfSamplesBS)*(length(omegaNum)+length(thetaNum)+1)), ncol = length(omegaNum)+length(thetaNum)+1))

      ## Loop over the samples values and compute the omega prims and add them together with the thetas and sigma
      for(i in (1:nrow(dfSamplesBS))) {
        fremParRses[i,] <- calcSummary(dfSamplesBS[i,],thetaNum,omegaNum,sigmaNum,numNonFREMThetas,numSkipOm)
      }

      ## Compute the RSEs
      sampleMeans <- fremParRses %>% summarise_all(mean)
      sampleSD    <- fremParRses %>% summarise_all(sd)
      sampleRSE   <- abs(100*sampleSD/sampleMeans)

      # Add the RSE information from the bootstrap
      parTable[parTable$type=="Thetas" |
                 parTable$type=="Omegas"|
                 parTable$type=="Epsilon","myrse"] <- as.numeric(sampleRSE)

      ## Compute the condition number from the fremParRses and add it to the parTable
      conditionNumber <- max(eigen(cor(fremParRses))$values)/min(eigen(cor(fremParRses))$values)

      parTable[parTable$parnames=="Condition number","mypar"] <- as.numeric(conditionNumber)
    }
  }

  if(returnParTable) {
    return(parTable)
  } else {
    NM.Parameter.table.latex(parTable,
                             unit.names = parTable$unit.names,
                             no.shr = TRUE,
                             no.rse = !includeRSE,
                             est.digits = 3)
  }
}
