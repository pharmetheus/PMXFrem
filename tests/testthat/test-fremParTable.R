# PMXRenv::activate.unqualified.packages()
test_that("fremParTest works", {
  library(PhRame)
  library(PMXForest)

  set.seed(12312)

  finalRun   <- 31
  modDevDir  <- system.file("extdata/SimNeb/",package = "PMXFrem")
  finalBSrun <- 31
  extFile    <- file.path(modDevDir,paste0("run",finalRun,".ext"))
  covFile    <- file.path(modDevDir,paste0("run",finalRun,".cov"))
  bsFile     <- file.path(modDevDir,paste0("bs",finalBSrun,".dir/raw_results_run",finalBSrun,".csv"))

  numNonFREMThetas <- 7
  numSkipOm        <- 2
  thetaNum         <- 2:7
  omegaNum         <- c(1,3:5)
  sigmaNum         <- 1

  parnames <- c("OFV","Condition number","\\glstext{CL}", "\\glstext{V}",
                "\\glstext{MAT}","\\glstext{MATFR}", "\\Food on \\glstext{FREL}","\\Food on \\glstext{MAT}",
                "\\glstext{IIV} \\glstext{RUV}", "\\glstext{IIV} \\glstext{CL}",
                "\\glstext{IIV} \\glstext{V}", "\\glstext{IIV} \\glstext{MAT}","\\glstext{RUV}")
  unitnames  <- c("","","(L/h)","(L)","(h)", "","","","(CV)","(CV)","(CV)","(CV)","(CV)")

  parnames2 <- c("OFV","\\glstext{CL}", "\\glstext{V}",
                "\\glstext{MAT}","\\glstext{MATFR}", "\\Food on \\glstext{FREL}","\\Food on \\glstext{MAT}",
                "\\glstext{IIV} \\glstext{RUV}", "\\glstext{IIV} \\glstext{CL}",
                "\\glstext{IIV} \\glstext{V}", "\\glstext{IIV} \\glstext{MAT}","\\glstext{RUV}")
  unitnames2  <- c("","(L/h)","(L)","(h)", "","","","(CV)","(CV)","(CV)","(CV)","(CV)")

  myParTableFinal <- fremParTable(finalRun,
                                  modDevDir,
                                  thetaNum         = thetaNum,
                                  omegaNum         = omegaNum,
                                  sigmaNum         = sigmaNum,
                                  numNonFREMThetas = numNonFREMThetas,
                                  numSkipOm        = numSkipOm,
                                  extFile          = extFile,
                                  model.name       = "Final model",
                                  # ignore.list      = ("Condition number"),
                                  parnames         = parnames,
                                  unitnames        = unitnames,
                                  includeRSE       = FALSE,
                                  rseFile          = bsFile,
                                  returnParTable   = TRUE
  )

  expect_snapshot(myParTableFinal)

  myParTableIgnore <- fremParTable(finalRun,
                                   modDevDir,
                                   thetaNum         = thetaNum,
                                   omegaNum         = omegaNum,
                                   sigmaNum         = sigmaNum,
                                   numNonFREMThetas = numNonFREMThetas,
                                   numSkipOm        = numSkipOm,
                                   extFile          = extFile,
                                   model.name       = "Final model",
                                   ignore.list      = ("Condition number"),
                                   parnames         = parnames2,
                                   unitnames        = unitnames2,
                                   includeRSE       = FALSE,
                                   rseFile          = bsFile,
                                   returnParTable   = TRUE
  )

  expect_snapshot(myParTableIgnore)

  myParTableSE <- fremParTable(finalRun,
                                   modDevDir,
                                   thetaNum         = thetaNum,
                                   omegaNum         = omegaNum,
                                   sigmaNum         = sigmaNum,
                                   numNonFREMThetas = numNonFREMThetas,
                                   numSkipOm        = numSkipOm,
                                   extFile          = extFile,
                                   model.name       = "Final model",
                                   ignore.list      = ("Condition number"),
                                   parnames         = parnames2,
                                   unitnames        = unitnames2,
                                   includeRSE       = TRUE,
                                   rseFile          = bsFile,
                                   returnParTable   = TRUE
  )

  expect_snapshot(myParTableSE)

})
