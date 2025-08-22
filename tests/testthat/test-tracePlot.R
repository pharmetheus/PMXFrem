test_that("traceplot works", {

  if (compareVersion(paste0(R.version$major,".",R.version$minor),"4.2.2") > 0) {
    library(vdiffr)
  } else {
    PMXRenv::library.unqualified("vdiffr")
  }

  retList3 <- traceplot(30,modDevDir=system.file("extdata","SimNeb/",package="PMXFrem"))
  expect_type(retList3,"list")
  expect_equal(length(retList3),3)

  retList2 <- traceplot(30,modDevDir=system.file("extdata","SimNeb/",package="PMXFrem"),
                       includeOFV = FALSE )
  expect_type(retList2,"list")
  expect_equal(length(retList2),2)

  retList1 <- traceplot(30,modDevDir=system.file("extdata","SimNeb/",package="PMXFrem"),
                        includeOmega = FALSE,includeOFV = FALSE)
  expect_type(retList1,"list")
  expect_equal(length(retList1),1)


  vdiffr::expect_doppelganger("OFV", retList3[[1]])
  vdiffr::expect_doppelganger("Thetas", retList3[[2]])
  vdiffr::expect_doppelganger("Omegas", retList3[[3]])


  retListX <- traceplot(30,modDevDir=system.file("extdata","SimNeb/",package="PMXFrem"),thetaNum=c(2,3),
            includeOFV = FALSE,includeOmega = FALSE)

  vdiffr::expect_doppelganger("Only thetas 2 and 3", retListX[[1]])

})
