PMXRenv::activate.unqualified.packages()
test_that("getFiles work", {

  expect_error(getFileNames(runno=NULL,modName=NULL))
  expect_error(getFileNames(runno=9,modDevDir = NULL))

  fileList1 <- getFileNames(30)
  fileList2 <- getFileNames("run30")
  fileList3 <- getFileNames(30,modDevDir=system.file("extdata/SimNeb/", package = "PMXFrem"))
  fileList4 <- getFileNames(30,modDevDir=system.file("extdata/SimNeb/", package = "PMXFrem"),modExt=".ctl",lstExt=".out")

  testFun <- function(fileList) {
    expect_equal(length(fileList),5)
    expect_equal(c("mod","ext","phi","lst","cov"), names(fileList))
  }

  testFun(fileList1)
  testFun(fileList2)
  testFun(fileList3)
  testFun(fileList4)
  })
