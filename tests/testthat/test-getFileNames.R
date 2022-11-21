test_that("getFiles work", {
  
  expect_error(getFileNames(runno=NULL,modName=NULL))
  expect_error(getFileNames(runno=9,modDevDir = NULL))
  
  fileList1 <- getFileNames(9)
  fileList2 <- getFileNames("run9")
  fileList3 <- getFileNames(9,modDevDir=system.file("extdata/SimVal/", package = "PMXFrem"))
  fileList4 <- getFileNames(9,modDevDir=system.file("extdata/SimVal/", package = "PMXFrem"),modExt=".ctl",lstExt=".out")
  
  testFun <- function(fileList) {
    expect_equal(length(fileList),5)
    expect_equal(c("mod","ext","phi","lst","cov"), names(fileList))
  }
  
  testFun(fileList1)
  testFun(fileList2)
  })
