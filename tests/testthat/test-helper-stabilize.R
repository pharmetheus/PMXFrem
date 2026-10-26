## replaceTempPath() is what keeps a random temporary directory out of a
## snapshot, on every platform.

test_that("a temporary directory is replaced however the path was written", {
  td <- "C:\\Users\\someone\\AppData\\Local\\Temp\\Rtmpabc\\file123"
  expect_equal(
    replaceTempPath(paste0("$DATA ", td, "/x.csv IGNORE=@"), td),
    "$DATA [placeholder_path]/x.csv IGNORE=@"
  )
  ## a path written with forward slashes
  expect_equal(
    replaceTempPath("$DATA C:/Users/someone/AppData/Local/Temp/Rtmpabc/file123/x.csv", td),
    "$DATA [placeholder_path]/x.csv"
  )
  ## the form createFFEMmodel() writes on Windows, backslashes consumed
  expect_equal(
    replaceTempPath("$DATA C:UserssomeoneAppDataLocalTempRtmpabcfile123/x.csv", td),
    "$DATA [placeholder_path]/x.csv"
  )
  ## a posix path is unchanged by the other two forms
  expect_equal(
    replaceTempPath("$DATA /tmp/Rtmpabc/file123/x.csv", "/tmp/Rtmpabc/file123"),
    "$DATA [placeholder_path]/x.csv"
  )
  expect_equal(replaceTempPath("no path here", td), "no path here")
})
