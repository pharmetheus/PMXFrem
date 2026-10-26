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

## stabilize() must not rewrite what it was not asked to: a helper that does
## makes a snapshot record something the package never produced.

test_that("stabilize() leaves NM-TRAN dot operators intact", {
  lines <- c(
    "IF(FOOD.EQ.1) MATFOOD = 1",
    "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))",
    "IF(SEX.NE.2.AND.WT.GT.70) X = 1",
    "IF(A.LE.1.OR.B.GE.2) Y = 0",
    "IF(C.LT.1) Z = 1"
  )
  expect_equal(stabilize(lines), lines)
})

test_that("stabilize() still reformats numbers next to operators", {
  expect_equal(stabilize("IF(FOOD.EQ.1.50000000001) X = 0.5"), "IF(FOOD.EQ.1.5) X = 0.5")
  expect_equal(stabilize("THETA(1) = .25"), "THETA(1) = 0.25")
  expect_equal(stabilize("$THETA (0,1.123456789012)"), "$THETA (0,1.1234568)")
  expect_equal(stabilize("OM = 1.0E-02"), "OM = 0.01")
  expect_equal(stabilize("IF(C.LT.1.0) Z = 1"), "IF(C.LT.1) Z = 1")
})
