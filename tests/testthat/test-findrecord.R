test_that("findrecord works", {

  basemodel      <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")

  expect_snapshot_value(
    stabilize(findrecord(basemodel, record = "\\$PROBLEM", replace = "$PROBLEM FFEM model", quiet = T)),
    style = "serialize"
  )

  expect_snapshot_value(
    stabilize(findrecord(basemodel, record = "\\$INPUT", quiet = T)),
    style = "serialize"
  )

})
