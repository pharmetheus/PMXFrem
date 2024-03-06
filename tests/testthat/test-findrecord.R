test_that("findrecord works", {

  basemodel      <- system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")

  expect_snapshot(findrecord(basemodel,record="\\$PROBLEM",replace="$PROBLEM FFEM model",quiet=T))

  expect_snapshot(findrecord(basemodel,record="\\$INPUT",quiet=T))


})
