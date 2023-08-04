test_that("stage_gvt_amd counts government amendments for a single stage", {
  expect_equal(nrow(stage_gvt_amd(3155, 16533)),
    13)
})

test_that("get_total_gvt_amendments gets government amendments and all ping pong items for a bill", {
  expect_equal(length(unlist(get_total_gvt_amendments(3153))),
                             2290)
})
