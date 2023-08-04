test_that("get_stage_ministers identifies the right ministers", {
  expect_equal(nrow(get_stage_ministers(3155, 16533)), 306
)})

