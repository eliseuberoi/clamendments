test_that("get_bill_stages_ids gets the right data", {
  expect_equal( as.numeric(get_bill_stages_ids(3155) %>% dplyr::filter(stageId == 8) %>%
                             dplyr::select(billStageId) %>% unique()),
                16533)
})


test_that("get_stage_amendments downloads all amendments", {
  expect_equal( nrow(get_stage_amendments(3155, 16533)),
                327)
})


test_that("get_stage_amendments assigns lead sponsor correctly", {
  expect_equal(as.character(get_stage_amendments(3155, 16533) %>%
                 dplyr::select(lead_sponsor_name) %>% dplyr::slice_head()),
               "Matthew Pennycook" )
})


test_that("get_ping_pong_items gets the right number of items", {
  expect_equal(nrow(get_ping_pong_items(3340)), 14)
})
