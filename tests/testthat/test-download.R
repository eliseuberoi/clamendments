test_that("download() downloads and processes data", {
  expect_equal(names(download(
    "https://bills-api.parliament.uk/api/v1/Bills/3311/Stages/16843/Amendments")),
               c("items", "totalResults", "itemsPerPage"))
})
