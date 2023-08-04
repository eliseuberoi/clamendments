#' Title Get stage ministers
#'
#' @param bill_id The unique ID of a bill
#' @param bill_stage_id The unique ID of a bill stage
#'
#' @return A tibble with one row per ministerial role held during the bill stage
#' @export

get_stage_ministers <- function(bill_id, bill_stage_id) {

  # Identify interval of bill stage
  bill_stages <- get_bill_stages_ids(bill_id)
  stage_dates <- bill_stages %>% dplyr::filter(billStageId == bill_stage_id)
  stage_dates <- as.Date(stage_dates$date) %>% sort() %>% lubridate::ymd()

  # Download MPs and Lords with government roles during this period
  stage_mps <- clmnis::fetch_mps_government_roles(
    from_date = stage_dates[1],
    to_date = stage_dates[length(stage_dates)])
  stage_lords <- clmnis::fetch_lords_government_roles(
    from_date = stage_dates[1],
    to_date = stage_dates[length(stage_dates)])

  # Combine MPs and Lords
  stage_ministers <- dplyr::bind_rows(stage_mps, stage_lords)

}

