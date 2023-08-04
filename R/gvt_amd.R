#' Stage government amendments
#'
#' @param bill_id The unique ID of a bill
#' @param bill_stage_id The unique ID of a bill stage
#'
#' @return A dataframe with amendments introduced during a bill stage by ministers
#' @export
#'
stage_gvt_amd <- function(bill_id, bill_stage_id) {

  bill_stages <- get_bill_stages_ids(bill_id)

  # Download all amendments for a stage
  stage_amendments <- get_stage_amendments(bill_id, bill_stage_id)

  # Find out who were ministers during this stage
  stage_ministers <- get_stage_ministers(bill_id, bill_stage_id)

  # Keep only amendments introduced by ministers
  if(nrow(stage_amendments) > 0) {

    government_amendments <- dplyr::filter(stage_amendments,
                                           lead_sponsor_id %in%
                                             stage_ministers$mnis_id)

    } else {

    government_amendments <- tibble::tibble()
    }

  if ("statusIndicator" %in% colnames(government_amendments)) {
    government_amendments$statusIndicator <- as.character(
      government_amendments$statusIndicator)
  }


  government_amendments
}


#' Title Get total government amendments
#'
#' @param bill_id The unique ID of a bill
#'
#' @return A list including a dataframe with all government amendments and one with ping pong items
#' @export
#'
get_total_gvt_amendments <- function(bill_id) {

  # Get the IDs for all bill stages with amendments
  stages_ids <- get_bill_stages_ids(bill_id)
  ids <- unique(stages_ids$billStageId)

  # Get government amendments by stage
  stage_gvt_amendments <- purrr::map_dfr(ids, ~stage_gvt_amd(bill_id, .))

  # Get ping pong items
  ping_pong_items <- get_ping_pong_items(bill_id)

  output <- list(stage_gvt_amendments, ping_pong_items)
  names(output) <- c("amendments", "ping_pong")

  output

}

