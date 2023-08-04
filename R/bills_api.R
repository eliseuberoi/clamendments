#' Title Get bill stages
#'
#' @return A tibble with all bill stages
#' @export
#'
get_bill_stages <- function() {

  # There are 48 stages in total, across 3 pages
  urls <- list("https://bills-api.parliament.uk/api/v1/Stages",
               "https://bills-api.parliament.uk/api/v1/Stages?Skip=20",
               "https://bills-api.parliament.uk/api/v1/Stages?Skip=40")

  all_stages <- purrr::map(urls, ~ download(.))

  stages <- tibble::tibble()
  for (i in all_stages) {

    stage <- i$items
    stages <- dplyr::bind_rows(stages, stage)

  }

  stages
}



#' Get bill stages IDs for a bill
#'
#' @param bill_id The unique ID of a bill
#'
#' @return A dataframe with relevant bill stages and their IDs
#' @export
#'
get_bill_stages_ids <- function(bill_id) {

  url <- paste0("https://bills-api.parliament.uk/api/v1/Bills/",
                bill_id, "/Stages", collapse = ", ")

  bill_stages <- download(url)

  stageSittings <- purrr::list_rbind(bill_stages$items$stageSittings)

  # Filter only stages where bills can be amended
  # Ping pong items cannot be downloaded at this endpoint
  relevant_stages <- stageSittings %>% dplyr::filter(stageId == 8 |
                                                    stageId == 9 | stageId == 48|
                                                      stageId == 49 |
                                                      stageId == 3 | stageId == 4 |
                                                  stageId == 5)
  if (nrow(relevant_stages) != 0){

    relevant_stages

  } else {

    warning("Data for relevant bill stages is not available")

    }

}


#' Get stage amendments
#'
#' @param bill_id The unique ID of a bill
#' @param bill_stage_id The unique ID of a bill stage
#'
#' @return A dataframe with all amendments for a bill stage
#' @export
#'
get_stage_amendments <- function(bill_id, bill_stage_id) {

  path <- "https://bills-api.parliament.uk/api/v1/Bills/"

  url <- paste0(path, bill_id, "/Stages/", bill_stage_id,
                           "/Amendments", collapse = ", ")

  stage_amendments <- download(url)
  amendments <- stage_amendments$items

  # Each page includes 20 amendments: how many pages?
  amd_total <- stage_amendments$totalResults
  total_pages <- ceiling(amd_total/ 20)

  if (total_pages > 1) {

    skips <- 20*(seq(1:total_pages-1))

    # Get the URLs for each page with amendment data
    get_url <- function(skip) {

      url_skip <- paste0(url, "?Skip=", skip, collapse = ", ")

    }

    urls <- purrr::map(skips, ~get_url(.))

    # Download data from all pages
    stage_amendments_list <- purrr::map(urls, ~ download(.))

    # Collate the amendments data
     for (i in stage_amendments_list) {

      amd <- i$items
      amendments <- dplyr::bind_rows(amendments, amd)

    }
  }
  # Get key data for lead sponsors of amendments
  lead_sponsors <- purrr::map_dfr(amendments$sponsors, ~get_sponsor(.))
  amendments <- cbind(amendments, lead_sponsors)

  if(nrow(amendments) != amd_total) {

    warning("The number of amendments downloaded is not the same as the
            total number of amendments")

  }

  if (nrow(amendments) == 0 ) {

    warning(paste0("No amendments: check whether data is not available on the API for bill_stage_id ", bill_stage_id))
  }

  amendments
}


#' Title Get ping pong items
#'
#' @param bill_id The unique ID of a bill
#'
#' @return A tibble with detail, but not content, of ping pong items
#' @export
#'
get_ping_pong_items <- function(bill_id) {

  url <- paste0("https://bills-api.parliament.uk/api/v1/Bills/",
                bill_id, "/Stages", collapse = ", ")

  bill_stages <- download(url)

  stageSittings <- purrr::list_rbind(bill_stages$items$stageSittings)

  # Filter only ping pong stages
  relevant_stages <- stageSittings %>% dplyr::filter(stageId == 12 | #  Lords amendments
                                                       stageId == 42 | # Lords message
                                                       stageId == 13  # Commons amendments
                                                       )

  # Get ping pong items by stage
  get_stage_pp <- function(bill_id, bill_stage_id) {

    path <- "https://bills-api.parliament.uk/api/v1/Bills/"

    url <- paste0(path, bill_id, "/Stages/", bill_stage_id,
                  "/PingPongItems", collapse = ", ")

    pp <- download(url)
    pp_items <- pp$items

    # Each page includes 20 ping pong items: how many pages?
    pp_total <- pp$totalResults
    total_pages <- ceiling(pp_total/ 20)

    if (total_pages > 1) {

      skips <- 20*(seq(1:total_pages-1))

      # Get the URLs for each page with ping pong items data
      get_url <- function(skip) {

        url_skip <- paste0(url, "?Skip=", skip, collapse = ", ")

      }

      urls <- purrr::map(skips, ~get_url(.))

      # Download data from all pages
      pp_list <- purrr::map(urls, ~ download(.))

      # Collate the amendments data
      for (i in pp_list) {

        pp_item <- i$items
        pp_items <- dplyr::bind_rows(pp_items, pp_item)

      }
    }
    # Get key data for lead sponsors of ping pong items
    lead_sponsors <- purrr::map_dfr(pp_items$motion.sponsors, ~get_sponsor(.))
    pp_items <- cbind(pp_items, lead_sponsors)

    }

 if (nrow(relevant_stages) == 0) {

   warning("There are no ping pong items")

 } else {

   pp_items_stages <- purrr::map_dfr(relevant_stages$billStageId,
                                     ~get_stage_pp(bill_id, .))

   }

}


