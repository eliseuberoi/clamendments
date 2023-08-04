#' Title Download
#'
#' @param url The URL with JSON content to download
#'
#' @return A list including data on all amendments;
#' the total number of amendments; and the number of amendments per page
#' @export
#'
download <- function(url) {

  request <- httr::GET(url)

  output <- request %>% httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  output

}


#' Title Get sponsor
#'
#' @param list_element The dataframe that contains sponsor data
#'
#' @return A tibble with key data for the lead sponsor
#' @export
#'
get_sponsor <- function(list_element) {

  if (length(list_element) != 0) {

  lead_sponsor_data <- list_element %>% subset(isLead == TRUE) %>%
    dplyr::select(memberId, name, party, house)
  names(lead_sponsor_data) <- c("lead_sponsor_id", "lead_sponsor_name",
                                "lead_sponsor_party", "lead_sponsor_house")

  lead_sponsor_data

  } else { tibble::tibble("lead_sponsor_id" = NA,
                          "lead_sponsor_name" = NA,
                          "lead_sponsor_party" = NA,
                          "lead_sponsor_house" = NA
                          )}

}
