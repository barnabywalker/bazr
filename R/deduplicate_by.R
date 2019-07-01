#' Deduplicate a DataFrame.
#'
#' A tidy function that deduplicates a DataFrame by unique values
#' in the user supplied columns. Keeps the first row for each value.
#'
#' @param .data DataFrame to be deduplicated
#' @param ... columns to deduplicate by
#' @return deduplicated DataFrame
#' @import dplyr
#' @export
deduplicate_by <- function(.data, ...) {
  group_vars <- enquos(...)

  .data %>%
    group_by(!!! group_vars) %>%
    filter(row_number() == 1) %>%
    ungroup()
}
