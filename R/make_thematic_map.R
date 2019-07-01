
#' Make a thematic map.
#'
#' Makes a quick thematic (choropleth) map from data provided,
#' with a fixed colour scheme.
#'
#' @param .data DataFrame or sf DataFrame with the data to plot.
#' @param fill_var column to map to the fill aesthetic.
#' @param .map sf DataFrame of the map to fill, if `.data`` is not already an sf object.
#' @param join_by column or dplyr join specification to join `.data`` and `.map by`.
#' @param remove_na whether to remove shapes that have NA values or not.
#' @param background_map sf DataFrame of a map to plot as the background.
#' @param breaks number of vector of breaks for the colour scale of `fill_var`.
#' @param break_name_expr regex to clean up break names with.
#' @param scale_name name for the fill scale label.
#' @return ggplot thematic map
#' @import dplyr ggplot2 sf
#' @export
make_thematic_map <- function(.data, fill_var, .map=NULL, join_by=NULL,
                              remove_na=TRUE, background_map=NULL,
                              breaks=NULL, break_name_expr="\\d+\\.?\\d+(?=\\])",
                              scale_name=NULL) {
  fill_var <- enquo(fill_var)

  if (! "sf" %in% class(.data)) {
    if (is.null(.map)) {
      stop("If your data is not already an sf data frame, you must provide a map to join it to.")
    }

    if (is.null(join_by)) {
      stop("You must specify what columns to join your map and data with.")
    }

    .data <- left_join(.map, .data, by=join_by)
  }

  if (remove_na) {
    .data <- filter(.data, ! is.na(!! fill_var))
  }

  if (is.null(breaks)) {
    breaks <- 7
  }

  if (length(breaks) == 1) {
    break_min <- .data %>% pull(!! fill_var) %>% min()
    break_max <- .data %>% pull(!! fill_var) %>% max()

    break_min <- break_min - break_min * 0.05

    breaks <- seq(from=break_min, to=break_max, length.out=breaks)
  }

  .data <-
    .data %>%
    mutate(fill_var=cut(!! fill_var, breaks=breaks, include.lowest=FALSE, dig.lab=4, ordered_result=TRUE)) %>%
    mutate(fill_var=forcats::fct_relabel(fill_var, ~stringr::str_extract(.x, break_name_expr)))

  if (is.null(scale_name)) {
    scale_name <- quo_name(fill_var)
  }

  p <- ggplot()

  if (! is.null(background_map)) {
    p <- p + geom_sf(data=background_map, fill="grey80", colour="white")
  }

  p +
    geom_sf(data=.data, mapping=aes(fill=fill_var), colour="white") +
    theme_void() +
    theme(legend.position="bottom") +
    viridis::scale_fill_viridis(
      option = "magma",
      name = scale_name,
      drop=FALSE,
      begin = 0.1,
      end = 0.9,
      discrete = T,
      direction = 1,
      na.value="grey80",
      guide = guide_legend(
        direction="horizontal",
        keyheight=unit(4, units="mm"),
        keywidth=unit(15, units="mm"),
        label.hjust=1,
        title.position="top",
        nrow=1,
        byrow=TRUE,
        label.position="bottom"
      ))
}
