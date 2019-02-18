# in order to use it source this file and then invoke the function.
# you get a ggplot object back
plot_eidw_tma <- function() {
  # FIXME: super nasty
  library(ggplot2)
  library(here)
  library(readr)

  source(here("R", "utils.R"), encoding = 'UTF-8', local = TRUE)
  source(here("R", "eidw-data.R"), encoding = 'UTF-8', local = TRUE)

  eidw_vor <- eidw_vor %>%
    dplyr::filter(!id %in% c("BOYNE", "BAGSO", "SORIN"))


  stacks <- eidw_vor %>%
    dplyr::mutate(hjust = 0, vjust = -0.7) %>%
    # TODO: adapt to EIDW
    dplyr::mutate(
      hjust = ifelse(id == "PEKOK",  1.0, hjust),
      vjust = ifelse(id == "PEKOK",  1.5, vjust)) %>%
    dplyr::mutate(
      hjust = ifelse(id == "EPIDU",  1.2, hjust),
      vjust = ifelse(id == "EPIDU", 0.7, vjust)) %>%
    dplyr::mutate(
      hjust = ifelse(id == "KERAV",  1.2, hjust),
      vjust = ifelse(id == "KERAV",  1.5, vjust)) %>%
    dplyr::mutate(
      hjust = ifelse(id == "OCL",  -0.2, hjust),
      vjust = ifelse(id == "OCL", -0.7, vjust))

  gvor <- ggplot2::geom_point(data = eidw_vor,
    ggplot2::aes(x = longitude,
      y = latitude),
    colour = "blue",
    size = 2)
  gvorl <- ggplot2::geom_text(data = stacks,
    ggplot2::aes(x = longitude,
      y = latitude,
      label = id,
      hjust = hjust,
      vjust = vjust))


  gbox <- ggplot2::geom_sf(data = eidw_holdings_sf, alpha = 0)


  g <- ggplot2::ggplot()

  grw_sf <- ggplot2::geom_sf(data = eidw_rw_sf, size = 1.2)

  rw <- eidw_apt %>%
    dplyr::filter(
      stringr::str_detect(name, stringr::fixed("Runway", ignore_case = TRUE))) %>%
    group_by(id) %>%
    mutate(latitude2 = lag(latitude), longitude2 = lag(longitude)) %>%
    ungroup() %>%
    filter(!is.na(latitude2)) %>%
    select(-name)

  grw <- ggplot2::geom_segment(
    data = rw,
    ggplot2::aes(x = longitude, y = latitude, xend = longitude2, yend = latitude2),
    size = 1.2)
  g2d <- g + grw_sf + gbox + gvor + gvorl
  g2d
}
