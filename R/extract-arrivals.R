# you can RUN this from command line (once you set the dates of interest
# below _AND_ bunzip2 the relevant position files) as foloows:
#
# date; Rscript R/extract-arrivals.R; date
#
# for me it took:
# extract_beg <- "Fri Jan  4 14:43:25 CET 2019" %>%
#   as_datetime(tz = "CET", format = "%a %b %e %T CET %Y") %>%
#   as_datetime()
# extract_end <- "Fri Jan  4 15:05:45 CET 2019" %>%
#   as_datetime(tz = "CET", format = "%a %b %e %T CET %Y") %>%
#   as_datetime()
#
# extract_end - extract_beg
# Time difference of 22.33333 mins

library(readr)
library(dplyr)
library(purrr)
library(geosphere)
library(rlang)
library(stringr)
library(fs)

source(here::here("R", "utils.R"))

days_of_data <- c(
  "2017-08-03"
)

apts <- c(
  "eidw",
  "egll")


# extract flights arrivals at airport apt on date
flts_for_apt_on_date <- function(apt, date) {
  flights_bz2 <- fs::path(
    here::here("data-raw/"),
    str_glue("cpr_fr24_flights_{d}", d = date),
    ext = "csv.bz2")
  extract_arrival_flights(rawdatafile = flights_bz2, apt = apt)
}

purrr::pwalk(
  tidyr::crossing(apts, days_of_data),
  ~ flts_for_apt_on_date(.x, .y))


# extract position reports for flight arrivals at airport apt on date
poss_for_apt_on_date <- function(apt, date) {
  flights_apt_csv <- fs::path(
    here::here("data"),
    str_glue("{apt}_flt_rt_{d}", apt = apt, d = date),
    ext = "csv")
  flt_rt <- readr::read_csv(flights_apt_csv)
  flt_ids <- flt_rt %>% dplyr::pull(flight_id)

  source(
    here::here(
      "R",
      stringr::str_glue("{apt}-data.R", apt = apt)))
  apt_data <- get(paste0(apt, "_apt"))
  arp <- get(paste0(apt, "_arp"))
  arp <- c(arp$longitude, arp$latitude)

  poss_bz2 <- fs::path(
    here::here("data-raw/"),
    str_glue(
      "mas_05_cpr_fr24_synth_positions_{d}",
      d = date),
    ext = "csv.bz2")

  # bunzip2
  #R.utils::bunzip2(bz2file, csvfile, remove = FALSE, skip = TRUE)

  poss_csv <- fs::path_ext_remove(poss_bz2)

  extract_arrival_positions(
    poss_csv,
    apt,
    arp,
    flt_ids, prefix = "pos_rt")
}

purrr::pwalk(
  tidyr::crossing(apts, days_of_data),
  ~ poss_for_apt_on_date(.x, .y))


# extract positions at nm nautical miles for arrivals
# at airport apt on date
poss_for_apt_on_date_within <- function(apt, date, nm = 40) {
  here::here(
    "data",
    str_glue("{apt}_pos_rt_{d}.csv", apt = apt, d = date)
  ) %>%
    extract_airport_distance(arp, nm)
}

purrr::pwalk(
  tidyr::crossing(apts, days_of_data),
  ~ poss_for_apt_on_date_within(.x, .y))
