library(readr)
library(dplyr)
library(sf)
library(stringr)

date_of_interest <- "2017-08-08"
buffer_miles <- 2
# # EGLL
# apt <- "egll"
# lower_ft <- 6700
# EIDW
apt <- "eidw"
lower_ft <- 2700


# get airport relevant details
source(here::here("R", str_glue("{apt}-data.R", apt = apt)))
arp <- get(str_glue("{apt}_arp", apt = apt))

# use st_buffer to enlarge the "manually" defined holding polygons
# see https://stackoverflow.com/a/46706245/963575
library(units)

# buffer for holding polygon: `buffer_miles` nautical miles
buf <- set_units(buffer_miles, nmile) %>%
  set_units(m)

holdings <- get(str_glue("{apt}_holdings_sf", apt = apt)) %>%
  st_transform(29902) %>%
  st_buffer(buf) %>%
  st_transform(4326)


# read flight info
flt_rt_file <- str_glue("data/{apt}_flt_rt_{d}.csv", apt = apt, d = date_of_interest)
cols <- cols(
  flight_id = col_character(),
  callsign = col_character(),
  aircraft_reg = col_character(),
  aircraft_type = col_character(),
  aircraft_address = col_character(),
  adep = col_character(),
  ades = col_character(),
  ssr_codes = col_character(),
  period_start = col_datetime(format = ""),
  period_finish = col_datetime(format = ""),
  source_ids = col_character()
)

flt_rt <- read_csv(flt_rt_file)
flt_rt_no_dups <- flt_rt %>%
  group_by(callsign) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1 | (n >= 2 & !is.na(aircraft_type)))

# read positions within 40 NM
cols <- cols(
  flight_id = col_character(),
  timestamp = col_datetime(format = ""),
  latitude = col_double(),
  longitude = col_double(),
  altitude = col_double(),
  speed_gnd = col_double(),
  track_gnd = col_double(),
  vert_speed = col_double(),
  on_ground = col_logical(),
  distance = col_double(),
  distance_arp = col_double()
)
pos_rt_40NM_file <- str_glue(
  "data/{apt}_pos_rt_40NM_{d}.csv",
  apt = apt,
  d = date_of_interest)
pos_rt_40NM <- read_csv(pos_rt_40NM_file, col_types = cols) %>%
  select(flight_id,
         timestamp,
         longitude, latitude, altitude,
         distance, distance_arp) %>%
  rename(distance_flown = distance) %>%
  group_by(flight_id) %>%
  arrange(timestamp) %>%
  # add
  # - sequence number
  # - total of points
  # convert
  # - distance_arp from meters to NM
  mutate(sequence_number = dplyr::row_number(),
         n = n(),
         distance_arp = distance_arp / 1852) %>%
  ungroup()



#' extract transition points at 40 NM, holdings and landing
#'
#' @param df A reference trajectory data frame
#' @param holdings_sf An sf polygon defining the various holding areas
#' @param min_fl Minimum flight level (with margin) for holdings.
#'               For example at EGLL lowest flying level is at 7000 ft,
#'               so in this function we set it at 6700...
#'
#' @return A data frame with points transitionsing from the various zones
extract_transition_points <- function(df, holdings_sf, min_ft = 6700) {
  p <- df %>% group_by(flight_id)

  # indexes within 40 NM
  p_40 <- p %>%
    filter(row_number() == 1) %>%
    mutate(type = "P40") %>%
    ungroup()

  # indexes closest to ARP: an approximation of the landing spot
  p_landing <- p %>%
    filter(distance_arp == min(distance_arp)) %>%
    filter(timestamp == min(timestamp)) %>%
    slice(1) %>% # break the ties: take the first if there are duplicates
    mutate(type = "PLAND") %>%
    ungroup()

  # indexes for holding portions
  # NOTE: for EGLL holding lower flight level is at 7000 ft
  st_as_xyz <- function(x) {
    # TODO: stop if geometry is not XY POINT
    data.frame(st_set_geometry(x, NULL), st_coordinates(x)) %>%
      as_tibble() %>%
      rename(longitude = X, latitude = Y)
  }

  p_holding <- p %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      dim = "XY",
      crs = 4326) %>%
    # NOTE: stay close minumum level in holding
    filter(altitude > min_ft) %>%
    st_intersection(holdings) %>%
    group_by(flight_id, id) %>%
    filter(row_number() == 1 | row_number() == n()) %>%
    mutate(type = "PHOLD") %>%
    ungroup() %>%
    st_as_xyz()

  bind_rows(p_40, p_holding, p_landing) %>%
    rename(holding_id = id)
}


transition_points <- extract_transition_points(pos_rt_40NM, holdings, min_ft = lower_ft)

# add bearing and quadrant
transition_points <- transition_points %>%
  add_bearing(arp) %>%
  add_quadrant()

# join positions with relevant flight info...and keep only a subset of vars
transition_points <- transition_points %>%
  left_join(flt_rt, by = "flight_id") %>%
  select(-c(ssr_codes, period_start, period_finish, source_ids, n))



write_csv(transition_points,
          str_glue("data/{apt}_transition_points_{d}.csv",
                   apt = apt,
                   d = date_of_interest))
