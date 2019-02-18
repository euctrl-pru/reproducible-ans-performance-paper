library(dplyr)
library(stringr)

# copied from trrrj::ddm2dd
ddm2dd <- function(value) {
  # RegEx for DdM (eventually w/ spaces):
  #   ([N|S|W|E]\s*)?(\d+)°\s*(\d+(?:\.\d+)?)'
  pattern <- "([N|S|W|E]\\s*)?(\\d+)°\\s*(\\d+(?:\\.\\d+)?)'"
  pieces <- value %>% stringr::str_subset(pattern) %>% stringr::str_match(pattern)
  dd <- as.double(pieces[, 3]) + as.double(pieces[, 4]) / 60
  dd <- ifelse(stringr::str_detect(pieces[, 2], "S|W"), -dd, dd)
  # constrain longitude to (-180, 180]: ((lon - 180) %% 360) - 180
  # constrain latitude to (-90, 90]: ((lat - 90) %% 180) - 90
  dd <- ifelse(
    stringr::str_detect(pieces[, 2], "W|E"), # if not lon, assume lat
    ( (dd - 180) %% 360) - 180,
    ( (dd -  90) %% 180) -  90)
  dd
}

# copied from trrrj::dms2dd
dms2dd <- function(value) {
  # RegEx for DMS (eventually w/ spaces):
  #   (\d+)°\s*(\d+)'\s*(\d+(?:\.\d+)?)\"\s*([N|S|W|E]?)
  pattern <- "(\\d+)°\\s*(\\d+)'\\s*(\\d+(?:\\.\\d+)?)\"\\s*([N|S|W|E]?)"
  pieces <- value %>% stringr::str_subset(pattern) %>% stringr::str_match(pattern)
  dd <- ( (as.double(pieces[, 4]) / 60) + as.double(pieces[, 3])) / 60 + as.double(pieces[, 2])
  dd <- ifelse(stringr::str_detect(pieces[, 5], "S|W"), -dd, dd)
  # constrain longitude to (-180, 180]: ((lon - 180) %% 360) - 180
  # constrain latitude to (-90, 90]: ((lat - 90) %% 180) - 90
  dd <- ifelse(
    stringr::str_detect(pieces[, 5], "W|E"), # if not lon, assume lat
    ( (dd - 180) %% 360) - 180,
    ( (dd - 90) %% 180)  -  90)
  dd
}


# extract position reports at `distance` from position (lon,lat) arp
# input file is named like `<icao airport>_pos_rt_<date>.csv`,
# i.e. `lszh_pos_rt_2017-08-02.csv`
# output file is named like `<icao airport>_pos_rt_<distance>NM_<date>.csv`
extract_airport_distance <- function(rawdatafile, arp, distance) {
  if(!fs::file_exists(rawdatafile)) {
    stop(paste0("Error: file", rawdatafile, " does not exist."))
  }

  file_parts <- rawdatafile %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_split("_") %>% `[[`(1)

  apt_lower <- file_parts[1]
  infix <- stringr::str_c(file_parts[2], file_parts[3], sep = "_")
  data_date <- file_parts %>% dplyr::last()

  file <- stringr::str_glue(
    apt_lower,
    infix,
    str_glue(distance, "NM"),
    data_date,
    .sep = "_") %>%
    fs::path_ext_set("csv")
  file <- fs::path_join(c(here::here(), "data", file))

  d <- distance * 1852
  rawdatafile %>%
    readr::read_csv() %>%
    dplyr::filter(distance_arp <= d) %>%
    readr::write_csv(file)
}

# extract arrivals at airport, `apt`. Use `prefix` for output filename
# input file is like `cpr_fr24_flights_2017-08-02.csv`
extract_arrival_flights <- function(rawdatafile, apt, prefix = "flt_rt") {
  if(!fs::file_exists(rawdatafile)) {
    stop(paste0("Error: file", rawdatafile, " does not exist."))
  }

  data_date <- rawdatafile %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    fs::path_ext_remove() %>%   # remove second extension (if present)
    stringr::str_split("_") %>% `[[`(1) %>% last()
  apt_lower <- apt %>% tolower()
  file <- stringr::str_glue(apt_lower, prefix, data_date, .sep = "_") %>%
    fs::path_ext_set("csv")
  file <- fs::path(here::here("data"), file)

  apt_upper <- apt %>% toupper()
  rawdatafile %>%
    readr::read_csv() %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(ades == apt_upper) %>%
    readr::write_csv(file)
}



#' extract positions for airport
#'
#' @param rawdatafile reference trajectory file
#' @param apt airport, i.e. "egll"
#' @param arp airport reference point (lon/lat), i.e. `c(-0.461389, 51.4775)`
#' @param ids flight ids to keep
#' @param infix string for filename, default "pos_rt".
#'              The default will result in filename
#'              egll_pos_rt_<date>.csv where <date> is extracted from the date portion
#'              in the `rawdatafile` name
#' @return a data frame
#'
extract_arrival_positions <- function(rawdatafile, apt, arp, ids, prefix = "pos_rt") {
  if(!fs::file_exists(rawdatafile)) {
    stop(paste0("Error: file", rawdatafile, " does not exist."))
  }
  data_date <- rawdatafile %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_split("_") %>% `[[`(1) %>% dplyr::last()
  apt_lower <- apt %>% tolower()
  apt_upper <- apt %>% toupper()
  file <- stringr::str_glue(apt_lower, prefix, data_date, .sep = "_") %>%
    fs::path_ext_set("csv")
  file <- fs::path_join(c(here::here(), "data", file))

  # filter by flight ID
  filter1 <- function(x, arp, ids) {
    x %>%
      dplyr::filter(FLIGHT_ID %in% ids)
  }
  partial1 <- purrr::partial(filter1, ids = ids)

  # get all arrivals
  read_csv_chunked(
    rawdatafile, DataFrameCallback$new(partial1),
    chunk_size = 5000) %>%
    rename_all(tolower) %>%
    rename(timestamp = time,
           longitude = lon, latitude = lat,
           altitude = alt) %>%
    group_by(flight_id) %>%
    arrange(timestamp) %>%
    ungroup() %>%
    mutate(distance_arp = geosphere::distGeo(
      arp,
      cbind(.data$longitude, .data$latitude))) %>%
    select(flight_id,
           timestamp, latitude, longitude, altitude,
           speed_gnd, track_gnd,
           vert_speed, on_ground,
           distance,
           distance_arp) %>%
    write_csv(file)
}


# filter by distance from point
# arp: airport reference point, lon/lat
# NOTE: Heathrow hardcoded for now (maybe for ever!-)
filter_distance_from <- function(x, arp = c(-0.461389, 51.4775), distance = 40) {
  d <- distance * 1852 # in NM
  x %>%
    # `cbind`` is the trick!!!!!!!!!!!!!!!!!
    dplyr::mutate(distance_arp = geosphere::distGeo(arp, cbind(.data$LON, .data$LAT))) %>%
    dplyr::filter(distance_arp <= d)
}


plot_flight <- function(flt) {
  ggplot2::ggplot(data = flt) +
    ggplot2::geom_path(
      mapping = ggplot2::aes(x = longitude,
                             y = latitude,
                             colour = callsign,
                             group = callsign),
      size = 1.5,
      alpha = .3,
      lineend = "round")
}


save_egll_holding_plot <- function(data_pos, flight_ids, idx) {
  id <- flight_ids[idx]
  flt <- data_pos %>%
    dplyr::filter(flight_id == id)

  stacks <- egll_vor %>%
    dplyr::mutate(hjust = 0, vjust = 0) %>%
    dplyr::mutate(
      hjust = ifelse(id == "BNN",  -0.2, hjust),
      vjust = ifelse(id == "BNN",  1.5, vjust)) %>%
    dplyr::mutate(
      hjust = ifelse(id == "BIG",  1.2, hjust),
      vjust = ifelse(id == "BIG", -0.7, vjust)) %>%
    dplyr::mutate(
      hjust = ifelse(id == "LAM",  1.2, hjust),
      vjust = ifelse(id == "LAM",  1.5, vjust)) %>%
    dplyr::mutate(
      hjust = ifelse(id == "OCL",  -0.2, hjust),
      vjust = ifelse(id == "OCL", -0.7, vjust))

  gvor <- ggplot2::geom_point(data = egll_vor,
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


  gbox <- ggplot2::geom_sf(data = hss_sf, alpha = 0)

  if(nrow(flt) > 0) {
    # indexes of the peaks
    peaks <- pracma::findpeaks(flt$distance_arp, nups = 5)[,2]
    valleys <- pracma::findpeaks(-flt$distance_arp, nups = 5)[,2]
    minmax <- c(valleys, peaks)
    flt <- flt %>%
      dplyr::mutate(is_peak = ifelse(
        dplyr::row_number() %in% minmax,
        TRUE,
        FALSE))

    # minimum track duration (secs)
    min_track_duration <- 60 * 1
    # minimum track altitude, (lowest track is at 7000 ft), keep a margin
    min_track_altitude <- 6500 %>%
      units::set_units("ft") %>%
      units::set_units("m") %>%
      as.numeric()

    # maximum track altitude, (highiest track is at ??? ft), keep a margin
    max_track_altitude <- 250000 %>%
      units::set_units("ft") %>%
      units::set_units("m") %>%
      as.numeric()

    s <- trrrj::extract_segment(flt) %>%
      dplyr::mutate(duration = as.duration(end_ts - beg_ts), flight_id = id) %>%
      dplyr::filter(duration >= lubridate::as.duration(min_track_duration)) %>%
      dplyr::filter(beg_alt >= min_track_altitude, beg_alt <  max_track_altitude) %>%
      mutate(segment_number = dplyr::row_number())

    g <- ggplot2::ggplot(flt)

    gd <- g +
      ggplot2::geom_line(ggplot2::aes(x = timestamp, y = distance_arp)) +
      ggplot2::geom_point(data = (flt %>% dplyr::filter(is_peak == TRUE)),
                          ggplot2::aes(x = timestamp, y = distance_arp), colour = "yellow")

    gv <- g + ggplot2::geom_line(ggplot2::aes(x = timestamp, y = altitude))

    g2d <- ggplot2::ggplot(data = flt) +
      ggplot2::geom_path(
        mapping = ggplot2::aes(x = longitude,
                               y = latitude,
                               colour = callsign,
                               group = callsign),
        size = 1.5, alpha = .3, lineend = "round")

    # keep holding segment: no luck, see 33
    # h <- flt %>% left_join(s, by = "flight_id") %>%
    #   filter(timestamp >= beg_ts, timestamp <= end_ts)
    # gh <- geom_path(data = h,
    #                 mapping = aes(x = longitude,
    #                               y = latitude,
    #                               group = callsign),
    #                 size = 1.5, alpha = .3, lineend = "round", colour = "blue")
    # g2d + gh

    # g2d <- plot_flight_2d(flt)

    if (nrow(s) > 0) {
      gs <- ggplot2::geom_segment(
        data = s,
        ggplot2::aes(x = beg_ts, y = beg_alt, xend = end_ts, yend = end_alt),
        colour = "blue",
        size = 2)
      gs0 <- ggplot2::geom_segment(
        data = s,
        ggplot2::aes(x = beg_ts, y = 0, xend = end_ts, yend = 0),
        colour = "blue",
        size = 3)
      g0 <- gv + gs
      g1 <- gd + gs0
    } else {
      g0 <- gv
      g1 <- gd
    }

    grw_sf <- ggplot2::geom_sf(data = rw_sf, size = 1.2)
    grw <- ggplot2::geom_segment(
      data = rw,
      ggplot2::aes(x = longitude, y = latitude, xend = longitude2, yend = latitude2),
      size = 1.2)
    g2d <- g2d + grw_sf + gbox + gvor + gvorl


    patch <- g0 + g1 + g2d + patchwork::plot_layout(ncol = 1, heights = c(1, 1, 3))

    fl <- stringr::str_glue("figures", "generated", "holding_{idx}.png", idx = idx, .sep = "/")
    ggplot2::ggsave(fl, plot = patch, device = "png")
  }
}



# utility to plot and manually edit a box around a holding pattern
# in order to manually define the holding stacks areas
library(leaflet)
library(mapview)
library(mapedit)
edit_flight <- function(data, id) {
  # id <- egll_flt_ids[idx]
  flt <- data %>%
    filter(flight_id == id)
  f_sf <-
    flt %>% sf::st_as_sf(coords = c("longitude", "latitude")) %>% sf::st_set_crs(4326)
  editMap(mapview(f_sf), targetLayerID = "flight")
}

# example of edit holding area
# 1. create a polygon, it will be ib ppp once done
# ppp <- edit_flight(4)
# 2. make it a tribble (and assign the relevant `id`, i.e. "LAM")
# ppp %>%
#   `[[`(1) %>%
#   st_geometry() %>%
#   st_coordinates() %>%
#   as_tibble() %>%
#   rename(longitude = X, latitude = Y) %>%
#   mutate(id = "LAM") %>%
#   select(-L1, -L2) %>%
#   datapasta::dpasta()


# calculate the bearing
# add a variable `bearing` (non NA only for the first point)
add_bearing <- function(df, arp) {
  df %>%
    dplyr::mutate(
      bearing =
        (geosphere::bearing(
          cbind(longitude, latitude),
          cbind(arp$longitude, arp$latitude)) + 360) %% 360)
}


# add initial quadrant from bearing [at 40 NM]
# for each flight, add a variable `quadrant`
add_quadrant <- function(df) {
  df %>%
    dplyr::mutate(
      quadrant = case_when(
        bearing >=   0 & bearing <  90 ~ "I",
        bearing >=  90 & bearing < 180 ~ "II",
        bearing >= 180 & bearing < 270 ~ "III",
        bearing >= 270 & bearing < 360 ~ "IV",
        TRUE ~ NA_character_
      )) %>%
    mutate(quadrant = factor(
      quadrant,
      levels = c("I", "II", "III", "IV")))
}


nth_group <- function(x, n) {
  x %>%
    tidyr::nest() %>%
    dplyr::slice(n) %>%
    tidyr::unnest(data)
}

first_group <- function(x) nth_group(x, n = 1)
secondt_group <- function(x) nth_group(x, n = 2)


######################################################
# TODO: make it work _correctly_
#   pt/th1/th2: lon/lat vectors; name1 is RWY threshold 1
# return the RWY name and its distance from it
cross_track_from_runway <- function(pt, th1, th2, name1, name2) {
  r_Earth <- 6371e3  # mean Earth radius (m)
  n_ET1_E <- nvctr::lat_lon2n_E(nvctr::rad(th1['latitude']), nvctr::rad(th1['longitude']))
  n_ET2_E <- nvctr::lat_lon2n_E(nvctr::rad(th2['latitude']), nvctr::rad(th2['longitude']))
  n_EP_E  <- nvctr::lat_lon2n_E(nvctr::rad(pt['latitude']), nvctr::rad(pt['longitude']))
  # unit normal to the great circle between n_EA1_E and n_EA2_E
  c_E <- nvctr::unit(pracma::cross(n_ET1_E, n_ET2_E))
  # position of normal on Earth
  n_EC_E <- r_Earth * c_E

  # find the intersection point between the great circle arcs
  #  n_ET1_E -- n_ET1_E and n_EP_E -- c_E
  # This is the intersection point on the great circle n_ET1_E -- n_ET1_E
  n_ED_E <- nvctr::unit(pracma::cross(
    pracma::cross(n_ET1_E, n_ET2_E),
    pracma::cross(n_EP_E, n_EC_E)))
  # there are 2 intersections: n_ED_E and its antipodal n_EF_E = -n_ED_E
  n_EF_E <- -n_ED_E
  # Select the one that has the minimum distance from the 2 thresholds
  d_d_t1 <- base::norm(n_ET1_E - n_ED_E, type = "2") * r_Earth
  d_d_t2 <- base::norm(n_ET2_E - n_ED_E, type = "2") * r_Earth
  d_f_t1 <- base::norm(n_ET1_E - n_EF_E, type = "2") * r_Earth
  d_f_t2 <- base::norm(n_ET2_E - n_EF_E, type = "2") * r_Earth

  d <- c(d_d_t1, d_d_t2, d_f_t1, d_f_t2)
  rwy <- c(name1, name2, name1, name2)
  idx <- which.min(d)

  c(rwy[idx], d[idx])
}

# opt1 is the result of applying cross_track_from_runway for RWY1
rwy_for_point <- function(opt1, opt2) {
  distances <- c(opt1[2], opt2[2]) %>% as.numeric()
  rwys <- c(opt1[1], opt2[1])
  rwys[which.min(distances)]
}


guess_egll_rwy <- function(longitude, latitude) {
  a <- cross_track_from_runway(c(longitude = longitude, latitude = latitude),
    th1 = c(latitude = 51.47750, longitude = -0.4850000),
    th2 = c(latitude = 51.47767, longitude = -0.4333333),
    name1 = "09L", name2 = "27R")
  b <- cross_track_from_runway(c(longitude = longitude, latitude = latitude),
    th1 = c(latitude = 51.46483, longitude = -0.4823333),
    th2 = c(latitude = 51.46500, longitude = -0.4340000),
    name1 = "09R", name2 = "27L")
  rwy_for_point(a, b)
}
##############################################
