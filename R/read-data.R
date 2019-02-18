read_trj_40NM <- function(apt, date = "2017-08-01"){
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
    distance_arp = col_double(),
    sequence_number = col_integer()
  )

  files <- stringr::str_glue("data/{apt}_pos_rt_40NM_{d}.csv", d = date)
  files %>% purrr::map_df(~ readr::read_csv(., col_types = cols))
}

read_trj_trans <- function(apt, date = "2017-08-01"){
  cols <- cols(
    flight_id = col_character(),
    timestamp = col_datetime(format = ""),
    longitude = col_double(),
    latitude = col_double(),
    altitude = col_double(),
    distance_flown = col_double(),
    distance_arp = col_double(),
    sequence_number = col_integer(),
    type = col_character(),
    holding_id = col_character(),
    bearing = col_double(),
    quadrant = col_character(),
    callsign = col_character(),
    aircraft_reg = col_character(),
    aircraft_type = col_character(),
    aircraft_address = col_character(),
    adep = col_character(),
    ades = col_character()
  )

  files <- stringr::str_glue("data/{apt}_transition_points_{d}.csv", d = date)
  files %>% purrr::map_df(~ readr::read_csv(., col_types = cols))
}
