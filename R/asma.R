# Utility function for the ASMA part of the paper
# 
# The functions fall broadly into X categories
# 
# 1. read functions for the analytical data, including variable renaming and type coercions
# 


# ================= READ IN ANALYTICAL DATA =======================

read_arrs_apdf <- function(apt, .path = "./data/"){
  data_file <- paste(.path, apt, "_arrs_fact_2017.csv", sep = "")
  ds <- readr::read_csv(
    data_file
    ,col_types = cols(
      CALLSIGN = col_character()
      ,REG = col_character()
      ,ADEP = col_character()
      ,ADES = col_character()
      ,ALDT = col_datetime(format = "")
      ,AIBT = col_datetime(format = "")
      ,STA = col_datetime(format = "")
      ,TYP = col_character()
      ,WTC = col_character()
      ,C40_TIME = col_datetime(format = "")
      ,C40_LAT = col_double()
      ,C40_LON = col_double()
      ,C40_ALT = col_double()
      )
    )
  return(ds)
}


read_flts_trj <- function(apt, .path = "./data/"){
  data_path <- .path
  data_files<- dir(data_path, pattern = paste(apt, "_flt_rt", sep = ""))
  
  flts <- data_files %>%
    purrr::map(~ read_csv(file.path(data_path, .),
                   col_types = cols(
                     period_start = col_datetime(format = "")
                     ,period_finish = col_datetime(format = "")
                   ))
    ) %>%
    dplyr::bind_rows()
  return(flts)
}


# ================= COVERAGE DATA PREP =======================

prep_cov_apdf <- function(ds){
  ds <- ds %>% 
    dplyr::mutate(
      ARR_TIME = if_else(!is.na(AIBT), AIBT, STA) 
      ,DOF     = lubridate::date(ARR_TIME)
      ,SOURCE  = "APDF") %>% 
    select(REG, TYP, ADEP, ADES, ARR_TIME, DOF, SOURCE)
  return(ds)
}


prep_cov_trj <- function(ds){
  ds <- ds %>% 
    # rename variables
    dplyr::select( REG  = aircraft_reg, TYP = aircraft_type
                  ,ADEP = adep, ADES = ades
           , period_start, period_finish) %>%
    # inject ARR_TIME 
    dplyr::mutate(
      ARR_TIME = dplyr::if_else(    #dplyr::if_else returns proper types
        !is.na(period_finish), period_finish
        , period_start)
      ,DOF     = lubridate::date(ARR_TIME)
      ,SOURCE  = "RTRJ") %>%
    select(-contains("period"))  # remove unused period items
  return(ds)
}


start_date = lubridate::dmy_hms("01-08-2017 00:00:00", tz = "UTC")
end_date   = lubridate::dmy_hms("08-08-2017 23:59:59", tz = "UTC")

trim_time_horizon <- function(arrs, .start_date = start_date, .end_date = end_date){
  arrs <- arrs %>% 
    filter(ARR_TIME >= start_date, ARR_TIME <= end_date)
  return(arrs)
}

# ================== calc ASMA ===================================

path_to_data = "./data"
pts_pattern = "_transition_points"

read_pts_trj <- function(apt, .path = path_to_data, .pattern = pts_pattern){
  file_names <- dir(path = .path, pattern = paste(apt, .pattern, sep = ""))
  file_names <- paste(.path,"/", file_names, sep = "")
  
  pts <- file_names %>%
    purrr::map_df(readr::read_csv)
}