library(tibble)
library(dplyr)
library(sf)

# do not use trrrj yet, just copied needed functions in utils.R ...
# library(trrrj)
#source(here::here("R", "utils.R"), encoding = 'UTF-8', local = TRUE)

######### Airport Reference Point
# Dublin (from https://skyvector.com/airport/EIDW/Dublin-Airport )
# Location Information for EIDW
# Coordinates: N53°25.28' / W6°16.20'
# View all Airports in Dublin, Ireland.
# Elevation is 242.0 feet MSL.
# Magnetic Variation is 3° West

########## Runways
# Runway 10/28
# Dimensions:	8652 x 148 feet / 2637 x 45 meters
# Surface:	Hard
# Runway 10	Runway 28
# Coordinates:	N53°25.35' / W6°17.40'	N53°25.22' / W6°15.03'
# Elevation: 	242 	202
# Runway Heading: 	101° 	281°
#
# Runway 16/34
# Dimensions:	6800 x 200 feet / 2073 x 61 meters
# Surface:	Unknown
# Runway 16	Runway 34
# Coordinates:	N53°26.22' / W6°15.72'	N53°25.19' / W6°14.98'
# Elevation: 	217 	202
# Runway Heading: 	162° 	342°


eidw_apt <- tibble::tribble(
  ~latitude,     ~longitude, ~elevation, ~heading,  ~icao,        ~id, ~type,       ~name,
  "N53°25.28'", "W6°16.20'",        242,       NA, "EIDW",     "EIDW", "ARP",  "EIDW ARP",
  "N53°25.35'", "W6°17.40'",        242,      101, "EIDW", "RWY10/28", "RWY", "Runway 10",
  "N53°25.22'", "W6°15.03'",        202,      281, "EIDW", "RWY10/28", "RWY", "Runway 28",
  "N53°26.22'", "W6°15.72'",        217,      162, "EIDW", "RWY16/34", "RWY", "Runway 16",
  "N53°25.19'", "W6°14.98'",        202,      342, "EIDW", "RWY16/34", "RWY", "Runway 34"
) %>%
  dplyr::mutate(latitude = ddm2dd(latitude), longitude = ddm2dd(longitude))

# lon/lat!!!!!!!!!!!!!!!
eidw_arp <- eidw_apt %>%
  dplyr::filter(type == "ARP") %>%
  dplyr::select(-heading)


eidw_rw_sf <- eidw_apt %>%
  dplyr::filter(
    stringr::str_detect(name, stringr::fixed("Runway", ignore_case = TRUE))) %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(4326) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise() %>%
  sf::st_cast("LINESTRING")


eidw_vor <- tibble::tribble(
  ~latitude,    ~longitude,  ~icao,  ~id, ~type, ~name,
  # WEST side
  "53° 46' 02.00\" N", "005° 30' 00.00\" W", "EIDW", "BOYNE", "", "BOYNE",
  "53° 40' 48.00\" N", "005° 30' 00.00\" W", "EIDW", "BAGSO", "", "BAGSO",
  "53° 41' 03.00\" N", "005° 39' 34.00\" W", "EIDW", "ADSIS", "", "ADSIS",
  "53° 37' 43.00\" N", "005° 45' 57.00\" W", "EIDW", "KERAV", "", "KERAV",
  "53° 24' 11.00\" N", "005° 56' 44.00\" W", "EIDW", "LAPMO", "", "LAPMO", # RWY from here
  "53° 07' 39.00\" N", "005° 34' 01.00\" W", "EIDW", "PEKOK", "", "PEKOK",
  "53° 11' 52.00\" N", "005° 38' 28.00\" W", "EIDW", "SIVNA", "", "SIVNA",
  "53° 08' 29.00\" N", "005° 48' 23.00\" W", "EIDW", "SORIN", "", "SORIN",
  # EAST side
  # South
  "53° 14' 31.00\" N", "006° 12' 36.00\" W", "EIDW", "DETAX", "", "DETAX",
  "53° 06' 31.00\" N", "006° 26' 52.00\" W", "EIDW", "KANUS", "", "KANUS",
  "53° 21' 56.00\" N", "006° 41' 45.00\" W", "EIDW", "OSLEX", "", "OSLEX",
  # North
  "53° 33' 47.00\" N", "006° 22' 26.00\" W", "EIDW", "ASDER", "", "ASDER",
  "53° 49' 03.00\" N", "006° 35' 55.00\" W", "EIDW", "EPIDU", "", "EPIDU",
  "53° 31' 17.00\" N", "006° 40' 24.00\" W", "EIDW", "NEKIL", "", "NEKIL"
) %>%
  dplyr::mutate(latitude = dms2dd(latitude), longitude = dms2dd(longitude))




eidw_stack_box <- tibble::tribble(
  ~latitude, ~longitude,   ~id,
  # EIDW WEST
  "53° 00' 00.0\" N", "005° 56' 44.1\" W", "RWY28W", # custom
  "53° 00' 00.0\" N", "005° 48' 22.5\" W", "RWY28W", # custom
  "53° 07' 39.3\" N", "005° 34' 00.8\" W", "RWY28W", # PEKOK
  "53° 17' 22.5\" N", "005° 31' 39.8\" W", "RWY28W", # SUGAD
  "53° 24' 03.7\" N", "005° 29' 10.1\" W", "RWY28W", # DW704
  "53° 30' 46.6\" N", "005° 31' 26.4\" W", "RWY28W", # DW705
  "53° 41' 03.1\" N", "005° 39' 34.0\" W", "RWY28W", # ADSIS
  "53° 47' 00.0\" N", "005° 38' 06.9\" W", "RWY28W", # custom
  "53° 47' 00.0\" N", "005° 57' 33.2\" W", "RWY28W", # custom
  "53° 38' 21.0\" N", "005° 57' 33.2\" W", "RWY28W", # GIRAS
  "53° 24' 11.0\" N", "005° 56' 44.1\" W", "RWY28W", # LAPMO
  "53° 00' 00.0\" N", "005° 56' 44.1\" W", "RWY28W", # custom
  # EIDW NORTH
  "53° 31' 16.6\" N", "006° 40' 23.6\" W", "RWY10N", # NEKIL
  "53° 33' 46.7\" N", "006° 22' 26.4\" W", "RWY10N", # ASDER
  "53° 41' 53.9\" N", "006° 22' 26.4\" W", "RWY10N", # custom N:ADNAL, W:ASDER
  "53° 46' 01.6\" N", "006° 27' 09.6\" W", "RWY10N", # custom: N:BOYNE, W:AKIVA
  "53° 49' 03.5\" N", "006° 35' 54.8\" W", "RWY10N", # EPIDU
  "53° 41' 49.0\" N", "006° 45' 34.9\" W", "RWY10N", # APRUT
  "53° 38' 42.8\" N", "006° 53' 58.4\" W", "RWY10N", # DW865
  "53° 33' 29.1\" N", "006° 58' 27.2\" W", "RWY10N", # DW866
  "53° 31' 16.6\" N", "006° 40' 23.6\" W", "RWY10N", # NEKIL
  # EIDW SOUTH
  "53° 21' 55.8\" N", "006° 41' 44.5\" W", "RWY10S", # OSLEX
  "53° 15' 54.2\" N", "006° 57' 04.5\" W", "RWY10S", # DW755
  "53° 12' 02.3\" N", "006° 49' 42.6\" W", "RWY10S", # DW754
  "53° 10' 59.5\" N", "006° 40' 05.6\" W", "RWY10S", # BIVDI
  "53° 06' 30.5\" N", "006° 26' 52.1\" W", "RWY10S", # KANUS
  "53° 14' 30.7\" N", "006° 12' 35.6\" W", "RWY10S", # DETAX
  "53° 19' 00.0\" N", "006° 24' 50.8\" W", "RWY10S",  # custom: N531900.0, W:BERMO
  "53° 21' 55.8\" N", "006° 41' 44.5\" W", "RWY10S" # OSLEX
) %>%
  dplyr::mutate(latitude = dms2dd(latitude), longitude = dms2dd(longitude))

eidw_holdings_sf <- eidw_stack_box %>%
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  # sf::st_set_crs(4326) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON")

