library(tibble)
library(dplyr)
library(sf)

######### Airport Reference Point
# Zurich ( from https://skyvector.com/airport/LSZH/Zurich-Airport )
#
# Location Information for LSZH
# Coordinates: N47°27.48' / E8°32.88'
# View all Airports in Zürich, Switzerland.
# Elevation is 1417.0 feet MSL.
# Magnetic Variation is 2° East

########## Runways
# Runway 16/34
# Dimensions:	12139 x 197 feet / 3700 x 60 meters
# Surface:	Hard
# Runway 16	Runway 34
# Coordinates:	N47°28.54' / E8°32.16'	N47°26.96' / E8°33.25'
# Elevation: 	1390 	1388
# Runway Heading: 	154° 	334°
# Displaced Threshold: 		1542 Feet

# Runway 14/32
# Dimensions:	10827 x 197 feet / 3300 x 60 meters
# Surface:	Unknown
# Runway 14	Runway 32
# Coordinates:	N47°28.93' / E8°32.16'	N47°27.68' / E8°33.87'
# Elevation: 	1402 	1402
# Runway Heading: 	136° 	316°
# Displaced Threshold: 	492 Feet

# Runway 10/28
# Dimensions:	8202 x 197 feet / 2500 x 60 meters
# Surface:	Unknown
# Runway 10	Runway 28
# Coordinates:	N47°27.54' / E8°32.25'	N47°27.40' / E8°34.23'
# Elevation: 	1391 	1416
# Runway Heading: 	095° 	275°


lszh_apt <- tribble(
  ~latitude,     ~longitude, ~elevation, ~heading,  ~icao,          ~id, ~type,        ~name,
  "N47°27.48'", "E8°32.88'",       1417,       NA, "LSZH",       "LSZH", "ARP",   "LSZH ARP",
  "N47°28.54'", "E8°32.16'",       1390,      154, "LSZH",   "RWY16/34", "RWY", "Runway 16",
  "N47°26.96'", "E8°33.25'",       1388,      334, "LSZH",   "RWY16/34", "RWY", "Runway 34",
  "N47°28.93'", "E8°32.16'",       1402,      136, "LSZH",   "RWY14/32", "RWY", "Runway 14",
  "N47°27.68'", "E8°33.87'",       1402,      316, "LSZH",   "RWY14/32", "RWY", "Runway 32",
  "N47°27.54'", "E8°32.25'",       1391,       95, "LSZH",   "RWY10/28", "RWY", "Runway 10",
  "N47°27.40'", "E8°34.23'",       1416,      275, "LSZH",   "RWY10/38", "RWY", "Runway 28"
) %>%
  dplyr::mutate(latitude = trrrj::ddm2dd(latitude), longitude = trrrj::ddm2dd(longitude))

# lon/lat!!!!!!!!!!!!!!!
lszh_arp <- lszh_apt %>%
  dplyr::filter(type == "ARP")
