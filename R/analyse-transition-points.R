library(fs)
library(here)
library(purrr)
library(readr)
library(dplyr)
library(skimr)

egll_transitions <- dir_ls(path = here("data"),
                           regexp = "egll_transition_points_.*.csv") %>%
  map_dfr(~ read_csv(.))


# TODO: add assert
# # just one pair P40 -- PLAND per flight!
# egll_transitions %>%
# dplyr::group_by(flight_id) %>%
# arrange(flight_id, timestamp) %>%
# select(flight_id, distance_flown, type) %>%
# filter(type %in% c("P40", "PLAND")) %>% count() %>% filter(n > 2)


flown_tma <- egll_transitions %>%
  dplyr::group_by(flight_id) %>%
  arrange(flight_id, timestamp) %>%
  select(flight_id, distance_flown, type) %>%
  filter(type %in% c("P40", "PLAND")) %>%
  mutate(flown_tma = distance_flown - lag(distance_flown)) %>%
  filter(!is.na(flown_tma)) %>%
  select(-type, -distance_flown) %>%
  ungroup()

flown_holding <- egll_transitions %>%
  filter(type %in% c("PHOLD")) %>%
  group_by(flight_id, holding_id) %>%
  arrange(flight_id, timestamp) %>%
  select(flight_id, timestamp, holding_id, distance_flown, type) %>%
  mutate(flown_holding = distance_flown - lag(distance_flown),
         duration_holding = timestamp - lag(timestamp)) %>%
  filter(!is.na(flown_holding)) %>%
  select(-type, -distance_flown, -timestamp) %>%
  ungroup()

# TODO: in 8 days only few instances, still...
# # flights crossing multiple holding stacks
# flown_holding %>%
#   arrange(flight_id) %>%
#   group_by(flight_id) %>%
#   filter(n() > 2)


# TODO: decide what happens in case of crossing multiple holdings
# from last PHOLD to PLAND, i.e. PLAND and the preceeding one, see slice()
flown_to_runway <- egll_transitions %>%
  group_by(flight_id) %>%
  arrange(flight_id, timestamp) %>%
  # take PLAND and the preceeding one
  slice((n()-1):n()) %>%
  select(flight_id, timestamp, holding_id, distance_flown, type) %>%
  mutate(flown_runway = distance_flown - lag(distance_flown),
         duration_runway = timestamp - lag(timestamp)) %>%
  filter(!is.na(flown_runway)) %>%
  select(-type, -distance_flown, -timestamp, -type, -holding_id) %>%
  ungroup()




