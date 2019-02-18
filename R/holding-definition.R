library(sf)
library(geosphere)

#' calculate turning radius
#'
#' @param tas true airspeed (kt)
#' @param ba  bank angle (degrees) [default = 30].
#'
#' @return turning radius (ft)
#'
turning_radius <- function(tas, ba = 30) {
  tas * tas * tan(ba * pi / 180) / 11.26
}

#' calculate rate of turn
#'
#' @param tas true airspeed (kt)
#' @param ba  bank angle (degrees) [default = 30].
#'
#' @return turning radius (ft)
#'
turn_rate <- function(tas, ba) {
  1.091 * tan(ba * pi / 180) / tas
}

# st_as_sfc("CIRCULARSTRING(2 0, 1 1, 0 0)") %>% st_cast("LINESTRING") %>% st_cast("POINT") %>% plot()

holding_fix <- st_sfc(st_point(cbind(-0.4472222, 51.305)), crs=4326)
inbound_direction <- 329
holding_side <- "right"
tas_max <- 220

P <- st_coordinates(holding_fix) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))
r <- turning_radius(tas_max)

# center of upper circle
Cu <- geosphere::destPoint(P, inbound_direction + 90, 0.3048 * r) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))

# top of upper turn
T <- geosphere::destPoint(Cu, inbound_direction, 0.3048 * r) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))


# opposite to holding fix
Q <- geosphere::destPoint(P, inbound_direction + 90, 0.3048 * 2 * r) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))


# first straight leg
R <- geosphere::destPoint(Q, inbound_direction + 180, 0.514444 * tas_max * 60) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))

# point before holding fix
S <- geosphere::destPoint(P, inbound_direction + 180, 0.514444 * tas_max * 60) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))

# center of lower circle
Cl <- geosphere::destPoint(S, inbound_direction + 90, 0.3048 * r) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))

# top of lower turn
V <- geosphere::destPoint(Cl, inbound_direction + 180, 0.3048 * r) %>%
  as.vector() %>%
  setNames(c("lon", "lat"))


#' Generate a CIRCULARSTRING for half circle of an holding turn
#'
#' NOTE: definition is counterclockwise.
#'
#' @param p1 start coordinate, a two-element vector (lon, lat)
#' @param p2 middle coordinate
#' @param p3 end coordinate
#'
#' @return a CIRCULARSTRING feature
#'
st_half_circle <- function(p1, p2, p3) {
  st_as_sfc(
    stringr::str_glue("CIRCULARSTRING({Qlon} {Qlat}, {Tlon} {Tlat}, {Plon} {Plat})",
                      Qlon = p1[1], Qlat = p1[2],
                      Tlon = p2[1], Tlat = p2[2],
                      Plon = p3[1], Plat = p3[2]))
}

st_holding_leg <- function(p1, p2) {
  st_as_sfc(
    stringr::str_glue("LINESTRING({Plon} {Plat}, {Slon} {Slat})",
                      Slon = p2[1], Slat = p2[2], Plon = p1[1], Plat = p1[2]))
}

upper_turn <- st_half_circle(Q, T, P) %>% st_cast("LINESTRING") %>% st_cast("POINT")
inbound_leg <- st_holding_leg(P, S) %>% smoothr::densify() %>% st_cast("POINT")
lower_turn <- st_half_circle(S, V, R) %>% st_cast("LINESTRING") %>% st_cast("POINT")
outbound_leg <- st_holding_leg(R, Q) %>% smoothr::densify() %>% st_cast("POINT")

# NOT working
c(upper_turn, inbound_leg, lower_turn, outbound_leg) %>% plot()

