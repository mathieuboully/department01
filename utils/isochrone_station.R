library(dplyr)
library(openrouteservice)
library(sf)

api_key = "5b3ce3597851110001cf624862acdd8f2bcc444c9771f00176cb6898"

get_isochrones = function(lat_l,
                          lon_l,
                          mode,
                          range_type,
                          range,
                          interval) {
  message("get_isochrones")
  res = openrouteservice::ors_isochrones(
    locations = Map(c, lon_l, lat_l),
    api_key = api_key,
    profile = ors_profile(mode = c(mode)),
    range = range,
    interval = interval,
    output = "sf",
    location_type = "start",
    range_type = range_type
  )
  return(res)
}

path_data = "./data"

stations = read.csv2(file.path(path_data, "liste-des-gares.csv"),
                     header = T,
                     dec = ".")

stations_clean = stations %>%
  rename(lat = Y_WGS84,
         lon = X_WGS84) %>%
  filter(DEPARTEMEN %in% c("AIN")) %>%
  distinct(LIBELLE, .keep_all = TRUE)

mode = "bike"
range = 600

iso_ls = list()
for (i in seq(length(stations_clean$CODE_UIC))) {
  iso = get_isochrones(
    lat_l = stations_clean$lat[i],
    lon_l = stations_clean$lon[i],
    range_type = "time",
    range = c(range),
    interval = range,
    # range_type = "time",
    # range = 1800,
    # interval = 1800,
    mode = mode
  )
  
  element <- list(
    iso = iso,
    station_name = stations_clean$LIBELLE[i],
    lat_center = iso$center[[1]][2],
    lon_center = iso$center[[1]][1],
    color = "#a05a2c"
  )
  
  iso_ls <- append(iso_ls, list(element))
}

saveRDS(iso_ls, file = file.path(path_data, paste0("isochrone_station_", mode, "_", range, ".rds")))
