# -----------------------------------------------------------------------------
# File name : preprocess.R
# Description : format, features, labels, NAs
# Author : Boully Mathieu
# Date : 2025-11-05
# -----------------------------------------------------------------------------

# Load config files
base::source("./global.R", local = F)
base::source("./config/constants.R", local = F)

#### Bicycle crash ####
# Import data
df_bc = read.csv2(
  file.path(CONSTS$path_data_raw, "accidentsVelo.csv"),
  header = T,
  sep = ",",
  dec = ".",
  encoding = "UTF-8"
)

# NAs
df_bc_clean = df_bc %>%
  filter(if_all(everything(), ~ !is.na(.)))

# Format
df_bc_clean = df_bc %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),
    date_full = format(date, "%A %d %B %Y"),
    mois = as.factor(mois),
    jour = as.factor(jour),
    dep = as.integer(dep),
    com = as.integer(com),
    lat = as.numeric(lat),
    long = as.numeric(long),
    agg = as.factor(agg),
    int = as.factor(int),
    col = as.factor(col),
    lum = as.factor(lum),
    atm = as.factor(atm),
    catr = as.factor(catr),
    circ = as.factor(circ),
    nbv = as.integer(nbv),
    prof = as.factor(prof),
    plan = as.factor(plan),
    lartpc = as.numeric(lartpc),
    larrout = as.numeric(larrout),
    surf = as.factor(surf),
    infra = as.factor(infra),
    situ = as.factor(situ),
    grav = as.factor(grav),
    sexe = as.factor(sexe),
    trajet = as.factor(trajet),
    secuexist = as.factor(secuexist),
    equipement = as.factor(equipement),
    obs = as.factor(obs),
    obsm = as.factor(obsm),
    choc = as.factor(choc),
    manv = as.factor(manv),
    typev = as.factor(typevehicules)
  )

colSums(is.na(df_bc_clean))
str(df_bc_clean)

# Label
df_bc_clean = df_bc_clean %>%
  dplyr::mutate(
    grav_label = case_when(
      grav %in% "1" ~ "Indemne",
      grav %in% "2" ~ "Tué",
      grav %in% "3" ~ "Blessé hospitalisé",
      grav %in% "4" ~ "Blessé léger",
      TRUE ~ as.character(grav)
    )
  )

# NAs
df_bc_clean = df_bc_clean %>%
  dplyr::mutate(
    lat = dplyr::if_else(lat == 0 | lat == -0, NA, lat),
    long = dplyr::if_else(long == 0 | lat == -0, NA, long),
  )

# Select columns
cols_to_remove <- c()
df_bc_clean = df_bc_clean %>%
  dplyr::select(-all_of(cols_to_remove))

# Save dataframe to RDS file
saveRDS(df_bc_clean, 
        file = file.path(CONSTS$path_data_processed, "bicycle_crash.rds"))


#### Stations ####
# Import data
stations = read.csv2(file.path(CONSTS$path_data_raw, "liste-des-gares.csv"),
                     header = T,
                     dec = ".")

# Data cleaning
stations_clean = stations %>%
  rename(lat = Y_WGS84, lon = X_WGS84) %>%
  filter(DEPARTEMEN %in% c("AIN") & VOYAGEURS %in% "O") %>%
  distinct(CODE_UIC, .keep_all = TRUE)

#### Traffic stations ####
# Import data
freq_stations = read.csv2(
  file.path(CONSTS$path_data_raw, "frequentation-gares.csv"),
  header = T,
  dec = "."
)

# Data merging stations and traffic
stations_clean = stations_clean %>%
  left_join(freq_stations, by = c("CODE_UIC" = "Code.UIC"))

# Save dataframe to RDS file
saveRDS(stations_clean, file = file.path(CONSTS$path_data_processed, "stations.rds"))

#### Population ####
# Import data
socio = read.csv2(
  file.path(CONSTS$path_data_raw, "Filosofi2017_carreaux_1km_met.csv"),
  header = T,
  sep = ",",
  dec = "."
)

# Data cleaning
socio_clean = socio %>%
  mutate(
    epsg = sub(".*CRS(\\d+)RES.*", "\\1", Idcar_1km),
    res  = as.numeric(sub(".*RES(\\d+)m.*", "\\1", Idcar_1km)),
    y    = as.numeric(sub(".*N(\\d+)E.*", "\\1", Idcar_1km)),
    x    = as.numeric(sub(".*E(\\d+)$", "\\1", Idcar_1km))
  ) %>%
  st_as_sf(coords = c("x", "y"), crs = 3035) %>%
  st_transform(crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  filter(substr(lcog_geo, 1, 2) == "01")

# Save dataframe to RDS file
saveRDS(socio_clean, file = file.path(CONSTS$path_data_processed, "socio.rds"))

#### Rent ####
# Import data
rent_house = read.csv2(
  file.path(CONSTS$path_data_raw, "pred-mai-mef-dhup.csv"),
  header = T,
  dec = ",",
  encoding = "UTF-8"
) %>%
  filter(DEP %in% "01")

rent_app3 = read.csv2(
  file.path(CONSTS$path_data_raw, "pred-app3-mef-dhup.csv"),
  header = T,
  dec = ",",
  encoding = "UTF-8"
) %>%
  filter(DEP %in% "01")

rent_app12 = read.csv2(
  file.path(CONSTS$path_data_raw, "pred-app12-mef-dhup.csv"),
  header = T,
  dec = ",",
  encoding = "UTF-8"
) %>%
  filter(DEP %in% "01")
   
# Save 3 dataframes to RDS file in a single list
saveRDS(list(mai = rent_house,
             app3 = rent_app3,
             app12 = rent_app12), file = file.path(CONSTS$path_data_processed, "rent.rds"))

#### Location INSEE ####
# Import data
loc_insee = read.csv2(
  file.path(path_data_raw, "20230823-communes-departement-region.csv"),
  header = T,
  sep = ",",
  dec = ".",
  encoding = "UTF-8"
) %>%
  filter(nom_departement %in% "Ain")

# Save dataframe to RDS file
saveRDS(loc_insee, file = file.path(path_data_output, "loc_insee.rds"))