# -----------------------------------------------------------------------------
# File name : computeBicycleCrash.R
# Description : format, features, labels, NAs
# Author : Boully Mathieu
# Date : 2025-11-05
# -----------------------------------------------------------------------------

# Load config files
base::source("./global.R", local = F)
base::source("./config/constants.R", local = F)

# Import data
df_bc = read.csv2(
  file.path(CONSTS$PATH_DATA_RAW, "accidentsVelo.csv"),
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
        file = file.path(CONSTS$PATH_DATA_PROCESSED, "bicycle_crash.rds"))
