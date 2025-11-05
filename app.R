# -----------------------------------------------------------------------------
# File name : app.R
# Author : Boully Mathieu
# Date : 2025-11-05
# -----------------------------------------------------------------------------

# Load all R packages
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)
library(tidyr)
library(grDevices)
library(DT)
library(crosstalk)
library(jsonlite)
library(scales)
library(RColorBrewer)
library(FactoMineR)
library(missMDA)

# Load config files
# base::source("./global.R", local = F)
# base::source("./config/constants.R", local = F)

# Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# options(
#   shiny.maxRequestSize = 100 * 1024 ^ 2,
#   dplyr.summarise.inform = FALSE,
#   stringsAsFactors = FALSE
# )

# Sys.setlocale(category = "LC_ALL", locale = "French")
app_name = "Analyse du territoire de l’Ain"
last_update = file.info(file.path(getwd(), "app.R"))$atime

# if (requireNamespace("rstudioapi", quietly = TRUE) &&
#     rstudioapi::isAvailable()) {
#   .wdPath <- dirname(rstudioapi::getSourceEditorContext()$path)
# } else {
#   .wdPath <- getwd()
# }
# setwd(.wdPath)

# if (!is.null(rstudioapi::getActiveProject())) {
#   .wdPath = setwd(rstudioapi::getActiveProject())
# } else {
#   .wdPath = dirname(rstudioapi::getSourceEditorContext()$path)
#   setwd(.wdPath)
# }

path_data_raw = "./data/raw"
path_data_output = "./data/output"
path_image = "./www"

col_pal = grDevices::colorRampPalette(c("#ebebeb", "#194264"))

markers = leaflet::iconList(logo_sncf = leaflet::makeIcon(iconUrl = "./www/logo_sncf.png", iconWidth = 15))

# Data
# Accidents à vélo
# bicycle_crash = read.csv2(
#   file.path(path_data_raw, "accidentsVelo.csv"),
#   header = T,
#   sep = ",",
#   dec = "."
# )
# bicycle_crash_clean = bicycle_crash %>%
#   filter(dep %in% "01" & long != 0) %>%
#   mutate(
#     date = as.Date(date, format = "%Y-%m-%d"),
#     date_clean = format(date, "%A %d %B %Y"),
#     grav_label = case_when(
#       grav == 1 ~ "Indemne",
#       grav == 2 ~ "Tué",
#       grav == 3 ~ "Blessé hospitalisé",
#       grav == 4 ~ "Blessé léger",
#       TRUE ~ as.character(grav)
#     )
#   )
# saveRDS(bicycle_crash_clean, file = file.path(path_data_output, "bike_crash.rds"))
bicycle_crash_clean = readRDS(file = file.path(path_data_output, "bike_crash.rds"))

# Aménagements cyclables
# bicycle = read.csv2(
#   file.path(path_data_raw, "amenagements-cyclables.csv"),
#   header = T,
#   sep = ",",
#   dec = "."
# )

# Stations
# stations = read.csv2(file.path(path_data_raw, "liste-des-gares.csv"),
#                      header = T,
#                      dec = ".")
#
# stations_clean = stations %>%
#   rename(lat = Y_WGS84, lon = X_WGS84) %>%
#   filter(DEPARTEMEN %in% c("AIN") & VOYAGEURS %in% "O") %>%
#   distinct(CODE_UIC, .keep_all = TRUE)

# Fréquentation stations
# freq_stations = read.csv2(
#   file.path(path_data_raw, "frequentation-gares.csv"),
#   header = T,
#   dec = "."
# )

# stations_clean = stations_clean %>%
#   left_join(freq_stations, by = c("CODE_UIC" = "Code.UIC"))

# saveRDS(stations_clean, file = file.path(path_data_output, "stations.rds"))
stations_clean = readRDS(file = file.path(path_data_output, "stations.rds"))

# Socio
# socio = read.csv2(
#   file.path(path_data_raw, "Filosofi2017_carreaux_1km_met.csv"),
#   header = T,
#   sep = ",",
#   dec = "."
# )
#
# socio_clean = socio %>%
#   mutate(
#     epsg = sub(".*CRS(\\d+)RES.*", "\\1", Idcar_1km),
#     res  = as.numeric(sub(".*RES(\\d+)m.*", "\\1", Idcar_1km)),
#     y    = as.numeric(sub(".*N(\\d+)E.*", "\\1", Idcar_1km)),
#     x    = as.numeric(sub(".*E(\\d+)$", "\\1", Idcar_1km))
#   ) %>%
#   st_as_sf(coords = c("x", "y"), crs = 3035) %>%
#   st_transform(crs = 4326) %>%
#   mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
#   st_drop_geometry() %>%
#   filter(substr(lcog_geo, 1, 2) == "01")
#
# saveRDS(socio_clean, file = file.path(path_data_output, "socio.rds"))
socio_clean = readRDS(file = file.path(path_data_output, "socio.rds"))

# Rent
# rent_house = read.csv2(
#   file.path(path_data_raw, "pred-mai-mef-dhup.csv"),
#   header = T,
#   dec = ",",
#   encoding = "UTF-8"
# ) %>%
#   filter(DEP %in% "01")
#
# rent_house = read.csv2(
#   file.path(path_data_raw, "pred-mai-mef-dhup.csv"),
#   header = T,
#   dec = ",",
#   encoding = "UTF-8"
# ) %>%
#   filter(DEP %in% "01")
#
# rent_app3 = read.csv2(
#   file.path(path_data_raw, "pred-app3-mef-dhup.csv"),
#   header = T,
#   dec = ",",
#   encoding = "UTF-8"
# ) %>%
#   filter(DEP %in% "01")
#
# rent_app12 = read.csv2(
#   file.path(path_data_raw, "pred-app12-mef-dhup.csv"),
#   header = T,
#   dec = ",",
#   encoding = "UTF-8"
# ) %>%
#   filter(DEP %in% "01")
#
# saveRDS(list(mai = rent_house,
#              app3 = rent_app3,
#              app12 = rent_app12), file = file.path(path_data_output, "rent.rds"))
rent_ls = readRDS(file = file.path(path_data_output, "rent.rds"))

# Location INSEE
# loc_insee = read.csv2(
#   file.path(path_data_raw, "20230823-communes-departement-region.csv"),
#   header = T,
#   sep = ",",
#   dec = ".",
#   encoding = "UTF-8"
# ) %>%
#   filter(nom_departement %in% "Ain")
#
# saveRDS(loc_insee, file = file.path(path_data_output, "loc_insee.rds"))
loc_insee = readRDS(file = file.path(path_data_output, "loc_insee.rds"))

ui = page_navbar(
  title = app_name,
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "minty",
    bg = "#ffffff",
    fg = "#000000",
    primary = "#194264",
    secondary = "#000000",
    success = "#ebebeb",
    navbar_bg = "#194264",
    base_font = font_google("Arimo"),
    code_font = font_google("Arimo"),
    heading_font = font_google("Phudu")
  ),
  sidebar = sidebar(
    nav_panel(title = ""),
    # accordion(
    #   open = F,
    #   accordion_panel("Gares et accessibilité", icon = bsicons::bs_icon("train-front-fill"))
    # ),
    tags$p(
      "Cette application interactive permet d’analyser l’accessibilité aux gares et les accidents de vélo dans le département de l’Ain en les reliant à des variables socio-démographiques telles que la densité de population."
    ),
    tags$a(
      tags$img(
        src = "logo.png",
        width = "70%",
        height = "auto"
      ),
      href = "https://www.ain.fr/",
      target = "_blank"
    ),
    tags$sub(paste(
      "Mis à jour le", format(last_update, "%d %B %Y")
    ))
  ),
  nav_spacer(),
  nav_panel(
    title = "Gares et accessibilité",
    icon = bsicons::bs_icon("train-front-fill"),
    layout_columns(
      card(
        full_screen = T,
        layout_sidebar(
          fillable = TRUE,
          border = T,
          sidebar = sidebar(
            position = "right",
            width = 300,
            open = T,
            selectInput(
              inputId = "iso_select",
              label = "Couvertures des gares",
              c(
                "10 minutes à vélo",
                "20 minutes à vélo",
                "10 minutes en voiture",
                "20 minutes en voiture"
              ),
              multiple = F,
              selected = "10 minutes à vélo"
            ),
            selectInput(
              inputId = "socio_var_select",
              label = "Carte de chaleur",
              c("Nombre d'individus", "Nombre de logements sociaux"),
              multiple = F,
              selected = "Nombre d'individus"
            )
          ),
          # popover(
          #   bsicons::bs_icon("gear"),
          #   title = "Paramètres de la carte",
          #   placement = "right",
          #   bslib::input_switch(id = "show_shop", label = "99Bikes"),
          #   bslib::input_switch(id = "show_other_shop", label = "Magasins de cycles")
          # )),
          leafletOutput("mobility_map")
        )
      ),
      value_box(
        theme_color = "secondary",
        showcase = bsicons::bs_icon("bullseye"),
        value = textOutput("pop_cov_box"),
        title = "Proportion d'habitants couverts par une gare",
        color = "secondary"
      ),
      value_box(
        title = "Nombre de voyageurs en 2024",
        value = label_number(big.mark = " ", decimal.mark = ",")(sum(stations_clean$Total.Voyageurs.2024)),
        theme_color = "secondary",
        showcase = bsicons::bs_icon("suitcase-fill")
      ),
      value_box(
        title = "Nombre de gares actives",
        value = length(stations_clean$CODE_UIC),
        theme_color = "secondary",
        showcase = bsicons::bs_icon("building-fill")
      ),
      card(full_screen = TRUE, plotly::plotlyOutput("passengers_evolution")),
      card(
        full_screen = TRUE,
        leafletOutput("frequentation_map"),
        card_footer("Fréquentations des gares en 2024")
      ),
      col_widths = c(12, 4, 4, 4, 6, 6)
    )
  ),
  nav_panel(
    "Accidents de vélo",
    icon = bsicons::bs_icon("bicycle"),
    layout_columns(
      card(
        full_screen = TRUE,
        plotly::plotlyOutput("crash_evolution"),
        card_footer(popover(
          a("En savoir plus", href = "#"),
          markdown(
            "Pour chaque accident corporel (soit un accident survenu sur une voie ouverte à la circulation publique, impliquant au moins un véhicule et ayant fait au moins une victime ayant nécessité des soins), des saisies d’information décrivant l’accident sont effectuées par l’unité des forces de l’ordre (police, gendarmerie...) qui est intervenue sur le lieu de l’accident."
          )
        ))
      ),
      card(full_screen = TRUE, plotly::plotlyOutput("famd_ind_bc")),
      card(dataTableOutput("table_crash"), card_footer(
        tags$h5('Jours de la semaine les plus à risque')
      )),
      col_widths = c(12, 12)
    )
  ),
  nav_panel(
    "Loyers",
    icon = bsicons::bs_icon("house-fill"),
    layout_columns(card(
      layout_sidebar(
        fillable = TRUE,
        border = T,
        sidebar = sidebar(
          position = "right",
          width = 300,
          open = T,
          selectInput(
            inputId = "sel_rent",
            label = "Type de location",
            c("Maison individuelle", "Appartement T1-T2", "Appartement T3+"),
            multiple = F,
            selected = "Maison individuelle"
          )
        ),
        leafletOutput("house_map"),
        card_footer(tags$h5("Loyers par type de logement"), popover(
          a("En savoir plus", href = "#"),
          markdown(
            "Cette carte interactive présente la répartition des loyers à l’échelle communale pour le trimestre 2023, selon le type de bien : maisons individuelles, appartements T1, T2 et T3+.<br>
Les données proviennent de l’<a href='https://www.data.gouv.fr/datasets/communes-de-france-base-des-codes-postaux/', target='_blank'>Open Data gouvernemental</a> sur les loyers d’annonce par commune."
          )
        ))
      )
    ), col_widths = c(12))
  ),
  nav_panel(
    title = "À propos",
    icon = bs_icon("info-circle"),
    fluidRow(
      column(
        width = 6,
        h3("Département de l'Ain"),
        tags$p(
          tags$a(href = "https://www.ain.fr/", "Le département de l’Ain", target = "_blank"),
          "est situé dans la région Auvergne-Rhône-Alpes, à l’est de la France. Il couvre une superficie d’environ 5 762 km² et compte près de 655 000 habitants.

Il se caractérise par une grande diversité de paysages : des plaines agricoles dans le centre et le sud, des collines et montagnes du Jura au nord-est, et des zones périurbaines autour des villes principales. Le département combine ainsi des zones urbaines dynamiques et des territoires ruraux plus calmes.

Le réseau de transport est relativement développé, avec des gares SNCF, un réseau routier dense, et une progression de la mobilité douce (vélo, transports en commun) dans certaines zones. La population et les activités économiques sont réparties de manière inégale, offrant un maillage territorial varié idéal pour des analyses socio-démographiques et de mobilité.

L’Ain constitue un territoire à la fois naturellement préservé et économiquement actif, mêlant agriculture, industries locales, tourisme et activités de loisirs de plein air."
        ),
        hr(),
        h3("Données"),
        tags$p(
          "Les données utilisées dans ce projet proviennent de différentes sources ouvertes (open data, API) :"
        ),
        tags$p(
          tags$ul(
            tags$li(
              tags$a(
                href = "https://www.data.gouv.fr/datasets/carte-des-loyers-indicateurs-de-loyers-dannonce-par-commune-en-2023/",
                "Loyers par commune 2018-2023",
                target = "_blank"
              )
            ),
            tags$li(
              tags$a(href = "https://www.data.gouv.fr/datasets/communes-de-france-base-des-codes-postaux/", "Communes de france", target = "_blank")
            ),
            tags$li(
              tags$a(
                href = "https://www.insee.fr/fr/statistiques/6215140",
                "Données socio-démographiques 2017",
                target = "_blank"
              )
            ),
            tags$li(
              tags$a(href = "https://ressources.data.sncf.com/explore/dataset/liste-des-gares/information/", "Gares ferroviaires", target = "_blank")
            ),
            tags$li(
              tags$a(href = "https://ressources.data.sncf.com/explore/dataset/frequentation-gares/information/", "Fréquentation des gares", target = "_blank")
            ),
            tags$li(
              tags$a(href = "https://www.data.gouv.fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2024/", "Accidents de vélo", target = "_blank")
            ),
            tags$li(
              tags$a(href = "https://openrouteservice.org/", "API openroute service", target = "_blank")
            ),
          ),
          
        )
      ),
      column(
        width = 6,
        tags$figure(
          tags$img(
            src = "01011032.jpg",
            width = "100%",
            height = "auto"
          ),
          tags$figcaption("Grand Colombier, 1534 m")
        ),
        hr(),
        h3("Contact"),
        p("Pour toute question ou suggestion, n'hésitez pas à me contacter :"),
        tags$ul(tags$li(
          tags$a(href = "mailto:mathieu.boully@hotmail.com", "mathieu.boully@hotmail.com")
        )),
        tags$ul(tags$li(
          tags$a(
            href = "https://www.linkedin.com/in/mathieuboully/",
            "LinkedIn",
            target = "_blank",
            tags$i(class = "bi bi-house-fill")
          )
        )),
        hr(),
        h3("Télécharger les données brutes"),
        downloadButton("download_station", tooltip(
          "Gares ferroviaires",
          HTML("Format .csv avec séparateur ';' et décimal '.'")
        ))
      )
    )
  ),
  # nav_menu(
  #   title = "À propos",
  #   icon = bs_icon("info-circle"),
  #   align = "right",
  #   nav_item(
  #     tags$a("99Bikes site web", href = "https://www.99bikes.com.au/", target = "_blank")
  #   ),
  #   nav_item(
  #     tags$a("GitHub", href = "https://github.com/mathieuboully/bicycle-sales", target = "_blank")
  #   )
  # ),
  footer = tags$sub(
    "© 2025",
    tags$a(href = "https://www.linkedin.com/in/mathieuboully/", "Mathieu Boully") ,
    " ∙ Cette application a été crée avec",
    tags$a("Shiny", href = "https://shiny.posit.co/", target = "_blank"),
    " ∙ ",
    tags$a("Code source GitHub", href = "https://github.com/mathieuboully/departement01", target = "_blank")
  )
)

server = function(input, output, session) {
  output$download_station <- downloadHandler(
    filename = function() {
      paste("station_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        stations_clean,
        file,
        row.names = FALSE,
        sep = ";",
        dec = ".",
        fileEncoding = "UTF-8"
      )
    }
  )
  
  selected_iso = reactive({
    if (input$iso_select %in% "10 minutes à vélo")
      iso_station = readRDS(file = file.path(path_data_output, "isochrone_station_bike_600.rds"))
    else if (input$iso_select %in% "20 minutes à vélo") {
      iso_station = readRDS(file = file.path(path_data_output, "isochrone_station_bike_1200.rds"))
    } else if (input$iso_select %in% "10 minutes en voiture") {
      iso_station = readRDS(file = file.path(path_data_output, "isochrone_station_car_600.rds"))
    } else if (input$iso_select %in% "20 minutes en voiture") {
      iso_station = readRDS(file = file.path(path_data_output, "isochrone_station_car_1200.rds"))
    } else {
      return(NULL)
    }
    return(iso_station)
    
  })
  
  output$mobility_map = renderLeaflet({
    # pal <- colorNumeric(c("#ffffff", "#194264"),, domain = socio_clean$ind)
    
    map = leaflet(options = leafletOptions(minZoom = 7, maxZoom = 14)) %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels,
                       options = providerTileOptions(opacity = 0.6)) %>%
      setView(
        lng = (4.9 + 5.9) / 2,
        lat = (45.95 + 46.5) / 2,
        zoom = 9
      ) %>%
      setMaxBounds(
        lng1 = 4.7,
        # limite ouest
        lat1 = 45.8,
        # limite sud
        lng2 = 6.7,
        # limite est
        lat2 = 46.5   # limite nord
      )
    iso_ls = selected_iso()
    for (i in iso_ls) {
      map = map %>%
        addPolygons(
          data = i$iso,
          weight = 2,
          dashArray = "6",
          color = "#ffffff",
          fillColor = "#ffffff",
          fillOpacity = 0.3
        ) %>%
        addMarkers(
          clusterId = "station",
          lng = i$iso$center[[1]][1],
          lat = i$iso$center[[1]][2],
          icon = markers["logo_sncf"],
          label = paste0("<strong>", i$station_name, "</strong>") %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
    
    map = map %>%
      addLegend(
        labels = c(paste("Gare à moins de ", input$iso_select)),
        color = c("#ffffff"),
        position = "bottomleft",
        opacity = 0.65
      )
    
    
    if (input$socio_var_select %in% "Nombre d'individus") {
      var = "Ind"
    } else {
      var = "Log_soc"
    }
    
    pal <- colorNumeric("YlOrRd", domain = socio_clean[[var]], na.color = "red")
    gradient_colors <- pal(seq(
      min(socio_clean[[var]], na.rm = TRUE),
      max(socio_clean[[var]], na.rm = TRUE),
      length.out = 10
    ))
    
    map = map %>%
      addHeatmap(
        data = socio_clean,
        lng = ~ lon,
        lat = ~ lat,
        intensity = socio_clean[[var]] / max(socio_clean[[var]], na.rm = TRUE),
        blur = 10,
        gradient = gradient_colors,
        radius = 30
      ) %>%
      addLegend(
        "bottomright",
        values = socio_clean[[var]],
        pal = pal,
        title = input$socio_var_select
      )
    
    return(map)
  })
  
  output$frequentation_map = renderLeaflet({
    radius_scale <- function(x) {
      scales::rescale(x, to = c(5, 20))
    }
    
    pal <- colorNumeric(palette = "YlOrRd",
                        # tu peux changer
                        domain = stations_clean$Total.Voyageurs.2024)
    
    stations_clean$freq_label = label_number(big.mark = " ", decimal.mark = ",")(stations_clean$Total.Voyageurs.2024)
    
    map = leaflet(options = leafletOptions(minZoom = 7, maxZoom = 14)) %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels,
                       options = providerTileOptions(opacity = 0.6)) %>%
      setView(
        lng = (4.9 + 5.9) / 2,
        lat = (45.95 + 46.5) / 2,
        zoom = 9
      ) %>%
      setMaxBounds(
        lng1 = 4.7,
        # limite ouest
        lat1 = 45.8,
        # limite sud
        lng2 = 6.7,
        # limite est
        lat2 = 46.5   # limite nord
      ) %>%
      addCircleMarkers(
        data = stations_clean,
        lng = ~ lon,
        lat = ~ lat,
        color = ~ pal(Total.Voyageurs.2024),
        radius = ~ radius_scale(Total.Voyageurs.2024),
        fillOpacity = 0.7,
        stroke = 10,
        label = ~ paste0(
          "<strong>",
          LIBELLE,
          "</strong><br>",
          "Voyageurs en 2024 : ",
          label_number(big.mark = " ", decimal.mark = ",")(stations_clean$Total.Voyageurs.2024)
        ) %>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "11px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        "bottomright",
        values = stations_clean$Total.Voyageurs.2024,
        pal = pal,
        title = "Nombre de voyageurs"
      )
    
    map
  })
  
  prop_cov_text_react = reactive({
    iso_ls = selected_iso()
    
    socio_clean = st_as_sf(socio_clean,
                           coords = c("lon", "lat"),
                           crs = 4326)
    iso_list = lapply(iso_ls, function(x)
      x$iso)
    iso_all <- do.call(rbind, iso_list)
    iso_union <- st_union(iso_all)
    df1_sf <- socio_clean %>%
      mutate(in_iso = st_intersects(., iso_union, sparse = FALSE)[, 1])
    pop_tot = sum(socio_clean$Ind, na.rm = TRUE)
    pop_cov = sum(socio_clean$Ind[df1_sf$in_iso], na.rm = TRUE)
    prop_cov = (pop_cov / pop_tot) * 100
    
    return(paste0(as.character(round(prop_cov, 0)), "%"))
  })
  
  output$pop_cov_box = renderText ({
    value_react = prop_cov_text_react()
    
    return(paste0(value_react, ' (', input$iso_select, ')'))
  })
  
  output$passengers_evolution = renderPlotly({
    df <- stations_clean %>%
      pivot_longer(
        cols = starts_with("Total.Voyageurs."),
        # toutes les colonnes d'année
        names_to = "Year",
        # nom de la nouvelle colonne pour les années
        names_prefix = "Total.Voyageurs.",
        # enlever le "X" devant l'année
        values_to = "Ind"  # nom de la colonne pour les valeurs
      ) %>%
      filter(!grepl("Non\\.voyageurs", Year)) %>%
      as.data.frame(.)
    
    df_top_10 = df %>%
      filter(Year == 2024) %>%
      arrange(desc(Ind)) %>%
      slice_head(n = 10) %>%
      left_join(df, by = "LIBELLE")
    
    plot_ly(
      df_top_10,
      x = ~ Year.y,
      y = ~ Ind.y,
      color = ~ as.factor(LIBELLE),
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = "spline"),
      text = ~ as.factor(LIBELLE),
      hovertemplate = paste(
        "%{text}<br>",
        "%{yaxis.title.text}: ",
        label_number(big.mark = " ", decimal.mark = ",")(df_top_10$Ind.y),
        "<br>",
        "%{xaxis.title.text}: %{x:.0f}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        font = list(size = 12),
        title = list(text = "<b>Top 10 des gares les plus fréquentées", font = list(
          size = 15, color = "grey"
        )),
        xaxis = list(
          title = "Année",
          zeroline = F,
          zerolinecolor = 'black',
          zerolinewidth = 2,
          showgrid = T,
          showticklabels = TRUE,
          fixedrange = F,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        yaxis = list(
          title = "Nombre de passagers",
          zeroline = F,
          zerolinecolor = 'black',
          zerolinewidth = 2,
          showgrid = F,
          showticklabels = TRUE,
          fixedrange = F,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        hoverlabel = list(font = list(color = "black")),
        showlegend = T,
        legend = list(
          title = list(text = '<b>Gare</b>'),
          orientation = 'v',
          x = 1,
          y = .9,
          itemsizing = 'constant',
          font = list(color = "grey"),
          bgcolor = "#E2E2E2",
          bordercolor = "#FFFFFF",
          borderwidth = 0
        ),
        margin = list(
          t = 40,
          b = 40,
          l = 40,
          r = 40
        )
      )
  })
  
  output$crash_evolution = renderPlotly({
    bicycle_crash_clean_agg = bicycle_crash_clean %>%
      group_by(an, grav_label) %>%
      summarise(nb_crash = n(), .groups = "drop")
    
    plot_ly(
      bicycle_crash_clean_agg,
      x = ~ an,
      y = ~ nb_crash,
      color = ~ as.factor(grav_label),
      type = 'scatter',
      mode = 'lines+markers',
      fill = 'tozeroy',
      line = list(shape = "spline"),
      text = ~ as.factor(grav_label),
      hovertemplate = paste(
        "%{text}<br>",
        "%{yaxis.title.text}: %{y:.0f}<br>",
        "%{xaxis.title.text}: %{x:.0f}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        font = list(size = 12),
        title = list(text = "<b>Nombre d'accidents à vélo", font = list(
          size = 15, color = "grey"
        )),
        xaxis = list(
          title = "Année",
          zeroline = F,
          zerolinecolor = 'black',
          zerolinewidth = 2,
          showgrid = T,
          showticklabels = TRUE,
          fixedrange = F,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        yaxis = list(
          title = "Nombre d'accidents",
          zeroline = F,
          zerolinecolor = 'black',
          zerolinewidth = 2,
          showgrid = F,
          showticklabels = TRUE,
          fixedrange = F,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        hoverlabel = list(font = list(color = "black")),
        showlegend = T,
        legend = list(
          title = list(text = '<b>Gravité</b>'),
          orientation = 'v',
          x = 1,
          y = .9,
          itemsizing = 'constant',
          font = list(color = "grey"),
          bgcolor = "#E2E2E2",
          bordercolor = "#FFFFFF",
          borderwidth = 0
        ),
        margin = list(
          t = 40,
          b = 40,
          l = 40,
          r = 40
        )
      )
  })
  
  output$table_crash = renderDataTable({
    DT::datatable(
      bicycle_crash_clean %>%
        group_by(jour) %>%
        summarise(n_crash = n()) %>%
        arrange(desc(n_crash)),
      rownames = FALSE,
      colnames = c("Jour", "Nombre d'accidents"),
      options = list(
        searching = F,
        lengthChange = F,
        # pageLength = 5,
        # lengthMenu = c(5, 10, 15, 20),
        # scrollY = 300, scrollCollapse = TRUE
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
      )
    )
  })
  
  selected_rent = reactive({
    if (input$sel_rent %in% "Maison individuelle") {
      return(rent_ls$mai)
    } else if (input$sel_rent %in% "Appartement T1-T2") {
      return(rent_ls$app3)
    } else if (input$sel_rent %in% "Appartement T3+") {
      return(rent_ls$app12)
    }
  })
  
  output$house_map = renderLeaflet({
    df = selected_rent()
    
    df = df %>%
      mutate(INSEE_C = sub("^0+", "", INSEE_C))
    
    df = df %>%
      left_join(loc_insee, by = c("INSEE_C" = "code_commune_INSEE"))
    
    radius_scale <- function(x) {
      scales::rescale(x, to = c(2, 10))
    }
    
    pal <- colorNumeric(palette = "YlOrRd", # tu peux changer
                        domain = df$loypredm2)
    
    map = leaflet(options = leafletOptions(minZoom = 7, maxZoom = 14)) %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels,
                       options = providerTileOptions(opacity = 0.6)) %>%
      setView(
        lng = (4.9 + 5.9) / 2,
        lat = (45.95 + 46.5) / 2,
        zoom = 9
      ) %>%
      setMaxBounds(
        lng1 = 4.7,
        # limite ouest
        lat1 = 45.8,
        # limite sud
        lng2 = 6.7,
        # limite est
        lat2 = 46.5   # limite nord
      ) %>%
      addCircleMarkers(
        data = df,
        lng = ~ longitude,
        lat = ~ latitude,
        color = ~ pal(loypredm2),
        radius = ~ radius_scale(loypredm2),
        fillOpacity = 0.6,
        stroke = 10,
        label = paste0(
          "<strong>",
          df$nom_commune,
          "</strong><br>",
          input$sel_rent,
          " : ",
          scales::unit_format(unit = " €/m2")(round(df$loypredm2, 0))
        ) %>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "11px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        "bottomright",
        values = df$loypredm2,
        pal = pal,
        title = "Loyer au m2"
      )
    
    map
  })
  
  output$famd_ind_bc = renderPlotly({
    df_bc = readRDS(file = file.path("./data/processed", "bicycle_crash.rds"))[1:1000, ]
    
    act_feat = c("agg", "int", "col", "lum", "age")
    sup_feat = c("grav")
    
    res_df_bc_impute = missMDA::imputeFAMD(df_bc[, act_feat], ncp = 3)
    
    res_famd <- FAMD(
      base = res_df_bc_impute$completeObs,
      ncp = 2,
      sup.var = NULL,
      graph = F
    )
    
    x_var <- res_famd$ind$coord[, 1]
    y_var <- res_famd$ind$coord[, 2]
    color_var <- df_bc$grav_label
    text_var <- df_bc$grav_label
    
    min_xaxis <- min(x_var)
    max_xaxis <- max(x_var)
    
    min_yaxis <- min(y_var)
    max_yaxis <- max(y_var)
    
    fig <- plot_ly(
      x = x_var,
      y = y_var,
      color = color_var,
      type = "scatter",
      mode = "markers",
      text = text_var,
      hovertemplate = paste(
        "Gravité : %{text}<br>",
        "%{xaxis.title.text}: %{x:.1f}<br>",
        "%{yaxis.title.text}: %{y:.1f}",
        "<extra></extra>"
      )
    ) %>% layout(
      title = list(text = '<b>Causes de la gravité des accidents</b></sup>', font = list(
        size = 15, color = "grey"
      )),
      xaxis = list(
        title = "Dim 1",
        zeroline = T,
        zerolinecolor = 'grey',
        zerolinewidth = 1,
        showgrid = F,
        showticklabels = F,
        fixedrange = F,
        color = "grey",
        tickfont = list(color = "grey"),
        range = c(min_xaxis, max_xaxis)
      ),
      yaxis = list(
        title = "Dim 2",
        zeroline = T,
        zerolinecolor = 'grey',
        zerolinewidth = 1,
        showgrid = F,
        showticklabels = F,
        fixedrange = F,
        color = "grey",
        tickfont = list(color = "grey"),
        range = c(min_yaxis, max_yaxis)
      ),
      hoverlabel = list(font = list(color = "black")),
      showlegend = T,
      legend = list(
        title = list(text = '<b>Gravité</b>'),
        orientation = 'v',
        x = 1,
        y = .1,
        itemsizing = 'constant',
        font = list(color = "grey"),
        bgcolor = "#E2E2E2",
        bordercolor = "#FFFFFF",
        borderwidth = 0
      ),
      margin = list(
        t = 40,
        b = 40,
        l = 40,
        r = 40
      )
    )
    
    for (i in seq(5)) {
      fig <- fig %>%
        add_segments(
          x = 0,
          xend = res_famd$var$coord[i, 1],
          y = 0,
          yend = res_famd$var$coord[i, 2],
          line = list(color = 'black'),
          inherit = FALSE,
          showlegend = FALSE
        ) %>%
        add_annotations(
          x = res_famd$var$coord[i, 1],
          y = res_famd$var$coord[i, 2],
          ax = 0,
          ay = 0,
          text = act_feat[i],
          xanchor = 'center',
          yanchor = 'bottom'
        )
    }
    
    fig
  })
}

# Launching app
shinyApp(ui = ui, server = server)
