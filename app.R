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
base::source("./global.R", local = F)
base::source("./config/constants.R", local = F)

# Get the last access time of the app.R file
last_update = base::file.info(file.path("app.R"))$atime

col_pal = grDevices::colorRampPalette(CONSTS$color_ramp_palette)

markers <- setNames(lapply(names(CONSTS$map$markers), function(name) {
  leaflet::makeIcon(
    iconUrl   = file.path(CONSTS$path_image, CONSTS$map$markers[[name]]$file),
    iconWidth = CONSTS$map$markers[[name]]$width
  )
}), names(CONSTS$map$markers))
markers = do.call(leaflet::iconList, markers)

# Load data
bicycle_crash_clean = readRDS(file = file.path(CONSTS$path_data_processed, "bike_crash.rds"))
stations_clean = readRDS(file = file.path(CONSTS$path_data_processed, "stations.rds"))
socio_clean = readRDS(file = file.path(CONSTS$path_data_processed, "socio.rds"))
rent_ls = readRDS(file = file.path(CONSTS$path_data_processed, "rent.rds"))
loc_insee = readRDS(file = file.path(CONSTS$path_data_processed, "loc_insee.rds"))

ui = page_navbar(
  title = CONSTS$ui$app_name,
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = CONSTS$theme$bootswatch,
    bg = CONSTS$theme$bg,
    fg = CONSTS$theme$fg,
    primary = CONSTS$theme$primary,
    secondary = CONSTS$theme$secondary,
    success = CONSTS$theme$success,
    navbar_bg = CONSTS$theme$navbar_bg,
    base_font = font_google(CONSTS$theme$base_font_google),
    code_font = font_google(CONSTS$theme$code_font_google),
    heading_font = font_google(CONSTS$theme$heading_font_google)
  ),
  sidebar = sidebar(
    nav_panel(title = CONSTS$ui$sidebar_title),
    # accordion(
    #   open = F,
    #   accordion_panel("Gares et accessibilité", icon = bsicons::bs_icon("train-front-fill"))
    # ),
    tags$p(CONSTS$ui$sidebar_desc),
    tags$a(
      tags$img(
        src = CONSTS$ui$sidebar_logo,
        width = CONSTS$ui$sidebar_logo_width,
        height = "auto"
      ),
      href = CONSTS$ui$sidebar_logo_link,
      target = "_blank"
    ),
    tags$sub(paste(
      "Mis à jour le", format(last_update, "%d %B %Y")
    )),
    tags$sub(paste("Version", CONSTS$ui$app_version))
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
      card(full_screen = TRUE, 
           layout_sidebar(
             fillable = TRUE,
             border = T,
             sidebar = sidebar(
               position = "right",
               width = 300,
               open = T,
               selectInput(
                 inputId = "grav_select",
                 label = "Gravité",
                 c(
                   unique(bicycle_crash_clean$grav_label)
                 ),
                 multiple = T,
                 selected = unique(bicycle_crash_clean$grav_label)
               )
             ),
           plotly::plotlyOutput("season_plot_crash"))),
      card(full_screen = TRUE, plotly::plotlyOutput("famd_ind_bc")),
      card(dataTableOutput("table_crash"), card_footer(
        tags$h5('Jours de la semaine les plus à risque')
      )),
      col_widths = c(6, 6, 12, 12)
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
        tags$ul(lapply(contacts, function(contact) {
          tags$li(tags$a(href = contact$href, target = "_blank", contact$name))
        })),
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
    paste0(CONSTS$footer$year$text),
    CONSTS$footer$text_sep,
    tags$a(href = CONSTS$footer$author$url, CONSTS$footer$author$text),
    CONSTS$footer$text_sep,
    "Cette application a été créée avec ",
    tags$a(
      href = CONSTS$footer$build_wth$url,
      CONSTS$footer$build_wth$text,
      target = "_blank"
    ),
    CONSTS$footer$text_sep,
    tags$a(
      href = CONSTS$footer$github$url,
      CONSTS$footer$github$text,
      target = "_blank"
    ),
    CONSTS$footer$text_sep,
    "Licence ",
    tags$a(
      href = CONSTS$footer$license$url,
      CONSTS$footer$license$text,
      target = "_blank"
    )
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
      iso_station = readRDS(file = file.path(
        CONSTS$path_data_processed,
        "isochrone_station_bike_600.rds"
      ))
    else if (input$iso_select %in% "20 minutes à vélo") {
      iso_station = readRDS(file = file.path(
        CONSTS$path_data_processed,
        "isochrone_station_bike_1200.rds"
      ))
    } else if (input$iso_select %in% "10 minutes en voiture") {
      iso_station = readRDS(file = file.path(
        CONSTS$path_data_processed,
        "isochrone_station_car_600.rds"
      ))
    } else if (input$iso_select %in% "20 minutes en voiture") {
      iso_station = readRDS(file = file.path(
        CONSTS$path_data_processed,
        "isochrone_station_car_1200.rds"
      ))
    } else {
      return(NULL)
    }
    return(iso_station)
    
  })
  
  output$mobility_map = renderLeaflet({
    center_lng <- mean(CONSTS$map$center$lng)
    center_lat <- mean(CONSTS$map$center$lat)
    
    map <- leaflet(options = leafletOptions(
      minZoom = CONSTS$map$zoom$min,
      maxZoom = CONSTS$map$zoom$max
    )) %>%
      addProviderTiles(providers[[CONSTS$map$tiles$base$provider]],
                       options = providerTileOptions(opacity = CONSTS$map$tiles$base$opacity)) %>%
      addProviderTiles(providers[[CONSTS$map$tiles$overlay$provider]],
                       options = providerTileOptions(opacity = CONSTS$map$tiles$overlay$opacity)) %>%
      setView(lng = center_lng,
              lat = center_lat,
              zoom = CONSTS$map$zoom$default) %>%
      setMaxBounds(
        lng1 = CONSTS$map$bounds$lng1,
        lat1 = CONSTS$map$bounds$lat1,
        lng2 = CONSTS$map$bounds$lng2,
        lat2 = CONSTS$map$bounds$lat2
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
    df_bc = readRDS(file = file.path(CONSTS$path_data_processed, "bicycle_crash.rds"))[1:1000, ]
    
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
  
  output$season_plot_crash = renderPlotly({
    df = selected_bicycle_crash() %>%
      mutate(
        date = as.Date(date),
        year = year(date),
        month = month(date, label = TRUE, abbr = TRUE)
      )
    
    df_monthly <- df %>%
      group_by(month) %>%
      summarise(crash = n()) %>%
      arrange(month)
    
    plot_ly(
      df_monthly,
      x = ~ month,
      y = ~ crash,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = "spline"),
      hovertemplate = paste(
        "%{yaxis.title.text}: ",
        label_number(big.mark = " ", decimal.mark = ",")(df_monthly$crash),
        "<br>",
        "%{xaxis.title.text}: %{x}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        font = list(size = 12),
        title = list(text = "<b>Saisonnalité des accidents", font = list(
          size = 15, color = "grey"
        )),
        xaxis = list(
          title = "Mois",
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
        showlegend = F,
        margin = list(
          t = 40,
          b = 40,
          l = 40,
          r = 40
        )
      )
  })
  
  selected_bicycle_crash = reactive({
    req(input$grav_select)
    
    all_values <- unique(bicycle_crash_clean$grav_label)
    
    if (setequal(input$grav_select, all_values)) {
      return(bicycle_crash_clean)
    }
    
    df = bicycle_crash_clean %>% 
      filter(grav_label %in% input$grav_select)
    return(df)
  })
  
}

# Launching app
shinyApp(ui = ui, server = server)
