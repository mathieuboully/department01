# -----------------------------------------------------------------------------
# File name : constants.R
# Description : project variables
# Author : Boully Mathieu
# Date : 2025-11-05
# -----------------------------------------------------------------------------

CONSTS = list(
  ui = list(
  app_name = "Analyse du territoire de l’Ain",
  app_version = "1.0.0",
  sidebar_title = "",
  sidebar_desc = "Cette application interactive permet d’analyser l’accessibilité aux gares et les accidents de vélo dans le département de l’Ain en les reliant à des variables socio-démographiques telles que la densité de population.",
  sidebar_logo = "logo.png",
  sidebar_logo_width= "70%",
  sidebar_logo_link= "https://www.ain.fr/"
  ),
  
  # Map
  map = list(
    markers = list(
      logo_sncf = list(
        file = "logo_sncf.png",
        width = 15
      )
    ),
    zoom = list(min = 7, max = 14, default = 9),
    center = list(
      lng = c(4.9, 5.9),
      lat = c(45.95, 46.5)
    ),
    bounds = list(
      lng1 = 4.7, lat1 = 45.8,
      lng2 = 6.7, lat2 = 46.5
    ),
    tiles = list(
      base = list(provider = "Stadia.AlidadeSmoothDark", opacity = 1),
      overlay = list(provider = "CartoDB.VoyagerOnlyLabels", opacity = 0.6)
    )
  ),
  
  # Footer
  footer = list(
    year = list(text = "© 2025",
                      url = ""),
    author     = list(
      text = "Mathieu Boully",
      url = "https://www.linkedin.com/in/mathieuboully/"
    ),
    build_wth      = list(
      text = "Shiny",
      url  = "https://shiny.posit.co/"
    ),
    github     = list(
      text = "Code source GitHub",
      url  = "https://github.com/mathieuboully/departement01"
    ),
    license    = list(
      text = "MIT License",
      url  = "https://opensource.org/licenses/MIT"
    ),
    text_sep   = " ∙ "
  ),
  
  # Contacts
  contacts <- list(
    email = list(
      name = "mathieu.boully@hotmail.com",
      href = "mailto:mathieu.boully@hotmail.com"
    ),
    linkedin = list(
      name = "LinkedIn",
      href = "https://www.linkedin.com/in/mathieuboully/"
    )
  ),
  
  
  # Relative paths
  path_data_raw = "./data/raw",
  path_data_processed = "./data/processed",
  path_image = "./www",
  
  # Colors
  color_ramp_palette = c("#ebebeb", "#194264"),
  
  theme = list(
    bootswatch = "minty",
    bg = "#ffffff",
    fg = "#000000",
    primary = "#194264",
    secondary = "#000000",
    success = "#ebebeb",
    navbar_bg = "#194264",
    base_font_google = "Arimo",
    code_font_google = "Arimo",
    heading_font_google = "Phudu"
  ),
  
  metrics_list <- list(
    revenue = list(
      id = "revenue",
      title = "Sales Revenue",
      currency = "$",
      category = "sales",
      legend = "Revenue"
    ),
    cost = list(
      id = "cost",
      title = "Production Costs",
      currency = "$",
      category = "production",
      legend = "Cost",
      invert_colors = TRUE
    ),
    profit = list(
      id = "profit",
      title = "Profit",
      currency = "$",
      category = "sales",
      legend = "Profit"
    ),
    orders_count = list(
      id = "orders_count",
      title = "Orders",
      currency = NULL,
      category = "sales",
      legend = "Number of orders"
    ),
    produced_items = list(
      id = "produced_items",
      title = "Produced Items",
      currency = NULL,
      category = "production",
      legend = "Produced items"
    ),
    users_active = list(
      id = "users_active",
      title = "Active Users",
      currency = NULL,
      category = "users",
      legend = "Active users"
    ),
    users_dropped_out = list(
      id = "users_dropped_out",
      title = "Dropped Out Users",
      currency = NULL,
      category = "users",
      legend = "Dropped out users",
      invert_colors = TRUE
    ),
    complaints_opened = list(
      id = "complaints_opened",
      title = "Opened Complaints",
      currency = NULL,
      category = "complaints",
      legend = "Opened complaints",
      invert_colors = TRUE
    ),
    complaints_closed = list(
      id = "complaints_closed",
      title = "Closed Complaints",
      currency = NULL,
      category = "complaints",
      legend = "Closed complaints"
    )
  ),
  
  map_metrics <- c(
    "revenue",
    "orders_count",
    "users_active",
    "users_dropped_out",
    "complaints_opened",
    "complaints_closed"
  ),
  
  prev_time_range_choices <- list("Previous Year" = "prev_year", "Previous Month" = "prev_month"),
  
  appsilonLogo <- HTML(
    "
  <svg class='logo-svg' viewBox='0 0 660.52 262.96'>
    <use href='assets/icons/icons-sprite-map.svg#appsilon-logo'></use>
  </svg>
"
  ),
  
  shinyLogo <- HTML(
    "
  <svg class='logo-svg' viewBox='0 0 100 68'>
    <use href='assets/icons/icons-sprite-map.svg#shiny-logo'></use>
  </svg>
"
  ),
  
  colors <- list(
    white = "#FFF",
    black = "#0a1e2b",
    primary = "#0099F9",
    secondary = "#15354A",
    ash = "#B3B8BA",
    ash_light = "#e3e7e9"
  )
)
