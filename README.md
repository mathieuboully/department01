# Analyse du territoire de l’Ain

## Objectif

Cette application interactive [R Shiny](https://shiny.posit.co/) permet d’analyser l’accessibilité aux gares, les accidents de vélo et les loyers dans le département de l’Ain en les reliant à des variables socio-démographiques telles que la densité de population.

- Explorer la **répartition des loyers** à l’échelle communale pour le trimestre 2023, selon le type de logement :
  - Maisons individuelles
  - Appartements T1, T2 et T3+
- Analyser la **mobilité et la sécurité à vélo**, notamment l’accessibilité aux gares et la localisation des accidents, en fonction de variables socio-démographiques (densité de population, revenu, âge).

![](www/app_screen.png)

## Contenu du projet

- `app.R` : fichier principal de l’application Shiny
- `data/` : dossiers contenant les jeux de données
  - **Loyers** : [Open Data - Carte des loyers 2023](https://www.data.gouv.fr/datasets/carte-des-loyers-indicateurs-de-loyers-dannonce-par-commune-en-2023/)
  - **Accidents vélo** : fichier CSV ou GeoJSON
- `www/` : fichiers CSS, images ou icônes
- `utils.R` : fonctions R pour le traitement et la visualisation (optionnel)
- `README.md` : ce fichier explicatif

---

## 🖥️ Installation et lancement

1. Installer R et RStudio.
2. Installer les packages requis :

```r
install.packages(c(
  "shiny", "leaflet", "dplyr", "ggplot2", 
  "scales", "DT", "stringr", "shinydashboard", "shinyWidgets"
))
