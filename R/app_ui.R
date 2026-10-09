#' Interface utilisateur pour la cartographie d'incidence
#'
#' @import shiny
#' @import leaflet
#' @importFrom bslib page_sidebar sidebar card
#' @importFrom shinyjs useShinyjs
#' @export
app_ui <- function() {

  bslib::page_sidebar(
    title = "Cartographie d'incidence des cancers",
    shinyjs::useShinyjs(),
    
    sidebar = bslib::sidebar(
      title = "Configuration",
      
      shiny::selectInput(
        inputId = "sel_cancer", 
        label = "Localisation :", 
        choices = localisations[1] # Chargé dynamiquement depuis le serveur ou une donnée du package
      ),
      
      shiny::radioButtons(
        inputId = "sel_sex", 
        label = "Sexe :", 
        choices = c("Hommes" = 1, "Femmes" = 2),
        selected = sexe_choices[1],
        inline = 1
      ),
      shiny::radioButtons(
        inputId = "sel_indic", 
        label = "Incidence ou proxy :", 
        choices = indic_choices,
        selected = indic_choices[1],
        inline = 1
      ),

      shiny::radioButtons(
        inputId = "sel_prox", 
        label = "Proxy :", 
        choices = prox_choices,
        selected = prox_choices[1]
      ),
      
      shiny::radioButtons(
        inputId = "sel_var",
        label = "Indicateur à afficher :",
        choices = c(
          "Moyenne" = "pm",
          ## "Médiane" = "median",
          ## "Borne inférieure IC" = "low",
          ## "Borne supérieure IC" = "up",
          "Probabilité" = "prob"
        ),
        selected = "pm"
      ),
      
      shiny::hr(),
      
      # Légende HTML personnalisée si nécessaire
      shiny::tags$div(
        style = "background-color: #f8f9fa; padding: 12px; border-radius: 6px; border: 1px solid #e9ecef;",
        shiny::tags$h6("Information", style = "margin-top: 0; font-weight: bold; color: #495057;"),
        shiny::tags$p("Sélectionnez une localisation, un sexe et un proxy pour mettre à jour la carte d'incidence.", style = "font-size: 0.85rem; color: #6c757d; margin: 0;")
      )
    ),
    
    # Zone principale : carte Leaflet
    bslib::card(
      full_screen = TRUE,
      style = "padding: 0;", # Supprime les marges internes autour de la carte
      leaflet::leafletOutput("map_incidence", height = "100%")
    )
  )
}
