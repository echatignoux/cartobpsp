#' Interface utilisateur pour la cartographie d'incidence
#'
#' @import shiny
#' @import leaflet
#' @importFrom bslib page_sidebar sidebar card
#' @importFrom shinyjs useShinyjs
#' @export
app_ui <- function() {

  localisations <- .pkg_env$localisations 
  sexe_choices  <- .pkg_env$sexe_choices
  indic_choices <- .pkg_env$indic_choices
  prox_choices  <- .pkg_env$prox_choices 
  
  bslib::page_sidebar(
    title = "Cartographie d'incidence des cancers",
    shinyjs::useShinyjs(),

    # --- CSS de compactage pour la sidebar ---
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* Réduction de la largeur de la sidebar et de la police globale */
        .bslib-sidebar-layout > .sidebar {
          font-size: 0.8rem !important;
          padding: 12px 10px !important;
        }
        /* Resserrage des titres et labels de contrôles */
        .sidebar .form-label, .sidebar label {
          font-size: 0.78rem !important;
          font-weight: 600;
          margin-bottom: 2px !important;
        }
        /* Compactage des radioButtons et selectInputs */
        .sidebar .form-check {
          min-height: 1.1rem !important;
          margin-bottom: 2px !important;
          padding-left: 1.3em !important;
        }
        .sidebar .form-check-input {
          margin-top: 0.15em !important;
        }
        .sidebar .selectize-input {
          padding: 4px 8px !important;
          min-height: 28px !important;
          font-size: 0.8rem !important;
        }
        .sidebar .selectize-dropdown {
          font-size: 0.8rem !important;
        }
        /* Separation horizontale fine */
        .sidebar hr {
          margin: 8px 0 !important;
          border-color: #e2e8f0;
        }
        /* Accordéon compact */
        .sidebar .accordion-button {
          padding: 6px 8px !important;
          font-size: 0.78rem !important;
          font-weight: 600;
        }
        .sidebar .accordion {
          margin-bottom: 4px !important;
        }
        .sidebar .accordion-body {
          padding: 6px 4px !important;
        }
      "))
    ),
    sidebar = bslib::sidebar(
      width = 340,
      ## bslib::accordion(
      ##   open = FALSE,
      ##   bslib::accordion_panel(
      ##     title = "Définitions",
      ##     icon = shiny::icon("book"),
      ##     tags$ul(
      ##       tags$li(tags$strong(htmltools::HTML("&theta;")), " : risque de cancer de la zone géographique, ajusté sur l'âge, par rapport à la moyenne des zones (référence = 1)."),
      ##       tags$li(tags$strong(htmltools::HTML("P<sub>d</sub>(&theta;)")), " : Probabilité de direction (", htmltools::HTML("max(P(&theta; > 1), P(&theta; < 1)) ), variant de 0.5 (distribution de &theta; également répartie autour de 1) à 1.0 (distribution postérieure de &theta; ne contient pas 1)."))
      ##     )
      ##   )
      ## ),
      shiny::selectInput(
        inputId = "sel_cancer", 
        label = "Localisation ", 
        choices = localisations,
        selected = 1
      ),
      
      shiny::radioButtons(
        inputId = "sel_sex", 
        label = "Sexe ", 
        choices = c("Hommes" = 1, "Femmes" = 2),
        selected = 1,
        inline = 1
      ),
      shiny::radioButtons(
        inputId = "sel_indic", 
        label = "Incidence ou proxy ", 
        choices = indic_choices,
        selected = indic_choices[1],
        inline = 1
      ),

      shiny::radioButtons(
        inputId = "sel_prox", 
        label = "Proxy ", 
        choices = prox_choices,
        selected = prox_choices[1],
        inline = 1
      ),
      shiny::radioButtons(
        inputId = "sel_var",
        label = div(
          class = "d-flex align-items-center justify-content-between",
          "Indicateur  ",
          bslib::tooltip(
            shiny::icon("circle-question", class = "text-muted cursor-pointer"),
            # Contenu enrichi avec une largeur forcée (ex: 320px)
            htmltools::tags$div(
              style = "max-width: 600px; text-align: left; padding: 4px;",
              htmltools::tags$p(
                class = "mb-2", 
                htmltools::HTML("<b>&theta;</b> : Risque de cancer ajusté sur l'âge, par rapport à la moyenne des zones (référence = 1).")
              ),
              htmltools::tags$p(
                class = "mb-0", 
                htmltools::HTML("<b>P<sub>d</sub>(&theta;)</b> : probabilité de détection - max(P(&theta; > 1), P(&theta; < 1)), variant de 0.5 (distribution postérieure de &theta; également répartie autour de 1) à 1.0 (distribution postérieure de &theta; ne contient pas 1).")
              )
            ),
            # Options Bootstrap pour accepter le HTML et styler le fond en gris clair
            options = list(
              html = TRUE,
              # Utilisation des classes Bootstrap pour un fond gris clair élégant
              template = '<div class="tooltip" role="tooltip"><div class="tooltip-arrow"></div><div class="tooltip-inner bg-light text-dark border shadow-sm" style="max-width: 500px; text-align: left;"></div></div>'
            )
          )
        ),
        choices = c("Risque - θ" = "pm", 
                    "Probabilité - Pd(θ)" = "prob"),
      selected = "pm",
        inline = TRUE
      ),
      ## shiny::hr(),
      # 2. Table réductible des paramètres du modèle (ALD, PMSI, AUP)
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Paramètres de calibration",
          shiny::uiOutput("ui_param_table")
        )
      ),

      ## shiny::hr(),
      # Légende HTML personnalisée si nécessaire
      shiny::uiOutput("ui_legend_box"),
      waiter::use_waiter()
      
    ),
    
    # Zone principale : carte Leaflet
    bslib::card(
      full_screen = TRUE,
      style = "padding: 0;",
      # Ajout d'un id à la card pour piloter son état de chargement si besoin, 
      # ou utilisation directe du output de la carte
      leaflet::leafletOutput("map_incidence", height = "100%")
    )
  )
}
