#' Logique Serveur pour la cartographie d'incidence
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @import shiny
#' @import leaflet
#' @importFrom dplyr filter select collect tbl left_join pull
#' @importFrom duckdb duckdb dbConnect dbDisconnect
#' @importFrom rlang sym
#' @export
app_server <- function(input, output, session) {
  options(leaflet.preferCanvas = TRUE)
  # Accès lazy à la table DuckDB
  dt_inc_tbl <- .pkg_env$dt_inc
  dt_par <- .pkg_env$dt_par
  shap_geo <- .pkg_env$shap_geo
  shap_RA <- .pkg_env$shap_RA
  loc_sexe <- .pkg_env$loc_sexe
  
  # --- Mise à jour des sexes disponibles en fonction de la localisation choisie ---
  shiny::observeEvent(input$sel_cancer, {
    req(input$sel_cancer)
    
    # 1. Trouver les sexes valides pour ce cancer précis dans ta table de métadonnées
    sexes_dispo <- loc_sexe %>%
      dplyr::filter(loc == !!input$sel_cancer) %>%
      dplyr::pull(sexe) %>%
      unique()
    
    # 2. Définir l'ensemble de base et filtrer selon ce qui existe pour ce cancer
    all_sexe_choices <- c("Homme" = 1, "Femme" = 2)
    sexe_choices <- all_sexe_choices[all_sexe_choices %in% sexes_dispo]
    
    # 3. Conserver la sélection courante si elle est toujours dispo pour ce cancer, sinon prendre la 1ère
    current_sex <- isolate(input$sel_sex)
    selected_sex <- if (!is.null(current_sex) && current_sex %in% sexe_choices) current_sex else sexe_choices[1]
    
    # 4. Mettre à jour les boutons radio du sexe
    shiny::updateRadioButtons(
      session,
      "sel_sex",
      choices = sexe_choices,
      selected = selected_sex,
      inline = TRUE
    )
  }, ignoreInit = FALSE)
  
  # --- 2. Légendes Leaflet ---
  output$ui_legend_box <- shiny::renderUI({
    shiny::req(input$sel_var)

    if (input$sel_var == "pm") {
      # --- Cas 1 : SIR (Echelle linéaire) ---
      ## pal_sir <- leaflet::colorNumeric(
      ##   palette = "RdYlGn", 
      ##   domain = c(0.7, 1.3), 
      ##   reverse = TRUE
      ## )
      
      render_html_legend(
        title = "Risque θ",
        palette = pal_sir,
        domain = c(0.7, 1.3),
        ticks = c(0.7, 0.85, 1.0, 1.15, 1.3),
        format_fun = function(x) sprintf("%.2f", x),
        trans_fun = identity
      )

    } else {
      # --- Cas 2 : Probabilité (Echelle non linéaire) ---
      pal_proba <- leaflet::colorNumeric(
        palette = "BuPu", 
        domain = c(0, 1)
      )
      
      render_html_legend(
        title = htmltools::HTML("P(&theta; &ne; 1)"),
        palette = pal_proba,
        domain = c(0, 1),
        ticks = c(0.5, 0.7, 0.8, 0.9, 0.95, 1.0),
        format_fun = function(x) sprintf("%.2f", x),
        trans_fun = trans_proba # Ta fonction de transformation [0, 1]
      )
    }
  })

  # --- 3. Rendu du tableau de paramètres ---
  output$ui_param_table <- shiny::renderUI({
    shiny::req(input$sel_cancer, input$sel_sex)

    df_par_sub <- dt_par %>%
      dplyr::filter(
        loc == !!as.numeric(input$sel_cancer),
        sexe == !!as.numeric(input$sel_sex)
      ) %>%
      dplyr::collect()

    build_param_table(df_par_sub)
  })
  
  # --- 4. Carte de base Leaflet ---
  output$map_incidence <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$Stadia.StamenTonerLite) %>%
      leaflet::setView(lng = 2.45, lat = 46.5, zoom = 6)
  })

  # --- 5. Extractions des données et mise à jour de la carte ---
  # Extraction réactive depuis DuckDB
  filtered_data <- shiny::reactive({
    shiny::req(input$sel_cancer, input$sel_sex, input$sel_prox, input$sel_var, input$sel_indic)

    dt_inc_tbl %>%
      dplyr::filter(
        loc == !!input$sel_cancer,
        sexe == !!as.numeric(input$sel_sex),
        prox == !!input$sel_prox,
        comp == !!input$sel_indic
      ) %>%
      dplyr::mutate( val = !!rlang::sym(input$sel_var)) %>% 
      dplyr::select(
        geo, 
        RA,
        val,
        pm,
        prob,
        low, 
        up,
        E
      ) %>%
      dplyr::collect()
  })

  # Mise à jour réactive des polygones via leafletProxy
  shiny::observeEvent(filtered_data(), {

    waiter::waiter_show(
      id = "map_incidence", 
      html = waiter::spin_fading_circles(), # ou spin_loaders(6)
      color = "rgba(255, 255, 255, 0.7)"    # Fond semi-transparent élégant
    )
    # S'assure qu'il se masque quoiqu'il arrive à la fin du bloc
    on.exit(waiter::waiter_hide("map_incidence"), add = TRUE)
    
    df <- filtered_data()
    shiny::req(nrow(df) > 0)
    
    # Jointure spatiale avec ton objet SF (shap_geo doit être disponible dans le package)
    sub_sf <- shap_geo %>%
      dplyr::left_join(df, by = "geo")
    
    # Contours régionaux / registres
    ra_sub <- shap_RA %>%
      dplyr::filter(loc == input$sel_cancer)

# Palette de couleurs adaptative
    if (isTRUE(input$sel_var == "pm")) {
      pal_base <- pal_sir
      pal_leg <- leaflet::colorNumeric(palette = "RdYlGn", domain = c(0.7, 1.3), reverse = TRUE, na.color = "#E0E0E0")
      trans <- function(x) x
      val_leg <- c(0.7, 1.3)
      
    } else {
      pal_base <- pal_proba
      # Correction du domaine de la légende de 0.5 à 1
      pal_leg <- leaflet::colorNumeric(palette = "BuPu", domain = c(0.5, 1))
      
      # Pour les probas, pal_proba gère déjà la transformation en interne.
      # Donc la fonction de transformation externe doit être l'identité (ne rien faire).
      trans <- function(x) x 
      
      val_leg <- c(0.5, 1)
    }

    # Construction des popups
    popups <- build_popups_fast(sub_df = sub_sf)

    # Mise à jour des couches Leaflet sans recharger le fond de carte
    leaflet::leafletProxy("map_incidence", data = sub_sf) %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(
        stroke = TRUE,                 # <-- Désactive les contours
        group = "polys",
        ## color = "#ffffff",
        # On passe directement val, car pal_base (= pal_proba) applique déjà trans_proba en interne
        fillColor = ~pal_base(val),
        color = ~pal_base(val),
        weight = 1,
        fillOpacity = 0.8,
        highlightOptions = leaflet::highlightOptions(weight = 2, color = "#333", bringToFront = TRUE),
        popup = popups
      ) %>%
      leaflet::addPolygons(
        data = ra_sub,
        group = "ra_border",
        fill = FALSE,
        color = "#000000",
        weight = 1.2,
        opacity = 0.9,
        options = leaflet::pathOptions(interactive = FALSE)
      ) ## %>%
      ## leaflet::addLegend(
      ##   pal = pal_leg,
      ##   values = val_leg, # Domaine de la palette transformée
      ##   title = "Titre de test",
      ##   position = "bottomright",
      ##   opacity = 0.8,
      ##   # Re-transforme les étiquettes pour afficher la vraie valeur
      ##   labFormat = leaflet::labelFormat(
      ##     transform = function(x) sort(trans(x), decreasing = TRUE)
      ##   )
      ## )
  })
}
