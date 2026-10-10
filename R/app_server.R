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
  dt_inc_tbl <- dplyr::tbl(con, "dt_inc")

  # --- 1. Initialisation des choix de localisations au démarrage ---
  # On met à jour les localisations sans recréer de boucle sur sel_sex
  shiny::observeEvent(input$sel_sex, {
    localisations_df <- loc_sexe %>%
      dplyr::filter(sexe == !!input$sel_sex) %>% 
      dplyr::arrange(loc)
    
    loc_choices <- setNames(localisations_df$loc, localisations_df$lib)
    
    # Conserve la sélection courante si elle existe toujours pour le sexe choisi
    current_sel <- isolate(input$sel_cancer)
    selected_val <- if (!is.null(current_sel) && current_sel %in% loc_choices) current_sel else loc_choices[1]

    shiny::updateSelectInput(
      session, 
      "sel_cancer", 
      choices = loc_choices, 
      selected = selected_val
    )
  }, ignoreInit = FALSE)

  # --- 2. Légendes Leaflet ---
  output$ui_legend_box <- shiny::renderUI({
    shiny::req(input$sel_var)

    if (input$sel_var == "pm") {
      # --- Cas 1 : SIR (Echelle linéaire) ---
      pal_sir <- leaflet::colorNumeric(
        palette = "RdYlGn", 
        domain = c(0.7, 1.3), 
        reverse = TRUE
      )
      
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
        title = "P(|θ-1| > 0)",
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
    df <- filtered_data()
    shiny::req(nrow(df) > 0)

    sir_sub <- dt_sir %>% 
      dplyr::filter(
        loc == !!input$sel_cancer,
        sexe == !!as.numeric(input$sel_sex),
        prox == !!input$sel_prox) %>%
      dplyr::collect()

    
    # Jointure spatiale avec ton objet SF (shap_geo doit être disponible dans le package)
    sub_sf <- shap_geo %>%
      dplyr::left_join(df, by = "geo")
    
    # Contours régionaux / registres
    ra_sub <- shap_RA %>%
      dplyr::filter(loc == input$sel_cancer)

    # Palette de couleurs adaptative
    if (isTRUE(input$sel_var == "pm")) {
      pal_base <- pal_sir
      pal_leg <-  leaflet::colorNumeric(palette = "RdYlGn", domain = c(0.7, 1.3), reverse = TRUE, na.color = "#E0E0E0" )
      trans <- function(x) x
      val_leg <- c(0.7,1.3)

    } else {
      pal_base <- pal_proba
      pal_leg <- leaflet::colorNumeric(palette = "BuPu", domain = c(0, 1) )
      trans <- inv_trans_proba
      val_leg <- c(0,1)
    }
    # Construction des tooltips au survol
    ## labels <- build_labels_fast(sub_sf)
    popups <- build_popups_fast(
      sub_df = sub_sf
    )

    # Mise à jour des couches Leaflet sans recharger le fond de carte
    leaflet::leafletProxy("map_incidence", data = sub_sf) %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(
        group = "polys",
        stroke = TRUE,
        weight = 0.8,
        color = "#ffffff",
        fillColor = ~pal_base(trans(val)),
        fillOpacity = 0.8,
        highlightOptions = leaflet::highlightOptions(weight = 2, color = "#333", bringToFront = TRUE),
        ## label = labels,
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
