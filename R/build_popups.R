##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param p 
##' @return 
##' @author Edouard Chatignoux
trans_proba <- function(x, p = 3) {
  val <- exp(x - 0.5)^p
  val_min <- exp(-0.5)^p
  val_max <- exp(0.5)^p
  
  # Normalisation min-max
  (val - val_min) / (val_max - val_min)
}

inv_trans_proba <- function(y, p = 4) {
  log(y)/p+.5
}


# --- Palettes & Légendes ---
pal_sir <- function(x) {
  leaflet::colorNumeric("RdYlGn", domain = c(.7, 1.3), reverse = TRUE, na.color = "#E0E0E0")(scales::oob_squish(x, c(.7, 1.3)))
}

pal_proba <- function(x) {
  # On applique la transformation puissance avant d'injecter dans colorNumeric
  pal_raw <- leaflet::colorNumeric(palette = "BuPu", domain = c(0, 1))
  pal_raw(trans_proba(pmin(pmax(x, 0), 1), p = 3))
}


render_html_legend <- function(title, palette, domain, format_fun = identity, ticks = NULL, trans_fun = identity) {
  # Si aucune graduation n'est spécifiée, on prend min, milieu, max par défaut
  if (is.null(ticks)) {
    ticks <- c(domain[1], mean(domain), domain[2])
  }
  
  # 1. Dégradé de la barre : échantillonnage uniforme entre 0 et 1
  vals_seq <- seq(domain[1], domain[2], length.out = 100)
  cols <- palette(vals_seq)
  grad_css <- sprintf("background: linear-gradient(to right, %s);", paste(cols, collapse = ", "))
  
  # 2. Positionnement des étiquettes selon la TRANSFORMATION (trans_fun)
  # On calcule la position normalisée après transformation [0, 1]
  t_min <- trans_fun(domain[1])
  t_max <- trans_fun(domain[2])
  
  tick_spans <- lapply(ticks, function(val) {
    # Position contractée/dilatée en %
    t_val <- trans_fun(val)
    pct <- (t_val - t_min) / (t_max - t_min) * 100
    
    tags$span(
      style = sprintf("position: absolute; left: %.1f%%; transform: translateX(-50%%);", pct),
      format_fun(val)
    )
  })
  
  tagList(
    tags$div(style = "margin-top: 15px; margin-bottom: 5px; font-weight: bold; font-size: 0.9em; color: #333;", title),
    tags$div(style = paste("height: 12px; width: 100%; border-radius: 3px; border: 1px solid #ccc;", grad_css)),
    tags$div(
      style = "position: relative; height: 18px; font-size: 0.75em; color: #666; margin-top: 3px;",
      tick_spans
    )
  )
}

#' Génération rapide des popups HTML pour Leaflet
#'
#' @param sub_df Dataframe principal (sf) contenant les colonnes : geo, lib_geo (ou nom), pm, low, up, prob (ou p10)
#' @param sir_sub Dataframe contenant les données brutes : geo, prox, N, E, sir
#' @param prox_selected Proxy actuellement sélectionné dans l'interface (ex: "aup", "ald", "pmsi")
#'
#' @importFrom dplyr mutate filter group_by summarise left_join coalesce
#' @importFrom sf st_drop_geometry
#' @importFrom htmltools HTML
#' @export
build_popups_fast <- function(sub_df, sir_sub) {

  # 1. Formatage des données brutes (N, E, SIR)
  sir_formatted <- sir_sub %>%
    dplyr::mutate(
      p_lab   = toupper(as.character(prox)),
      obs_txt = dplyr::if_else(is.na(N), "-", sprintf("%.0f", N)),
      exp_txt = dplyr::if_else(is.na(E), "-", sprintf("%.1f", E)),
      sir_txt = dplyr::if_else(is.na(sir), "-", sprintf("%.2f", sir)),
      
      row_html = glue::glue(
        "<tr style='border-bottom: 1px solid #f0f0f0;'>
          <td style='padding:3px 5px;font-weight:600;'>{p_lab}</td>
          <td style='padding:3px 5px;text-align:right;'>{obs_txt}</td>
          <td style='padding:3px 5px;text-align:right;'>{exp_txt}</td>
          <td style='padding:3px 5px;text-align:right;font-weight:bold;'>{sir_txt}</td>
        </tr>"
      )
    ) %>%
    dplyr::group_by(geo) %>%
    dplyr::summarise(table_body = paste(row_html, collapse = ""), .groups = "drop")

  # 2. Préparation du dataframe fusionné
  df_merged <- sub_df %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(sir_formatted, by = "geo")

  if (!"lib_geo" %in% colnames(df_merged)) {
    df_merged$lib_display <- df_merged$geo
  } else {
    df_merged$lib_display <- dplyr::coalesce(df_merged$lib_geo, df_merged$geo)
  }

  df_merged <- df_merged %>%
    dplyr::mutate(
      table_body = dplyr::coalesce(
        table_body, 
        "<tr><td colspan='4' style='text-align:center;color:#999;padding:4px;'>Données non disponibles</td></tr>"
      ),
      
      pm_txt   = dplyr::if_else(is.na(pm), "-", sprintf("%.3f", pm)),
      ic_txt   = dplyr::if_else(is.na(low) | is.na(up), "-", sprintf("[%.3f ; %.3f]", low, up)),
      prob_txt = dplyr::if_else(is.na(prob), "-", sprintf("%.2f", prob))
    )

  # 3. Assemblage vectorisé sécurisé avec glue
  popups <- glue::glue_data(df_merged, "
    <div style='font-family: Arial, sans-serif; font-size: 11px; min-width: 250px;'>
      
      <!-- En-tête : Nom + Code Géo -->
      <div style='font-weight: bold; font-size: 13px; margin-bottom: 6px; border-bottom: 2px solid #2b5c8f; padding-bottom: 3px; color: #1a365d;'>
        {lib_display} <span style='color: #718096; font-weight: normal; font-size: 11px;'>({geo})</span>
      </div>

      <!-- Tableau des cas observés / attendus / SIR -->
      <table style='width: 100%; border-collapse: collapse; margin-bottom: 8px;'>
        <thead>
          <tr style='background-color: #f7fafc; color: #4a5568; font-weight: bold; border-bottom: 1px solid #e2e8f0;'>
            <td style='padding: 3px 5px;'>Proxy</td>
            <td style='padding: 3px 5px; text-align: right;'>Obs. (N)</td>
            <td style='padding: 3px 5px; text-align: right;'>Att. (E)</td>
            <td style='padding: 3px 5px; text-align: right;'>SIR</td>
          </tr>
        </thead>
        <tbody>{table_body}</tbody>
      </table>

      <!-- Carte d'estimation bayésienne -->
      <div style='background-color: #f8f9fa; border: 1px solid #e2e8f0; border-radius: 4px; padding: 6px 8px;'>
        <div style='display: flex; justify-content: space-between; margin-bottom: 3px;'>
          <span style='color: #4a5568;'>Incidence (moyenne) :</span>
          <span style='font-weight: bold; color: #2b6cb0;'>{pm_txt}</span>
        </div>
        <div style='display: flex; justify-content: space-between; margin-bottom: 3px;'>
          <span style='color: #4a5568;'>IC 95% :</span>
          <span style='font-weight: 500;'>{ic_txt}</span>
        </div>
        <div style='display: flex; justify-content: space-between;'>
          <span style='color: #4a5568;'>Probabilité :</span>
          <span style='font-weight: bold; color: #2f855a;'>{prob_txt}</span>
        </div>
      </div>

    </div>"
  )

  lapply(popups, htmltools::HTML)
}

## build_popups.R ends here
