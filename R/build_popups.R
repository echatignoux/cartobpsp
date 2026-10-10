##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param p 
##' @return 
##' @author Edouard Chatignoux


# --- Palettes & Légendes ---
## SIR
pal_sir <- function(x) {
  leaflet::colorNumeric("RdYlGn", domain = c(.7, 1.3), reverse = TRUE, na.color = "#E0E0E0")(scales::oob_squish(x, c(.7, 1.3)))
}

pal_leg_sir <- leaflet::colorNumeric(
  palette = "RdYlGn", 
  domain = c(0.7, 1.3), 
  reverse = TRUE, 
  na.color = "#E0E0E0"
)

# Transformation pour passer de [0.5, 1] à [0, 1] avec un étirement non linéaire (p)
trans_proba <- function(x, p = 3) {
  # x est désormais entre 0.5 et 1
  x_clipped <- pmin(pmax(x, 0.5), 1.0)
  
  # On décale à partir de 0.5 (qui devient 0)
  val <- exp(x_clipped - 0.5)^p
  val_min <- exp(0)^p       # = 1 (quand x = 0.5)
  val_max <- exp(0.5)^p     # (quand x = 1)
  
  (val - val_min) / (val_max - val_min)
}

# Fonction inverse (utile si tu dois afficher des seuils sur la légende par exemple)
inv_trans_proba <- function(y, p = 3) {
  val_min <- 1
  val_max <- exp(0.5)^p
  val <- y * (val_max - val_min) + val_min
  
  # Résolution inverse de l'exponentielle
  log(val) / p + 0.5
}

pal_leg_proba <- leaflet::colorNumeric(
  palette = "BuPu", 
  domain = c(0.5, 1)
)

# La fonction de coloration applique la transformation sur l'intervalle [0.5, 1]
pal_proba <- function(x) {
  pal_raw <- leaflet::colorNumeric(palette = "BuPu", domain = c(0, 1))
  pal_raw(trans_proba(x, p = 3))
}

render_html_legend <- function(title, palette, domain, format_fun = identity, ticks = NULL, trans_fun = identity) {
  if (is.null(ticks)) {
    ticks <- c(domain[1], mean(domain), domain[2])
  }
  
  # 1. Dégradé de la barre
  vals_seq <- seq(domain[1], domain[2], length.out = 100)
  cols <- palette(vals_seq)
  grad_css <- sprintf("background: linear-gradient(to right, %s);", paste(cols, collapse = ", "))
  
  # 2. Positionnement des étiquettes
  t_min <- trans_fun(domain[1])
  t_max <- trans_fun(domain[2])
  
  tick_spans <- lapply(ticks, function(val) {
    t_val <- trans_fun(val)
    pct <- (t_val - t_min) / (t_max - t_min) * 100
    
    tags$span(
      style = sprintf("position: absolute; left: %.1f%%; transform: translateX(-50%%);", pct),
      format_fun(val)
    )
  })
  
  tagList(
    # Suppression du margin-top de 15px (passé à 2px)
    tags$div(style = "margin-top: 2px; margin-bottom: 4px; font-weight: 600; font-size: 0.78rem; color: #2d3748;", title),
    tags$div(style = paste("height: 10px; width: 100%; border-radius: 3px; border: 1px solid #cbd5e0;", grad_css)),
    tags$div(
      style = "position: relative; height: 14px; font-size: 0.7rem; color: #718096; margin-top: 2px;",
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

  # 1. Génération vectorisée des lignes HTML du tableau des proxys
  sir_sub$row_html <- sprintf(
    "<tr style='border-bottom: 1px solid #f0f0f0;'>
      <td style='padding:3px 5px;font-weight:600;'>%s</td>
      <td style='padding:3px 5px;text-align:right;'>%s</td>
      <td style='padding:3px 5px;text-align:right;'>%s</td>
      <td style='padding:3px 5px;text-align:right;font-weight:bold;'>%s</td>
    </tr>",
    toupper(as.character(sir_sub$prox)),
    ifelse(is.na(sir_sub$N), "-", sprintf("%.0f", sir_sub$N)),
    ifelse(is.na(sir_sub$E), "-", sprintf("%.1f", sir_sub$E)),
    ifelse(is.na(sir_sub$sir), "-", sprintf("%.2f", sir_sub$sir))
  )

  # Agrégation par code géographie
  table_list <- tapply(sir_sub$row_html, sir_sub$geo, paste, collapse = "")
  sir_formatted <- data.frame(
    geo = names(table_list),
    table_body = as.vector(table_list),
    stringsAsFactors = FALSE
  )

  # 2. Joindre à sub_df
  df_merged <- sub_df %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(sir_formatted, by = "geo")

  # Libellé d'affichage
  lib_display <- if ("lib_geo" %in% colnames(df_merged)) {
    dplyr::coalesce(df_merged$lib_geo, df_merged$geo)
  } else {
    df_merged$geo
  }

  table_body <- dplyr::coalesce(
    df_merged$table_body, 
    "<tr><td colspan='4' style='text-align:center;color:#999;padding:4px;'>Données non disponibles</td></tr>"
  )

  # 3. Formatage de theta = pm [low ; up] et de la probabilité
  theta_txt <- ifelse(
    is.na(df_merged$pm) | is.na(df_merged$low) | is.na(df_merged$up),
    "-",

    sprintf("&theta; = %.3f [%.3f ; %.3f]", df_merged$pm, df_merged$low, df_merged$up)
  )
  
  prob_txt <- ifelse(
    is.na(df_merged$prob), 
    "-", 
    sprintf("P(&theta; &ne; 1) = %.2f", df_merged$prob)
  )

  # 4. Assemblage final avec bloc gris épuré (texte fluide)
  popups <- sprintf("
    <div style='font-family: system-ui, -apple-system, sans-serif; font-size: 11px; min-width: 240px;'>
      <div style='font-weight: bold; font-size: 13px; margin-bottom: 6px; border-bottom: 2px solid #2b5c8f; padding-bottom: 3px; color: #1a365d;'>
        %s <span style='color: #718096; font-weight: normal; font-size: 11px;'>(%s)</span>
      </div>

      <table style='width: 100%%; border-collapse: collapse; margin-bottom: 8px;'>
        <thead>
          <tr style='background-color: #f7fafc; color: #4a5568; font-weight: bold; border-bottom: 1px solid #e2e8f0;'>
            <td style='padding: 3px 5px;'>Proxy</td>
            <td style='padding: 3px 5px; text-align: right;'>Obs. (N)</td>
            <td style='padding: 3px 5px; text-align: right;'>Att. (E)</td>
            <td style='padding: 3px 5px; text-align: right;'>SIR</td>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>

      <!-- Bloc d'estimation Bayésienne en texte fluide -->
      <div style='background-color: #f8f9fa; border: 1px solid #e2e8f0; border-radius: 4px; padding: 6px 8px; font-size: 11.5px; color: #2d3748;'>
        <div style='font-weight: 600; color: #2b6cb0; margin-bottom: 3px;'>
          %s
        </div>
        <div style='font-weight: 600; color: #2f855a;'>
          %s
        </div>
      </div>

    </div>",
    lib_display, df_merged$geo, table_body, theta_txt, prob_txt
  )

  lapply(popups, htmltools::HTML)
}

build_popups_fast <- function(sub_df, sir_sub = NULL) {
  sprintf(
    "<div style='font-family: system-ui, -apple-system, sans-serif; font-size: 11px; color: #2d3748; min-width: 175px;'>
       <div style='font-weight: bold; font-size: 12px; color: #1a365d; border-bottom: 1.5px solid #cbd5e0; padding-bottom: 3px; margin-bottom: 6px;'>
         %s <span style='font-weight: normal; color: #718096;'>(%s)</span>
       </div>
       <table style='width: 100%%; border-collapse: collapse;'>
         <tr style='border-bottom: 1px solid #edf2f7;'>
           <td style='color: #718096; padding: 2px 0;'>Cas attendus (E) :</td>
           <td style='text-align: right; font-weight: 500; padding: 2px 0;'>%.1f</td>
         </tr>
         <tr style='border-bottom: 1px solid #edf2f7;'>
           <td style='color: #718096; padding: 2px 0;'>&theta; [IC 95%%] :</td>
           <td style='text-align: right; font-weight: bold; color: #2b6cb0; padding: 2px 0;'>
             %.2f <span style='font-weight: normal; color: #4a5568;'>[%.2f ; %.2f]</span>
           </td>
         </tr>
         <tr>
           <td style='color: #718096; padding: 2px 0;'>P(&theta; &ne; 1) :</td>
           <td style='text-align: right; font-weight: 600; padding: 2px 0;'>%.2f</td>
         </tr>
       </table>
     </div>",
    sub_df$lib_geo, sub_df$geo,
    sub_df$E,      # Attendus E (arrondis à 1 décimale)
    sub_df$pm,     # Moyenne a posteriori SIR
    sub_df$low,    # Borne inf IC 95%
    sub_df$up,     # Borne sup IC 95%
    sub_df$prob    # Probabilité d'excès
  )
}
build_labels_fast <- function(sub_sf) {
  
  # 1. Gestion du libellé géographique avec fallback sur le code geo
  lib_display <- if ("lib_geo" %in% colnames(sub_sf)) {
    dplyr::coalesce(sub_sf$lib_geo, sub_sf$geo)
  } else {
    sub_sf$geo
  }

  # 2. Formatage vectorisé de theta = pm [low ; up]
  theta_txt <- ifelse(
    is.na(sub_sf$pm) | is.na(sub_sf$low) | is.na(sub_sf$up),
    "-",
    sprintf("%.3f [%.3f ; %.3f]", sub_sf$pm, sub_sf$low, sub_sf$up)
  )

  # 3. Formatage vectorisé de P(|theta - 1| > 0) = prob
  prob_txt <- ifelse(
    is.na(sub_sf$prob),
    "-",
    sprintf("P(&theta; &ne; 1) = %.2f", sub_sf$prob)
  )

  # 4. Assemblage HTML léger
  labels_html <- sprintf(
    "<div style='font-family: system-ui, -apple-system, sans-serif; font-size: 11px; padding: 2px 4px;'>
       <div style='font-weight: bold; font-size: 12px; color: #1a365d; margin-bottom: 3px;'>
         %s <span style='color: #718096; font-weight: normal; font-size: 10px;'>(%s)</span>
       </div>
       <div style='color: #2b6cb0; font-weight: 600; margin-bottom: 2px;'>
         &theta; = %s
       </div>
       <div style='color: #2f855a; font-weight: 600;'>
         %s
       </div>
     </div>",
    lib_display,
    sub_sf$geo,
    theta_txt,
    prob_txt
  )

  lapply(labels_html, htmltools::HTML)
}

build_param_table <- function(df_par) {
  if (is.null(df_par) || nrow(df_par) == 0) {
    return(shiny::tags$div(
      style = "color: #a0aec0; font-size: 11px; text-align: center; padding: 4px;",
      "Aucun paramètre."
    ))
  }

  # 1. Dictionnaire et ordre strict
  par_map <- c(
    "s_c"   = "&sigma;<sub>c</sub>",
    "psi"   = "&psi;",
    "s_e"   = "&sigma;<sub>e</sub>",
    "kappa" = "&kappa;",
    "b_sp1" = "Se",
    "b_sp2" = "VPP"
  )

  # 2. Badges de classement très compacts (padding 1px 4px)
  get_class_badge <- function(cls) {
    if (is.na(cls) || cls == "") return("-")
    
    style_css <- switch(
      toupper(trimws(cls)),
      "A++" = "background-color: #1b5e20; color: #ffffff; font-weight: bold;",
      "A+"  = "background-color: #4caf50; color: #ffffff; font-weight: bold;",
      "A"   = "background-color: #81c784; color: #1a202c; font-weight: bold;",
      "B"   = "background-color: #ffb74d; color: #1a202c; font-weight: bold;",
      "B-"  = "background-color: #e57373; color: #ffffff; font-weight: bold;",
      "B--" = "background-color: #b71c1c; color: #ffffff; font-weight: bold;",
      "background-color: #edf2f7; color: #4a5568;"
    )

    sprintf(
      "<span style='display: inline-block; padding: 1px 4px; border-radius: 3px; font-size: 9.5px; %s'>%s</span>",
      style_css, cls
    )
  }

  # 3. Formatage des cellules (intervalle raccourci avec virgule)
  df_formatted <- df_par %>%
    dplyr::filter(par %in% names(par_map)) %>%
    dplyr::mutate(
      prox_lab  = factor(prox,levels=c("aup","ald","pmsi"),labels=toupper(c("aup","ald","pmsi"))),
      par_ord   = factor(par, levels = names(par_map)),
      par_clean = dplyr::recode(par, !!!par_map),
      # Format d'intervalle condensé pour économiser de la largeur
      val_txt   = sprintf("%.2f <span style='color:#718096; font-size:9.5px;'>[%.2f,%.2f]</span>", pm, low, up)
    ) %>%
    dplyr::arrange(par_ord)

  df_params_wide <- df_formatted %>%
    dplyr::select(par_clean, prox_lab, val_txt) %>%
    tidyr::pivot_wider(
      names_from  = prox_lab, 
      values_from = val_txt,
      values_fill = "-"
    ) %>%
    select(par_clean,AUP,ALD,PMSI)

  # Ligne de classement (label vide pour réduire la largeur de la col 1)
  df_class_wide <- df_formatted %>%
    dplyr::select(prox_lab, class) %>%
    dplyr::distinct() %>%
    dplyr::mutate(class_badge = sapply(class, get_class_badge)) %>%
    dplyr::select(prox_lab, class_badge) %>%
    tidyr::pivot_wider(
      names_from  = prox_lab, 
      values_from = class_badge,
      values_fill = "-"
    ) %>%
    dplyr::mutate(par_clean = "★")

  prox_cols <- setdiff(colnames(df_params_wide), "par_clean")
  df_final  <- dplyr::bind_rows(df_params_wide, df_class_wide)

  # 4. Table HTML ultra-dense
  shiny::tags$table(
    style = "width: 100%; font-size: 11px; border-collapse: collapse; font-family: system-ui, -apple-system, sans-serif; table-layout: auto;",
    
    shiny::tags$thead(
      shiny::tags$tr(
        style = "background-color: #f7fafc; color: #4a5568; border-bottom: 1.5px solid #cbd5e0;",
        shiny::tags$th(style = "padding: 2px 3px; text-align: left; width: 1%; white-space: nowrap;", ""),
        lapply(prox_cols, function(col) {
          shiny::tags$th(style = "padding: 2px 3px; text-align: right; font-size: 11px;", col)
        })
      )
    ),
    
    shiny::tags$tbody(
      lapply(seq_len(nrow(df_final)), function(i) {
        row <- df_final[i, ]
        is_class_row <- (i == nrow(df_final))
        
        row_style <- if (is_class_row) {
          "border-top: 1.5px solid #cbd5e0; background-color: #f8fafc;"
        } else {
          "border-bottom: 1px solid #edf2f7;"
        }

        shiny::tags$tr(
          style = row_style,
          shiny::tags$td(
            style = "padding: 1px 3px; color: #2d3748; white-space: nowrap; font-weight: 600;",
            htmltools::HTML(row$par_clean)
          ),
          lapply(prox_cols, function(col) {
            shiny::tags$td(
              style = "padding: 1px 3px; text-align: right; white-space: nowrap;",
              htmltools::HTML(row[[col]])
            )
          })
        )
      })
    )
  )
}
## build_popups.R ends here
