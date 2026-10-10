#' Lancer l'application Shiny comprox
#'
#' @param ... Arguments supplémentaires passés à \code{\link[shiny]{shinyApp}}
#' @export
#' @examples
#'   comprox::run_app()
run_app <- function(...) {
db_path <- system.file("extdata", "cartobpsp.duckdb", package = "cartobpsp")

## db_path <- "C:/Users/e.chatignoux/Documents/work/Rpackages/cartobpsp/inst/extdata/cartobpsp.duckdb"

con <- DBI::dbConnect(
  duckdb::duckdb(), 
  dbdir = db_path, 
  read_only = TRUE
)

# Fermeture propre de la connexion à la fin de la session
shiny::onStop(function() {
  DBI::dbDisconnect(con, shutdown = TRUE)
  ## rm(con)
  gc()
})

####' Chargement et préparation des données ---------------------------------------- 
# Charger les tables DuckDB (lazy evaluation)
.pkg_env$dt_inc <- dplyr::tbl(con, "dt_inc") %>% mutate(prob=pmax(prob,1-prob))
.pkg_env$dt_par <- dplyr::tbl(con, "dt_par")
.pkg_env$shap_geo<-duckspatial::ddbs_read_table(con,"shap_geo")  %>%
    dplyr::collect()  
.pkg_env$shap_RA<-duckspatial::ddbs_read_table(con,"shap_RA")  %>%
    dplyr::collect()
# Extraction unique des métadonnées pour l'UI
.pkg_env$loc_sexe <- .pkg_env$dt_inc %>% 
  dplyr::select(lib, loc, sexe) %>% 
  dplyr::distinct() %>% 
  dplyr::collect() %>%
  dplyr::arrange(loc,lib,  sexe) 

.pkg_env$localisations  <- stats::setNames(unique(.pkg_env$loc_sexe$loc), unique(.pkg_env$loc_sexe$lib))
.pkg_env$sexe_choices   <- c("Homme" = 1, "Femme" = 2)
.pkg_env$indic_choices  <- c("Incidence" = "k", "Proxy" = "p", "Erreur" = "e")
.pkg_env$prox_choices   <- c("AUP" = "aup", "ALD" = "ald", "PMSI" = "pmsi")
  
  shiny::shinyApp(ui = app_ui, server = app_server, ...)
}
