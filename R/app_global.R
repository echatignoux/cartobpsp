db_path <- system.file("extdata", "cartobpsp.duckdb", package = "cartobpsp")

if (db_path == "") {
  stop("Fichier DuckDB introuvable dans inst/extdata/")
}


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
dt_inc <- dplyr::tbl(con, "dt_inc")
dt_par <- dplyr::tbl(con, "dt_par")
shap_geo<-duckspatial::ddbs_read_table(con,"shap_geo")  %>%
  dplyr::collect()  
shap_RA<-duckspatial::ddbs_read_table(con,"shap_RA")  %>%
  dplyr::collect() %>%
  left_join(dt_inc %>% dplyr::collect() %>% dplyr::as_tibble() %>% dplyr::select(lib,loc) %>% unique())

# Extraction unique des métadonnées pour l'UI
loc_sexe <- dt_inc %>% 
  dplyr::select(lib, loc, sexe) %>% 
  dplyr::distinct() %>% 
  dplyr::collect() %>%
  dplyr::arrange(loc,lib,  sexe) 

localisations <- setNames(loc_sexe$loc %>% unique(),
                          loc_sexe$lib %>% unique())

sexe_choices <- setNames(c(1,2), c("Homme", "Femme"))

indic_choices <- setNames(c("k","p"), c("Incidence", "Proxy"))

prox_choices <- setNames( c("aup", "ald", "pmsi"),c("AUP","ALD","PMSI"))
