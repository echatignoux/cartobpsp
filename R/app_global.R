db_path <- system.file("extdata", "cartobpsp.duckdb", package = "cartobpsp")
db_path <- "./inst/extdata/cartobpsp.duckdb"

if (db_path == "") {
  stop("Fichier DuckDB introuvable dans inst/extdata/")
}


con <-  duckspatial::ddbs_create_conn(dbdir = db_path)
# Fermeture propre de la connexion à la fin de la session
onStop(function() {
  DBI::dbDisconnect(con, shutdown = TRUE)
  ## rm(con)
  gc()
})

####' Chargement et préparation des données ---------------------------------------- 
# Charger les tables DuckDB (lazy evaluation)
dt_inc <- dplyr::tbl(con, "dt_inc")
dt_sir <- dplyr::tbl(con, "dt_sir")
shap_geo<-duckspatial::ddbs_read_table(con,"shap_geo")  %>%
  dplyr::collect()  
shap_RA<-duckspatial::ddbs_read_table(con,"shap_RA")  %>%
  dplyr::collect() %>%
  left_join(dt_inc %>% dplyr::collect() %>% dplyr::as_tibble() %>% dplyr::select(lib,loc) %>% unique())

# Extraction unique des métadonnées pour l'UI
sexes_par_cancer <- dt_inc %>% 
  dplyr::select(loc, sexe) %>% 
  dplyr::distinct() %>% 
  dplyr::collect() %>%
  dplyr::arrange(loc, sexe) 

df_loc <- dt_inc %>% 
  dplyr::select(loc, lib) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(loc) %>% 
  dplyr::collect()

localisations <- setNames(df_loc$loc, df_loc$lib)

sexe_choices <- setNames(c(1,2), c("Homme", "Femme"))

indic_choices <- setNames(c("theta_1","theta_2"), c("Incidence", "Proxy"))

prox_choices <- setNames( c("aup", "ald", "pmsi"),c("AUP","ALD","PMSI"))
