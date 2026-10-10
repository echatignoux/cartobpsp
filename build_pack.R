##======================================================================
### --- build_pack.R ---
##======================================================================
## Auteur: Edouard Chatignoux
## Créé le mer.  1 oct. 2014
## Dernière mise à jour le 2022-04-04
##======================================================================
##  Description:
##--------------
##' Suite des étapes pour construire un pacquage avec devtools.
##======================================================================

##'Création des répertoires
usethis::create_package("./")
usethis::use_directory("R")
usethis::use_directory("inst/extdata")
usethis::use_directory("data_raw")
usethis::use_directory("R")

## Fichier/dossiers à ignorer
usethis::use_build_ignore("data-raw")
usethis::use_build_ignore("build_pack.R")
usethis::use_build_ignore("dev_server.R")

usethis::use_version()
usethis::use_news_md()

##'Liste des packages dont on a besoin
##' (use_package ajoute le paquet à DOCUMENT)
usethis::use_package("shiny")
usethis::use_package("leaflet")
usethis::use_package("duckdb")
usethis::use_package("dplyr")
usethis::use_package("sf")
usethis::use_package("htmlwidgets")
usethis::use_package("bslib")
usethis::use_package("magrittr")
usethis::use_package("rlang")


##' Création des .Rd à partir des commentaires roxygen
devtools::document()

##'On charge tout (et on regarde si ça marche)
devtools::load_all()
cartobpsp::run_app()


##' On regarde si tous va bien
##' dans les exemples...
devtools::run_examples()
##' et dans la structure
devtools::check()

## Manuel en pdf
devtools::build_manual()

## Vignette
devtools::build_vignettes(keep_md = TRUE)

## Construction du package (.zip)
devtools::build(binary = TRUE)

## Installation dans mon R
devtools::install()

