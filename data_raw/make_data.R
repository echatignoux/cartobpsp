##-----------------
##  USE_PARQ  
##-----------------
## Author: Edouard Chatignoux <edouard.chatignoux@gmail.com>
## Created: 30 avril 2026
##-----------------
### Commentary:
##'
##'  
##' 
##-----------------

###' Init
##'==================================== 
####' Packages
##'---------------------------------------- 
library(magrittr)
library(tidyverse)
theme_set(theme_bw())
bis <- geom_abline(intercept=0, slope=1)
library(duckdb)
library(duckspatial)
library(posterior)
library(bayesplot)
library(sf)
library(tictoc)

###'Lecture des données  ====================================
path_db <- "C:/Users/e.chatignoux/Documents/work/C_calib_conjoint/production/eval_1319/"
con <- dbConnect(duckdb(), dbdir = file.path(path_db,"duckdb/shap_geo.duckdb"))
dbListTables(con)
shap_geo=ddbs_read_table(con,"pmsi_2022")
dbDisconnect(con, shutdown = TRUE) 

dt_raw <- tbl(dbConnect(duckdb(), dbdir = file.path(path_db,"duckdb/raw.duckdb")),"raw")

####'Connection aux bases  ---------------------------------------- 
con_sum <- dbConnect(duckdb(), dbdir = file.path(path_db,"duckdb/sum.duckdb"),read_only = TRUE)
con_post <- dbConnect(duckdb(), dbdir = file.path(path_db,"duckdb/post.duckdb"),read_only = TRUE)

dbListTables(con_sum)
s_theta <- tbl(con_sum, "theta")
s_nu    <- tbl(con_sum, "nu")
s_par   <- tbl(con_sum, "par")
s_ar   <- tbl(con_sum, "ar")

p_theta <- tbl(con_post, "theta")
p_nu    <- tbl(con_post, "nu")
p_par   <- tbl(con_post, "par")
p_ar   <- tbl(con_post, "ar")


###'Tables pour les cartes  ==================================== 
####'Calcul des probas  ---------------------------------------- 
prob_t1 <- p_theta %>%
  group_by(loc,sexe,geo,prox) %>%
  summarise(prob = mean(as.numeric(theta_1>1))) %>%
  collect() %>%
  mutate(prox=factor(prox,
                     levels=c("p1","p2","p3"),
                     labels=c("ald","pmsi","aup")
                     )) %>%
  mutate(comp="theta_1")

prob_t2 <- p_theta %>%
  group_by(loc,sexe,geo,prox) %>%
  summarise(prob = mean(as.numeric(theta_2>1))) %>%
  collect()%>%
  mutate(prox=factor(prox,
                     levels=c("p1","p2","p3"),
                     labels=c("ald","pmsi","aup")
                     )) %>%
  mutate(comp="theta_2")

prob_post <-
  prob_t1 %>%
  bind_rows(prob_t2)

####'Table incidende  ---------------------------------------- 
dt_inc <- s_theta %>%
  collect() %>% 
  mutate(prox=factor(prox,
                     levels=c("ald","pmsi","aup"),
                              labels=c("ald","pmsi","aup")
                     )) %>%
  left_join(  prob_post ) %>%
  mutate(comp=ifelse(comp=="theta_1","k","p"))

####'Table des sir  ---------------------------------------- 
dt_sir <- dt_raw %>%
  filter(prox!="ap") %>%
  group_by(loc,sexe,prox,age) %>%
  mutate(E=pa*sum(N)/sum(pa)) %>%
  group_by(lib,loc,sexe,prox,geo) %>%
  summarise(across(c(E,N,pa),sum)) %>%
  mutate(sir=N/E) %>%
  collect()

####'On rajoute les erreurs proxy  ---------------------------------------- 
dt_e <- dt_inc %>%
  select(loc,sexe,prox) %>%
  unique() %>%
  mutate(prox=factor(prox,
                     levels=c("ald","pmsi","aup"),
                     labels=c("p1","p2","p3")
                     ) %>% as.character)

dt_e %<>%
  split(1:nrow(.)) %>% # On transforme chaque ligne en un petit objet
  map_df(function(g) {
    message("Calcul pour : ", g$loc, " - ", g$sexe, " - ", g$prox)
    # On ne tire que ce groupe de la base
    p_nu %>%
      filter(loc == !!g$loc, sexe == !!g$sexe, prox == !!g$prox) %>%
      mutate(e=nu_1/nu_2) %>%
      select(-nu_1,-nu_2,-val_theta) %>% 
      group_by(lib, loc, sexe, prox, geo) %>%
      summarise(pm=mean(e),low=quantile(e,p=.025),up=quantile(e,p=.975),
                prob = mean(as.numeric(e>1))) %>%
      collect() %>%
      mutate(comp="e",RA=TRUE)
  }) %>%
  unnest()

dt_e %<>% 
    mutate(prox=factor(prox,
                     levels=c("p1","p2","p3"),
                              labels=c("ald","pmsi","aup")
                     ))


## On ne garde que le E qu'on ajoute à dt_inc
dt_sir %<>% select(loc,sexe,prox,geo,E)
dt_inc %<>%
  select(lib,loc,sexe,prox,geo,RA,pm,low,up,prob,comp) %>% 
  bind_rows(dt_e) %>% 
  left_join(dt_sir)


###'Table de qualité des proxys  ==================================== 
dt_par <- s_par %>%
  filter(str_detect(par,"s_[ec]|psi|kappa|b_sp"))  %>%
  collect()

dt_class <- dt_par %>% 
  filter(str_detect(par,"s_[e]|kappa"))  %>%
  select(loc,sexe,prox,par,pm) %>%
  spread(par,pm) %>%
  mutate(class=case_when(s_e<0.05 ~"A++",
                         s_e>0.1 ~"B--",
                         kappa>0.8~"A+",
                         TRUE~"B-"
                         )) %>%
  select(loc,prox,sexe,class)

dt_par %<>%
  left_join(dt_class) %>%
  select(lib:pm,sd,low,up,class)

####'Shape RA  ---------------------------------------- 
shap_RA <- shap_geo %>%
  right_join(s_theta %>%
               filter(RA) %>% 
               select(loc,geo) %>%
               distinct() %>%
               collect() ) %>%
  group_by(loc) %>%
  nest(.key="geometry") %>%
  mutate(geometry=map(geometry,st_union)) %>%
  unnest(geometry) %>%
  st_as_sf()

###'On sauve  ==================================== 
con <-  ddbs_create_conn(dbdir = "./inst/extdata/cartobpsp.duckdb")
ddbs_write_table(con, shap_RA %>% as_duckspatial_df(), name = "shap_RA",overwrite=T)
ddbs_write_table(con, shap_geo %>% as_duckspatial_df(), name = "shap_geo",overwrite=T)
dbWriteTable(con, "dt_inc", dt_inc %>% collect() , overwrite = TRUE)
dbWriteTable(con, "dt_par", dt_par , overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)
rm(con); gc()


