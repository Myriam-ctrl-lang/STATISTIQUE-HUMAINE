#########################################
#LE BILAN DEMOGRAPHIQUE, à l'année n-1  #
# Par Myriam YAHYAOUI, Insee DPS        #
# server.R file                         #
#########################################

#Téléchargement des données en local. Le temps de chargement des métadonnées est 
#trop long
# PACKAGES #####################################################################

library(dplyr)

# Get the development version from GitHub
# install.packages("devtools")
# devtools::install_github("pyr-opendatafr/R-Insee-Data")
library(insee)

# Fonction pour télécharger les données et les enregistrer localement
download_and_save_data <- function() {
  # Récupère les idbank pour les estimations de la population avec filtrage sur les tranches d'âge
  list_idbank_elp <- get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |>
    filter(AGE %in% c("00-24", "25-59", "60-")) |>
    pull(idbank)
  
  # Télécharge les données pour les estimations de la population
  elp_grps_age <- get_insee_idbank(list_idbank_elp)
  
  list_idbank_elp = 
    get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |> 
    filter(AGE == "00-") |> 
    filter(SEXE %in% c (1, 2, 0) ) |>
    pull(idbank) 
  
  elp = 
    get_insee_idbank(list_idbank_elp)
  
  # Récupère les idbank pour les naissances et décès
  list_idbank_Naissances_Décès <- get_idbank_list("DECES-MORTALITE", "NAISSANCES-FECONDITE") |>
    filter(FREQ == "A") |>
    filter(DEMOGRAPHIE %in% c("NAISS-DOM", "DECES-DOM")) |> 
    pull(idbank)
  
  # Télécharge les données pour les naissances et décès
  Naissances_Décès_ec <- get_insee_idbank(list_idbank_Naissances_Décès)
  
  # Sauvegarde les données dans un fichier RDS local
  saveRDS(list(elp_grps_age = elp_grps_age,
               elp = elp, 
               Naissances_Décès_ec = Naissances_Décès_ec), "tableau_bord/data/local_data.rds")
}

download_and_save_data()
