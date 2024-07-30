################################################################################
################################# 1- LIBRAIRIES ################################
################################################################################

library(sf)
library(sp)
library(tmap)
library(dplyr)
library(tidyr)
library(tibble)
library(ncdf4)
library(reshape2)
library(gstat)
library(ggplot2)
library(nngeo)
library(foreach)
library(doParallel)
library(parallel)
library(cowplot)
library(tidyverse)


################################################################################
################################# 2- ENTREES ###################################
################################################################################

#Chemin d'accès vers la zone géographique France
path_AREA = "data/1-processed-data/SHERPA/emiRedOn_01005_France.nc"
#Chemin d'accès vers les données INSEE
path_fichier_shp_1 <- "data/2-output-data/donnees_shp_1"
path_fichier_shp_2 <- "data/2-output-data/donnees_shp_2"
path_fichier_shp_3 <- "data/2-output-data/donnees_shp_3"
path_fichier_pourcents <- "data/2-output-data/pourcents_shp"
title_shp_1 <- "donnees_insee_iris_toutage_1"
title_shp_2 <- "donnees_insee_iris_toutage_2"
title_shp_3 <- "donnees_insee_iris_toutage_3"
title_pourcents = "pourcents-shp"

#Risque relatif RR pour la mortalité totale des plus de 30 ans pour les PM2.5
RR_PM25 = 1.15 
RR_PM25_haut = 1.25
RR_PM25_bas = 1.05
RR_Chen = 1.07
RR_Chen_haut = 1.11
RR_Chen_bas = 1.03
#Intervalle : [1.05:1.25] for a 10 μg/m3 increase in PM2.5 (Pascal et al., 2016)
#Risque relatif RR pour la mortalité totale des plus de 30 ans pour les NO2
RR_NO2 = 1.023 
RR_NO2_haut = 1.037
RR_NO2_bas = 1.008
RR_Huang = 1.02
RR_Huang_haut = 1.04
RR_Huang_bas = 1.01
#Intervalle : [1.008:1.037] for a 10 μg/m3 increase in NO2 (COMEAP, 2018)


################################################################################
################################# 3- FONCTIONS #################################
################################################################################
#Fonction qui renvoie une liste de points en France et de concentrations##########

coordo_sherpa <- function(sc, pol, annee) {
  if (pol=="ug_PM25_RH50_haut") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_PM25_RH50_bas") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_NO2_haut") {
    pol <- "ug_NO2"
  }
  else if (pol=="ug_NO2_bas") {
    pol <- "ug_NO2"
  }
  #Chemin d'accès vers les delta_concentrations
  path_SHERPA_delta = paste0("data/1-processed-data/SHERPA/scenarios/",sc,"/DCconc_",sc,"_",annee,"_SURF_",pol,".nc")
  #Chemin d'accès vers les concentrations absolues de 2019
  path_SHERPA_2019 = paste0("data/1-processed-data/SHERPA/conc-2019/BCconc_emepV4_45_cams61_withCond_01005_2019_SURF_",pol,".nc")
  # Charger les fichier 
  nc_file_delta <- nc_open(path_SHERPA_delta)
  latitude <- ncvar_get(nc_file_delta, "latitude")
  longitude <- ncvar_get(nc_file_delta, "longitude")
  latitude_df <- data.frame(latitude)
  longitude_df <- data.frame(longitude)
  delta_df <- data.frame(ncvar_get(nc_file_delta, "delta_conc"))
  conc19_df <- data.frame(ncvar_get(nc_open(path_SHERPA_2019), "conc"))
  area_df <- data.frame(ncvar_get(nc_open(path_AREA), "AREA"))
  # Créer une dataframe vide pour stocker les valeurs filtrées
  filtered_df <- matrix(0, nrow = nrow(delta_df), ncol = ncol(delta_df))
  filtered_df_delta <- matrix(0, nrow = nrow(delta_df), ncol = ncol(delta_df))
  # Parcourir chaque élément des dataframes delta_df et area_df
  for (i in 1:nrow(delta_df)) {
    for (j in 1:ncol(delta_df)) {
      # Vérifier si la valeur correspondante dans area_df est > 0
      if (area_df[i, j] > 0) {
        # Si c'est le cas, stocker la valeur correspondante de delta_df+conc19_df dans filtered_df
        if (!is.nan(delta_df[i, j])) {
          filtered_df[i, j] <- conc19_df[i, j]
          filtered_df_delta[i, j] <- delta_df[i, j]
        }}}}
  # Reformater le data frame
  filtered_df <- melt(filtered_df, varnames = c("longitude", "latitude"))
  filtered_df_delta <- melt(filtered_df_delta, varnames = c("longitude", "latitude"))
  colnames(filtered_df) <- c("longitude_ind", "latitude_ind", "valeurs")
  # Créer un dataframe avec trois colonnes latitude, longitude, valeurs
  map <- matrix(0, nrow = nrow(filtered_df), ncol = 2)
  map_delta <- matrix(0, nrow = nrow(filtered_df), ncol = 1)
  coordo <- matrix(0, nrow = nrow(filtered_df), ncol = 2)
  for (i in 1:nrow(filtered_df)) {
    ind_lat = filtered_df[i, 2]
    ind_long = filtered_df[i, 1]
    coordo[i, 2] = latitude_df[ind_lat, 1]
    coordo[i, 1] = longitude_df[ind_long, 1]
    map[i,1] = filtered_df[i, 3]
    map[i,2] = filtered_df_delta[i, 3]
  }
  colnames(map) <- c("conc", "delta_conc")
  colnames(coordo) <- c("longitude", "latitude")
  # Sélectionner les valeurs non nulles de map
  map_nonzero <- as.data.frame(map[map[, 1] != 0, , drop = FALSE])
  # Sélectionner les coordonnées correspondant aux indices où map_nonzero est non nul
  coordo_nonzero <- as.data.frame(coordo[map[, 1] != 0, , drop = FALSE])
  # Arrondir les coordonnées au centième de degré
  coordo_nonzero[, 1] <- round(coordo_nonzero[, 1], 3)
  coordo_nonzero[, 2] <- round(coordo_nonzero[, 2], 3)
  #Bons formats de données
  conc_points <- st_set_crs(st_as_sf(SpatialPointsDataFrame(coordo_nonzero, map_nonzero)), 4326)
  print(paste0("Les concentrations en 2019 et ", annee, " sont calculées pour le polluant ", 
               pol, " selon le scénario ", sc, "."))
  return(conc_points)
}

#Fonction qui renvoie une liste de points en France et de concentrations pour les fichiers d'étude des 3 secteurs séparéments##########

coordo_sherpa_new <- function(sc, pol, annee, g) {
  if (pol=="ug_PM25_RH50_haut") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_PM25_RH50_bas") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_NO2_haut") {
    pol <- "ug_NO2"
  }
  else if (pol=="ug_NO2_bas") {
    pol <- "ug_NO2"
  }
  #Chemin d'accès vers les delta_concentrations
  path_SHERPA_delta = paste0("data/1-processed-data/SHERPA/scenarios/",sc,"/DCconc_",sc,"_",annee,"_",g,"_SURF_",pol,".nc")
  #Chemin d'accès vers les concentrations absolues de 2019
  path_SHERPA_2019 = paste0("data/1-processed-data/SHERPA/conc-2019/BCconc_emepV4_45_cams61_withCond_01005_2019_SURF_",pol,".nc")
  # Charger les fichier 
  nc_file_delta <- nc_open(path_SHERPA_delta)
  latitude <- ncvar_get(nc_file_delta, "latitude")
  longitude <- ncvar_get(nc_file_delta, "longitude")
  latitude_df <- data.frame(latitude)
  longitude_df <- data.frame(longitude)
  delta_df <- data.frame(ncvar_get(nc_file_delta, "delta_conc"))
  conc19_df <- data.frame(ncvar_get(nc_open(path_SHERPA_2019), "conc"))
  area_df <- data.frame(ncvar_get(nc_open(path_AREA), "AREA"))
  # Créer une dataframe vide pour stocker les valeurs filtrées
  filtered_df <- matrix(0, nrow = nrow(delta_df), ncol = ncol(delta_df))
  filtered_df_delta <- matrix(0, nrow = nrow(delta_df), ncol = ncol(delta_df))
  # Parcourir chaque élément des dataframes delta_df et area_df
  for (i in 1:nrow(delta_df)) {
    for (j in 1:ncol(delta_df)) {
      # Vérifier si la valeur correspondante dans area_df est > 0
      if (area_df[i, j] > 0) {
        # Si c'est le cas, stocker la valeur correspondante de delta_df+conc19_df dans filtered_df
        if (!is.nan(delta_df[i, j])) {
          filtered_df[i, j] <- conc19_df[i, j]
          filtered_df_delta[i, j] <- delta_df[i, j]
        }}}}
  # Reformater le data frame
  filtered_df <- melt(filtered_df, varnames = c("longitude", "latitude"))
  filtered_df_delta <- melt(filtered_df_delta, varnames = c("longitude", "latitude"))
  colnames(filtered_df) <- c("longitude_ind", "latitude_ind", "valeurs")
  # Créer un dataframe avec trois colonnes latitude, longitude, valeurs
  map <- matrix(0, nrow = nrow(filtered_df), ncol = 2)
  map_delta <- matrix(0, nrow = nrow(filtered_df), ncol = 1)
  coordo <- matrix(0, nrow = nrow(filtered_df), ncol = 2)
  for (i in 1:nrow(filtered_df)) {
    ind_lat = filtered_df[i, 2]
    ind_long = filtered_df[i, 1]
    coordo[i, 2] = latitude_df[ind_lat, 1]
    coordo[i, 1] = longitude_df[ind_long, 1]
    map[i,1] = filtered_df[i, 3]
    map[i,2] = filtered_df_delta[i, 3]
  }
  colnames(map) <- c("conc", "delta_conc")
  colnames(coordo) <- c("longitude", "latitude")
  # Sélectionner les valeurs non nulles de map
  map_nonzero <- as.data.frame(map[map[, 1] != 0, , drop = FALSE])
  # Sélectionner les coordonnées correspondant aux indices où map_nonzero est non nul
  coordo_nonzero <- as.data.frame(coordo[map[, 1] != 0, , drop = FALSE])
  # Arrondir les coordonnées au centième de degré
  coordo_nonzero[, 1] <- round(coordo_nonzero[, 1], 3)
  coordo_nonzero[, 2] <- round(coordo_nonzero[, 2], 3)
  #Bons formats de données
  conc_points <- st_set_crs(st_as_sf(SpatialPointsDataFrame(coordo_nonzero, map_nonzero)), 4326)
  print(paste0("Les concentrations en 2019 et ", annee, " sont calculées pour le polluant ", 
               pol, " selon le scénario ", sc, "."))
  return(conc_points)
}

#Fonction qui renvoie une liste de points en France et de concentrations de l'INERIS (cartothèque)############

coordo_ineris <- function(pol) {
  if (pol=="ug_PM25_RH50_haut") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_PM25_RH50_bas") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_NO2_haut") {
    pol <- "ug_NO2"
  }
  else if (pol=="ug_NO2_bas") {
    pol <- "ug_NO2"
  }
  if (pol=="ug_PM25_RH50") {
    path_INERIS_2019 = paste0("data/1-processed-data/SHERPA/conc-2019/Reanalysed_FRA_2019_PM25_avgannual_Ineris_v.Jan2024.nc")
    var = "PM25"}
  else if (pol=="ug_NO2") {
    path_INERIS_2019 = paste0("data/1-processed-data/SHERPA/conc-2019/Reanalysed_FRA_2019_NO2_avgannual_Ineris_v.Jan2024.nc")
    var = "NO2"}
  # Charger les fichier 
  nc_file_conc <- nc_open(path_INERIS_2019)
  latitude <- ncvar_get(nc_file_conc, "lat")
  longitude <- ncvar_get(nc_file_conc, "lon")
  conc19 <- ncvar_get(nc_file_conc, var)
  df <- expand.grid(lon = longitude, lat = latitude) %>%
    mutate(conc19 = as.vector(conc19)) %>%
    filter(!is.na(conc19))
  # Convertir en simple feature collection
  conc_ineris <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) %>%
  # Afficher un aperçu de la collection de fonctionnalités simples
  return(conc_ineris)        
}

#Fonction qui calcule la moyenne de la concentration##############

moy_conc <- function(conc_points) {
  mean_conc <- conc_points %>%
    st_drop_geometry() %>%  # Supprime la colonne géométrie pour les opérations non-spatiales
    summarise(mean_conc = mean(conc, na.rm = TRUE)) %>%
    pull(mean_conc)
  return(mean_conc)
}

moy_meanconc <- function(donnees_expo) {
  mean <- donnees_expo %>%
    st_drop_geometry() %>%  # Supprime la colonne géométrie pour les opérations non-spatiales
    summarise(mean = mean(meanconc, na.rm = TRUE)) %>%
    pull(mean)
  return(mean)
}

moy_meandelta <- function(donnees_expo) {
  mean <- donnees_expo %>%
    st_drop_geometry() %>%  # Supprime la colonne géométrie pour les opérations non-spatiales
    summarise(mean = mean(meandelta, na.rm = TRUE)) %>%
    pull(mean)
  return(mean)
}

#Fonction qui corrige le delta de concentration à partir de la cartothèque de l'INERIS##########

correction <- function(conc_points, conc_ineris) {
  # Détecter le nombre de cœurs disponibles
  nb_cores <- detectCores() - 1
  # Diviser conc_points en sous-ensembles
  subsets <- split(conc_points, factor(sort(rank(row.names(conc_points)) %% nb_cores)))
  # Créer un cluster de cœurs
  cl <- makeCluster(nb_cores)
  # Enregistrer le cluster pour foreach
  registerDoParallel(cl)
  # Charger les packages nécessaires dans chaque cœur du cluster
  clusterEvalQ(cl, {
    library(sf)
    library(nngeo)
  })
  # Appliquer la fonction correction à chaque sous-ensemble en parallèle
  result <- foreach(subset = subsets, .combine = rbind, .packages = c("sf", "nngeo")) %dopar% {
    # Trouver le point le plus proche dans conc_ineris pour chaque point dans le sous-ensemble
    nearest_indices <- st_nn(subset, conc_ineris, k = 1, returnDist = FALSE)
    # Extraire les concentrations conc19 des points les plus proches
    nearest_conc19 <- sapply(nearest_indices, function(idx) conc_ineris$conc19[idx])
    # Calculer les nouvelles valeurs de delta_conc
    new_delta_conc <- subset$delta_conc * nearest_conc19 / subset$conc
    new_conc = nearest_conc19*(1 - subset$delta_conc/subset$conc)
    # Créer une nouvelle simple feature collection avec les valeurs corrigées
    st_sf(
      geometry = st_geometry(subset),
      conc = new_conc,
      delta_conc = new_delta_conc
    )
  }
  # Arrêter le cluster après avoir terminé
  stopCluster(cl)
  return(result)
}

#Fonction pour calculer le pourcentage d'intersection (nécessaire dans le fichier donnees_pourcents)###################

calculate_perc <- function(grille_combinee, donnees_exportees_transformed) {
  for (i in 1:nrow(grille_combinee)) {
    # Extraire les coordonnées du point
    coords <- st_coordinates(grille_combinee[i, "geometry"])
    x <- coords[1]
    y <- coords[2]
    # Construire le rectangle
    p1 <- c(round(x - 0.05, 3), round(y - 0.025, 3))
    p2 <- c(round(x + 0.05, 3), round(y - 0.025, 3))
    p3 <- c(round(x - 0.05, 3), round(y + 0.025, 3))
    p4 <- c(round(x + 0.05, 3), round(y + 0.025, 3))
    rect <- st_polygon(list(rbind(p1, p2, p4, p3, p1)))
    # Conversion du rectangle en simple feature geometry (sfg)
    rectangle_sfg <- st_sfc(rect, crs = 4326)
    # Définition de la couche d'information spatiale (Spatial Feature Collection)
    rectangle <- st_set_crs(st_as_sf(rectangle_sfg), 4326)
    # Trouver le polygone correspondant dans donnees_exportees_transformed
    iriscode <- as.character(grille_combinee[i, "iriscod"])[1]
    polygone <- donnees_exportees_transformed[donnees_exportees_transformed$iriscod == iriscode, ]
    # Intersection entre le polygone et le rectangle
    intersection <- st_intersection(rectangle, polygone$geometry)
    if (nrow(intersection) != 0) {
      area_intersection <- st_area(intersection)
      area_iris <- st_area(polygone$geometry)
      # Calculer le pourcentage d'intersection
      perc <- round(area_intersection / area_iris,3)
      grille_combinee[i, "perc"] <- perc}
  }
  grille_combinee <- grille_combinee %>%
    filter(perc != 0)
  return(grille_combinee)}

#Fonction pour associer les points de concentration à chaque IRIS (nécessaire dans le fichier donnees_pourcents)#####################

association <- function(donnees_exportees_transformed, conc_points) {
  # Initialiser un objet sf vide pour stocker les résultats
  result <- st_sf(iriscod = character(), perc = numeric(), geometry = st_sfc(), crs = st_crs(conc_points))
  # Liste pour stocker les grilles de points générées
  nb_cores <- detectCores() - 1
  subsets <- split(donnees_exportees_transformed, factor(sort(rank(row.names(donnees_exportees_transformed)) %% nb_cores)))
  # Créer un cluster de cœurs
  cl <- makeCluster(nb_cores)
  # Enregistrer le cluster pour foreach
  registerDoParallel(cl)
  # Charger les packages nécessaires dans chaque cœur du cluster
  clusterEvalQ(cl, {library(sf) 
    library(dplyr)})
  clusterExport(cl, c("generate_points", "calculate_perc"))
  grids <- foreach(j = 1:length(subsets), .combine = rbind, .packages = c("sf", "dplyr")) %dopar% {
    donnees <- subsets[[j]]
    grille <- list()
    for (i in 1:nrow(donnees)) {
      polygone <- donnees[i, ]
      points_grille <- generate_points(polygone, conc_points)
      grille[[i]] <- points_grille}
    # Combiner toutes les grilles en un seul simple feature collection
    grille_combinee <- do.call(rbind, grille)
    # Ajouter une colonne perc remplie de zéros à grille_combinee
    grille_combinee <- grille_combinee %>%
      mutate(perc = 0)
    # Calculer les pourcentages d'intersection
    grille_combinee <- calculate_perc(grille_combinee, donnees)
    # Afficher les premières lignes de la grille combinée avec les pourcentages calculés
    grille_combinee}
  # Arrêter le cluster après avoir terminé
  stopCluster(cl)
  # Combiner tous les résultats en un seul
  result <- rbind(result, grids)
  return(result)}

#Fonction pour associer les moyennes de concentration à chaque IRIS (à partir du fichier données_pourcents)######################

expo <- function(donnees_exportees_transformed, conc_corrigee, grille_combinee) {
  # Spécifier le nombre de cœurs à utiliser
  nb_cores <- detectCores() - 1
  subsets <- split(donnees_exportees_transformed, factor(sort(rank(row.names(donnees_exportees_transformed)) %% nb_cores)))
  # Créer un cluster de cœurs
  cl <- makeCluster(nb_cores)
  # Enregistrer le cluster pour foreach
  registerDoParallel(cl)
  # Charger les packages nécessaires dans chaque cœur du cluster
  clusterEvalQ(cl, {library(sf)})
  # Parcourir chaque ligne de donnees_exportees_transformed
  results <- foreach(j = 1:length(subsets), .combine = rbind, .packages = c("sf", "dplyr")) %dopar% {
    donnees <- subsets[[j]]
    for (i in seq_len(nrow(donnees))) {
      iriscode <- donnees$iriscod[i]
      # Extraire les points de grille_combinee correspondants à cet iriscod
      points <- grille_combinee[grille_combinee$iriscod == iriscode, ]
      # Initialiser les valeurs de meanconc et meandelta
      meanconcv <- 0
      meandeltav <- 0
      # Pour chaque point, trouver la concentration correspondante dans conc_points
      for (k in seq_len(nrow(points))) {
        point <- points[k, ]
        nearest_point <- st_nearest_feature(point, conc_corrigee)
        # Extraire les valeurs de conc et delta_conc pour le point trouvé
        conc_value <- conc_corrigee$conc[nearest_point]
        delta_conc_value <- conc_corrigee$delta_conc[nearest_point]
        meanconcv <- meanconcv + conc_value * points$perc[k]
        meandeltav <- meandeltav + delta_conc_value * points$perc[k]}
      # Mettre à jour les valeurs dans donnees_exportees_transformed
      donnees$meanconc[i] <- meanconcv
      donnees$meandelta[i] <- meandeltav}
    donnees}
  # Fermer le cluster
  stopCluster(cl)
  return(results)
}

#Fonction pour exporter au format shapefile##############################

export_data_shp <- function(donnees_shp, path_fichier_shp, title_shp) {
  shp_path <- file.path(path_fichier_shp, paste0(title_shp, ".shp"))
  st_write(donnees_shp, shp_path, delete_layer = TRUE)
}

#Fonction qui calcule l'exposition moyenne pondérée par la population#########################

expo_ponderee_meanconc <- function(donnees_expo, popannee) {
  pop_col <- sym(popannee)  # Convertir le nom de la colonne en symbole
  df <- donnees_expo %>%
    summarize(
      expo = sum(meanconc * !!pop_col, na.rm = TRUE) / sum(!!pop_col, na.rm = TRUE)
    ) %>%
    pull(expo)
  return(df)
}

expo_ponderee_meandelta <- function(donnees_expo, popannee) {
  pop_col <- sym(popannee)  # Convertir le nom de la colonne en symbole
  df <- donnees_expo %>%
    summarize(
      expo = sum(meandelta * !!pop_col, na.rm = TRUE) / sum(!!pop_col, na.rm = TRUE)
    ) %>%
    pull(expo)
  return(df)
}

#Fonction qui calcule la mortalité évitée des plus de 30 ans à l'IRIS#########################

#1. pour les RR de l'analyse principale

mortalite_evitee_iris <- function(donnees_expo, pol, annee) {
  if (pol=="ug_PM25_RH50") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_PM25)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_NO2") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_NO2)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_PM25_RH50_haut") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_PM25_haut)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_NO2_haut") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_NO2_haut)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_PM25_RH50_bas") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_PM25_bas)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_NO2_bas") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_NO2_bas)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  print(paste0("Le calcul de mortalité évitée est effectué pour le polluants ", pol, " à l'IRIS pour tous les âges supérieurs à 30 ans en ", annee, "."))
  return(donnees_expo)
}

#2. pour les RR alternatifs

mortalite_evitee_iris2 <- function(donnees_expo, pol, annee) {
  if (pol=="ug_PM25_Chen") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_Chen)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_NO2_Huang") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_Huang)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_PM25_Chen_haut") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_Chen_haut)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_NO2_Huang_haut") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_Huang_haut)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_PM25_Chen_bas") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_Chen_bas)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  if (pol=="ug_NO2_Huang_bas") {
    donnees_expo[[paste0("mortpol", annee)]] <- donnees_expo[[paste0("mort",annee)]] - donnees_expo[[paste0("mort",annee)]]*exp(-log(RR_Huang_bas)*donnees_expo[["meandelta"]]/10)
    donnees_expo[[paste0("percpol", annee)]] <- donnees_expo[[paste0("mortpol", annee)]] / donnees_expo[[paste0("pop", annee)]]
  }
  print(paste0("Le calcul de mortalité évitée est effectué pour le polluants ", pol, " à l'IRIS pour tous les âges supérieurs à 30 ans en ", annee, "."))
  return(donnees_expo)
}

#Fonction qui réagrège la mortalité évitée à l'échelle nationale########################

mortalite_evitee_nat <- function(donnees_mixtes, annee, pol) {
  donnees_mixtes[[paste0("mortpol", annee)]][is.na(donnees_mixtes[[paste0("mortpol", annee)]])] <- 0
  mortalite_evitee = sum(as.numeric(donnees_mixtes[[paste0("mortpol", annee)]]))
  print(paste0("La mortalité nationale évitée concernant les ", pol, " en ", annee, " est de : ", mortalite_evitee, "."))
  return(mortalite_evitee)
}

#Fonction qui calcule la mortalité évitée par âge #################

#1. pour les RR de l'analyse principale

mortalite_age <- function(donnees_merged, donnees_expo, annee, pol) {
  # Rajouter les colonnes meancnc et meandlt pour les mêmes valeurs de iriscod
  donnees_expo <- donnees_expo %>%
    st_drop_geometry()
  merged_data <- merge(donnees_merged, donnees_expo[, c("iriscod", "meanconc", "meandelta")], by = "iriscod")
  # Calculer la mortalité pour chaque âge
  if (pol=="ug_PM25_RH50") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_PM25)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_PM25_RH50_haut") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_PM25_haut)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_PM25_RH50_bas") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_PM25_bas)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_NO2") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_NO2)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_NO2_haut") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_NO2_haut)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_NO2_bas") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_NO2_bas)*merged_data[["meandelta"]]/10)
  }
  else {
    print("Polluant non reconnu")
  }
  if (annee == "2030") {
    esp = 84
  }
  else if (annee == "2019") {
    esp = 80
  }
  else if (annee == "2050") {
    esp = 86
  }
  else {
    print("Année non reconnu.")
  }
  popannee = paste0("pop", annee)
  iris_interet <- subset(merged_data, age >= 30 & age <= 99)
  mort_interet <- sum(iris_interet[[paste0("mortpol", annee)]], na.rm=TRUE)
  donnees <- data.frame(age = 30:99, mort = rep(0, 70), annees_gagnees = rep(0, 70), tot_mort_30_99 = rep(mort_interet, 70), taux_initial = rep(0, 70), taux_corrige = rep(0, 70),
                        conc = rep(moy_meanconc(donnees_expo), 70), conc_ponderee = rep(expo_ponderee_meanconc(donnees_expo, popannee), 70), 
                        delta_conc = rep(moy_meandelta(donnees_expo), 70), delta_conc_ponderee = rep(expo_ponderee_meandelta(donnees_expo, popannee), 70)) # Initialiser le tableau mort_age
  for (age_ind in 30:99) {
    iris_mort <- subset(merged_data, age == age_ind) # Sous-ensemble de données pour un âge donné
    donnees[donnees$age == age_ind, "mort"] <- sum(iris_mort[[paste0("mortpol", annee)]], na.rm=TRUE) # Somme des morts pour cet âge
    donnees[donnees$age == age_ind, "taux_corrige"] <- sum(iris_mort[[paste0("mort", annee)]]-iris_mort[[paste0("mortpol", annee)]], na.rm=TRUE)/sum(iris_mort[[paste0("pop", annee)]], na.rm=TRUE)
    donnees[donnees$age == age_ind, "taux_initial"] <- sum(iris_mort[[paste0("mort", annee)]], na.rm=TRUE)/sum(iris_mort[[paste0("pop", annee)]], na.rm=TRUE)
    donnees[donnees$age == age_ind, "annees_gagnees"] <- donnees[donnees$age == age_ind, "mort"]*(esp - age_ind)
  }
  if (annee=="2030") {
    somme_annees <- sum(donnees$annees_gagnees[1:55], na.rm = TRUE)
  }
  else if (annee=="2050") {
    somme_annees <- sum(donnees$annees_gagnees[1:57], na.rm = TRUE)
  }
  else {
    somme_annees <- sum(donnees$annees_gagnees[1:51], na.rm = TRUE)
  }
  donnees$tot_annees_gagnees_30_esp <- somme_annees
  donnees <- donnees[order(donnees$age), ]
  return(donnees)
}

#2. pour les RR alternatifs

mortalite_age2 <- function(donnees_merged, donnees_expo, annee, pol) {
  # Rajouter les colonnes meancnc et meandlt pour les mêmes valeurs de iriscod
  donnees_expo <- donnees_expo %>%
    st_drop_geometry()
  merged_data <- merge(donnees_merged, donnees_expo[, c("iriscod", "meanconc", "meandelta")], by = "iriscod")
  # Calculer la mortalité pour chaque âge
  if (pol=="ug_PM25_Chen") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_Chen)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_PM25_Chen_haut") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_Chen_haut)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_PM25_Chen_bas") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_Chen_bas)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_NO2_Huang") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_Huang)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_NO2__Huang_haut") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_Huang_haut)*merged_data[["meandelta"]]/10)
  }
  else if (pol=="ug_NO2_Huang_bas") {
    merged_data[[paste0("mortpol", annee)]] <- merged_data[[paste0("mort",annee)]] - merged_data[[paste0("mort",annee)]]*exp(-log(RR_Huang_bas)*merged_data[["meandelta"]]/10)
  }
  else {
    print("Polluant non reconnu")
  }
  if (annee == "2030") {
    esp = 84
  }
  else if (annee == "2019") {
    esp = 80
  }
  else if (annee == "2050") {
    esp = 86
  }
  else {
    print("Année non reconnu.")
  }
  popannee = paste0("pop", annee)
  iris_interet <- subset(merged_data, age >= 30 & age <= 99)
  mort_interet <- sum(iris_interet[[paste0("mortpol", annee)]], na.rm=TRUE)
  donnees <- data.frame(age = 30:99, mort = rep(0, 70), annees_gagnees = rep(0, 70), tot_mort_30_99 = rep(mort_interet, 70), taux_initial = rep(0, 70), taux_corrige = rep(0, 70),
                        conc = rep(moy_meanconc(donnees_expo), 70), conc_ponderee = rep(expo_ponderee_meanconc(donnees_expo, popannee), 70), 
                        delta_conc = rep(moy_meandelta(donnees_expo), 70), delta_conc_ponderee = rep(expo_ponderee_meandelta(donnees_expo, popannee), 70)) # Initialiser le tableau mort_age
  for (age_ind in 30:99) {
    iris_mort <- subset(merged_data, age == age_ind) # Sous-ensemble de données pour un âge donné
    donnees[donnees$age == age_ind, "mort"] <- sum(iris_mort[[paste0("mortpol", annee)]], na.rm=TRUE) # Somme des morts pour cet âge
    donnees[donnees$age == age_ind, "taux_corrige"] <- sum(iris_mort[[paste0("mort", annee)]]-iris_mort[[paste0("mortpol", annee)]], na.rm=TRUE)/sum(iris_mort[[paste0("pop", annee)]], na.rm=TRUE)
    donnees[donnees$age == age_ind, "taux_initial"] <- sum(iris_mort[[paste0("mort", annee)]], na.rm=TRUE)/sum(iris_mort[[paste0("pop", annee)]], na.rm=TRUE)
    donnees[donnees$age == age_ind, "annees_gagnees"] <- donnees[donnees$age == age_ind, "mort"]*(esp - age_ind)
  }
  if (annee=="2030") {
    somme_annees <- sum(donnees$annees_gagnees[1:55], na.rm = TRUE)
  }
  else if (annee=="2050") {
    somme_annees <- sum(donnees$annees_gagnees[1:57], na.rm = TRUE)
  }
  else {
    somme_annees <- sum(donnees$annees_gagnees[1:51], na.rm = TRUE)
  }
  donnees$tot_annees_gagnees_30_esp <- somme_annees
  donnees <- donnees[order(donnees$age), ]
  return(donnees)
}

#Fonction qui calcule l'espérance de vie gagnée entre 2019 et l'année d'intéressant#####################

life_exp <- function(tab, annee) {
  taux_mort = read.csv("data/1-processed-data/INSEE/taux-mort.csv", sep=";")
  if (annee=="2030") {
    max_year <- 84
    age <- c(taux_mort$age, tab$age)
    taux_initial <- c(taux_mort$taux_2030,  tab$taux_initial) 
    taux_corrige <- c(taux_mort$taux_2030, tab$taux_corrige) 
  }
  if (annee=="2050") {
    max_year <- 86
    age <- c(taux_mort$age, tab$age)
    taux_initial <- c(taux_mort$taux_2050,  tab$taux_initial) 
    taux_corrige <- c(taux_mort$taux_2050, tab$taux_corrige) 
  }
  if (annee=="2019") {
    max_year <- 80
    age <- c(taux_mort$age, tab$age)
    taux_initial <- c(taux_mort$taux_2050,  tab$taux_initial) 
    taux_corrige <- c(taux_mort$taux_2050, tab$taux_corrige) 
  }
  result <- data.frame(age = age, taux_initial = taux_initial, taux_corrige = taux_corrige)
  
  life_expectancy <- function(age, mortality_rate, max_year) {
    for(i in 1:length(age)) {
      if (age[i]>max_year) {
        mortality_rate[i]=1}}
    prop_alive = c(1, cumprod(1-mortality_rate))
    deaths <- -diff(prop_alive)
    life_exp = sum(deaths * 0:max(age))
    return(life_exp)}
  
  # Calculer l'espérance de vie avec le taux initial
  life_exp_initial <- life_expectancy(result$age, result$taux_initial, max_year)
  # Calculer l'espérance de vie avec le taux corrigé
  life_exp_corrige <- life_expectancy(result$age, result$taux_corrige, max_year)
  # Calculer la différence en années d'espérance de vie
  delta_life_exp <- (life_exp_corrige -  life_exp_initial)*12
  result$life_exp_init <- life_exp_initial
  result$life_exp_corrige <- life_exp_corrige
  result$delta_life_mois <- delta_life_exp
  return(result)
}

#Fonction qui trace la carte de population des plus de 30 ans pour une année##################

plot_carte_iris <- function(donnees_mixtes, annee) {
  pop = paste0("pop", annee)
  tm_shape(donnees_mixtes) +
    tm_polygons(pop, border.col = "#00000000") +
    tm_layout(legend.outside = TRUE)
}

save_carte_iris <- function(donnees_mixtes, annee, path) {
  pop = paste0("pop", annee)
  tm <- tm_shape(donnees_mixtes) +
    tm_polygons(pop, border.col = "#00000000") +
    tm_layout(legend.outside = TRUE)
  # Sauvegarde le graphique au format .png
  tmap_save(tm, filename = path)
}

#Fonction qui trace l'exposition de la population en um/m3#########################

plot_carte_expo <- function(result, col, n) {
  tm_shape(result) +
    tm_polygons(col, border.col = "#00000000", breaks = seq(0, n, length.out = 10)) +
    tm_layout(legend.outside = TRUE)
}

save_carte_expo <- function(result, path, col, n) { #col = paste0("meanconc")
  p<-ggplot(data = result) +
    geom_sf(aes_string(fill = col), color = NA) +
    scale_fill_viridis_c(limits = c(0, n)) +
    labs(title = "Exposition en ug/m3",
         fill = "Exposition") +
    theme_minimal()
  ggsave(filename = path, plot = p, device = "png", width = 7, height = 7, units = "in")
}

echelle_n1 <- function(pol) {
  if (pol=="ug_PM25_RH50_haut") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_PM25_RH50_bas") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_NO2_haut") {
    pol <- "ug_NO2"
  }
  else if (pol=="ug_NO2_bas") {
    pol <- "ug_NO2"
  }
  if (pol=="ug_PM25_RH50") {
    n1 <- 13}
  else if (pol=="ug_NO2") {
    n1 <- 34}
  return(n1)
}

echelle_n2 <- function(pol) {
  if (pol=="ug_PM25_RH50_haut") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_PM25_RH50_bas") {
    pol <- "ug_PM25_RH50"
  }
  else if (pol=="ug_NO2_haut") {
    pol <- "ug_NO2"
  }
  else if (pol=="ug_NO2_bas") {
    pol <- "ug_NO2"
  }
  if (pol=="ug_PM25_RH50") {
    n2 <- 9}
  else if (pol=="ug_NO2") {
    n2 <- 25}
  return(n2)
}

#Fonction qui trace le nombre d'IRIS en fonction de la distance caractéristique#####################

distance <- function(donnees_exportees) {
  donnees_exportees <- donnees_exportees %>%
    mutate(area = st_area(geometry),
           sqrt_area = sqrt(as.numeric(area)))  # Convertir les aires en numériques pour prendre la racine carrée
  # Créer un graphique du nombre d'IRIS en fonction de la racine de l'aire du multipolygone
  plot <- ggplot(donnees_exportees, aes(x = sqrt_area)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = 1200, color = "red", linetype = "dashed", size = 1) +  # Ajouter la ligne verticale rouge
    labs(title = "Nombre d'IRIS en fonction de la racine de l'aire du multipolygone",
         x = "Racine carrée de l'aire du multipolygone (m)",
         y = "Nombre d'IRIS") +
    theme_minimal()
  print(plot)
}

################################################################################
##################### 4a- RUNS POUR COMPARER LES MOYENNES #######################
################################################################################

conc_points_2019_pm = coordo_sherpa("s1", "ug_PM25_RH50", "2019")
mean_conc_2019_pm = moy_conc(conc_points_2019_pm)
conc_points_2019_no = coordo_sherpa("s1", "ug_NO2", "2019")
mean_conc_2019_no = moy_conc(conc_points_2019_no)

conc_points_s1_2030_pm = coordo_sherpa("s1", "ug_PM25_RH50", "2030")
mean_conc_s1_2030_pm = moy_conc(conc_points_s1_2030_pm)
conc_points_s2_2030_pm = coordo_sherpa("s2", "ug_PM25_RH50", "2030")
mean_conc_s2_2030_pm = moy_conc(conc_points_s2_2030_pm)
conc_points_s3_2030_pm = coordo_sherpa("s3", "ug_PM25_RH50", "2030")
mean_conc_s3_2030_pm = moy_conc(conc_points_s3_2030_pm)
conc_points_s4_2030_pm = coordo_sherpa("s4", "ug_PM25_RH50", "2030")
mean_conc_s4_2030_pm = moy_conc(conc_points_s4_2030_pm)

conc_points_s1_2050_pm = coordo_sherpa("s1", "ug_PM25_RH50", "2050")
mean_conc_s1_2050_pm = moy_conc(conc_points_s1_2050_pm)
conc_points_s2_2050_pm = coordo_sherpa("s2", "ug_PM25_RH50", "2050")
mean_conc_s2_2050_pm = moy_conc(conc_points_s2_2050_pm)
conc_points_s3_2050_pm = coordo_sherpa("s3", "ug_PM25_RH50", "2050")
mean_conc_s3_2050_pm = moy_conc(conc_points_s3_2050_pm)
conc_points_s4_2050_pm = coordo_sherpa("s4", "ug_PM25_RH50", "2050")
mean_conc_s4_2050_pm = moy_conc(conc_points_s4_2050_pm)

conc_points_s1_2030_no = coordo_sherpa("s1", "ug_NO2", "2030")
mean_conc_s1_2030_no = moy_conc(conc_points_s1_2030_no)
conc_points_s2_2030_no = coordo_sherpa("s2", "ug_NO2", "2030")
mean_conc_s2_2030_no = moy_conc(conc_points_s2_2030_no)
conc_points_s3_2030_no = coordo_sherpa("s3", "ug_NO2", "2030")
mean_conc_s3_2030_no = moy_conc(conc_points_s3_2030_no)
conc_points_s4_2030_no = coordo_sherpa("s4", "ug_NO2", "2030")
mean_conc_s4_2030_no = moy_conc(conc_points_s4_2030_no)

conc_points_s1_2050_no = coordo_sherpa("s1", "ug_NO2", "2050")
mean_conc_s1_2050_no = moy_conc(conc_points_s1_2050_no)
conc_points_s2_2050_no = coordo_sherpa("s2", "ug_NO2", "2050")
mean_conc_s2_2050_no = moy_conc(conc_points_s2_2050_no)
conc_points_s3_2050_no = coordo_sherpa("s3", "ug_NO2", "2050")
mean_conc_s3_2050_no = moy_conc(conc_points_s3_2050_no)
conc_points_s4_2050_no = coordo_sherpa("s4", "ug_NO2", "2050")
mean_conc_s4_2050_no = moy_conc(conc_points_s4_2050_no)

################################################################################
########################### 4b- FICHIERS POURCENTAGES ##########################
################################################################################

donnees_exportees <- st_read(file.path(path_fichier_shp, paste0(title_shp, ".shp")))
donnees_exportees_transformed <- st_transform(donnees_exportees, crs=st_crs(conc_points))
donnees_pourcents <- association(donnees_exportees_transformed, conc_points)
path_fichier_pourcents = "data/2-output-data/pourcents_shp"
title_pourcents = "pourcents-shp"
export_data_shp(donnees_pourcents, path_fichier_pourcents, title_pourcents)

################################################################################
########################### 5-TEST SUR UN SCENARIO #############################
################################################################################

#Choisir le scénario (choix : s1, s2, s3 ou s4)
sc ="s1"
#Choisir le polluant de sortie (choix : ug_PM25_RH50 ou ug_NO2)
pol ="ug_NO2"
#Choisir une année (choix : 2019, 2030 ou 2050)
annee ="2030"

grille_combinee <- st_read(file.path(path_fichier_pourcents, paste0(title_pourcents, ".shp")))
donnees_shp_1 <- st_read(file.path(path_fichier_shp_1, paste0(title_shp_1, ".shp")))
donnees_shp_2 <- st_read(file.path(path_fichier_shp_2, paste0(title_shp_2, ".shp")))
donnees_shp_3 <- st_read(file.path(path_fichier_shp_3, paste0(title_shp_3, ".shp")))
# Coller les trois sf donnees_shp_1, donnees_shp_2 et donnees_shp3
donnees_merged <- rbind(donnees_shp_1, donnees_shp_2)

path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo-shp")
title_expo = "expo-shp"
path_fichier_mixte = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/mixte-shp")
title_mixte = "mixte-shp"

conc_points = coordo_sherpa(sc, pol, annee)
donnees_exportees_transformed <- st_transform(donnees_exportees, crs=st_crs(conc_points))
conc_ineris = coordo_ineris(pol)
conc_corrigee = correction(conc_points, conc_ineris)
donnees_expo = expo(donnees_exportees_transformed, conc_corrigee, grille_combinee)
export_data_shp(donnees_expo, path_fichier_expo, title_expo)
donnees_mixtes = mortalite_evitee_iris(donnees_expo, pol, annee) 
export_data_shp(donnees_mixtes, path_fichier_mixte, title_mixte)

tab = mortalite_age(donnees_merged, donnees_expo, annee, pol)
path = paste0("data/2-output-data/", sc, "/", pol, "/", annee)
csv_path <- file.path(path, paste0("mortalite_evitee.csv"))
write.csv(tab, csv_path, row.names = FALSE)

res = life_exp(tab, annee)
csv_path <- file.path(path, paste0("esp_vie.csv"))
write.csv(res, csv_path, row.names = FALSE)

path_graph_expo_meanconc = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo_meanconc.png")
path_graph_expo_meandelta = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo_meandelta.png")
n1 = echelle_n1(pol)
n2 = echelle_n2(pol)
save_carte_expo(donnees_expo, path_graph_expo_meanconc, "meanconc", n1)
save_carte_expo(donnees_expo, path_graph_expo_meandelta, "meandelta", n2)

################################################################################
################################## 6-BOUCLE ###################################
################################################################################

scenario <- list("s1", "s2", "s3", "s4")
pollutant <- list("ug_NO2", "ug_PM25_RH50")
annees <- list("2030", "2050")

grille_combinee <- st_read(file.path(path_fichier_pourcents, paste0(title_pourcents, ".shp")))
donnees_shp_1 <- st_read(file.path(path_fichier_shp_1, paste0(title_shp_1, ".shp")))
donnees_shp_2 <- st_read(file.path(path_fichier_shp_2, paste0(title_shp_2, ".shp")))
donnees_shp_3 <- st_read(file.path(path_fichier_shp_3, paste0(title_shp_3, ".shp")))
# Coller les trois sf donnees_shp_1, donnees_shp_2 et donnees_shp3
donnees_merged <- rbind(donnees_shp_1, donnees_shp_2, donnees_shp_3)

for (sc in scenario) {
  for (annee in annees) {
    for (pol in pollutant) {
      
      path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo-shp")
      title_expo = "expo-shp"
      path_fichier_mixte = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/mixte-shp")
      title_mixte = "mixte-shp"
      
      conc_points = coordo_sherpa(sc, pol, annee)
      donnees_exportees_transformed <- st_transform(donnees_exportees, crs=st_crs(conc_points))
      conc_ineris = coordo_ineris(pol)
      conc_corrigee = correction(conc_points, conc_ineris)
      donnees_expo = expo(donnees_exportees_transformed, conc_corrigee, grille_combinee)
      export_data_shp(donnees_expo, path_fichier_expo, title_expo)
      donnees_mixtes = mortalite_evitee_iris(donnees_expo, pol, annee) 
      export_data_shp(donnees_mixtes, path_fichier_mixte, title_mixte)

      tab = mortalite_age(donnees_merged, donnees_expo, annee, pol)
      path = paste0("data/2-output-data/", sc, "/", pol, "/", annee)
      csv_path <- file.path(path, paste0("mortalite_evitee.csv"))
      write.csv(tab, csv_path, row.names = FALSE)
      
      res = life_exp(tab, annee)
      csv_path <- file.path(path, paste0("esp_vie.csv"))
      write.csv(res, csv_path, row.names = FALSE)
      
      path_graph_expo_meanconc = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo_meanconc.png")
      path_graph_expo_meandelta = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo_meandelta.png")
      n1 = echelle_n1(pol)
      n2 = echelle_n2(pol)
      save_carte_expo(donnees_expo, path_graph_expo_meanconc, "meanconc", n1)
      save_carte_expo(donnees_expo, path_graph_expo_meandelta, "meandelta", n2)
    }
  }
}

titles_colors <- list(
  "s1" = list("title" = "Scénario 1", "color" = "cornflowerblue"),
  "s2" = list("title" = "Scénario 2", "color" = "darkorchid1"),
  "s3" = list("title" = "Scénario 3", "color" = "chartreuse3"),
  "s4" = list("title" = "Scénario 4", "color" = "darkorange")
)

library(cowplot)

create_map <- function(data, scenario, variable, n) {
  ggplot(data) +
    geom_sf(aes_string(fill = variable), color = NA) +
    scale_fill_viridis_c(limits = c(0, n), "δ (μg/m³)") +
    theme_minimal() +
    ggtitle(titles_colors[[scenario]]$title) +
    theme(plot.title = element_text(color = titles_colors[[scenario]]$color, size = 14, face = "bold"))
}

for (annee in annees) {
  for (pol in pollutant) {
    maps <- list()
    
    for (sc in scenario) {
      # Lire le fichier shapefile pour chaque scénario
      path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo-shp/expo-shp.shp")
      donnees_expo <- st_read(path_fichier_expo)
      
      # Créer la carte pour ce scénario
      n1 = echelle_n1(pol)
      n2 = echelle_n2(pol)
      map_meanconc <- create_map(donnees_expo, sc, "meanconc", n1)
      map_meandelta <- create_map(donnees_expo, sc, "meandelta", n2)
      
      maps[[sc]] <- list("meanconc" = map_meanconc, "meandelta" = map_meandelta)
    }
    
    # Combiner les cartes en une seule figure pour meanconc
    combined_map_meanconc <- plot_grid(
      maps[["s1"]][["meanconc"]], maps[["s2"]][["meanconc"]],
      maps[["s3"]][["meanconc"]], maps[["s4"]][["meanconc"]],
      labels = c("(a)", "(b)", "(c)", "(d)"), ncol = 2, align = 'v'
    )
    
    # Enregistrer la figure combinée pour meanconc
    path_combined_meanconc = paste0("data/2-output-data/combined_meanconc_", pol, "_", annee, ".png")
    ggsave(path_combined_meanconc, combined_map_meanconc, width = 16, height = 12)
    
    # Combiner les cartes en une seule figure pour meandelta
    combined_map_meandelta <- plot_grid(
      maps[["s1"]][["meandelta"]], maps[["s2"]][["meandelta"]],
      maps[["s3"]][["meandelta"]], maps[["s4"]][["meandelta"]],
      labels = c("(a)", "(b)", "(c)", "(d)"), ncol = 2, align = 'v'
    )
    
    # Enregistrer la figure combinée pour meandelta
    path_combined_meandelta = paste0("data/2-output-data/combined_meandelta_", pol, "_", annee, ".png")
    ggsave(path_combined_meandelta, combined_map_meandelta, width = 16, height = 12)
  }
}

################################################################################
############################### 7- INCERTITUDES #################################
################################################################################

scenario <- list("s1", "s2", "s3", "s4")
pollutant <- list("ug_PM25_RH50_bas", "ug_PM25_RH50_haut", "ug_NO2_bas", "ug_NO2_haut")
annees <- list("2030", "2050")

donnees_shp_1 <- st_read(file.path(path_fichier_shp_1, paste0(title_shp_1, ".shp")))
donnees_shp_2 <- st_read(file.path(path_fichier_shp_2, paste0(title_shp_2, ".shp")))
donnees_shp_3 <- st_read(file.path(path_fichier_shp_3, paste0(title_shp_3, ".shp")))
# Coller les trois sf donnees_shp_1, donnees_shp_2 et donnees_shp3
donnees_merged <- rbind(donnees_shp_1, donnees_shp_2, donnees_shp_3)

for (sc in scenario) {
  for (annee in annees) {
    for (pol in pollutant) {
      if ((pol=="ug_PM25_RH50_bas") || (pol=="ug_PM25_RH50_haut")) {
        pol_new = "ug_PM25_RH50"
      }
      else if ((pol=="ug_NO2_bas") || (pol=="ug_NO2_haut")) {
        pol_new = "ug_NO2"
      }
      
      path_fichier_expo_init = paste0("data/2-output-data/", sc, "/", pol_new, "/", annee, "/expo-shp")
      path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo-shp")
      title_expo = "expo-shp"
      path_fichier_mixte = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/mixte-shp")
      title_mixte = "mixte-shp"
      path = paste0("data/2-output-data/", sc, "/", pol, "/", annee)

      donnees_expo = st_read(file.path(path_fichier_expo_init, paste0(title_expo, ".shp")))
      export_data_shp(donnees_expo, path_fichier_expo, title_expo)
      donnees_mixtes = mortalite_evitee_iris(donnees_expo, pol, annee) 
      export_data_shp(donnees_mixtes, path_fichier_mixte, title_mixte)
      
      tab = mortalite_age(donnees_merged, donnees_expo, annee, pol)
      csv_path <- file.path(path, paste0("mortalite_evitee.csv"))
      write.csv(tab, csv_path, row.names = FALSE)
      
      res = life_exp(tab, annee)
      csv_path <- file.path(path, paste0("esp_vie.csv"))
      write.csv(res, csv_path, row.names = FALSE)
   }
  }
}

################################################################################
#################################### 8- GNFR ###################################
################################################################################

scenario <- list("s1", "s2", "s3", "s4")
pollutant <- list("ug_PM25_RH50", "ug_NO2")
annees <- list("2030")
gnfr <- list("trans", "res", "agri")

donnees_shp_1 <- st_read(file.path(path_fichier_shp_1, paste0(title_shp_1, ".shp")))
donnees_shp_2 <- st_read(file.path(path_fichier_shp_2, paste0(title_shp_2, ".shp")))
donnees_shp_3 <- st_read(file.path(path_fichier_shp_3, paste0(title_shp_3, ".shp")))
# Coller les trois sf donnees_shp_1, donnees_shp_2 et donnees_shp3
donnees_merged <- rbind(donnees_shp_1, donnees_shp_2, donnees_shp_3)

for (sc in scenario) {
  for (annee in annees) {
    for (pol in pollutant) {
      for (g in gnfr) {
        
        path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo-shp")
        title_expo = paste0("expo-shp-", g)
        path_fichier_mixte = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/mixte-shp")
        title_mixte = paste0("mixte-shp-",g)
        title_expo = paste0("expo-shp")
        
        conc_points = coordo_sherpa_new(sc, pol, annee, g)
        donnees_exportees_transformed <- st_transform(donnees_exportees, crs=st_crs(conc_points))
        conc_ineris = coordo_ineris(pol)
        conc_corrigee = correction(conc_points, conc_ineris)
        donnees_expo = expo(donnees_exportees_transformed, conc_corrigee, grille_combinee)
        export_data_shp(donnees_expo, path_fichier_expo, title_expo)
        donnees_mixtes = mortalite_evitee_iris(donnees_expo, pol, annee) 
        export_data_shp(donnees_mixtes, path_fichier_mixte, title_mixte)
        
        path = paste0("data/2-output-data/", sc, "/", pol, "/", annee)
        tab = mortalite_age(donnees_merged, donnees_expo, annee, pol)
        csv_path <- file.path(path, paste0("mortalite_evitee_", g, ".csv"))
        write.csv(tab, csv_path, row.names = FALSE)
        
        res = life_exp(tab, annee)
        csv_path <- file.path(path, paste0("esp_vie_", g, ".csv"))
        write.csv(res, csv_path, row.names = FALSE)
      }
    }
  }
}

################################################################################
############################# 9- RR CHEN ET HUANG ##############################
################################################################################


scenario <- list("s1", "s2", "s3", "s4")
pollutant <- list("ug_PM25_Chen", "ug_NO2_Huang", "ug_PM25_Chen_haut", "ug_PM25_Chen_bas", "ug_NO2_Huang_bas", "ug_NO2_Huang_haut")
annees <- list("2030", "2050")

donnees_shp_1 <- st_read(file.path(path_fichier_shp_1, paste0(title_shp_1, ".shp")))
donnees_shp_2 <- st_read(file.path(path_fichier_shp_2, paste0(title_shp_2, ".shp")))
donnees_shp_3 <- st_read(file.path(path_fichier_shp_3, paste0(title_shp_3, ".shp")))
# Coller les trois sf donnees_shp_1, donnees_shp_2 et donnees_shp3
donnees_merged <- rbind(donnees_shp_1, donnees_shp_2, donnees_shp_3)

for (sc in scenario) {
  for (annee in annees) {
    for (pol in pollutant) {
      if ((pol=="ug_PM25_Chen") || (pol=="ug_PM25_Chen_haut") || (pol=="ug_PM25_Chen_bas")) {
        pol_new = "ug_PM25_RH50"
      }
      else if ((pol=="ug_NO2_Huang") || (pol=="ug_NO2_Huang_haut") || (pol=="ug_NO2_Huang_bas")) {
        pol_new = "ug_NO2"
      }
      path_fichier_expo_init = paste0("data/2-output-data/", sc, "/", pol_new, "/", annee, "/expo-shp")
      path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo-shp")
      title_expo = "expo-shp"
      path_fichier_mixte = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/mixte-shp")
      title_mixte = "mixte-shp"
      path = paste0("data/2-output-data/", sc, "/", pol, "/", annee)

      donnees_expo = st_read(file.path(path_fichier_expo_init, paste0(title_expo, ".shp")))
      export_data_shp(donnees_expo, path_fichier_expo, title_expo)
      donnees_mixtes = mortalite_evitee_iris2(donnees_expo, pol, annee) 
      export_data_shp(donnees_mixtes, path_fichier_mixte, title_mixte)
      
      tab = mortalite_age2(donnees_merged, donnees_expo, annee, pol)
      csv_path <- file.path(path, paste0("mortalite_evitee.csv"))
      write.csv(tab, csv_path, row.names = FALSE)
      
      res = life_exp(tab, annee)
      csv_path <- file.path(path, paste0("esp_vie.csv"))
      write.csv(res, csv_path, row.names = FALSE)
    }
  }
}
################################################################################
############################# 10- FIGURES ##############################
#########################################################
#10.1- Figure pour comparer les concentrations SHERPA et INERIS#################
# Données pour les graphiques
df <- data.frame(sherpa = conc_points_NO2$conc, ineris = conc_corrigee_NO2$conc)
carte <- conc_corrigee_NO2 %>%
  mutate(conc = conc_points_NO2$conc / conc_corrigee_NO2$conc) %>%
  select(conc)
# Données pour les graphiques PM
df_pm <- data.frame(sherpa = conc_points_PM$conc, ineris = conc_corrigee_PM$conc)
carte_pm <- conc_corrigee_PM %>%
  mutate(conc = conc_points_PM$conc / conc_corrigee_PM$conc) %>%
  select(conc)
# Limites des axes
xlim <- c(0, 12)
ylim <- c(0, 12)
xlimn <- c(0, 25)
ylimn <- c(0, 25)
# Premier graphique
custom_palette <- colorRampPalette(c("blueviolet", "darkgray", "darkgoldenrod1"))(100)
p3 <- ggplot(df, aes(x = sherpa, y = ineris)) +
  geom_point(color = "darkgreen", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(x = "Concentration SHERPA", y = "Concentration INERIS") +
  xlim(xlimn) + ylim(ylimn) +
  coord_fixed(ratio = 1)
# Deuxième graphique
p4 <- ggplot(data = carte) +
  geom_sf(aes(color = conc)) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 2)) +
  labs(color = "SHERPA / INERIS") +
  theme_minimal()
# Premier graphique pour PM (p3)
p1 <- ggplot(df_pm, aes(x = sherpa, y = ineris)) +
  geom_point(color = "darkgreen", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(x = "Concentration SHERPA", y = "Concentration INERIS") +
  xlim(xlim) + ylim(ylim) +
  coord_fixed(ratio = 1)
# Deuxième graphique pour PM (p4)
p2 <- ggplot(data = carte_pm) +
  geom_sf(aes(color = conc)) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 2)) +
  labs(color = "SHERPA / INERIS") +
  theme_minimal()
# Combinaison des graphiques en une grille à deux colonnes
combined_plot <- plot_grid(p1, p2, p3, p4, labels = c("(a)", "(b)", "(c)", "(d)"), ncol = 2)
# Enregistrer la grille combinée en tant qu'image PNG
ggsave(filename = "data/2-output-data/combined_ineris_sherpa_no2_pm.png", plot = combined_plot, device = "png", width = 14, height = 14, units = "in")

#10.2- Figure pour obtenir les concentrations en 2019#################

conc_points_NO2 = coordo_sherpa("s1", "ug_NO2", "2019")
conc_points_PM = coordo_sherpa("s1", "ug_PM25_RH50", "2019")
donnees_exportees_transformed_NO2 <- st_transform(donnees_exportees, crs=st_crs(conc_points_NO2))
donnees_exportees_transformed_PM <- st_transform(donnees_exportees, crs=st_crs(conc_points_PM))
conc_ineris_PM = coordo_ineris("ug_PM25_RH50")
conc_ineris_NO2 = coordo_ineris("ug_NO2")
conc_corrigee_PM = correction(conc_points_PM, conc_ineris_PM)
conc_corrigee_NO2 = correction(conc_points_NO2, conc_ineris_NO2)
conc_corrigee_PM$delta_conc <- ifelse(conc_corrigee_PM$conc > 5, conc_corrigee_PM$conc - 5, 0)
conc_corrigee_NO2$delta_conc <- ifelse(conc_corrigee_NO2$conc > 10, conc_corrigee_NO2$conc - 10, 0)

donnees_expo_NO2 = expo(donnees_exportees_transformed_NO2, conc_corrigee_NO2, grille_combinee)
donnees_expo_PM = expo(donnees_exportees_transformed_PM, conc_corrigee_PM, grille_combinee)

path_fichier_PM = paste0("data/2-output-data/s0/ug_PM25_RH50/2019/expo-shp")
path_fichier_NO2 = paste0("data/2-output-data/s0/ug_NO2/2019/expo-shp")
path_fichier_mixte_PM = paste0("data/2-output-data/s0/ug_PM25_RH50/2019/mixte-shp")
path_fichier_mixte_NO2 = paste0("data/2-output-data/s0/ug_NO2/2019/mixte-shp")
title_expo = "expo-shp"
title_mixte = "mixte-shp"
export_data_shp(donnees_expo_PM, path_fichier_PM, title_expo)
export_data_shp(donnees_expo_NO2, path_fichier_NO2, title_expo)
donnees_mixtes_PM = mortalite_evitee_iris(donnees_expo_PM, "ug_PM25_RH50", "2019") 
donnees_mixtes_NO2 = mortalite_evitee_iris(donnees_expo_NO2, "ug_NO2", "2019") 
export_data_shp(donnees_mixtes_PM, path_fichier_mixte_PM, title_mixte)
export_data_shp(donnees_mixtes_NO2, path_fichier_mixte_NO2, title_mixte)

tab_PM = mortalite_age(donnees_merged, donnees_expo_PM, "2019", "ug_PM25_RH50")
tab_NO2 = mortalite_age(donnees_merged, donnees_expo_NO2, "2019", "ug_NO2")
path_PM = paste0("data/2-output-data/s0/ug_PM25_RH50/2019")
path_NO2 = paste0("data/2-output-data/s0/ug_NO2/2019")
csv_path_PM <- file.path(path_PM, paste0("mortalite_evitee.csv"))
csv_path_NO2 <- file.path(path_NO2, paste0("mortalite_evitee.csv"))
write.csv(tab_PM, csv_path_PM, row.names = FALSE)
write.csv(tab_NO2, csv_path_NO2, row.names = FALSE)
res_PM = life_exp(tab_PM, "2019")
res_NO2 = life_exp(tab_NO2, "2019")
csv_path_PM <- file.path(path_PM, paste0("esp_vie.csv"))
csv_path_NO2 <- file.path(path_NO2, paste0("esp_vie.csv"))
write.csv(res_PM, csv_path_PM, row.names = FALSE)
write.csv(res_NO2, csv_path_NO2, row.names = FALSE)
path_graph_expo_meanconc_PM = paste0("data/2-output-data/s0/ug_PM25_RH50/2019/expo_meanconc.png")
path_graph_expo_meandelta_PM = paste0("data/2-output-data/s0/ug_PM25_RH50/2019/expo_meandelta.png")
n1 = echelle_n1("ug_PM25_RH50")
n2 = echelle_n2("ug_PM25_RH50")
save_carte_expo(donnees_expo_PM, path_graph_expo_meanconc_PM, "meanconc", n1)
save_carte_expo(donnees_expo_PM, path_graph_expo_meandelta_PM, "meandelta", n2)
path_graph_expo_meanconc_NO2 = paste0("data/2-output-data/s0/ug_NO2/2019/expo_meanconc.png")
path_graph_expo_meandelta_NO2 = paste0("data/2-output-data/s0/ug_NO2/2019/expo_meandelta.png")
n1 = echelle_n1("ug_NO2")
n2 = echelle_n2("ug_NO2")
save_carte_expo(donnees_expo_NO2, path_graph_expo_meanconc_NO2, "meanconc", n1)
save_carte_expo(donnees_expo_NO2, path_graph_expo_meandelta_NO2, "meandelta", n2)

tab_PM = mortalite_age(donnees_merged, donnees_expo_PM, "2019", "ug_PM25_RH50_bas")
tab_NO2 = mortalite_age(donnees_merged, donnees_expo_NO2, "2019", "ug_NO2_bas")
path_PM = paste0("data/2-output-data/s0/ug_PM25_RH50_bas/2019")
path_NO2 = paste0("data/2-output-data/s0/ug_NO2_bas/2019")
csv_path_PM <- file.path(path_PM, paste0("mortalite_evitee.csv"))
csv_path_NO2 <- file.path(path_NO2, paste0("mortalite_evitee.csv"))
write.csv(tab_PM, csv_path_PM, row.names = FALSE)
write.csv(tab_NO2, csv_path_NO2, row.names = FALSE)
res_PM = life_exp(tab_PM, "2019")
res_NO2 = life_exp(tab_NO2, "2019")
csv_path_PM <- file.path(path_PM, paste0("esp_vie.csv"))
csv_path_NO2 <- file.path(path_NO2, paste0("esp_vie.csv"))
write.csv(res_PM, csv_path_PM, row.names = FALSE)
write.csv(res_NO2, csv_path_NO2, row.names = FALSE)

tab_PM = mortalite_age(donnees_merged, donnees_expo_PM, "2019", "ug_PM25_RH50_haut")
tab_NO2 = mortalite_age(donnees_merged, donnees_expo_NO2, "2019", "ug_NO2_haut")
path_PM = paste0("data/2-output-data/s0/ug_PM25_RH50_haut/2019")
path_NO2 = paste0("data/2-output-data/s0/ug_NO2_haut/2019")
csv_path_PM <- file.path(path_PM, paste0("mortalite_evitee.csv"))
csv_path_NO2 <- file.path(path_NO2, paste0("mortalite_evitee.csv"))
write.csv(tab_PM, csv_path_PM, row.names = FALSE)
write.csv(tab_NO2, csv_path_NO2, row.names = FALSE)
res_PM = life_exp(tab_PM, "2019")
res_NO2 = life_exp(tab_NO2, "2019")
csv_path_PM <- file.path(path_PM, paste0("esp_vie.csv"))
csv_path_NO2 <- file.path(path_NO2, paste0("esp_vie.csv"))
write.csv(res_PM, csv_path_PM, row.names = FALSE)
write.csv(res_NO2, csv_path_NO2, row.names = FALSE)


scenario <- list("s0")
pollutant <- list("ug_NO2", "ug_PM25_RH50")
annees <- list("2019")

create_map <- function(data, scenario, variable, n) {
  ggplot(data) +
    geom_sf(aes_string(fill = variable), color = NA) +
    scale_fill_viridis_c(limits = c(0, n), name = "C (μg/m³)") +
    theme_minimal()
}

for (annee in annees) {
  for (sc in scenario) {
    maps <- list()
    
    for (pol in pollutant) {
      # Lire le fichier shapefile pour chaque scénario
      path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/", annee, "/expo-shp/expo-shp.shp")
      donnees_expo <- st_read(path_fichier_expo)
      # Créer la carte pour ce scénario
      n1 = echelle_n1(pol)
      n2 = echelle_n2(pol)
      map_meanconc <- create_map(donnees_expo, sc, "meanconc", n1)
      map_meandelta <- create_map(donnees_expo, sc, "meandelta", n2)
      maps[[pol]] <- list("meanconc" = map_meanconc, "meandelta" = map_meandelta)
    }
    # Combiner les cartes en une seule figure pour meanconc
    combined_map_meanconc <- plot_grid(
      maps[["ug_PM25_RH50"]][["meanconc"]], maps[["ug_PM25_RH50"]][["meandelta"]],
      maps[["ug_NO2"]][["meanconc"]], maps[["ug_NO2"]][["meandelta"]],
      labels = c("(a)", "(b)", "(c)", "(d)"), ncol = 2, align = 'v'
    )
    # Enregistrer la figure combinée pour meanconc
    path_combined_meanconc = paste0("data/2-output-data/combined_", annee, ".png")
    ggsave(path_combined_meanconc, combined_map_meanconc, width = 14, height = 14)
  }
}

#10.3- Figures boites à moustache####################################################

path_fichier_mixte_NO2_s1_2030 = paste0("data/2-output-data/s1/ug_NO2/2030/mixte-shp")
path_fichier_mixte_NO2_s2_2030 = paste0("data/2-output-data/s2/ug_NO2/2030/mixte-shp")
path_fichier_mixte_NO2_s3_2030 = paste0("data/2-output-data/s3/ug_NO2/2030/mixte-shp")
path_fichier_mixte_NO2_s4_2030 = paste0("data/2-output-data/s4/ug_NO2/2030/mixte-shp")
path_fichier_mixte_NO2_s1_2050 = paste0("data/2-output-data/s1/ug_NO2/2050/mixte-shp")
path_fichier_mixte_NO2_s2_2050 = paste0("data/2-output-data/s2/ug_NO2/2050/mixte-shp")
path_fichier_mixte_NO2_s3_2050 = paste0("data/2-output-data/s3/ug_NO2/2050/mixte-shp")
path_fichier_mixte_NO2_s4_2050 = paste0("data/2-output-data/s4/ug_NO2/2050/mixte-shp")

path_fichier_mixte_PM_s1_2030 = paste0("data/2-output-data/s1/ug_PM25_RH50/2030/mixte-shp")
path_fichier_mixte_PM_s2_2030 = paste0("data/2-output-data/s2/ug_PM25_RH50/2030/mixte-shp")
path_fichier_mixte_PM_s3_2030 = paste0("data/2-output-data/s3/ug_PM25_RH50/2030/mixte-shp")
path_fichier_mixte_PM_s4_2030 = paste0("data/2-output-data/s4/ug_PM25_RH50/2030/mixte-shp")
path_fichier_mixte_PM_s1_2050 = paste0("data/2-output-data/s1/ug_PM25_RH50/2050/mixte-shp")
path_fichier_mixte_PM_s2_2050 = paste0("data/2-output-data/s2/ug_PM25_RH50/2050/mixte-shp")
path_fichier_mixte_PM_s3_2050 = paste0("data/2-output-data/s3/ug_PM25_RH50/2050/mixte-shp")
path_fichier_mixte_PM_s4_2050 = paste0("data/2-output-data/s4/ug_PM25_RH50/2050/mixte-shp")

path_fichier_mixte_NO2_2019 = paste0("data/2-output-data/s0/ug_NO2/2019/mixte-shp")
path_fichier_mixte_PM_2019 = paste0("data/2-output-data/s0/ug_PM25_RH50/2019/mixte-shp")

title_mixte = "mixte-shp"
donnees_NO2_s1_2030 = st_read(file.path(path_fichier_mixte_NO2_s1_2030, paste0(title_mixte, ".shp")))
donnees_NO2_s2_2030 = st_read(file.path(path_fichier_mixte_NO2_s2_2030, paste0(title_mixte, ".shp")))
donnees_NO2_s3_2030 = st_read(file.path(path_fichier_mixte_NO2_s3_2030, paste0(title_mixte, ".shp")))
donnees_NO2_s4_2030 = st_read(file.path(path_fichier_mixte_NO2_s4_2030, paste0(title_mixte, ".shp")))
donnees_NO2_s1_2050 = st_read(file.path(path_fichier_mixte_NO2_s1_2050, paste0(title_mixte, ".shp")))
donnees_NO2_s2_2050 = st_read(file.path(path_fichier_mixte_NO2_s2_2050, paste0(title_mixte, ".shp")))
donnees_NO2_s3_2050 = st_read(file.path(path_fichier_mixte_NO2_s3_2050, paste0(title_mixte, ".shp")))
donnees_NO2_s4_2050 = st_read(file.path(path_fichier_mixte_NO2_s4_2050, paste0(title_mixte, ".shp")))

donnees_PM_s1_2030 = st_read(file.path(path_fichier_mixte_PM_s1_2030, paste0(title_mixte, ".shp")))
donnees_PM_s2_2030 = st_read(file.path(path_fichier_mixte_PM_s2_2030, paste0(title_mixte, ".shp")))
donnees_PM_s3_2030 = st_read(file.path(path_fichier_mixte_PM_s3_2030, paste0(title_mixte, ".shp")))
donnees_PM_s4_2030 = st_read(file.path(path_fichier_mixte_PM_s4_2030, paste0(title_mixte, ".shp")))
donnees_PM_s1_2050 = st_read(file.path(path_fichier_mixte_PM_s1_2050, paste0(title_mixte, ".shp")))
donnees_PM_s2_2050 = st_read(file.path(path_fichier_mixte_PM_s2_2050, paste0(title_mixte, ".shp")))
donnees_PM_s3_2050 = st_read(file.path(path_fichier_mixte_PM_s3_2050, paste0(title_mixte, ".shp")))
donnees_PM_s4_2050 = st_read(file.path(path_fichier_mixte_PM_s4_2050, paste0(title_mixte, ".shp")))

donnees_PM_2019 = st_read(file.path(path_fichier_mixte_PM_2019, paste0(title_mixte, ".shp")))
donnees_NO2_2019 = st_read(file.path(path_fichier_mixte_PM_2019, paste0(title_mixte, ".shp")))

# Ajouter des colonnes pour le scénario et l'année
donnees_NO2_s1_2030$scen <- "s1"
donnees_NO2_s2_2030$scen <- "s2"
donnees_NO2_s3_2030$scen <- "s3"
donnees_NO2_s4_2030$scen <- "s4"
donnees_NO2_s1_2030$year <- 2030
donnees_NO2_s2_2030$year <- 2030
donnees_NO2_s3_2030$year <- 2030
donnees_NO2_s4_2030$year <- 2030
donnees_NO2_s1_2050$scen <- "s1"
donnees_NO2_s2_2050$scen <- "s2"
donnees_NO2_s3_2050$scen <- "s3"
donnees_NO2_s4_2050$scen <- "s4"
donnees_NO2_s1_2050$year <- 2050
donnees_NO2_s2_2050$year <- 2050
donnees_NO2_s3_2050$year <- 2050
donnees_NO2_s4_2050$year <- 2050

# Combiner les données
donnees_NO2_combined <- bind_rows(donnees_NO2_s1_2030, donnees_NO2_s2_2030, donnees_NO2_s3_2030, donnees_NO2_s4_2030, 
                                  donnees_NO2_s1_2050, donnees_NO2_s2_2050, donnees_NO2_s3_2050, donnees_NO2_s4_2050)
path = "data/2-output-data/comparaison_sc_NO2_delta.png"
# Créer le graphique en boîte à moustache
p<-ggplot(donnees_NO2_combined, aes(x = factor(year), y = meandlt, fill = scen)) +
  geom_boxplot(coef = 0) +
  scale_fill_manual(values = c("s1" = "cornflowerblue", "s2" = "darkorchid1", "s3" = "chartreuse3", "s4" = "darkorange")) +
  labs(y = "Différence de concentration par rapport à 2019 (μg/m³)",
       fill = "Scénario") +
  theme_minimal()
ggsave(filename = path, plot = p, device = "png", width = 7, height = 7, units = "in")

# Ajouter des colonnes pour le scénario et l'année
donnees_PM_s1_2030$scen <- "s1"
donnees_PM_s2_2030$scen <- "s2"
donnees_PM_s3_2030$scen <- "s3"
donnees_PM_s4_2030$scen <- "s4"
donnees_PM_s1_2030$year <- 2030
donnees_PM_s2_2030$year <- 2030
donnees_PM_s3_2030$year <- 2030
donnees_PM_s4_2030$year <- 2030
donnees_PM_s1_2050$scen <- "s1"
donnees_PM_s2_2050$scen <- "s2"
donnees_PM_s3_2050$scen <- "s3"
donnees_PM_s4_2050$scen <- "s4"
donnees_PM_s1_2050$year <- 2050
donnees_PM_s2_2050$year <- 2050
donnees_PM_s3_2050$year <- 2050
donnees_PM_s4_2050$year <- 2050

# Combiner les données
donnees_PM_combined <- bind_rows(donnees_PM_s1_2030, donnees_PM_s2_2030, donnees_PM_s3_2030, donnees_PM_s4_2030, 
                                 donnees_PM_s1_2050, donnees_PM_s2_2050, donnees_PM_s3_2050, donnees_PM_s4_2050)

path = "data/2-output-data/comparaison_sc_PM_delta.png"
# Créer le graphique en boîte à moustache sans les barres
p<-ggplot(donnees_PM_combined, aes(x = factor(year), y = meandlt, fill = scen)) +
  geom_boxplot(coef = 0) +
  scale_fill_manual(values = c("s1" = "cornflowerblue", "s2" = "darkorchid1", "s3" = "chartreuse3", "s4" = "darkorange")) +
  labs(y = "Différence de concentration par rapport à 2019 (μg/m³)",
       fill = "Scénario") +
  theme_minimal()
ggsave(filename = path, plot = p, device = "png", width = 7, height = 7, units = "in")


# Ajouter des colonnes pour le scénario et l'année
donnees_PM_s1_2030$scen <- "s1"
donnees_PM_s2_2030$scen <- "s2"
donnees_PM_s3_2030$scen <- "s3"
donnees_PM_s4_2030$scen <- "s4"
donnees_PM_s1_2030$year <- 2030
donnees_PM_s2_2030$year <- 2030
donnees_PM_s3_2030$year <- 2030
donnees_PM_s4_2030$year <- 2030
donnees_PM_s1_2050$scen <- "s1"
donnees_PM_s2_2050$scen <- "s2"
donnees_PM_s3_2050$scen <- "s3"
donnees_PM_s4_2050$scen <- "s4"
donnees_PM_s1_2050$year <- 2050
donnees_PM_s2_2050$year <- 2050
donnees_PM_s3_2050$year <- 2050
donnees_PM_s4_2050$year <- 2050
donnees_PM_2019$scen <- "2019"
donnees_PM_2019$year <- 2019

# Combiner les données
donnees_PM_combined <- bind_rows(donnees_PM_2019, 
                                 donnees_PM_s1_2030, donnees_PM_s2_2030, donnees_PM_s3_2030, donnees_PM_s4_2030, 
                                 donnees_PM_s1_2050, donnees_PM_s2_2050, donnees_PM_s3_2050, donnees_PM_s4_2050)

path = "data/2-output-data/comparaison_sc_PM_conc.png"
# Créer le graphique en boîte à moustache sans les barres
p1<- ggplot(donnees_PM_combined, aes(x = factor(year), y = meancnc, fill = scen)) +
  geom_boxplot(coef = 0) +
  scale_fill_manual(values = c("s1" = "cornflowerblue", "s2" = "darkorchid1", "s3" = "chartreuse3", "s4" = "darkorange", "2019" = "grey")) +
  labs(y = "Concentration en PM2.5 (μg/m³)",
       fill = "Scénario") +
  theme_minimal()
ggsave(filename = path, plot = p1, device = "png", width = 7, height = 7, units = "in")


# Ajouter des colonnes pour le scénario et l'année
donnees_NO2_s1_2030$scen <- "s1"
donnees_NO2_s2_2030$scen <- "s2"
donnees_NO2_s3_2030$scen <- "s3"
donnees_NO2_s4_2030$scen <- "s4"
donnees_NO2_s1_2030$year <- 2030
donnees_NO2_s2_2030$year <- 2030
donnees_NO2_s3_2030$year <- 2030
donnees_NO2_s4_2030$year <- 2030
donnees_NO2_s1_2050$scen <- "s1"
donnees_NO2_s2_2050$scen <- "s2"
donnees_NO2_s3_2050$scen <- "s3"
donnees_NO2_s4_2050$scen <- "s4"
donnees_NO2_s1_2050$year <- 2050
donnees_NO2_s2_2050$year <- 2050
donnees_NO2_s3_2050$year <- 2050
donnees_NO2_s4_2050$year <- 2050
donnees_NO2_2019$scen <- "2019"
donnees_NO2_2019$year <- 2019

# Combiner les données
donnees_NO2_combined <- bind_rows(donnees_NO2_2019, 
                                  donnees_NO2_s1_2030, donnees_NO2_s2_2030, donnees_NO2_s3_2030, donnees_NO2_s4_2030, 
                                  donnees_NO2_s1_2050, donnees_NO2_s2_2050, donnees_NO2_s3_2050, donnees_NO2_s4_2050)

path = "data/2-output-data/comparaison_sc_NO2_conc.png"
# Créer le graphique en boîte à moustache sans les barres
p2 <- ggplot(donnees_NO2_combined, aes(x = factor(year), y = meancnc, fill = scen)) +
  geom_boxplot(coef = 0) +
  scale_fill_manual(values = c("s1" = "cornflowerblue", "s2" = "darkorchid1", "s3" = "chartreuse3", "s4" = "darkorange", "2019" = "grey")) +
  labs(y = "Concentration en NO2 (μg/m³)",
       fill = "Scénario") +
  theme_minimal()

# Sauvegarder le graphique
ggsave(filename = path, plot = p2, device = "png", width = 7, height = 7, units = "in")


path = "data/2-output-data/comparaison_sc_conc.png"
# Afficher les graphiques côte à côte
p<-plot_grid(p1, p2, ncol = 2, labels=c("(a)", "(b)"))
ggsave(filename = path, plot = p, device = "png", width = 10, height = 7, units = "in")

#10.4- Figures histogrammes de mortalité et d'années de vie gagnées####################################################

# Fonction pour lire et traiter les données pour un polluant donné
process_data <- function(pollutant, pollutant_bas, pollutant_haut) {
  # Initialiser les tableaux pour les morts et les années gagnées
  morts_data <- tibble(age = age_groups)
  annees_data <- tibble(age = age_groups)
  
  morts_data_bas <- tibble(age = age_groups)
  morts_data_haut <- tibble(age = age_groups)
  annees_data_bas <- tibble(age = age_groups)
  annees_data_haut <- tibble(age = age_groups)
  
  # Initialiser les colonnes pour chaque scénario
  for (sc in scenarios) {
    morts_data <- morts_data %>% mutate(!!paste0("mort_", sc) := 0)
    annees_data <- annees_data %>% mutate(!!paste0("annee_", sc) := 0)
    
    morts_data_bas <- morts_data_bas %>% mutate(!!paste0("mort_", sc) := 0)
    morts_data_haut <- morts_data_haut %>% mutate(!!paste0("mort_", sc) := 0)
    annees_data_bas <- annees_data_bas %>% mutate(!!paste0("annee_", sc) := 0)
    annees_data_haut <- annees_data_haut %>% mutate(!!paste0("annee_", sc) := 0)
  }
  
  # Fonction interne pour traiter un scénario donné et un type de données (central, bas, haut)
  process_scenario <- function(sc, pollutant_type, morts_data, annees_data) {
    file_path <- paste0("data/2-output-data/", sc, "/", pollutant_type, "/2030/mortalite_evitee.csv")
    
    # Lire le fichier
    data <- read_csv(file_path)
    
    # Filtrer et sommer les données par tranche d'âge
    for (age_group in age_groups) {
      age_min <- as.numeric(strsplit(age_group, "-")[[1]][1])
      age_max <- as.numeric(strsplit(age_group, "-")[[1]][2])
      
      # Filtrer les données par tranche d'âge
      age_data <- data %>% filter(age >= age_min & age <= age_max)
      
      # Ajouter les sommes au tableau des morts
      morts_data <- morts_data %>%
        mutate(!!paste0("mort_", sc) := ifelse(age == age_group, sum(age_data$mort, na.rm = TRUE), .data[[paste0("mort_", sc)]]))
      
      # Ajouter les sommes au tableau des années gagnées (en excluant les valeurs négatives)
      annees_data <- annees_data %>%
        mutate(!!paste0("annee_", sc) := ifelse(age == age_group, sum(age_data$annees_gagnees[age_data$annees_gagnees > 0], na.rm = TRUE), .data[[paste0("annee_", sc)]]))
    }
    
    list(morts_data = morts_data, annees_data = annees_data)
  }
  
  # Boucle sur chaque scénario pour chaque type de donnée
  for (sc in scenarios) {
    result_central <- process_scenario(sc, pollutant, morts_data, annees_data)
    morts_data <- result_central$morts_data
    annees_data <- result_central$annees_data
    
    result_bas <- process_scenario(sc, pollutant_bas, morts_data_bas, annees_data_bas)
    morts_data_bas <- result_bas$morts_data
    annees_data_bas <- result_bas$annees_data
    
    result_haut <- process_scenario(sc, pollutant_haut, morts_data_haut, annees_data_haut)
    morts_data_haut <- result_haut$morts_data
    annees_data_haut <- result_haut$annees_data
  }
  
  list(morts = morts_data, annees = annees_data, morts_bas = morts_data_bas, morts_haut = morts_data_haut, annees_bas = annees_data_bas, annees_haut = annees_data_haut)
}

# Traiter les données pour chaque polluant
data_NO2 <- process_data("ug_NO2", "ug_NO2_bas", "ug_NO2_haut")
data_PM25 <- process_data("ug_PM25_RH50", "ug_PM25_RH50_bas", "ug_PM25_RH50_haut")

# Fonction pour créer les histogrammes
plot_histograms <- function(data, pollutant) {
  morts_data <- data$morts
  annees_data <- data$annees
  
  morts_data_bas <- data$morts_bas
  morts_data_haut <- data$morts_haut
  annees_data_bas <- data$annees_bas
  annees_data_haut <- data$annees_haut
  
  morts_long <- morts_data %>% pivot_longer(cols = starts_with("mort_"), names_to = "scenario", values_to = "morts")
  annees_long <- annees_data %>% pivot_longer(cols = starts_with("annee_"), names_to = "scenario", values_to = "annees")
  
  morts_long_bas <- morts_data_bas %>% pivot_longer(cols = starts_with("mort_"), names_to = "scenario", values_to = "morts_bas")
  morts_long_haut <- morts_data_haut %>% pivot_longer(cols = starts_with("mort_"), names_to = "scenario", values_to = "morts_haut")
  annees_long_bas <- annees_data_bas %>% pivot_longer(cols = starts_with("annee_"), names_to = "scenario", values_to = "annees_bas")
  annees_long_haut <- annees_data_haut %>% pivot_longer(cols = starts_with("annee_"), names_to = "scenario", values_to = "annees_haut")
  
  morts_long$variable <- "Mortalité évitée"
  annees_long$variable <- "Années de vie perdues évitées"
  morts_long_bas$variable <- "Mortalité évitée"
  annees_long_bas$variable <- "Années de vie perdues évitées"
  morts_long_haut$variable <- "Mortalité évitée"
  annees_long_haut$variable <- "Années de vie perdues évitées"
  
  combined_data <- bind_rows(
    morts_long %>% rename(valeur = morts),
    annees_long %>% rename(valeur = annees)
  )
  
  combined_data_bas <- bind_rows(
    morts_long_bas %>% rename(valeur_bas = morts_bas),
    annees_long_bas %>% rename(valeur_bas = annees_bas)
  )
  
  combined_data_haut <- bind_rows(
    morts_long_haut %>% rename(valeur_haut = morts_haut),
    annees_long_haut %>% rename(valeur_haut = annees_haut)
  )
  
  combined_data <- combined_data %>%
    left_join(combined_data_bas, by = c("age", "scenario", "variable")) %>%
    left_join(combined_data_haut, by = c("age", "scenario", "variable"))
  
  combined_data$scenario <- factor(combined_data$scenario, levels = c(paste0("mort_s", 1:4), paste0("annee_s", 1:4)))
  
  combined_data <- combined_data %>%
    mutate(scenario = recode(scenario,
                             "mort_s1" = "Scénario 1",
                             "mort_s2" = "Scénario 2",
                             "mort_s3" = "Scénario 3",
                             "mort_s4" = "Scénario 4",
                             "annee_s1" = "Scénario 1",
                             "annee_s2" = "Scénario 2",
                             "annee_s3" = "Scénario 3",
                             "annee_s4" = "Scénario 4"))
  colors <- c("Scénario 1" = "cornflowerblue", "Scénario 2" = "darkorchid1", "Scénario 3" = "chartreuse3", "Scénario 4" = "darkorange")
  combined_data$variable <- factor(combined_data$variable, levels = c("Mortalité évitée", "Années de vie perdues évitées"))
  
  ggplot(combined_data, aes(x = age, y = valeur, fill = scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = valeur_bas, ymax = valeur_haut), position = position_dodge(0.9), width = 0.25) +
    scale_fill_manual(values = colors) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +  # Facet_wrap avec 2 colonnes
    labs(title = paste("Histogrammes pour", pollutant, "en 2030"), x = "Groupe d'âge", y = "Nombre") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = "Scénarios"))
}

# Créer les histogrammes
plot_NO2 <- plot_histograms(data_NO2, "ug_NO2")
plot_PM25 <- plot_histograms(data_PM25, "ug_PM25_RH50")

# Afficher les histogrammes
print(plot_NO2)
print(plot_PM25)

path1 = "data/2-output-data/comparaison_anneemort_NO2.png"
path2 = "data/2-output-data/comparaison_anneemort_PM.png"

# Sauvegarder le graphique
ggsave(filename = path1, plot = plot_NO2, device = "png", width = 7, height = 7, units = "in")
ggsave(filename = path2, plot = plot_PM25, device = "png", width = 7, height = 7, units = "in")


################################################################################
######################## 11- ANALYSE DE SENSIB ########################
#########################################################
scenario <- list("s1", "s2", "s3", "s4")
pollutant <- list("ug_NO2", "ug_PM25_RH50")
annees <- list("2030")

donnees_shp_1 <- st_read(file.path(path_fichier_shp_1, paste0(title_shp_1, ".shp")))
donnees_shp_2 <- st_read(file.path(path_fichier_shp_2, paste0(title_shp_2, ".shp")))
donnees_shp_3 <- st_read(file.path(path_fichier_shp_3, paste0(title_shp_3, ".shp")))
# Coller les trois sf donnees_shp_1, donnees_shp_2 et donnees_shp3
donnees_merged <- rbind(donnees_shp_1, donnees_shp_2, donnees_shp_3)

for (sc in scenario) {
  for (annee in annees) {
    for (pol in pollutant) {
      
      path_fichier_expo = paste0("data/2-output-data/", sc, "/", pol, "/front/expo-shp")
      title_expo = "expo-shp"
      path_fichier_mixte = paste0("data/2-output-data/", sc, "/", pol, "/front/mixte-shp")
      title_mixte = "mixte-shp"
      
      conc_points = coordo_sherpa(sc, pol, annee)
      donnees_exportees_transformed <- st_transform(donnees_exportees, crs=st_crs(conc_points))
      conc_ineris = coordo_ineris(pol)
      conc_corrigee = correction(conc_points, conc_ineris)
      donnees_expo = expo(donnees_exportees_transformed, conc_corrigee, grille_combinee)
      export_data_shp(donnees_expo, path_fichier_expo, title_expo)
      donnees_mixtes = mortalite_evitee_iris(donnees_expo, pol, annee) 
      export_data_shp(donnees_mixtes, path_fichier_mixte, title_mixte)
      
      tab = mortalite_age(donnees_merged, donnees_expo, annee, pol)
      path = paste0("data/2-output-data/", sc, "/", pol, "/front")
      csv_path <- file.path(path, paste0("mortalite_evitee.csv"))
      write.csv(tab, csv_path, row.names = FALSE)
    }
  }
}
