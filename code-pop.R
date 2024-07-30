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
library(data.table)
library(ggplot2)

################################################################################
################################# 2- ENTREES ###################################
################################################################################

# CONTOURS IRIS : donne les coordonnées de chaque IRIS 2019
path_contours <- "data/1-processed-data/CONTOURS-IRIS"

# DONNEES IRIS : donne la population totale (âge confondu) en 2019 à l'IRIS
path_iris <- "data/1-processed-data/INSEE/donnees_iris.txt"

# PROJECTIONS : donne les projections de population par tranche d'âge de 5 ans au département entre 2019 et 2050 
path_proj <- "data/1-processed-data/INSEE/donnees_projections.txt"

# DEPARTEMENT : donne les noms et numéros des départements entre 2019 et 2050 à l'échelle nationale
path_depart <- "data/1-processed-data/INSEE/num_depart.csv"

# AGE : donne les prédictions du nombre de personnes de chaque âge entre 2019 et 2050 à l'échelle nationale
path_age_femmes <- "data/1-processed-data/INSEE/donnees_age_femmes.csv"
path_age_hommes <- "data/1-processed-data/INSEE/donnees_age_hommes.csv"

# MORTALITE : donne les prédictions du nombre de morts de chaque âge entre 2019 et 2050 à l'échelle nationale
path_mortalite_hf <- "data/1-processed-data/INSEE/donnees_deces_hf.csv"

# EXPORT : chemin d'exportation et titres :
path <- "data/2-output-data"
path_fichier_shp <- "data/2-output-data/donnees_shp"
path_fichier_shp_1 <- "data/2-output-data/donnees_shp_1"
path_fichier_shp_2 <- "data/2-output-data/donnees_shp_2"
path_fichier_shp_3 <- "data/2-output-data/donnees_shp_3"
title_shp <- "donnees_insee_iris"
title_shp_1 <- "donnees_insee_iris_toutage_1"
title_shp_2 <- "donnees_insee_iris_toutage_2"
title_shp_3 <- "donnees_insee_iris_toutage_3"


################################################################################
################################# 3- FONCTIONS #################################
################################################################################

#Fonction qui renvoie une liste de polygones représentant les IRIS ainsi que la population en 2019 à l'IRIS
#####

geometries <- function(path_iris, path_depart, path_contours) {
  shape_iris <- st_read(path_contours)
  donnees_iris <- read.table(path_iris)
  colnames(donnees_iris) <- donnees_iris[1,]  # Utiliser la première ligne comme noms de colonnes
  donnees_iris <- donnees_iris[-1,]  # Supprimer la ligne de titre
  iris_manquants <- data.frame(
    IRIS = c("311490901", "532740000", "452870000", "532390000", "163510000", 
             "215070000", "212130000", "690720103", "690720101", "690720102", 
             "276760000", "940220103", "940220104", "940220101"),
    REG = c(76, 52, 24, 52, 75, 27, 27, 84, 84, 84, 28, 11, 11, 11),
    DEP = c(31, 53, 45, 53, 16, 21, 21, 69, 69, 69, 27, 94, 94, 94),
    P19_POP = rep(0, 14))
  donnees_iris <- rbind(donnees_iris, iris_manquants)
  num_depart <- read.csv(path_depart, fill = TRUE, sep=";" )
  donnees_geom <- merge(shape_iris, donnees_iris, by.x = "CODE_IRIS", by.y = "IRIS", all.x = TRUE) 
  donnees_geom <- merge(donnees_geom, num_depart, by.x = "DEP", by.y = "num", all.x = TRUE) %>%
    rename(dep_cod = DEP,
           iris_cod = CODE_IRIS,
           com_cod = INSEE_COM,
           com_name = NOM_COM,
           iris_name = NOM_IRIS,
           region_cod = REG)
  donnees_geom <- donnees_geom[, c("dep_cod", "dep_name", "region_cod", "region_name", 
                                     "com_cod", "com_name", "iris_cod", "iris_name", "P19_POP", "geometry")]
  colnames(donnees_geom)[colnames(donnees_geom) == "P19_POP"] <- "POP_2019_IRIS_TotAge"
  donnees_geom$aire_m2 <- st_area(donnees_geom)
  print(paste0("Les données INSEE sont extraites à l'échelle IRIS."))
  return(donnees_geom)}

#####
#Fonction qui renvoie le total de la population par âge de 2019 à 2050 à l'échelle nationale
#####
age_nat <- function(path_age_femmes, path_age_hommes) {
  donnees_age_femmes <- read.csv(path_age_femmes, sep=";", na.strings = "NA")
  donnees_age_hommes <- read.csv(path_age_hommes, sep=";", na.strings = "NA")
  donnees_age_femmes <- head(donnees_age_femmes, -1)
  donnees_age_hommes <- head(donnees_age_hommes, -1)
  donnees_age_femmes[,1] <- gsub("105+", "105", donnees_age_femmes[,1], fixed = TRUE)
  donnees_age_hommes[,1] <- gsub("105+", "105", donnees_age_hommes[,1], fixed = TRUE)
  age_hf <- data.frame(matrix(NA, nrow = nrow(donnees_age_femmes), ncol = 33))
  age_hf[,1] <- donnees_age_femmes[,1]
  colnames(age_hf)[1] <- "age"
  for (i in 0:31) {
    annee = 2019+i
    colnames(age_hf)[2+i] <- paste0("POP_", annee, "_NAT")
    age_hf[,2+i] <- as.numeric(donnees_age_femmes[, 3+i] + donnees_age_hommes[, 3+i])
  }
  print(paste0("Le total de personnes de chaque âge entre 2019 et 2050 est prédit à l'échelle nationale."))
  return(age_hf)
}
#####
#Fonction qui décompose un tableau de tranche d'âge de 5 ans en tranche d'âge d'un an
#####
decomposition_age <- function(age_hf) {
  perc_hf <- data.frame(matrix(NA, nrow = nrow(age_hf), ncol = ncol(age_hf)+ncol(age_hf)))
  perc_hf[,1] <- age_hf[,1]
  colnames(perc_hf)[1] <- "age"
  colnames(perc_hf)[ncol(age_hf)+1] <- "tranche_age"
  # Boucler sur les colonnes des années
  for (j in 2:ncol(age_hf)) {
    colnames(perc_hf)[j] <- colnames(age_hf)[j] 
    annee = 2017 + j
    perc_hf[,j] <- age_hf[,j]
    colnames(perc_hf)[ncol(age_hf)+j] <- paste0("FR_", annee, "_agesurtrage")
    # Boucler sur les lignes
    for (i in 1:95) {
      # Calculer le quotient de la division euclidienne
      p <- as.integer(as.numeric(age_hf[i, 1]) / 5)
      # Remplacer la valeur de la cellule
      perc_hf[i, ncol(age_hf)+1] <- paste0("[",5*p,";",5*p+5,"[")
      perc_hf[i, j+ncol(age_hf)] <- as.numeric(age_hf[i, j]) / (
          as.numeric(age_hf[(5 * p + 1),j]) +
          as.numeric(age_hf[(5 * p + 2),j]) +
          as.numeric(age_hf[(5 * p + 3),j]) +
          as.numeric(age_hf[(5 * p + 4),j]) +
          as.numeric(age_hf[(5 * p + 5),j])
        ) }
    for (i in 96:99) {
      perc_hf[i, ncol(age_hf)+1] <- "[95;99+]"
      perc_hf[i, j+ncol(age_hf)] <- as.numeric(age_hf[i, j]) / (
          as.numeric(age_hf[96,j]) +
          as.numeric(age_hf[97,j]) +
          as.numeric(age_hf[98,j]) +
          as.numeric(age_hf[99,j]) +
          as.numeric(age_hf[100,j]) +
          as.numeric(age_hf[101,j]) +
          as.numeric(age_hf[102,j]) +
          as.numeric(age_hf[103,j]) +
          as.numeric(age_hf[104,j]) +
          as.numeric(age_hf[105,j]) +
          as.numeric(age_hf[106,j])
        )
      perc_hf[i, ncol(age_hf)+1] <- "[95;99+]"}
    perc_hf[100, ncol(age_hf)+1] <- "[95;99+]"
    perc_hf[100, j+ncol(age_hf)] <- (as.numeric(age_hf[100, j]) +
                            as.numeric(age_hf[101,j]) +
                            as.numeric(age_hf[102,j]) +
                            as.numeric(age_hf[103,j]) +
                            as.numeric(age_hf[104,j]) +
                            as.numeric(age_hf[105,j]) +
                            as.numeric(age_hf[106,j])) / (
                                as.numeric(age_hf[96,j]) +
                                as.numeric(age_hf[97,j]) +
                                as.numeric(age_hf[98,j]) +
                                as.numeric(age_hf[99,j]) +
                                as.numeric(age_hf[100,j]) +
                                as.numeric(age_hf[101,j]) +
                                as.numeric(age_hf[102,j]) +
                                as.numeric(age_hf[103,j]) +
                                as.numeric(age_hf[104,j]) +
                                as.numeric(age_hf[105,j]) +
                                as.numeric(age_hf[106,j]) ) }
  perc_hf <- perc_hf %>%
    slice(-c(101:nrow(perc_hf)))
  perc_hf[100,1] <- "99"
  print(paste0("Le pourcentage de chaque âge par rapport à une tranche d'âge de 5 ans entre 2019 et 2050 est prédit à l'échelle nationale."))
  return(perc_hf)
}
#####
#Fonction qui regroupe les prédictions de population de 2019 à 2050 par département
#####
recense <- function(path_proj, perc_hf) {
  donnees_proj <- read.table(path_proj, fill = TRUE )
  colnames(donnees_proj) <- donnees_proj[1,]
  donnees_proj <- donnees_proj[-1,]
  donnees_proj <- donnees_proj[,1:36]
  donnees_proj <- donnees_proj[, -which(names(donnees_proj) == "POP_2018")]
  donnees_proj <- donnees_proj %>%
    mutate(ZONE = replace(ZONE, ZONE == "Ard\x8fche", "Ardèche")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Ari\x8fge", "Ariège")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Bouches-du-Rh\x99ne", "Bouches-du-Rhône")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Corr\x8fze", "Corrèze")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "C\x99te-d'Or", "Côte-d'Or")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "C\x99tes-d'Armor", "Côtes-d'Armor")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Deux-S\x8fvres", "Deux-Sèvres")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Dr\x99me", "Drôme")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Finist\x8fre", "Finistère")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Haute-Sa\x99ne", "Haute-Saône")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Hautes-Pyr\x8en\x8ees", "Hautes-Pyrénées")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "H\x8erault", "Hérault")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Is\x8fre", "Isère")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Loz\x8fre", "Lozère")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Ni\x8fvre", "Nièvre")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Puy-de-D\x99me", "Puy-de-Dôme")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Pyr\x8en\x8ees-Atlantiques", "Pyrénées-Atlantiques")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Pyr\x8en\x8ees-Orientales", "Pyrénées-Orientales")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Rh\x99ne", "Rhône")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "R\x8eunion", "Réunion")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Sa\x99ne-et-Loire", "Saône-et-Loire")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Val-d'Oise", "Val-d-Oise")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Côte-d'Or", "Côte-d-Or")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Côtes-d'Armor", "Côtes-d-Armor")) %>%
    mutate(ZONE = replace(ZONE, ZONE == "Vend\x8ee", "Vendée"))
  donnees_proj <- donnees_proj %>%
    group_by(ZONE, TRAGE) %>%
    summarise(
      POP_2019_DEP_TrAge = sum(as.numeric(POP_2019), na.rm = TRUE),
      POP_2020_DEP_TrAge = sum(as.numeric(POP_2020), na.rm = TRUE),
      POP_2021_DEP_TrAge = sum(as.numeric(POP_2021), na.rm = TRUE),
      POP_2022_DEP_TrAge = sum(as.numeric(POP_2022), na.rm = TRUE),
      POP_2023_DEP_TrAge = sum(as.numeric(POP_2023), na.rm = TRUE),
      POP_2024_DEP_TrAge = sum(as.numeric(POP_2024), na.rm = TRUE),
      POP_2025_DEP_TrAge = sum(as.numeric(POP_2025), na.rm = TRUE),
      POP_2026_DEP_TrAge = sum(as.numeric(POP_2026), na.rm = TRUE),
      POP_2027_DEP_TrAge = sum(as.numeric(POP_2027), na.rm = TRUE),
      POP_2028_DEP_TrAge = sum(as.numeric(POP_2028), na.rm = TRUE),
      POP_2029_DEP_TrAge = sum(as.numeric(POP_2029), na.rm = TRUE),
      POP_2030_DEP_TrAge = sum(as.numeric(POP_2030), na.rm = TRUE),
      POP_2031_DEP_TrAge = sum(as.numeric(POP_2031), na.rm = TRUE),
      POP_2032_DEP_TrAge = sum(as.numeric(POP_2032), na.rm = TRUE),
      POP_2033_DEP_TrAge = sum(as.numeric(POP_2033), na.rm = TRUE),
      POP_2034_DEP_TrAge = sum(as.numeric(POP_2034), na.rm = TRUE),
      POP_2035_DEP_TrAge = sum(as.numeric(POP_2035), na.rm = TRUE),
      POP_2036_DEP_TrAge = sum(as.numeric(POP_2036), na.rm = TRUE),
      POP_2037_DEP_TrAge = sum(as.numeric(POP_2037), na.rm = TRUE),
      POP_2038_DEP_TrAge = sum(as.numeric(POP_2038), na.rm = TRUE),
      POP_2039_DEP_TrAge = sum(as.numeric(POP_2039), na.rm = TRUE),
      POP_2040_DEP_TrAge = sum(as.numeric(POP_2040), na.rm = TRUE),
      POP_2041_DEP_TrAge = sum(as.numeric(POP_2041), na.rm = TRUE),
      POP_2042_DEP_TrAge = sum(as.numeric(POP_2042), na.rm = TRUE),
      POP_2043_DEP_TrAge = sum(as.numeric(POP_2043), na.rm = TRUE),
      POP_2044_DEP_TrAge = sum(as.numeric(POP_2044), na.rm = TRUE),
      POP_2045_DEP_TrAge = sum(as.numeric(POP_2045), na.rm = TRUE),
      POP_2046_DEP_TrAge = sum(as.numeric(POP_2046), na.rm = TRUE),
      POP_2047_DEP_TrAge = sum(as.numeric(POP_2047), na.rm = TRUE),
      POP_2048_DEP_TrAge = sum(as.numeric(POP_2048), na.rm = TRUE),
      POP_2049_DEP_TrAge = sum(as.numeric(POP_2049), na.rm = TRUE),
      POP_2050_DEP_TrAge = sum(as.numeric(POP_2050), na.rm = TRUE))
  donnees_tot_dep <- donnees_proj %>%
    group_by(ZONE) %>%
    summarise(POP_2019_DEP_TotAge = sum(as.numeric(POP_2019_DEP_TrAge), na.rm = TRUE))
  donnees_proj <- merge(donnees_proj, donnees_tot_dep, by = c("ZONE"))
  colnames(donnees_proj)[2] <- "tranche_age"
  donnees_proj <- merge(donnees_proj, perc_hf, by = c("tranche_age"))
  donnees_proj$age <- as.numeric(donnees_proj$age)
  donnees_proj <- donnees_proj[order(donnees_proj$ZONE, donnees_proj$age), ]
  print(paste0("Le nombre de personnes dans chaque tranche d'âge de 5 ans entre 2019 et 2050 est prédit à l'échelle du département."))
  return(donnees_proj)
}
#####
#Fonction qui calcule la population entre 2019 et 2050 par âge à l'IRIS
#####
desagreg <- function(donnees_geom, donnees_proj) {
  donnees_insee <- merge(donnees_geom, donnees_proj, by.x = "dep_name", by.y = "ZONE")
  print(paste0("Les données INSEE mesurées à l'échelle IRIS sont superposées aux données INSEE prédites à l'échelle départementale."))
  donnees_insee$age <- as.numeric(donnees_insee$age)
  donnees_insee$POP_2019_IRIS_TotAge <- as.numeric(donnees_insee$POP_2019_IRIS_TotAge)
  donnees_insee$POP_2019_DEP_TotAge <- as.numeric(donnees_insee$POP_2019_DEP_TotAge)
  donnees_insee$POP_2019_IRIS <- donnees_insee$POP_2019_IRIS_TotAge * donnees_insee$POP_2019_DEP_TrAge * donnees_insee$FR_2019_agesurtrage / donnees_insee$POP_2019_DEP_TotAge
  for (annee in 2020:2050) {
    donnees_insee[[paste0("POP_",annee,"_IRIS")]] <- as.numeric(donnees_insee[[paste0("POP_",annee,"_DEP_TrAge")]]) * as.numeric(donnees_insee[[paste0("FR_",annee,"_agesurtrage")]]) * as.numeric(donnees_insee[["POP_2019_IRIS"]]) / (donnees_insee$POP_2019_DEP_TrAge * donnees_insee$FR_2019_agesurtrage)
  }
  print(paste0("Les prédictions de population entre 2019 et 2050 ont été calculées à l'échelle IRIS."))
  return(donnees_insee)
}
#####
#Fonction qui calcule la densité de population entre 2019 et 2050 par âge à l'IRIS
#####
dens <- function(donnees_insee) {
  for (annee in 2020:2050) {
    donnees_insee[[paste0("DENS_", annee)]] <- as.numeric(donnees_insee[[paste0("POP_", annee, "_IRIS")]]) / as.numeric(donnees_insee$aire_m2)
  }
  print(paste0("Les prédictions de densité entre 2019 et 2050 ont été calculées à l'échelle IRIS."))
  return(donnees_insee)
}
#####
#Fonction qui désagrège la mortalité nationale entre 2019 et 2050 par âge à l'IRIS
#####
mortalite <- function(donnees_dens, path_mortalite_hf) {
  donnees_mortalite_hf <- read.csv(path_mortalite_hf, sep=";" )
  donnees_mortalite_hf[100,2:ncol(donnees_mortalite_hf)] <- (as.numeric(donnees_mortalite_hf[100, 2:ncol(donnees_mortalite_hf)]) +
                                                               as.numeric(donnees_mortalite_hf[101,2:ncol(donnees_mortalite_hf)]) +
                                                               as.numeric(donnees_mortalite_hf[102,2:ncol(donnees_mortalite_hf)]) +
                                                               as.numeric(donnees_mortalite_hf[103,2:ncol(donnees_mortalite_hf)]) +
                                                               as.numeric(donnees_mortalite_hf[104,2:ncol(donnees_mortalite_hf)]) +
                                                               as.numeric(donnees_mortalite_hf[105,2:ncol(donnees_mortalite_hf)]) +
                                                               as.numeric(donnees_mortalite_hf[106,2:ncol(donnees_mortalite_hf)]))
  donnees_mortalite_hf <- donnees_mortalite_hf %>%
    slice(-c(101:nrow(donnees_mortalite_hf)))
  donnees_mortalite_hf[100,1] <- "99"
  donnees_insee <- merge(donnees_dens, donnees_mortalite_hf, by = c("age"))
  for (annee in 2019:2050) {
    colnames(donnees_insee)[colnames(donnees_insee) == paste0("X", annee)] <- paste0("MORT_", annee, "_NAT")
    donnees_insee[[paste0("MORT_", annee, "_IRIS")]] <- as.numeric(donnees_insee[[paste0("MORT_",annee, "_NAT")]]) * as.numeric(donnees_insee[[paste0("POP_", annee, "_IRIS")]]) / as.numeric(donnees_insee[[paste0("POP_", annee, "_NAT")]])
  }
  print(paste0("Les prédictions de mortalité nationale entre 2019 et 2050 ont été correctement désagrégées à l'échelle IRIS."))
  return(donnees_insee)
}
######
#Fonction qui transforme un dataframe en fichier .csv (sans les coordonnées de géométrie)
#####
export_data_csv <- function(donnees_finales, path, title_csv) {
  donnees_finales <- donnees_finales[order(donnees_finales$iris_cod, donnees_finales$age), ]
  csv_path <- file.path(path, paste0(title_csv, ".csv"))
  write.csv(st_drop_geometry(donnees_finales), csv_path, row.names = FALSE)
  cat("Fichier CSV écrit à :", csv_path, "\n")
}
#####
#Fonction qui transforme un dataframe en fichier .shp (avec les coordonnées de géométrie)
#####
create_donnees_shp <- function(donnees_finales) {
  # Sélectionner les colonnes spécifiques et les colonnes POP_XXXX_IRIS et MORT_XXXX_IRIS uniquement
  cols_to_select <- c("iris_cod", "iris_name", "com_cod", "com_name", "dep_cod", "dep_name", "region_cod", "region_name",
                      "aire_m2", "age") 
  pop_cols <- grep("^POP_\\d{4}_IRIS$", names(donnees_finales), value = TRUE)
  mort_cols <- grep("^MORT_\\d{4}_IRIS$", names(donnees_finales), value = TRUE)
  all_cols <- c(cols_to_select, pop_cols, mort_cols)
  donnees_shp <- donnees_finales %>%
    select(all_of(all_cols)) %>%
    rename(
      regcod = region_cod,
      regname = region_name,
      iriscod = iris_cod,
      irisname = iris_name,
      comcod = com_cod,
      comname = com_name,
      depcod = dep_cod,
      depname = dep_name,
      airem2 = aire_m2,
      pop2019 = POP_2019_IRIS, pop2020 = POP_2020_IRIS, pop2021 = POP_2021_IRIS, pop2022 = POP_2022_IRIS,
      pop2023 = POP_2023_IRIS, pop2024 = POP_2024_IRIS, pop2025 = POP_2025_IRIS, pop2026 = POP_2026_IRIS,
      pop2027 = POP_2027_IRIS, pop2028 = POP_2028_IRIS, pop2029 = POP_2029_IRIS, pop2030 = POP_2030_IRIS,
      pop2031 = POP_2031_IRIS, pop2032 = POP_2032_IRIS, pop2033 = POP_2033_IRIS, pop2034 = POP_2034_IRIS,
      pop2035 = POP_2035_IRIS, pop2036 = POP_2036_IRIS, pop2037 = POP_2037_IRIS, pop2038 = POP_2038_IRIS,
      pop2039 = POP_2039_IRIS, pop2040 = POP_2040_IRIS, pop2041 = POP_2041_IRIS, pop2042 = POP_2042_IRIS,
      pop2043 = POP_2043_IRIS, pop2044 = POP_2044_IRIS, pop2045 = POP_2045_IRIS, pop2046 = POP_2046_IRIS,
      pop2047 = POP_2047_IRIS, pop2048 = POP_2048_IRIS, pop2049 = POP_2049_IRIS, pop2050 = POP_2050_IRIS,
      mort2019 = MORT_2019_IRIS, mort2020 = MORT_2020_IRIS, mort2021 = MORT_2021_IRIS, mort2022 = MORT_2022_IRIS,
      mort2023 = MORT_2023_IRIS, mort2024 = MORT_2024_IRIS, mort2025 = MORT_2025_IRIS, mort2026 = MORT_2026_IRIS,
      mort2027 = MORT_2027_IRIS, mort2028 = MORT_2028_IRIS, mort2029 = MORT_2029_IRIS, mort2030 = MORT_2030_IRIS,
      mort2031 = MORT_2031_IRIS, mort2032 = MORT_2032_IRIS, mort2033 = MORT_2033_IRIS, mort2034 = MORT_2034_IRIS,
      mort2035 = MORT_2035_IRIS, mort2036 = MORT_2036_IRIS, mort2037 = MORT_2037_IRIS, mort2038 = MORT_2038_IRIS,
      mort2039 = MORT_2039_IRIS, mort2040 = MORT_2040_IRIS, mort2041 = MORT_2041_IRIS, mort2042 = MORT_2042_IRIS,
      mort2043 = MORT_2043_IRIS, mort2044 = MORT_2044_IRIS, mort2045 = MORT_2045_IRIS, mort2046 = MORT_2046_IRIS,
      mort2047 = MORT_2047_IRIS, mort2048 = MORT_2048_IRIS, mort2049 = MORT_2049_IRIS, mort2050 = MORT_2050_IRIS
    ) %>%
  return(donnees_shp)
}
#####
#Fonction qui exporte en fichier shapefile
#####
export_data_shp <- function(donnees_shp, path_fichier_shp, title_shp) {
  shp_path <- file.path(path_fichier_shp, paste0(title_shp, ".shp"))
  st_write(donnees_shp, shp_path)
}
#####
#Fonction qui exporte des données des années 2019, 2030 et 2050
#####
#1. pour les âges agrégés
export_pollution <- function(donnees_finales) {
  donnees_indic <- donnees_geom %>%
    st_drop_geometry() %>%
    select(iris_cod, iris_name, com_cod, com_name, dep_cod, dep_name, region_cod, region_name) %>%
    rename(
      regcod = region_cod,
      regname = region_name,
      iriscod = iris_cod,
      irisname = iris_name,
      comcod = com_cod,
      comname = com_name,
      depcod = dep_cod,
      depname = dep_name)
  donnees_pop <- donnees_filtrees %>%
    filter(age >= 30) %>%
    group_by(iriscod) %>%
    summarise(
      pop2019 = sum(pop2019, na.rm = TRUE),
      pop2030 = sum(pop2030, na.rm = TRUE),
      pop2050 = sum(pop2050, na.rm = TRUE),
      mort2019 = sum(mort2019, na.rm = TRUE),
      mort2030 = sum(mort2030, na.rm = TRUE),
      mort2050 = sum(mort2050, na.rm = TRUE))
  donnees_shp <- merge(donnees_pop, donnees_indic, by = c("iriscod"))
  return(donnees_shp)}

#2. pour chaque âge
export_pollution_age <- function(donnees_finales) {
  donnees_indic <- donnees_finales %>%
    select(iriscod, irisname, comcod, comname, depcod, depname, regcod, regname, age, 
           pop2019, pop2030, pop2050, mort2019, mort2030, mort2050, geometry) %>%
    filter(age >= 30)
  donnees_indic <- donnees_indic[order(donnees_indic$iriscod, donnees_indic$age), ]
  return(donnees_indic)}
#####
#Fonction qui trace la carte de population de l'âge n pour l'année a
#####
plot_carte_iris <- function(donnees_insee, annee, age) {
  filtered_data <- subset(donnees_insee, age == n)
  pop = paste0("POP_", annee, "_IRIS")
  tm_shape(filtered_data) +
    tm_polygons(pop, border.col = "#00000000") +
    tm_layout(legend.outside = TRUE)
}
#####
#Fonction qui trace une pyramide des âges pour l'iris IRIS et l'année n
#####
pyramide_iris <- function(donnees_insee, iris_num, annee) {
  pop_col <- paste0("POP_", annee, "_IRIS")
  filtered_data <- donnees_insee %>%
    filter(iris_cod == iris_num) %>%
    select(age, !!sym(pop_col))
    filtered_data <- filtered_data %>%
      rename(pop = !!sym(pop_col))
  # Créer la pyramide des âges
  ggplot(filtered_data, aes(x = age, y = pop)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Pyramide des âges pour l'IRIS", iris_num, "en", annee),
         x = "Âge",
         y = paste("Population en", annee)) +
    theme_minimal()
}
#####
#Fonction qui trace une pyramide des âges pour l'iris IRIS et l'année n à l'échelle départementale
#####
pyramide_dep <- function(donnees_insee, dep_num, annee) {
  pop_col <- paste0("POP_", annee, "_IRIS")
  filtered_data <- donnees_insee %>%
    filter(dep_cod == dep_num) %>%
    select(age, !!sym(pop_col))
  filtered_data <- filtered_data %>%
    rename(pop = !!sym(pop_col))
  age_distribution <- filtered_data %>%
    group_by(age) %>%
    summarise(total_pop = sum(pop, na.rm = TRUE))
  # Créer la pyramide des âges
  ggplot(age_distribution, aes(x = age, y = total_pop)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Pyramide des âges pour le département", dep_num, "en", annee),
         x = "Âge",
         y = paste("Population en", annee)) +
    theme_minimal()
}
#####
#Fonction qui trace une pyramide des âges pour l'iris IRIS et l'année n à l'échelle nationale
#####
pyramide_nat <- function(donnees_insee, annee) {
  pop_col <- paste0("POP_", annee, "_IRIS")
  filtered_data <- donnees_insee %>%
    select(age, !!sym(pop_col))
  filtered_data <- filtered_data %>%
    rename(pop = !!sym(pop_col))
  age_distribution <- filtered_data %>%
    group_by(age) %>%
    summarise(total_pop = sum(pop, na.rm = TRUE))
  # Créer la pyramide des âges
  ggplot(age_distribution, aes(x = age, y = total_pop)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Pyramide des âges pour le département en", annee),
         x = "Âge",
         y = paste("Population en", annee)) +
    theme_minimal()
}

################################################################################
################################# 4- TESTS #####################################
################################################################################
#####
#Test national sur les personnes vivantes
#####
national_vivant_test <- function(donnees_finales, annee) {
  colonne_iris <- paste0("POP_", annee, "_IRIS")
  somme_iris <- sum(donnees_finales[[colonne_iris]], na.rm = TRUE)
  colonne_nat <- paste0("POP_", annee, "_NAT")
  donnees_nat <- subset(donnees_finales, iris_cod == donnees_finales$iris_cod[1])
  somme_nat <- sum(donnees_nat[[colonne_nat]], na.rm = TRUE)
  print(paste0("La somme de la population de tous les IRIS en ", annee, " vaut : ", somme_iris, "."))
  print(paste0("La population en France en ", annee, " est égale à : ", somme_nat, "."))
}
#####
#Test national sur les personnes mortes
#####
national_mort_test <- function(donnees_finales, annee) {
  colonne_iris <- paste0("MORT_", annee, "_IRIS")
  somme_iris <- sum(donnees_finales[[colonne_iris]], na.rm = TRUE)
  colonne_nat <- paste0("MORT_", annee, "_NAT")
  donnees_nat <- subset(donnees_finales, iris_cod == donnees_finales$iris_cod[1])
  somme_nat <- sum(donnees_nat[[colonne_nat]], na.rm = TRUE)
  print(paste0("La somme de la mortalité de tous les IRIS en ", annee, " vaut : ", somme_iris, "."))
  print(paste0("La mortalité en France en ", annee, " est égale à : ", somme_nat, "."))
}
#####
#Test au département
#####
departemental_test <- function(donnees_finales, dep_num, annee) {
  colonne_iris <- paste0("POP_", annee, "_IRIS")
  donnees_iris <- subset(donnees_insee, dep_cod == dep_num)
  somme_iris <- sum(donnees_iris[[colonne_iris]], na.rm = TRUE)
  colonne_dep <- paste0("POP_", annee, "_DEP_TrAge")
  donnees_dep <- subset(donnees_iris, iris_cod == donnees_iris$iris_cod[1])
  somme_dep <- sum(donnees_dep[[colonne_dep]], na.rm = TRUE)/5
  print(paste0("La somme de la population des IRIS du département ", dep_num, " en ", annee, " vaut : ", somme_iris, "."))
  print(paste0("La population du département ", dep_num, " en ", annee, " est égale à : ", somme_dep, "."))
}
#####
#Test à l'IRIS en 2019
######
iris_test <- function(donnees_finales) {
  random_iris_cod <- sample(donnees_finales$iris_cod, 1)
  donnees_filtrees <- subset(donnees_insee, iris_cod == random_iris_cod)
  somme_iris <- sum(donnees_filtrees[["POP_2019_IRIS"]], na.rm = TRUE)
  iris_init <- donnees_filtrees[["POP_2019_IRIS_TotAge"]][1]
  print(paste0("La somme de la population de l'IRIS ", random_iris_cod, " en 2019 vaut : ", somme_iris, "."))
  print(paste0("La population de l'IRIS ", random_iris_cod, " en 2019 est égale à : ", iris_init, "."))
}
######
#Test de comparaison entre l'IRIS et le département en 2019
######
dep_vs_iris_test <- function(donnees_finales) {
  random_dep_cod <- sample(donnees_finales$dep_cod, 1)
  donnees_filtrees <- subset(donnees_insee, dep_cod == random_dep_cod)
  somme_iris <- sum(donnees_filtrees[["POP_2019_IRIS"]], na.rm = TRUE)
  colonne_dep <- paste0("POP_", annee, "_DEP_TrAge")
  donnees_dep <- subset(donnees_filtrees, iris_cod == donnees_filtrees$iris_cod[1])
  somme_dep <- sum(donnees_dep[[colonne_dep]], na.rm = TRUE)/5
  print(paste0("La somme de la population des IRIS du département ", random_dep_cod, " en 2019 vaut : ", somme_iris, "."))
  print(paste0("La population du département ", random_dep_cod, " en 2019 est égale à : ", somme_dep, "."))
}

################################################################################
################################# 5- RUNS ######################################
################################################################################

#Fonctions pour obtenir la table finale

donnees_geom = geometries(path_iris, path_depart, path_contours)
age_hf = age_nat(path_age_femmes, path_age_hommes)
perc_hf = decomposition_age(age_hf)
donnees_proj = recense(path_proj, perc_hf) 
donnees_insee = desagreg(donnees_geom, donnees_proj)
donnees_dens = dens(donnees_insee)
donnees_finales = mortalite(donnees_dens, path_mortalite_hf)
donnees_filtrees = create_donnees_shp(donnees_finales)
donnees_shp = export_pollution(donnees_filtrees)
donnees_shp_1 <- donnees_filtrees %>% 
  select(iriscod, irisname, comcod, comname, depcod, depname, regcod, regname,age, pop2019, pop2030, pop2050, mort2019, mort2030, mort2050) %>%
  filter(age >= 30 & age <= 55)
donnees_shp_2 <- donnees_filtrees %>% 
  select(iriscod, irisname, comcod, comname, depcod, depname, regcod, regname,age, pop2019, pop2030, pop2050, mort2019, mort2030, mort2050) %>%
  filter(age >= 56 & age <= 87)
donnees_shp_3 <- donnees_filtrees %>% 
  select(iriscod, irisname, comcod, comname, depcod, depname, regcod, regname,age, pop2019, pop2030, pop2050, mort2019, mort2030, mort2050) %>%
  filter(age >= 88 & age <= 99)
export_data_shp(donnees_shp_1, path_fichier_shp_1, title_shp_1)
export_data_shp(donnees_shp_2, path_fichier_shp_2, title_shp_2)
export_data_shp(donnees_shp_3, path_fichier_shp_3, title_shp_3)


#Fonctions d'exportation

export_data_csv(donnees_finales, path, title_csv)
export_data_shp(donnees_indic, path_fichier_shp, title_shp)
donnees_exportees <- st_read(file.path(path_fichier_shp_1, paste0(title_shp_1, ".shp")))
head(donnees_exportees)

#Fonctions test

annee = 2030
dep_num = "01"
iris_num = "010040102"
n = 60
plot_carte_iris(donnees_insee, annee, n) 
national_vivant_test(donnees_finales, annee)
national_mort_test(donnees_finales, annee)
departemental_test(donnees_finales, dep_num, annee)
iris_test(donnees_finales)
dep_vs_iris_test(donnees_finales)
pyramide_iris(donnees_insee, iris_num, annee) 
pyramide_dep(donnees_insee, dep_num, annee) 
pyramide_nat(donnees_insee, annee)
