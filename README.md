Objectif :
========

Le projet consiste à évaluer l’impact sanitaire de la qualité de l’air en France à l’horizon 2050 selon différentes stratégies futures de décarbonation. 
Ces stratégies sont décrites par quatre scénarios de neutralité carbone rendus publics en 2021 par l’Ademe dans son projet prospectif Transition(s) 
(https://www.ademe.fr/les-futurs-en-transition/). Sur la base des évolutions des émissions des principaux polluants atmosphériques à l’échelle nationale 
dans les 4 scénarios visant la neutralité carbone à l’horizon 2050, un tableau regroupant les objectifs de réduction des émissions par rapport à 2019 en
2030 et 2050 pour chacun des scénario a été obtenu (partie 2.1.).

Puis, une estimation des concentrations de polluants dans l’atmosphère est conduite en supposant que les émissions de polluants évolueront de manière 
homogène sur l’ensemble du territoire français. Cette étape sera réalisée en utilisant le modèle SHERPA, développé par le Joint Research Center de la 
Commission européenne, spécialement conçu pour l’aide à la conception et l’évaluation de plans territoriaux de qualité de l’air (partie 2.2.). 

Enfin, il s’agit d’évaluer l’impact sanitaire lié aux scénarios Ademe : cette quantification est effectuée selon l’approche d’évaluation quantitative 
d’impact sanitaire (EQIS). Elle consiste dans un premier temps à évaluer les relations concentration-risque pour un polluant donné. 
Les concentrations des polluants atmosphériques obtenues par SHERPA seront appliquées à des données de population spatialisées disponibles via l’INSEE 
à l’échelle territoriale (IRIS, commune ou département). Les gains en termes de mortalité seront calculés en appliquant les relations concentrations-risque 
établies (Pascal et al., 2013, Pascal et al., 2016, Adelaide et al., 2021) (partie 2.3.). 



Architecture du dossier :
=======================


2.1. emissions.xlsx
---

emissions.xlsx est le fichier de calcul permettant d'obtenir les réductions jusqu'en 2050 prédites grâce au feuilleton air faisant suite au rapport prospectif
Transition(s) (https://librairie.ademe.fr/ged/6531/transitions2050-rapport-compresse2.pdf). Ce feuilleton s'intéresse aux quatre scénarios décrits par l'ADEME
(s1 : scénario le plus sobre, s4 : scénario le plus technologique).
Le feuilleton air permet de prédire les émissions suivantes :
- Les émissions de NH3 dans le secteur agricole pour s2, s3, s4 (GNFR11 et 12 agrégés).
- Les émissions de NOx,COVNM, PM25, PM10, SO2 du secteur résidentiel pour s1, s2, s3, s4 (GNFR3).
- Les émissions de NOx,COVNM, PM25, PM10, SO2 du secteur transport routier pour s1, s2, s3, s4 (GNFR6).
- Les émissions de NOx,COVNM, PM25, PM10, SO2 du secteur transport maritime pour s1, s2, s3, s4 (GNFR7).
- Les émissions de NOx,COVNM, PM25, PM10, SO2 du secteur transport aérien pour s1, s2, s3, s4 (GNFR8).
- Les émissions de NOx,COVNM, PM25, PM10, SO2 du secteur transport ferroviaire pour s1, s2, s3, s4 (GNFR9).

* Concernant les GNFR11 et 12 du secteur agricole, les émissions ont été désagrégées en faisant des régressions sur la quantité de NH3 émise par l'élevage en fonction
  du cheptel (GNFR11) et sur la quantité de NH3 émise par la culture en fonction de la fertilisation minérale (GNFR12).
* Afin d'obtenir des prédictions pour s1 concernant les émissions du secteur agricole : le rapport Transition(s) indiquant que les émissions de NH3 s2<s1<s3, nous avons
  réalisé une moyenne arithmétique entre les émissions de s2 et s3.
* Les émissions du GNFR7 (transport maritime) ont été calculées en sommant : transport maritime national + transport fluvial national.
* Les émissions du GNFR8 (aviation) sont issues des prédictions du total national.
* Les émissions du GNFR2 (industrie) sont estimées à partir des prédictions effectuées par le modèle GAINS sur l'évolution des émissions dans le secteur de l'industrie
  en accord avec les accords européens définis dans les plans nationaux de 2019 (https://gains.iiasa.ac.at/gains/docu.EUN/index.menu?open=none).
  Ces évolutions relatives sont ajustées aux émissions nationales de 2019 recensées par le CEIP (https://www.ceip.at/data-viewer-2/officially-reported-emissions-data).
* Toutes les autres émissions ont une réduction considérée nulle.

Ce fichier EXCEL permet de créer les fichiers .txt présents dans les input/reduction_perc pour chaque scénario, nécessaires pour faire fonctionner l'outil SHERPA.




2.2. Le code sherpa-model.ipynb : permet de prédire les concentrations de polluants atmosphériques dans l'air avec l'outil SHERPA en Python
---

Le jupyter notebook sherpa-model.ipynb permet de produire les sorties par scénario d'intérêt.




2.3. Le code code-pop.R : permet de mettre en forme les données de population à partir des données INSEE
--

2.3.1. Les données brutes
--
Ces données sont issues de la même projection parue en 2021 : https://www.insee.fr/fr/statistiques/5894886?sommaire=5760764
- La liste de la population par IRIS en 2019 : https://www.insee.fr/fr/statistiques/6543200
- La liste de la population par département par tranche d’âge de cinq ans de 2019 à 2050 : https://www.insee.fr/fr/statistiques/7747107?sommaire=6652140
- La liste de la population de chaque âge sur le territoire national de 2019 à 2050 : https://www.insee.fr/fr/statistiques/5894093?sommaire=5760764
- La liste du nombre de morts de chaque âge sur le territoire national de 2019 à 2050 : https://www.insee.fr/fr/statistiques/5894093?sommaire=5760764
- Le fichier CONTOURS_IRIS qui contient la liste des coordonnées des IRIS en 2019 : https://geoservices.ign.fr/contoursiris.

2.3.2. Reformater correctement les fichiers
--
Les fichiers bruts sont dans le dossier 0-raw-data et les fichiers utilisés se trouvent dans le dossier 1-processed-data
•	donnees_age_femmes et donnees_age_hommes sont issues de la liste de la population de chaque âge sur le territoire national de 2019 à 2050.
•	donnees_deces_hf est issue issues de la liste du nombre de morts de chaque âge sur le territoire national de 2019 à 2050.
•	donnees_iris est issue de la liste de la population par IRIS en 2019.
•	donnees_projections est issue de la liste de la population par département par tranche d’âge de cinq ans de 2019 à 2050.
•	CONTOURS_IRIS est issue de CONTOURS_IRIS. 
•	num_depart contient la liste des départements et des numéros de départements associés.

2.3.3 Les fonctions dans le code R code-pop.R
--
•	geometries <- function(path_iris, path_depart, path_contours)
-	geometries fusionne les tableaux issus de CONTOURS_IRIS, donnees_iris et num_depart. 
-	On obtient un Simple Feature Collection donnees_geom avec : age, dep_name, dep_cod, region_name, region_cod, com_name, com_cod, iris_name, iris_cod, POP_2019_IRIS_TotAge, geometry (polygone représentant l’IRIS) et aire_m2 (air de l’IRIS).

•	age_nat <- function(path_age_femmes, path_age_hommes)
-	age_nat additionne les âges de donnees_age_femmes et donnees_age_homme pour obtenir la liste de la population de chaque âge sur le territoire national de 2019 à 2050 indifféremment du sexe.
-	age_nat supprime les âges supérieurs à 99 ans et regroupe toutes les personnes âgées de 99 ans ou plus au sein de l’âge 99.
-	On obtient un dataframe age_hf contenant l’âge age et POP_2019_NAT jusqu’à POP_2050_NAT les colonnes donnant la liste des personnes de chaque âge sur le territoire national.

•	decomposition_age <- function(age_hf)
-	decomposition_age crée des fractions FRAC_2019_agesurtrage jusqu’à FRAC_2050_agesurtrage telle que pour chaque âge : FRAC_2019_agesurtrage = Nombre de personne de l’âge n / Nombre de personne de la classe d’âge de 5 ans comprenant n.
-	On obtient le dataframe perc_hf qui contient les colonnes de age_hf, les fractions pour chaque année, et une colonne tranche_age qui explicite la tranche d’âge de 5 considérée.

•	recense <- function(path_proj, perc_hf)
-	recense renomme correctement les départements mal orthographié
-	La fonction fusionne le dataframe perc_hf et les données issues de donnees_projections pour les mêmes valeurs de tranche_age (tranche d’âge de 5 ans).
-	On obtient un dataframe donnees_proj qui contient les colonnes de age_hf, ainsi que les colonnes POP_2019_DEP_TrAge jusqu’à POP_2050_DEP_TrAge issues de donnees_projections.

•	desagreg <- function(donnees_geom, donnees_proj)
-	desagreg fusionne le Simple feature collection donnees_geom et donnees_proj pour des valeurs communes du code des IRIS.
-	desagreg calcule POP_2019_IRIS tel que :
POP_2019_IRIS = POP_2019_IRIS_TotAge * POP_2019_DEP_TrAge * FRAC_2019_agesurtrage / POP_2019_DEP_TotAge
-	desagreg calcule POP_2020_IRIS à POP_2050_IRIS tels que :
POP_20xx_IRIS = POP_2019_IRIS * POP_20xx_DEP_TrAge * FRAC_20xx_agesurtrage / (POP_2019_DEP_TrAge * FRAC_2019_agesurtrage)
-	On obtient un Simple feature collection donnees_insee avec les colonnes de donnees_geom et donnees_proj, ainsi que les valeurs de population par âge par année à l’IRIS POP_2019_IRIS à POP_2050_IRIS. 

•	dens <- function(donnees_insee)
-	dens calcule les densités de population par âge par année à l’IRIS telle que :
DENS_20xx_IRIS = POP_20xx_IRIS / aire_m2
-	On obtient un Simple feature collection donnees_dens avec les colonnes de donnees_insee ainsi que les densités de population par âge par année à l’IRIS DENS_2019_IRIS à DENS_2050_IRIS.

•	mortalite <- function(donnees_dens, path_mortalite_hf)
-	mortalite extrait les données de mortalité présentes dans le fichier donnees_deces_hf puis les fusionne avec le Simple feature collection donnees_dens sous le nom MORT_2019_NAT à MORT_2050_NAT (données de décès nationaux pour chaque âge).
-	mortalite calcule les données de mortalité par âge à l’IRIS en faisant un produit en croix : MORT_20xx_IRIS = MORT_20xx_NAT * POP_20xx_IRIS / POP_20xx_NAT
-	On obtient un Simple feature collection donnees_finales avec les mêmes colonnes que donnees_dens et en plus les colonnes MORT_2019_NAT à MORT_2050_NAT et MORT_2019_IRIS à MORT_2050_IRIS.

•	export_data_csv <- function(donnees_finales, path, title_csv)
Exporte un Simple Feature Collection en fichier csv (sans la colonne geometry)

•	export_data_shp <- function(donnees_finales, path_fichier_shp, title_shp)
Exporte un Simple Feature Collection en fichier shp (pour pouvoir le lire avec la colonne geometry)

2.3.4. Les hypothèses faites dans ce calcul 
--
- Au sein d’une tranche d’âge de 5 ans dans un département en 20xx, la contribution de chaque âge au sein de la tranche d’âge est homogène à l’échelle nationale.
- Au sein d’un IRIS en 2019, la proportion de la population de chaque âge est homogène à l’échelle départementale.
- Au sein d’un IRIS en 20xx, l’évolution de la population à l’échelle départementale en 20xx est proportionnelle à l’évolution de la population à l’échelle de l’IRIS.
- En 20xx, la mortalité à l’échelle d’un IRIS est proportionnelle à la mortalité nationale en 20xx.



2.4. Le code code-eqis.R : permet de calculer l'EQIS à partir des données d'exposition (SHERPA) et des données de démographie (INSEE)
--
2.4.1. Les données brutes
--
Les données utilisées sont les données de sortie du code code-pop.R et de la partie python
- Dans le fichier output-data, le fichier donnees-shp donne les coordonnées géographiques des IRIS, la population de plus de 30 ans présente dans ces IRIS en 2019, 2030 et 2050, ainsi que le nombre de morts de plus de 30 ans dans ces IRIS en 2019, 2030, 2050 (= fichier de sortie du code-pop.R).
- Dans le fichier processed-data, le fichier SHERPA recense les sorties de la partie Python, ce qui permet d'avoir les concentrations de référence en 2019 en France pour les PM2.5 et les NO2, ainsi les concentrations en PM2.5 et NO2 pour chaque scénario (s1 à s4) en 2030 et 2050.

2.4.2. Les fonctions dans le code R code-eqis.R
--
•	coordo_sherpa <- function(sc, pol, annee)
- Pour le scénario sc, le pollant pol et l'année annee, coordo_sherpa trouve le fichier de modélisation python correspondant
- Renvoie un Simple Feature collection conc_points contenant les coordonnées de points, la concentration conc mesurée et la différence de concentration par rapport à 2019 delta_conc

•	moy_conc <- function(conc_points)
- Pour le Simple Feature Collection conc_points, renvoie la moyenne sur le territoire national de la concentration conc mesurée

•	coordo_ineris <- function(pol)
- renvoie une liste de points en France et de concentrations mesurées par l'INERIS (cartothèque)
  
• correction <- function(conc_points, conc_ineris)
- corrige le delta de concentration à partir de la cartothèque de l'INERIS en faisant un produit en croix

• calculate_perc <- function(grille_combinee, donnees_exportees_transformed)
- calculer le pourcentage d'intersection (nécessaire dans le fichier donnees_pourcents) entre la surface d'un IRIS et la surface de mesure de la concentration dans SHERPA

• expo <- function(donnees_exportees_transformed, conc_corrigee, grille_combinee)
- associer les moyennes de concentration à chaque IRIS (à partir du fichier données_pourcents) en pondérant les concentrations corrigées par les pourcentages pré-établis dans la fonction précédente

•	export_data_shp <- function(donnees_shp, path_fichier_shp, title_shp)
- Cette fonction permet d'exporter le Simple Collection Feature donnees_shp en shapefile situé au chemin path_fichier_shp et ayant pour titre title_shp

• expo_ponderee_meanconc <- function(donnees_expo, popannee)
- Fonction qui calcule l'exposition moyenne pondérée par la population

•	mortalite_age <- function(donnees_merged, donnees_expo, annee, pol)
- Pour le polluant pol et la mortalité évitée dans chaque IRIS associée à l'exposition au polluant pol présente dans la colonne mortpolannee, la fonction mortalite_age calcule la mortalité évitée associée à l'exposition au polluant pol, les années de vie gagnées, et renvoie un tableau avec les valeurs pour chaque âge

•	life_exp <- function(tab, annee)
- Fonction qui calcule l'espérance de vie gagnée entre 2019 et l'année d'intérêt




