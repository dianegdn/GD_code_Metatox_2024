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

2.2. part1-python : dossier qui permet de prédire les concentrations de polluants atmosphériques dans l'air avec l'outil SHERPA en Python
---

Contient :
  - les entrées (input)
  - les sorties (output)
  - les différents modules du code de SHERPA (lire le readme spécifique)
  - le jupyter notebook how_to_run_module1.ipynb qui permet de comprendre le fonctionnement du code du Module 1 pour l'utiliser dans sherpa-model.ipynb
    Ce code est détaillé dans la partie 3.
  - le jupyter notebook sherpa-model.ipynb qui permet de produire les sorties par scénario d'intérêt
    
2.3. part2-rstudio : dossier qui permet de calculer l'EQIS à partir des prédictions des concentrations données par SHERPA
--

Contient :
  - code-pop.R : code R qui permet de mettre en forme les données de population à partir des données INSEE.
    Ce code est détaillé dans le README de la partie part2-rstudio.
  - eqis-code.R : code R qui permet de calculer l'EQIS à partir des prédictions des concentrations données par SHERPA.
    Ce code est détaillé dans le README de la partie part2-rstudio.
  - rproject-eqis.Rproj : projet R pour l'EQIS
  - data : dossier dans lequel se trouve les fichiers d'entrée
    Contient :
    * 0-raw-data : fichiers bruts trouvés sur Internet (lire le fichier Word pour obtenir les URL correspondants)
      correspondent aux fichiers INSEE permettant d'avoir les contours des IRIS en France, et les statistiques nationales et départementales de population
    * 1-processed-data : fichiers nécessaires pour le code eqis-code.R :
      (i) CONTOURS-IRIS : permet de charger les fonds de carte IRIS nécessaires pour la spatialisation
      (ii) INSEE : permet de charger les données INSEE nécessaires pour les statistiques de population
      (iii) SHERPA : permet d'obtenir les prédictions de concentration pour chaque scénario.
    * 2-output-data : contient les fichiers de sortie du code code-pop : donnees_iris.csv et donnees_iris.shp. 


