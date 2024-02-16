# EDIFIS

## Présentation générale

Ce dossier contient les programmes permettant de reproduire l'application R-Shiny "Edifis". 

Cette application permet d'estimer le revenu disponible mensuel d'un ménage-type en fonction du salaire de la personne de référence. 

Le "coeur" de l'application (les programmes ui.R, server.R et global.R) se trouve dans le dossier racine. 
Ensuite, le projet contient 3 dossiers:
- data: contient un fichier excel de barème législatif par année
- R: contient l'ensemble des autres scripts R utilisés dans l'application
- www: contient des éléments de mise en forme de l'application (le fichier bootstrapv5.css et les différentes images notamment)

Pour lancer l'application, il faut télécharger l'ensemble du dossier sur son poste, ouvrir 'Edifis.Rproj' sous R, puis global.R, puis installer les packages requis (listés en tête de pgm) et enfin cliquer sur le bouton 'Run App'.

Lien vers l'application en ligne : https://drees.shinyapps.io/Drees_Maquette_Edifis/

La Direction de la recherche, des études, de l’évaluation et des statistiques (Drees) est une direction de l’administration centrale des ministères sanitaires et sociaux. 
https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/la-drees/qui-sommes-nous/ 

Les programmes ont été exécutés pour la dernière fois avec le logiciel R version 4.2.2, le 30/01/2024.

## Guide de mise à jour pour l'année N

### 1) Actualiser la maquette excel N (.xslm)

### 2) Actualiser data/Maquette_cas_types_N.xls : fichiers de paramètres législatifs en entrée d'Edifis

Mettre la nouvelle maquette au format des autres maquettes du dossier

### 3) Actualiser global.R : appel des programmes R et définition de listes

Création des listes 'colnames#', 'labels#', 'rev#', 'prelevements_soc#', 'impot_tax#', 'min_soc#', 'pf_#', 'alloc_log#' et 'rev_disp#' pour l'année N (utilisées dans l'onglet 3)
Enrichissement des listes 'libelles', 'measure_vars0' et 'measure_vars1' pour l'année N (utilisées dans l'onglet 4)

### 4) Actualiser ui.R : définition de l'interface utilisateur de l'application

Dans le 1er  onglet, adapter le texte à la nouvelle année
Dans le 2ème onglet, ajouter la nouvelle année aux option du sliderInput
Dans les 3ème et 4ème onglets, ajouter les conditionalPanel propres à la nouvelle année

### 5) Actualiser R/bareme.R : définition de listes servant à l'import des paramètres législatifs

Enrichir la liste 'var_names' avec les paramètres de la nouvelle année
Vérifier l'import avec la commande 'print(bareme_var[["seuil_D9"]])' en tête du pgm cas-type.R

### 6) Actualiser R/base.R : calcul de grandeurs intermédiaires

Si des changements législatifs affectent les calculs, les adapter

### 7) Actualiser R/cas-type.R : calcul de tous les transferts

Si des changements législatifs affectent les calculs, les adapter

### 8) Actualiser www/bootstrapv5.css

Enrichir la liste des éléments à mettre en gras dans les menus déroulants

### 9) Finaliser l'actualisation

Actualiser les en-têtes de tous les programmes
Faire tourner Edifis avec la dernière version disponible de R et réactualiser les fonctions utilisées en fonction des messages d'alerte de la console
Vérifier qu'Edifis donne bien les mêmes résultats que la maquette excel N (.xslm)