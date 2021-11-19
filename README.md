#EDIFIS

Ce dossier contient les programmes permettant de reproduire l'application R-Shiny "Edifis". 

Cette application permet d'estimer le revenu disponible mensuel d'un ménage-type en fonction du salaire de la personne de référence. 

Le "coeur" de l'application (les programmes ui.R, server.R et global.R) se trouve dans le dossier racine. 
Ensuite, le projet contient 3 dossiers:
- data: contient un fichier excel de barème législatif par année
- R: contient l'ensemble des autres scripts R utilisés dans l'application
- www: contient des éléments de mise en forme de l'application (le fichier bootstrapv5.css et les différentes images notamment)

Pour lancer l'application, il faut télécharger l'ensemble du dossier sur son poste, ouvrir 'Edifis.Rproj' sous R puis cliquer sur le bouton 'Run App'.

Lien vers l'application en ligne : https://drees.shinyapps.io/Drees_Maquette_Edifis/

La Direction de la recherche, des études, de l’évaluation et des statistiques (Drees) est une direction de l’administration centrale des ministères sanitaires et sociaux. 
https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/la-drees/qui-sommes-nous/ 

Les programmes ont été exécutés pour la dernière fois avec le logiciel R version 4.0.5, le 08/11/2021.
