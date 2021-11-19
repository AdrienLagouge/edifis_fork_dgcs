################################################################################
#
# Copyright (C) 2021. Logiciel élaboré par l'État, via la Drees.
#
# Nom du dernier auteur : Camille Dufour, Drees.
#
# Noms des co-auteurs : Simon Fredon et Chloé Pariset
#
# Ce programme informatique a été développé par la Drees. 
# Il permet de de reproduire l'application R-Shiny "Edifis". 
#
# Ce programme a été exécuté le 08/11/2021 avec la version 4.0.5 de R.
#
# L'application Edifis peut être consultée sur le site de la 
# DREES : https://drees.shinyapps.io/Drees_Maquette_Edifis/
# 
# Bien qu'il n'existe aucune obligation légale à ce sujet, les utilisateurs de 
# ce programme sont invités à signaler à la DREES leurs travaux issus de la 
# réutilisation de ce code, ainsi que les éventuels problèmes ou anomalies 
# qu'ils y rencontreraient, en écrivant à DREES-REDISTRIBUTION-INES@sante.gouv.fr
# 
# Ce logiciel est régi par la licence "GNU General Public License" GPL-3.0. 
# https://spdx.org/licenses/GPL-3.0.html#licenseText
# 
# À cet égard l'attention de l'utilisateur est attirée sur les risques associés
# au chargement, à l'utilisation, à la modification et/ou au développement et à
# la reproduction du logiciel par l'utilisateur étant donné sa spécificité de 
# logiciel libre, qui peut le rendre complexe à manipuler et qui le réserve donc 
# à des développeurs et des professionnels avertis possédant des connaissances 
# informatiques approfondies. Les utilisateurs sont donc invités à charger et 
# tester l'adéquation du logiciel à leurs besoins dans des conditions permettant 
# d'assurer la sécurité de leurs systèmes et ou de leurs données et, plus 
# généralement, à l'utiliser et l'exploiter dans les mêmes conditions de sécurité.
# 
# Le fait que vous puissiez accéder à cet en-tête signifie que vous avez pris 
# connaissance de la licence GPL-3.0, et que vous en avez accepté les termes.
#
# This program is free software: you can redistribute it and/or modify it under 
# the terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see <https://www.gnu.org/licenses/>.
#
################################################################################

################################################################################
#########                                                               ########    
#########           Chargement des packages, appel des pgms R,          ########
#########           définition de libellés utiles                       ########
#########                                                               ########  
################################################################################

options(encoding = "UTF-8")

library(foreign)
library(stringr)
library(shiny) 
library(DT)
library(ggplot2) 
library(ggthemes) 
library(gridExtra) 
library(reshape2)
library(shinythemes)
library(RColorBrewer)
library(readxl)
library(shinycssloaders)
library(data.table)
library(shinyWidgets)
library(shinyBS)
library(Hmisc)
library(plotly)
library(shinydashboard)
library(plotly)

# fonction SI() utilisée dans les pgms base.R et cas-type.R (à terme n'utiliser que IFELSE())
SI <- function(condition,valeur_si_vrai,valeur_si_faux){
  res <- (condition)*valeur_si_vrai + (1-(condition))*valeur_si_faux
  return(res)
}

###Import des paramètres législatifs contenus dans les fichiers excel du dossier data
source("R/bareme.R")
###Calcul de grandeurs intermédiaires nécessaires au calcul des transferts
source("R/base.R")
###Calcul des transferts et construction des tables de données et des graphiques
source("R/cas-type.R")



######################### 3e onglet ########################## 

###Définition des libellés de colonnes dans les tables de données (en correspondance avec les noms de variables définis dans le pgm cas-type.R)

#======================================================================================#
#========================================= 2021 =======================================#
#======================================================================================#


colnames21 <- c("vecteur","tps_travail","percen_smic_tps_plein","ARE_net","sal_ref_net",
                "cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded","total_ps",
                "cout_travail","rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire",
                "br_ASS","rev_impo_n_1","rev_impo_n_2","revimp_n_2_abatt",
                "mont_ASS","ASS_conj","abatt_AL","AL","mont_AF","plaf_CF","plaf_CF_majo","mont_CF","ASF","plaf_AB_partiel","plaf_AB_plein",
                "mont_AB_paje","br_AAH","mont_AAH","mva","br_conj_AAH","mont_conj_AAH","mva_conj","AAH_tot","max_aah_ass_conj",
                "fl_RSA","br_rsa","mont_RSA","prime_noel","max_noel_ass_conj","fl_PA","br_pa","bonus_pa","bonus_servi","mont_PA",
                "total_minima_soc","total_minima_soc_cheque_energie","ars","total_pf",
                "rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part","imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote",
                "RI_2017","imp_decote_RI","imp_recouvr","av_QC","elig_plaf_th","plaf_mont_th","mont_TH_predegr","mont_TH","rev_trav_net","rev_hors_trav",
                "presta","prelev","rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc","ACS","energie",
                "rev_ajust")

labels21 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux",
              "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
              "Total des revenus primaires du ménage",
              "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
              "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE",
              "ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","Abattements AL (R0 et bi-activité)","AL - Montant", "Allocations familiales - Montant","Plafond CF non majoré",
              "Plafond CF majoré","Complément familial - Montant","Allocation soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
              "AB (Paje) - Montant","AAH - Base ressources (PR)","AAH - Montant (PR)",
              "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
              "AAH - Maj. vie autonome (conjoint)","AAH - Montant",
              "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources",
              "RSA - Montant", "RSA Prime de Noël - Montant","Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement",
              "PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie",
              "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
              "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part supplémentaire)",
              "Impôt par part fiscale (sans demi-part supplémentaire)","Impôt total (sans demi-part supplémentaire)",
              "Avantage lié au QF","Impôt après plafonnement QF",
              "Décote","Réduction d'impôt","Impôt après décote et réduction",
              "Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement","Plafond du montant de TH",
              "Montant de TH avant dégrèvement","TH - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible",
              "Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
              "Taux d'imposition marginal implicite (sur le superbrut)","Décile de niv. de vie",
              "Droit à la CSS gratuite","Droit à la CSS payante","Chèque énergie - Montant","Revenu disponible y compris le chèque énergie")

rev21 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
           "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage",
           "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
           "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE")

prelevements_soc21 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax21 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale","Impôt par part fiscale","Impôt total",
                 "Revenu imposable par part fiscale (sans demi-part supplémentaire)","Impôt par part fiscale (sans demi-part supplémentaire)",
                 "Impôt total (sans demi-part supplémentaire)","Avantage lié au QF","Impôt après plafonnement QF","Décote","Réduction d'impôt",
                 "Impôt après décote et réduction","Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement",
                 "Plafond du montant de TH","Montant de TH avant dégrèvement","TH - Montant", "Total des impôts")

min_soc21 <- c("ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","AAH - Base ressources (PR)","AAH - Montant (PR)","AAH - Maj. vie autonome (PR)",
               "AAH - Base ressources (conjoint)","AAH - Montant (conjoint)","AAH - Maj. vie autonome (conjoint)","AAH - Montant",
               "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources","RSA - Montant", "RSA Prime de Noël - Montant",
               "Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement","PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Droit à la CSS gratuite","Droit à la CSS payante",
               "Chèque énergie - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie")

pf_21 <- c("Allocations familiales - Montant","Plafond CF non majoré","Plafond CF majoré","Complément familial - Montant","Allocation soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein","AB (Paje) - Montant", 
           "Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log21 <- c("Abattements AL (R0 et bi-activité)","AL - Montant")

rev_disp21 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie",
                "Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Décile de niv. de vie","Revenu disponible y compris le chèque énergie")



#======================================================================================#
#========================================= 2020 =======================================#
#======================================================================================#


colnames20 <- c("vecteur","tps_travail","percen_smic_tps_plein","ARE_net","sal_ref_net",
                "cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded","total_ps",
                "cout_travail","rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire",
                "br_ASS","rev_impo_n_1","rev_impo_n_2","revimp_n_2_abatt","mont_ASS",
                "ASS_conj","abatt_AL","AL","mont_AF","plaf_CF","plaf_CF_majo","mont_CF","ASF","plaf_AB_partiel","plaf_AB_plein",
                "mont_AB_paje","br_AAH","mont_AAH","mva","br_conj_AAH","mont_conj_AAH","mva_conj","AAH_tot","max_aah_ass_conj",
                "fl_RSA","br_rsa","mont_RSA","prime_noel","max_noel_ass_conj","fl_PA","br_pa","bonus_pa","bonus_servi","mont_PA",
                "total_minima_soc","total_minima_soc_cheque_energie","ars","total_pf",
                "rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part","imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote",
                "RI_2017","imp_decote_RI","imp_recouvr","av_QC","elig_plaf_th","plaf_mont_th","mont_TH_predegr","mont_TH","rev_trav_net","rev_hors_trav",
                "presta","prelev","rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc","ACS","energie","rev_ajust")

labels20 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux",
              "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
              "Total des revenus primaires du ménage",
              "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
              "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE",
              "ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","Abattements AL (R0 et bi-activité)","AL - Montant", "Allocations familiales - Montant","Plafond CF non majoré",
              "Plafond CF majoré","Complément familial - Montant","Allocation soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
              "AB (Paje) - Montant","AAH - Base ressources (PR)","AAH - Montant (PR)",
              "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
              "AAH - Maj. vie autonome (conjoint)","AAH - Montant",
              "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources",
              "RSA - Montant", "RSA Prime de Noël - Montant","Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement",
              "PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie",
              "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
              "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part supplémentaire)",
              "Impôt par part fiscale (sans demi-part supplémentaire)","Impôt total (sans demi-part supplémentaire)",
              "Avantage lié au QF","Impôt après plafonnement QF",
              "Décote","Réduction d'impôt","Impôt après décote et réduction",
              "Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement","Plafond du montant de TH",
              "Montant de TH avant dégrèvement","TH - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible",
              "Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
              "Taux d'imposition marginal implicite (sur le superbrut)","Décile de niv. de vie",
              "Droit à la CSS gratuite","Droit à la CSS payante","Chèque énergie - Montant","Revenu disponible y compris le chèque énergie")

rev20 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
           "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage",
           "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
           "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE")

prelevements_soc20 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax20 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale","Impôt par part fiscale","Impôt total",
                 "Revenu imposable par part fiscale (sans demi-part supplémentaire)","Impôt par part fiscale (sans demi-part supplémentaire)",
                 "Impôt total (sans demi-part supplémentaire)","Avantage lié au QF","Impôt après plafonnement QF","Décote","Réduction d'impôt",
                 "Impôt après décote et réduction","Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement",
                 "Plafond du montant de TH","Montant de TH avant dégrèvement","TH - Montant", "Total des impôts")

min_soc20 <- c("ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","AAH - Base ressources (PR)","AAH - Montant (PR)","AAH - Maj. vie autonome (PR)",
               "AAH - Base ressources (conjoint)","AAH - Montant (conjoint)","AAH - Maj. vie autonome (conjoint)","AAH - Montant",
               "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources","RSA - Montant", "RSA Prime de Noël - Montant",
               "Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement","PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Droit à la CSS gratuite","Droit à la CSS payante",
               "Chèque énergie - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie")

pf_20 <- c("Allocations familiales - Montant","Plafond CF non majoré","Plafond CF majoré","Complément familial - Montant","Allocation soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein","AB (Paje) - Montant", 
           "Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log20 <- c("Abattements AL (R0 et bi-activité)","AL - Montant")

rev_disp20 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie",
                "Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Décile de niv. de vie","Revenu disponible y compris le chèque énergie")


#======================================================================================#
#========================================= 2019 =======================================#
#======================================================================================#


colnames19 <- c("vecteur","tps_travail","percen_smic_tps_plein","ARE_net","sal_ref_net",
                       "cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded","total_ps","cout_travail",
                       "rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                       "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire",
                       "br_ASS","rev_impo_n_1","rev_impo_n_2","revimp_n_2_abatt",
                       "mont_ASS","ASS_conj","abatt_AL","AL","mont_AF","plaf_CF","plaf_CF_majo","mont_CF","ASF","plaf_AB_partiel","plaf_AB_plein",
                       "mont_AB_paje","br_AAH","mont_AAH","mva","br_conj_AAH","mont_conj_AAH","mva_conj","AAH_tot","max_aah_ass_conj",
                       "fl_RSA","br_rsa","mont_RSA","prime_noel","max_noel_ass_conj","fl_PA","br_pa","bonus_pa","bonus_servi","mont_PA","total_minima_soc",
                       "total_minima_soc_cheque_energie","ars","total_pf","rfr",
                       "rfr_par_part","imp_par_part","imp_tot","rev_imp_part","imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote",
                       "RI_2017","imp_decote_RI","imp_recouvr","av_QC","elig_plaf_th","plaf_mont_th","mont_TH_predegr","mont_TH","rev_trav_net","rev_hors_trav",
                       "presta","prelev","rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc","ACS","energie","rev_ajust")

labels19 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux",
              "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
              "Total des revenus primaires du ménage",
              "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
              "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE",
              "ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","Abattements AL (R0 et bi-activité)","AL - Montant", "Allocations familiales - Montant","Plafond CF non majoré",
              "Plafond CF majoré","Complément familial - Montant","Allocation de soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
              "AB (Paje) - Montant","AAH - Base ressources (PR)","AAH - Montant (PR)",
              "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
              "AAH - Maj. vie autonome (conjoint)","AAH - Montant",
              "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources",
              "RSA - Montant", "RSA Prime de Noël - Montant","Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement",
              "PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant",
              "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie",
              "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
              "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part supplémentaire)",
              "Impôt par part fiscale (sans demi-part supplémentaire)","Impôt total (sans demi-part supplémentaire)",
              "Avantage lié au QF","Impôt après plafonnement QF",
              "Décote","Réduction d'impôt","Impôt après décote et réduction",
              "Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement","Plafond du montant de TH",
              "Montant de TH avant dégrèvement","TH - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
              "Taux d'imposition marginal implicite (sur le superbrut)","Décile de niv. de vie",
              "Droit à la CSS gratuite","Droit à la CSS payante","Chèque énergie - Montant","Revenu disponible y compris le chèque énergie")

rev19 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
           "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage",
           "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
           "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE")
           
prelevements_soc19 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax19 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale","Impôt par part fiscale","Impôt total",
                 "Revenu imposable par part fiscale (sans demi-part supplémentaire)","Impôt par part fiscale (sans demi-part supplémentaire)",
                 "Impôt total (sans demi-part supplémentaire)","Avantage lié au QF","Impôt après plafonnement QF","Décote","Réduction d'impôt",
                 "Impôt après décote et réduction","Impôt sur le revenu - Montant","Avantage lié au QF","TH - éligible au plafonnement",
                 "Plafond du montant de TH","Montant de TH avant dégrèvement","TH - Montant","Total des impôts")

min_soc19 <- c("ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","AAH - Base ressources (PR)","AAH - Montant (PR)","AAH - Maj. vie autonome (PR)",
               "AAH - Base ressources (conjoint)","AAH - Montant (conjoint)","AAH - Maj. vie autonome (conjoint)","AAH - Montant",
               "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources","RSA - Montant", "RSA Prime de Noël - Montant",
               "Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement","PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Droit à la CSS gratuite","Droit à la CSS payante",
               "Chèque énergie - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie")

pf_19 <- c("Allocations familiales - Montant","Plafond CF non majoré","Plafond CF majoré","Complément familial - Montant","Allocation de soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein","AB (Paje) - Montant", 
           "Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log19 <- c("Abattements AL (R0 et bi-activité)","AL - Montant")

rev_disp19 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie",
                "Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Décile de niv. de vie","Revenu disponible y compris le chèque énergie")

#======================================================================================#
#========================================= 2018 =======================================#
#======================================================================================#

colnames18 <- c("vecteur","tps_travail","percen_smic_tps_plein","ARE_net","sal_ref_net",
                     "cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded","total_ps",
                     "cout_travail","rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                     "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire","br_ASS","rev_impo_n_1","rev_impo_n_2","revimp_n_2_abatt",
                     "mont_ASS","ASS_conj","abatt_AL","AL","mont_AF","plaf_CF","plaf_CF_majo","mont_CF","ASF","plaf_AB_partiel","plaf_AB_plein",
                     "mont_AB_paje","br_AAH","mont_AAH","mva","br_conj_AAH","mont_conj_AAH","mva_conj","AAH_tot","max_aah_ass_conj",
                     "fl_RSA","br_rsa","mont_RSA","prime_noel","max_noel_ass_conj","fl_PA","br_pa","bonus_pa","bonus_servi","mont_PA","total_minima_soc","total_minima_soc_cheque_energie",
                     "ars","total_pf","rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part","imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote",
                     "RI_2017","imp_decote_RI","imp_recouvr","av_QC","elig_plaf_th","plaf_mont_th","mont_TH_predegr","mont_TH","rev_trav_net","rev_hors_trav",
                     "presta","prelev","rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc","ACS","energie","rev_ajust")

labels18 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux","Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage","Total des revenus primaires du ménage",
              "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
              "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE",
              "ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","Abattements AL (R0 et bi-activité)","AL - Montant", "Allocations familiales - Montant","Plafond CF non majoré",
              "Plafond CF majoré","Complément familial - Montant","Allocation de soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
              "AB (Paje) - Montant","AAH - Base ressources (PR)","AAH - Montant (PR)",
              "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
              "AAH - Maj. vie autonome (conjoint)","AAH - Montant",
              "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources",
              "RSA - Montant", "RSA Prime de Noël - Montant","Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement",
              "PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant",
              "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie",
              "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
              "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part supplémentaire)",
              "Impôt par part fiscale (sans demi-part supplémentaire)","Impôt total (sans demi-part supplémentaire)",
              "Avantage lié au QF","Impôt après plafonnement QF",
              "Décote","Réduction d'impôt","Impôt après décote et réduction",
              "Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement","Plafond du montant de TH",
              "Montant de TH avant dégrèvement","TH - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
              "Taux d'imposition marginal implicite (sur le superbrut)","Décile de niv. de vie",
              "Droit à la CMUC","Droit à l'ACS","Chèque énergie - Montant","Revenu disponible y compris le chèque énergie")

rev18 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
           "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage",
           "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
           "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE")

prelevements_soc18 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax18 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale","Impôt par part fiscale","Impôt total",
                 "Revenu imposable par part fiscale (sans demi-part supplémentaire)","Impôt par part fiscale (sans demi-part supplémentaire)",
                 "Impôt total (sans demi-part supplémentaire)","Avantage lié au QF","Impôt après plafonnement QF","Décote","Réduction d'impôt",
                 "Impôt après décote et réduction","Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement",
                 "Plafond du montant de TH","Montant de TH avant dégrèvement","TH - Montant","Total des impôts")

min_soc18 <- c("ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","AAH - Base ressources (PR)","AAH - Montant (PR)","AAH - Maj. vie autonome (PR)",
               "AAH - Base ressources (conjoint)","AAH - Montant (conjoint)","AAH - Maj. vie autonome (conjoint)","AAH - Montant",
               "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources","RSA - Montant", "RSA Prime de Noël - Montant",
               "Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement","PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Droit à la CMUC","Droit à l'ACS",
               "Chèque énergie - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris le chèque énergie")

pf_18 <- c("Allocations familiales - Montant","Plafond CF non majoré","Plafond CF majoré","Complément familial - Montant","Allocation de soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein","AB (Paje) - Montant", 
           "Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log18 <- c("Abattements AL (R0 et bi-activité)","AL - Montant")

rev_disp18 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie",
                "Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Décile de niv. de vie","Revenu disponible y compris le chèque énergie")

#======================================================================================#
#========================================= 2017 =======================================#
#======================================================================================#

colnames17 <- c("vecteur","tps_travail","percen_smic_tps_plein","ARE_net","sal_ref_net",
                    "cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded","total_ps","cout_travail",
                     "rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                     "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire",
                     "br_ASS","rev_impo_n_1","rev_impo_n_2","revimp_n_2_abatt",
                     "mont_ASS","ASS_conj","abatt_AL","AL","mont_AF","plaf_CF","plaf_CF_majo","mont_CF","ASF","plaf_AB_partiel","plaf_AB_plein",
                     "mont_AB_paje","br_AAH","mont_AAH","mva","br_conj_AAH","mont_conj_AAH","mva_conj","AAH_tot","max_aah_ass_conj",
                     "fl_RSA","br_rsa","mont_RSA","prime_noel","max_noel_ass_conj","fl_PA","br_pa","bonus_pa","bonus_servi","mont_PA","total_minima_soc","total_minima_soc_cheque_energie",
                     "ars","total_pf","rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part","imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote",
                     "RI_2017","imp_decote_RI","imp_recouvr","av_QC","elig_plaf_th","plaf_mont_th","mont_TH","rev_trav_net","rev_hors_trav","presta","prelev",
                     "rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc","ACS","energie","rev_ajust")

 labels17 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
               "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
               "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
               "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
               "Total des prélèvements sociaux",
               "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
               "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
               "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
               "Total des revenus primaires du ménage",
               "Base ressource ASS (revenu imposable N-1 avant abattements)",
               "Revenu imposable (N-1)","Revenu imposable (N-2)",
               "Revenu imposable (N-2) avec abatt. 30% rev. act N-2 pour les bénéficiaires ARE",
               "ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","Abattements AL (R0 et bi-activité)","AL - Montant","Allocations familiales - Montant","Plafond CF non majoré",
               "Plafond CF majoré","Complément familial - Montant","Allocation de soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
               "AB (Paje) - Montant","AAH - Base ressources (PR)","AAH - Montant (PR)",
               "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
               "AAH - Maj. vie autonome (conjoint)","AAH - Montant",
               "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources",
               "RSA - Montant", "RSA Prime de Noël - Montant","Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement",
               "PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)",
               "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
               "Revenu imposable (RFR)","Revenu imposable par part fiscale",
               "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part supplémentaire)",
               "Impôt par part fiscale (sans demi-part supplémentaire)","Impôt total (sans demi-part supplémentaire)",
               "Avantage lié au QF","Impôt après plafonnement QF",
               "Décote","Réduction d'impôt","Impôt après décote et réduction",
               "Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement","Plafond du montant de TH",
               "TH - Montant",
               "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
               "Taux d'imposition marginal implicite (sur le superbrut)","Décile de niv. de vie",
               "Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant","Revenu disponible y compris les tarifs de première nécessité")
 
 rev17 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
            "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
            "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
            "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
            "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
            "Total des revenus primaires du ménage",
            "Base ressources du ménage pour l'ASS (revenu imposable N-1 avant abattements)","Revenu imposable du ménage (N-1)","Revenu imposable du ménage (N-2)",
            "Revenu imposable du ménage (N-2) avec abatt. 30% rev. act. N-2 pour les bénéficiaires ARE")
  
prelevements_soc17 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")
  
impot_tax17 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale","Impôt par part fiscale","Impôt total",
                   "Revenu imposable par part fiscale (sans demi-part supplémentaire)","Impôt par part fiscale (sans demi-part supplémentaire)",
                   "Impôt total (sans demi-part supplémentaire)","Avantage lié au QF","Impôt après plafonnement QF","Décote","Réduction d'impôt",
                   "Impôt après décote et réduction","Impôt sur le revenu - Montant","Avantage lié au QC","TH - éligible au plafonnement",
                   "Plafond du montant de TH","TH - Montant","Total des impôts")
  
min_soc17 <- c("ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","AAH - Base ressources (PR)","AAH - Montant (PR)","AAH - Maj. vie autonome (PR)",
               "AAH - Base ressources (conjoint)","AAH - Montant (conjoint)","AAH - Maj. vie autonome (conjoint)","AAH - Montant",
               "Max (AAH,ASS) du conjoint","RSA - Forfait logement","RSA - Base ressources","RSA - Montant", "RSA Prime de Noël - Montant",
               "Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement","PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Droit à la CMUC","Droit à l'ACS",
               "Tarifs de première nécessité (électricité) - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)")
  
pf_17 <- c("Allocations familiales - Montant","Plafond CF non majoré","Plafond CF majoré","Complément familial - Montant","Allocation de soutien familial - Montant","Plafond AB taux partiel","Plafond AB taux plein","AB (Paje) - Montant", 
             "Allocation de rentrée scolaire - Montant","Total des prestations familiales")
  
alloc_log17 <- c("Abattements AL (R0 et bi-activité)","AL - Montant")

rev_disp17 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Revenu disponible","Niveau de vie",
                "Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Décile de niv. de vie","Revenu disponible y compris le chèque énergie")


#======================================================================================#
#========================================= 2016 =======================================#
#======================================================================================#

colnames16 <- c("vecteur","tps_travail","percen_smic_tps_plein","ARE_net","sal_ref_net",
                       "cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded","total_ps",
                       "cout_travail","rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                       "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire",
                       "rev_impo_n_1","rev_impo_n_2","revimp_n_2_abatt","mont_ASS","ASS_conj","abatt_AL",
                       "AL","plaf_CF","plaf_CF_majo","mont_CF","plaf_AB_partiel","plaf_AB_plein","mont_AB_paje","br_AAH","mont_AAH",
                       "mva","br_conj_AAH","mont_conj_AAH","mva_conj","AAH_tot","fl_RSA","br_rsa","mont_RSA","prime_noel","max_noel_ass_conj","fl_PA","br_pa",
                       "bonus_pa","bonus_servi","mont_PA","total_minima_soc","total_minima_soc_cheque_energie",
                       "ars","total_pf","rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part","imp_part_sansdemi",
                       "imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote","imp_recouvr","av_QC","elig_plaf_th","plaf_mont_th","mont_TH","mont_AF",
                       "rev_trav_net","rev_hors_trav","presta","prelev",
                       "ASF","rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc",
                       "ACS","energie","rev_ajust")


labels16 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux",
              "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
              "Total des revenus primaires du ménage",
              "Revenu imposable (N-1)","Revenu imposable (N-2)",
              "Revenu imposable (N-2) avec abatt. 30% rev. act N-2 pour les bénéficiaires ARE",
              "ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","Abattements AL (R0 et bi-activité)","AL - Montant","Plafond CF non majoré",
              "Plafond CF majoré","Complément familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
              "AB (Paje) - Montant","AAH - Base ressources (PR)","AAH - Montant (PR)",
              "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
              "AAH - Maj. vie autonome (conjoint)","AAH - Montant","RSA - Forfait logement",
              "RSA - Base ressources",
              "RSA - Montant",
              "RSA Prime de Noël - Montant","Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement",
              "PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant",
              "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)",
              "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
              "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
              "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
              "Avantage lié au QF","Impôt après plafonnement QF",
              "Décote",
              "Impôt sur le revenu - Montant","Avantage lié au QC",
              "TH - éligible au plafonnement","Plafond du montant de TH",
              "TH - Montant","Allocations familiales - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts", "Allocation de soutien familial - Montant",
              "Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
              "Taux d'imposition marginal implicite (sur le superbrut)","Décile de niv. de vie",
              "Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant","Revenu disponible y compris les tarifs de première nécessité")

rev16 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
           "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage",
           "Revenu imposable (N-1)","Revenu imposable (N-2)",
           "Revenu imposable (N-2) avec abatt. 30% rev. act N-2 pour les bénéficiaires ARE")


prelevements_soc16 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
          "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax16 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale",
                 "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
                 "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
                 "Avantage lié au QF","Impôt après plafonnement QF",
                 "Décote",
                 "Impôt sur le revenu - Montant","Avantage lié au QC",
                 "TH - éligible au plafonnement","Plafond du montant de TH",
                 "TH - Montant","Total des impôts")

min_soc16 <- c("ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","AAH - Base ressources (PR)","AAH - Montant (PR)",
               "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
               "AAH - Maj. vie autonome (conjoint)","AAH - Montant","RSA - Forfait logement",
               "RSA - Base ressources",
               "RSA - Montant",
               "RSA Prime de Noël - Montant","Prime de Noël (max (RSA,ASS du conjoint))","PA - Forfait logement",
               "PA - Base ressources","PA - Bonus d'activité théorique","PA - Bonus d'activité servi","PA - Montant","Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)")

pf_16 <- c("Allocations familiales - Montant","Plafond CF non majoré","Plafond CF majoré",
           "Complément familial - Montant", "Allocation de soutien familial - Montant",
           "Plafond AB taux partiel","Plafond AB taux plein", 
           "AB (Paje) - Montant","Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log16 <- c("Abattements AL (R0 et bi-activité)","AL - Montant")

rev_disp16 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts", "Revenu disponible","Niveau de vie",
                 "Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                 "Décile de niv. de vie","Revenu disponible y compris les tarifs de première nécessité")



#======================================================================================#
#========================================= 2015 =======================================#
#======================================================================================#

colnames15 <- c("vecteur","tps_travail","percen_smic_tps_plein","ARE_net","sal_ref_net",
                           "cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded","total_ps","cout_travail",
                           "rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                           "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire",
                           "rev_impo_n_1","rev_impo_n_2","revimp_n_2_abatt","mont_ASS","ASS_conj","abatt_AL",
                           "AL","ppe_tps_trav","ppe_elig_rfr","ppe_declar_tps_plein","ppe_prime_indiv","ppe_conj_tps_plein"," ppe_prime_conj","ppe_majo_monoact","ppe_majo_PAC",
                           "ppe_tot_avRSA","plaf_CF","plaf_CF_majo","mont_CF","plaf_AB_partiel","plaf_AB_plein","mont_AB_paje","br_AAH","mont_AAH",
                           "mva","br_conj_AAH","mont_conj_AAH","mva_conj","AAH_tot","fl_RSA","br_rsa","RSA_tot","RSA_act","RSA_soc",
                           "prime_noel_av2016","max_noel_ass_conj","ppe_resid","rsa_act_ppe_resid","total_minima_soc","total_minima_soc_cheque_energie",
                           "ars","total_pf","rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part",
                           "imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote","imp_recouvr","imp_ppe_recouvr","av_QC","elig_plaf_th","plaf_mont_th",
                           "mont_TH","mont_AF","rev_trav_net","rev_hors_trav","presta","prelev",
                           "ASF","rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc","ACS","energie","rev_ajust")

labels15 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux",
              "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
              "Total des revenus primaires du ménage",
                "Revenu imposable (N-1)","Revenu imposable (N-2)",
                "Revenu imposable (N-2) avec abatt. 30% rev. act N-2 pour les beneficiaires ARE",
              "ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)","Abattements AL (R0 et bi-activité)","AL - Montant",
                "Temps de travail PPE","Eligibilite RFR","PPE déclarant si temps plein","Prime individuelle",
                "PPE conjoint si temps plein", "Prime individuelle conjoint","Majoration monoactif",
                "Majoration PAC","PPE totale (avant imputation RSA)",
                "Plafond CF non majoré",
                "Plafond CF majoré","Complément familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
                "AB (Paje) - Montant","AAH - Base ressources (PR)","AAH - Montant (PR)",
                "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
                "AAH - Maj. vie autonome (conjoint)","AAH - Montant","RSA - Forfait logement",
                "RSA - Base ressources",
                "RSA total", "RSA activité","RSA socle - Montant", "RSA Prime de Noël - Montant", "Prime de Noël (max (RSA,ASS du conjoint))","PPE résiduelle",
                "RSA activité + PPE résiduelle - Montant",
                "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)",
                "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
                "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
                "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
                "Avantage lié au QF","Impôt après plafonnement QF","Décote",
                "Impôt sur le revenu - Montant","Impôt après décote et PPE","Avantage lié au QC",
                "TH - éligible au plafonnement","Plafond du montant de TH",
                "TH - Montant","Allocations familiales - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts","Allocation de soutien familial - Montant",
                "Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Taux d'imposition marginal implicite (sur le superbrut)","Décile de niv. de vie",
                "Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant","Revenu disponible y compris les tarifs de première nécessité")

rev15 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)","ARE nette (PR)","Salaire de référence net pour l'ARE (N-1 et N-2) (PR)",
           "Salaire superbrut (PR)","Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage",
           "Revenu imposable (N-1)","Revenu imposable (N-2)",
           "Revenu imposable (N-2) avec abatt. 30% rev. act N-2 pour les beneficiaires ARE")


prelevements_soc15 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax15 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale",
                 "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
                 "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
                 "Avantage lié au QF","Impôt après plafonnement QF",
                 "Décote",
                 "Impôt sur le revenu - Montant","Impôt après décote et PPE","Avantage lié au QC",
                 "TH - éligible au plafonnement","Plafond du montant de TH",
                 "TH - Montant","Total des impôts")

min_soc15 <- c("ASS - Montant (conjoint)","ASS - Prime de Noël (conjoint)",
               #"Temps de travail PPE",
               "Eligibilite RFR","PPE déclarant si temps plein","Prime individuelle",
               "PPE conjoint si temps plein", "Prime individuelle conjoint","Majoration monoactif",
               "Majoration PAC","PPE totale (avant imputation RSA)",
              "AAH - Base ressources (PR)","AAH - Montant (PR)",
               "AAH - Maj. vie autonome (PR)","AAH - Base ressources (conjoint)","AAH - Montant (conjoint)",
               "AAH - Maj. vie autonome (conjoint)","AAH - Montant","RSA - Forfait logement",
               "RSA - Base ressources",
               "RSA total", "RSA activité","RSA socle - Montant", "RSA Prime de Noël - Montant", "Prime de Noël (max (RSA,ASS du conjoint))","PPE résiduelle",
               "RSA activité + PPE résiduelle - Montant","Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant",
              "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)")

pf_15 <- c( "Allocations familiales - Montant","Plafond CF non majoré","Plafond CF majoré",
            "Complément familial - Montant","Allocation de soutien familial - Montant",
            "Plafond AB taux partiel","Plafond AB taux plein",
            "AB (Paje) - Montant","Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log15 <- c("Abattements AL (R0 et bi-activité)","AL - Montant")

rev_disp15 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts",
                "Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                 "Décile de niv. de vie","Revenu disponible y compris les tarifs de première nécessité")


#======================================================================================#
#========================================= 2014 =======================================#
#======================================================================================#

colnames14 <- c("vecteur","tps_travail","percen_smic_tps_plein","cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded",
                             "total_ps","cout_travail","rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                             "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire","AL","ppe_tps_trav",
                             "ppe_elig_rfr","ppe_declar_tps_plein","ppe_prime_indiv","ppe_conj_tps_plein"," ppe_prime_conj","ppe_majo_monoact","ppe_majo_PAC",
                             "ppe_tot_avRSA","plaf_CF","plaf_CF_majo","mont_CF","plaf_AB_partiel","plaf_AB_plein","mont_AB_paje",
                             "fl_RSA","br_rsa","RSA_tot"," RSA_act","RSA_soc"," prime_noel_av2016"," ppe_resid",
                             "rsa_act_ppe_resid","total_minima_soc","total_minima_soc_cheque_energie",
                             "ars","total_pf","rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part",
                             "imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote","imp_recouvr","imp_ppe_recouvr","av_QC",
                             "elig_plaf_th","plaf_mont_th","mont_TH","mont_AF","rev_trav_net","rev_hors_trav","presta","prelev","ASF",
                             "rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","decile","CMUc","ACS","energie","rev_ajust")

labels14 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux",
              "Salaire superbrut (PR)",
              "Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
              "Total des revenus primaires du ménage","AL - Montant",
              "Temps de travail PPE","Eligibilite RFR","PPE déclarant si temps plein","Prime individuelle",
              "PPE conjoint si temps plein", "Prime individuelle conjoint","Majoration monoactif",
              "Majoration PAC","PPE totale (avant imputation RSA)","Plafond CF non majoré",
              "Plafond CF majoré","Complément familial - Montant","Plafond AB taux partiel","Plafond AB taux plein",
              "AB (Paje) - Montant","RSA - Forfait logement","RSA - Base ressources",
              "RSA total", "RSA activité","RSA socle - Montant", "RSA Prime de Noël - Montant", "PPE résiduelle",
              "RSA activité + PPE résiduelle - Montant","Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)",
              "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
              "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
              "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
              "Avantage lié au QF","Impôt après plafonnement QF","Décote",
              "Impôt sur le revenu - Montant","Impôt après décote et PPE","Avantage lié au QC",
              "TH - éligible au plafonnement","Plafond du montant de TH",
              "TH - Montant","Allocations familiales - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts", "Allocation de soutien familial - Montant",
              "Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
              "Taux d'imposition marginal implicite (sur le superbrut)",
              "Décile de niveau de vie",
              "Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant","Revenu disponible y compris les tarifs de première nécessité")

rev14 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)",
           "Salaire superbrut (PR)",
           "Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage")

prelevements_soc14 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax14 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale",
                 "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
                 "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
                 "Avantage lié au QF","Impôt après plafonnement QF",
                 "Décote",
                 "Impôt sur le revenu - Montant","Impôt après décote et PPE","Avantage lié au QC",
                 "TH - éligible au plafonnement","Plafond du montant de TH","TH - Montant","Total des impôts")

min_soc14 <- c("Temps de travail PPE","Eligibilite RFR","PPE declarant si temps plein","Prime individuelle",
               "PPE conjoint si temps plein", "Prime individuelle conjoint","Majoration monoactif",
               "Majoration PAC","PPE totale (avant imputation RSA)","RSA - Forfait logement","RSA - Base ressources",
               "RSA total", "RSA activité","RSA socle - Montant", "RSA Prime de Noël - Montant", "PPE résiduelle",
               "RSA activité + PPE résiduelle - Montant",
               "Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)")

pf_14 <- c("Allocations familiales - Montant", "Plafond CF non majoré","Plafond CF majoré",
           "Complément familial - Montant","Allocation de soutien familial - Montant",
           "Plafond AB taux partiel","Plafond AB taux plein",
           "AB (Paje) - Montant","Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log14 <- c("AL - Montant")

rev_disp14 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts",
                "Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Décile de niveau de vie","Revenu disponible y compris les tarifs de première nécessité")


#======================================================================================#
#========================================= 2013 =======================================#
#======================================================================================#

colnames13 <- c("vecteur","tps_travail","percen_smic_tps_plein","cotis_emp","fillon_exo","cotis_sal","csg_ded","csg_non_ded",
                               "total_ps","cout_travail","rev_act_net","tps_trav_net","perc_smic_net_tpsplein","rev_act_dec",
                               "sal_net_conj","are_nette_conjoint","autres_rev","rev_primaire","AL","ppe_tps_trav",
                               "ppe_elig_rfr","ppe_declar_tps_plein","ppe_prime_indiv","ppe_conj_tps_plein"," ppe_prime_conj","ppe_majo_monoact",
                               "ppe_majo_PAC","ppe_tot_avRSA","plaf_CF","mont_CF","plaf_AB_partiel","mont_AB_paje",
                               "fl_RSA","br_rsa","RSA_tot"," RSA_act","RSA_soc"," prime_noel_av2016"," ppe_resid",
                               "rsa_act_ppe_resid","total_minima_soc","total_minima_soc_cheque_energie",
                               "ars","total_pf","rfr","rfr_par_part","imp_par_part","imp_tot","rev_imp_part",
                               "imp_part_sansdemi","imp_tot_sansdemi","avantage_qf","imp_plaf_qf","decote","imp_recouvr","imp_ppe_recouvr","av_QC",
                               "elig_plaf_th","plaf_mont_th","mont_TH","mont_AF","rev_trav_net","rev_hors_trav","presta","prelev","ASF",
                               "rev_disp","nv_vie","TMI_net","EM_net","TMI_superbrut","CMUc","ACS","energie","rev_ajust")

labels13 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
              "Salaire brut en % du Smic brut temps plein (PR)",
              "Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
              "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)",
              "Total des prélèvements sociaux","Salaire superbrut (PR)",
              "Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
              "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
              "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
               "Total des revenus primaires du ménage","AL - Montant",
              "Temps de travail PPE","Eligibilite RFR","PPE déclarant si temps plein","Prime individuelle",
              "PPE conjoint si temps plein", "Prime individuelle conjoint","Majoration monoactif",
              "Majoration PAC","PPE totale (avant imputation RSA)",
              "Plafond CF","Complément familial - Montant",
              "Plafond AB","AB (Paje) - Montant","RSA - Forfait logement",
              "RSA - Base ressources",
              "RSA total", "RSA activité","RSA socle - Montant", "RSA Prime de Noël - Montant", "PPE residuelle",
              "RSA activité + PPE résiduelle - Montant",
              "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)",
              "Allocation de rentrée scolaire - Montant","Total des prestations familiales",
              "Revenu imposable (RFR)","Revenu imposable par part fiscale",
              "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
              "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
              "Avantage lié au QF","Impôt après plafonnement QF","Décote",
              "Impôt sur le revenu - Montant","Impôt après décote et PPE","Avantage lié au QC",
              "TH - éligible au plafonnement","Plafond du montant de TH",
              "TH - Montant","Allocations familiales - Montant",
              "Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts", "Allocation de soutien familial - Montant",
              "Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
              "Taux d'imposition marginal implicite (sur le superbrut)",
              "Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant","Revenu disponible y compris les tarifs de première nécessité")

rev13 <- c("Salaire brut (PR)","Temps de travail en % du temps plein Smic brut (PR)",
           "Salaire brut en % du Smic brut temps plein (PR)",
           "Salaire superbrut (PR)",
           "Salaire net (PR)","Temps de travail en % du temps plein Smic net (PR)",
           "Salaire net en % du Smic net temps plein (PR)","Salaire déclaré (PR)",
           "Salaire net du conjoint","ARE nette du conjoint","Autres revenus imposables du ménage",
           "Total des revenus primaires du ménage")


prelevements_soc13 <- c("Cotisations sociales employeurs avant all. généraux (PR)","Allègements généraux de cotisations (PR)","Cotisations sociales salariés (PR)",
                        "CSG déductible sur les salaires (PR)","CSG non déductible / CRDS (PR)","Total des prélèvements sociaux")

impot_tax13 <- c("Revenu imposable (RFR)","Revenu imposable par part fiscale",
                 "Impôt par part fiscale","Impôt total","Revenu imposable par part fiscale (sans demi-part suppl)",
                 "Impôt par part fiscale (sans demi-part suppl)","Impôt total (sans demi-part suppl)",
                 "Avantage lié au QF","Impôt après plafonnement QF",
                 "Décote",
                 "Impôt sur le revenu - Montant","Impôt après décote et PPE","Avantage lié au QC",
                 "TH - éligible au plafonnement","Plafond du montant de TH",
                 "TH - Montant","Total des impôts")

min_soc13 <- c("Temps de travail PPE","Eligibilité RFR","PPE déclarant si temps plein","Prime individuelle",
               "PPE conjoint si temps plein", "Prime individuelle conjoint","Majoration monoactif",
               "Majoration PAC","PPE totale (avant imputation RSA)","RSA - Forfait logement","RSA - Base ressources",
               "RSA total", "RSA activité","RSA socle - Montant", "RSA Prime de Noël - Montant", "PPE résiduelle",
               "RSA activité + PPE résiduelle - Montant","Droit à la CMUC","Droit à l'ACS","Tarifs de première nécessité (électricité) - Montant",
               "Total des minima sociaux et PA","Total des minima sociaux et PA, y compris les TPN (électricité)")

pf_13 <- c("Allocations familiales - Montant","Plafond CF","Complément familial - Montant",
           "Allocation de soutien familial - Montant","Plafond AB","AB (Paje) - Montant",
           "Allocation de rentrée scolaire - Montant","Total des prestations familiales")

alloc_log13 <- c("AL - Montant")

rev_disp13 <- c("Total des salaires nets","Total des revenus hors salaires","Total des prestations","Total des impôts",
                "Revenu disponible","Niveau de vie","Taux d'imposition marginal implicite (TIMI) sur le revenu net","Effet marginal sur le revenu disponible (1-TIMI)",
                "Revenu disponible y compris les tarifs de première nécessité")
  

######################### 4e onglet ########################## 

###Définition des intitulés de légende du graphique empilé

libelles <- c("Allocations logement"="AL",
              "Prime d'activité"="mont_PA",
              "RSA"="mont_RSA",
              "RSA socle"="RSA_soc",
              "RSA activité"="RSA_act",
              "PPE résiduelle"="ppe_resid",
              "Allocation de solidarité spécifique du conjoint"="mont_ASS",
              "Allocation adulte handicapé"="AAH_tot",
              "Complément familial"="mont_CF",
              "Allocation de base (PAJE)"="mont_AB_paje",
              "Allocation de rentrée scolaire"="ars",
              "Allocations familiales"="mont_AF",
              "Allocation de soutien familial"="ASF",
              "ARE nette"="ARE_net",
              "Salaire net"="rev_act_net",
              "Salaire net du conjoint"="sal_net_conj",
              "ARE nette du conjoint"="are_nette_conjoint",
              "Autres revenus"="autres_rev")

###Définition des menus déroulants permettant le choix des transferts représentés dans le graphique empilé

###Cas où le type de revenu de la personne de référence est du salaire (n2000==0)
measure_vars0 <- list("21"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Salaire net"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "20"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Salaire net"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "19"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Salaire net"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "18"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Salaire net"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "17"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Salaire net"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "16"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Salaire net"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "15"=c("Allocations logement"="AL",
                             "RSA socle"="RSA_soc", 
                             "RSA activité"="RSA_act",
                             "PPE résiduelle"="ppe_resid",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Revenus d'activite nets"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "14"=c("Allocations logement"="AL",
                             "RSA socle"="RSA_soc", 
                             "RSA activité"="RSA_act",
                             "PPE résiduelle"="ppe_resid",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "Revenus d'activite nets"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "13"=c("Allocations logement"="AL",
                             "RSA socle"="RSA_soc", 
                             "RSA activité"="RSA_act",
                             "PPE résiduelle"="ppe_resid",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Revenus d'activite nets"="rev_act_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"))

###Cas où le type de revenu de la personne de référence est de l'ARE (n2000==1)
measure_vars1 <- list("21"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "ARE nette"="ARE_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "20"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "ARE nette"="ARE_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "19"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "ARE nette"="ARE_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "18"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "ARE nette"="ARE_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "17"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "ARE nette"="ARE_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "16"=c("Allocations logement"="AL",
                             "Prime d'activité"="mont_PA",
                             "RSA"="mont_RSA",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "ARE nette"="ARE_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"),
                      "15"=c("Allocations logement"="AL",
                             "RSA socle"="RSA_soc", 
                             "RSA activité"="RSA_act",
                             "PPE résiduelle"="ppe_resid",
                             "Allocation de solidarité spécifique du conjoint"="mont_ASS",
                             "Complément familial"="mont_CF",
                             "Allocation de base (PAJE)"="mont_AB_paje",
                             "Allocation adulte handicapé"="AAH_tot",
                             "Allocation de rentrée scolaire"="ars",
                             "Allocations familiales"="mont_AF",
                             "Allocation de soutien familial"="ASF",
                             "ARE nette"="ARE_net",
                             "Salaire net du conjoint"="sal_net_conj",
                             "ARE nette du conjoint"="are_nette_conjoint",
                             "Autres revenus"="autres_rev"))
