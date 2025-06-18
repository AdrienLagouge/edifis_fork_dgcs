################################################################################
#
# Copyright (C) 2024. Logiciel élaboré par l'État, via la Drees.
#
# Nom du dernier auteur : Coraline Best, Drees.
#
# Noms des co-auteurs : Camille Dufour, Simon Fredon et Chloé Pariset
#
# Ce programme informatique a été développé par la Drees. Il permet de de reproduire l'application R-Shiny "Edifis". 
#
# Ce programme a été exécuté le 30/01/2024 avec la version 4.2.2 de R.
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
#########           Calcul de grandeurs intermédiaires nécessaires      ########
#########                      au calcul des transferts                 ########
#########                                                               ########  
################################################################################




#==================================================================================================#
#============================================= A LIRE =============================================#
#==================================================================================================#


# Dans ce script, sont definies differentes fonctions auxiliaires (cs_emp(), exo_fillon(),...) 
# utiles pour le calcul des transferts fait dans le pgm cas-type.R
# puis la fonction choix_input() qui permet de :
# 1 - renommer les paramètres donnés en entrée
# 2 - calculer des grandeurs intermédiaires (montant des AF, montant forfaitaire du RSA, etc) 


# Rentrer les parametres suivants dans la fonction choix_input():

# Composition familiale :
# - n0 : situation conjugale (1, 2 ou 3)
# - n1 : nombre d'enfants (de 0 à 5)
# - n2 : nombre d'enfants de moins de 3 ans
# - n3 : nombre d'enfants de 3 à 5 ans
# - n4 : nombre d'enfants de 6 à 10 ans
# - n5 : nombre d'enfants de 11 à 13 ans
# - n6 : nombre d'enfants de 14 ans
# - n7 : nombre d'enfants de 15 à 19 ans
# - n8 : nombre d'enfants de 20 ans

## Revenus ##
# - n9 : salaire mensuel brut du conjoint 
# - n10 : are nette du conjoint 
# - n11 : autres revenus 
# - n21 : Salaire maximum 
# - n13 : ARE maximum 
# - n22 : pas de salaire
# - n12 : pas d'ARE

## Recours ##
# - n15 : Eligibilite et recours du conjoint à l'ASS

## Handicap (au sens de l'AAH) ##
# - n16 : handicap de la personne de reference 
# - n17 : handicap du conjoint

## Divers ##
# - n18 : majoration isole RSA (0 ou 1) 
# - n19 : eligible ASF (0 ou 1) 
# - n20 : menage proprietaire (non accedant) ou heberge gratuitement (0 ou 1)

# Parametres
# - bareme : nom du dataframe contenant les variables du bareme (bareme_var)
# - year : annee de legislation
# - n2000 : type de revenus (salaire ou ARE)


# Creation de differentes fonctions auxiliaires utilisees plus bas
exo_fillon1 <<- function(x,bareme){
  y <<- (x>0)*pmax(x*((bareme[["tx_exo_fillon_smic"]]/(bareme[["plafond_exo_fillon"]]-1))*(bareme[["plafond_exo_fillon"]]*(bareme[["smic_b"]]/x)-1)),0)
  return(y)
}

exo_fillon <<- function(x,y,z,bareme){
  v <<- ifelse(z>0,pmax( bareme[["tx_exo_fillon_smic"]]*x* (bareme[["plafond_exo_fillon"]]*(y/z)-1)/(bareme[["plafond_exo_fillon"]]-1),0),0)
  return(v)
}

csg_deduc <<- function(x,bareme){
  y <<- (x>0)*bareme[["taux_csg_deduc_t1"]]*pmin(x, 4*bareme[["plafond_ss"]])+
    (x>4*bareme[["plafond_ss"]])*bareme[["taux_csg_deduc_t2"]]*(x-4*bareme[["plafond_ss"]])
  return(y)
}

csg_non_deduc <<- function(x,bareme){
  y <<- (x>0)*bareme[["taux_csgcrds_nondeduc_t1"]]*pmin(x, 4*bareme[["plafond_ss"]])+
    (x>4*bareme[["plafond_ss"]])*bareme[["taux_csgcrds_nondeduc_t2"]]*(x-4*bareme[["plafond_ss"]])
  return(y)
}

choix_input <<- function(n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n15,n16,n17,n18,n19,n20,n21,n22,n13,n12,bareme_var,year,n2000){
  
  #############################################################################################
  ######################## Parametres sur la famille à entrer en input ########################
  #############################################################################################
  
  ## Composition familiale ##
  
  n0 <<- n0 #situation conjugale (1,2 ou 3)
  nb_adultes <<- pmin(n0,2) 
  nb_enfants <<- n1 #nombre d'enfants
  nb_enft_3 <<- n2 #nombre d'enfants (de moins de 3 ans)
  nb_enft_35 <<- n3 #nombre d'enfants (3 à 5 ans)
  nb_enft_610 <<- n4 #nombre d'enfants (6 à 10 ans)
  nb_enft_1113 <<- n5 #nombre d'enfants (11 à 13 ans)
  nb_enft_14 <<- n6 #nombre d'enfants (de 14 ans)
  nb_enft_1519 <<- n7 #nombre d'enfants (de 15 à 19 ans)
  nb_enft_20 <<- n8 #nombre d'enfants (de 20 ans)
  
  ## Revenus autres que les salaires ##
  sal_brut_conjoint <<- n9 # salaire brut du conjoint
  tps_trav_conjoint <<- pmin(1,sal_brut_conjoint/bareme_var[["smic_b"]]) # temps de travail du conjoint
  are_nette_conjoint <<- n10
  autres_rev <<- n11
  pas_sal_brut <<- n22
  pas_are <<- n12
  are_max <<- n13
  
  ## Recours ##
  recours_PA <<- 1
  recours_CMU_ACS <<- 1
  recours_conjoint_ASS <<- pmin(n15,1) #recours et eligibilite du conjoint à l'ASS
  
  ## Handicap (au sens de l'AAH) ##
  handicap_personne <<- n16
  handicap_conjoint <<- n17
  
  ## Divers ##
  maj_isole_rsa <<- pmin(n18,1)
  eligible_asf <<- pmin(n19,1)
  proprietaire <<- pmin(n20,1) # menage proprietaire (non accedant) ou heberge gratuitement
  
  nn <<- (max(c(n21,n13))%/%max(c(n22,n12)))+2 #calcul du nombre de lignes de l'échelle de revenus
  
  #############################################################################################
  ######################## Parametres calcules (ne pas modifier) ##############################
  #############################################################################################
  
  
  ### RSA ###
  
  #Calcul montant forfaitaire du RSA
  mf_RSA <<- ifelse((nb_adultes==1)&&(maj_isole_rsa==0),
                    bareme_var[["mont_forfaitaire_rsa"]]*(
                      1+(nb_enfants>=1)*bareme_var[["tx_majo_rsa_pac1"]]+(nb_enfants>=2)*bareme_var[["tx_majo_rsa_enf12"]]+
                        pmax(nb_enfants-2,0)*bareme_var[["tx_majo_rsa_enf3"]]
                    ),
                    0)+ifelse(nb_adultes==2,
                              bareme_var[["mont_forfaitaire_rsa"]]*(1+bareme_var[["tx_majo_rsa_pac1"]]+pmin(nb_enfants,2)*bareme_var[["tx_majo_rsa_enf12"]]+
                                                                      pmax(nb_enfants-2,0)*bareme_var[["tx_majo_rsa_enf3"]]),
                              0)+ifelse((nb_adultes==1)&&(maj_isole_rsa==1),
                                        bareme_var[["mont_forfaitaire_rsa_maj"]]*(1+pmin(nb_enfants,2)*bareme_var[["tx_majo_rsa_majo_enf12"]]+
                                                                                    pmax(nb_enfants-2,0)*bareme_var[["tx_majo_rsa_majo_enf3"]]),
                                        0)
  
  #Calcul montant forfait logement
  fl_RSA <<- (((nb_adultes+nb_enfants)==1)*bareme_var[["forf_logement_rsa_1"]])+ (((nb_adultes+nb_enfants)==2)*bareme_var[["forf_logement_rsa_2"]])+
    (((nb_adultes+nb_enfants)>2)*bareme_var[["forf_logement_rsa_3"]])
  
  ### Prime d'activite ###
  
  #Calcul montant forfaitaire de la PA
  if (year>15){
    mf_PA <<- ifelse((nb_adultes==1)&&(maj_isole_rsa==0),
                     bareme_var[["mont_forfaitaire_PA"]]*(
                       1+(nb_enfants>=1)*bareme_var[["tx_majo_rsa_pac1"]]+(nb_enfants>=2)*bareme_var[["tx_majo_rsa_enf12"]]+pmax(nb_enfants-2,0)*bareme_var[["tx_majo_rsa_enf3"]]),
                     0
    ) + ifelse(nb_adultes==2,
               bareme_var[["mont_forfaitaire_PA"]]*(1+bareme_var[["tx_majo_rsa_pac1"]]+pmin(nb_enfants,2)*bareme_var[["tx_majo_rsa_enf12"]]+
                                                      pmax(nb_enfants-2,0)*bareme_var[["tx_majo_rsa_enf3"]]),
               0
    )+ ifelse((nb_adultes==1)&&(maj_isole_rsa==1),
              bareme_var[["montant_forfaitaire_PA_majo"]]*(1+pmin(nb_enfants,2)*bareme_var[["tx_majo_rsa_majo_enf12"]]+
                                                             pmax(nb_enfants-2,0)*bareme_var[["tx_majo_rsa_majo_enf3"]]),
              0
    )
    
    #Calcul montant forfait logement
    fl_PA <<- (((nb_adultes+nb_enfants)==1)*bareme_var[["forf_logement_PA_1"]])+ (((nb_adultes+nb_enfants)==2)*bareme_var[["forf_logement_PA_2"]])+
      (((nb_adultes+nb_enfants)>2)*bareme_var[["forf_logement_PA_3"]])
  } else {
    mf_PA <<- 0
    fl_PA <<- 0
  }
  
  
  ### Allocations familiales ###
  
  # Dont majo pour age
  af_majo_age <<- ifelse((nb_enfants-nb_enft_20)>=3,
                         (nb_enft_1519+nb_enft_14)*bareme_var[["majo_age_Af"]],
                         0)+
    ifelse((nb_enfants-nb_enft_20==2)&&(nb_enft_1519+nb_enft_14==2),
           bareme_var[["majo_age_Af"]],
           0)
  
  # Montant AF
  montant_af <<- ifelse((nb_enfants-nb_enft_20)>=2,
                        bareme_var[["AF_2enft"]]+pmax(0,bareme_var[["AF_enft_sup"]]*(nb_enfants-nb_enft_20-2)) 
                        + ifelse((nb_enft_20>=1)&&(nb_enfants>=3), bareme_var[["AF_forf_20ans"]],0) + af_majo_age,
                        0)
  
  
  # Plafond 1ere tranche AF
  plaf_1tranch_AF <<- bareme_var[["p1_modulation_af"]]+ifelse((nb_enfants-nb_enft_20)>2, 
                                                              (nb_enfants-nb_enft_20-2)*bareme_var[["sup_enf_modulation_af"]], 0)
  +ifelse((nb_enft_20)>=1&&(nb_enfants)>2, 
          nb_enft_20*bareme_var[["sup_enf_modulation_af"]], 0)
  
  # Plafond 2eme tranche AF
  plaf_2tranch_AF <<- bareme_var[["p2_modulation_af"]]+ifelse((nb_enfants-nb_enft_20)>2, 
                                                              (nb_enfants-nb_enft_20-2)*bareme_var[["sup_enf_modulation_af"]], 0)
  +ifelse((nb_enft_20)>=1&&(nb_enfants)>2, 
          nb_enft_20*bareme_var[["sup_enf_modulation_af"]], 0)
  
  
  ### Allocations logements ###
  
  # Loyer plafonne et charges (L+C)
  al_LC <<- ifelse(nb_enfants==0,
                   ifelse(nb_adultes==1,
                          bareme_var[["LC_isole"]],
                          bareme_var[["LC_couple"]]),
                   ifelse(nb_enfants==1,
                          bareme_var[["LC_1_pac"]],bareme_var[["LC_1_pac"]]+bareme_var[["Lc_supp_pac"]]*(nb_enfants-1))
  )
  
  # Participation minimale (PO)
  al_PO <<- pmax (bareme_var[["montant_PO"]],bareme_var[["taux_PO"]]*al_LC)
  
  # Taux famille (TF)
  al_TF <<- ifelse(nb_enfants==0,
                   ifelse(nb_adultes==1,
                          bareme_var[["TF_isole"]],
                          bareme_var[["TF_couple"]]),
                   ifelse(nb_enfants==1,
                          bareme_var[["TF_1pac"]],
                          ifelse(nb_enfants==2,
                                 bareme_var[["TF_2pac"]],
                                 ifelse(nb_enfants==3,
                                        bareme_var[["TF_3pac"]],
                                        ifelse(nb_enfants==4,
                                               bareme_var[["TF_4pac"]],
                                               bareme_var[["TF_4pac"]]+bareme_var[["Tf_pers_sup"]]*(nb_enfants-4)
                                        )
                                 )
                          )
                   )
  )
  
  
  # Abattement forfaitaire de ressources - R0
  al_RO <<- ifelse(nb_enfants==0,
                   ifelse(nb_adultes==1,
                          bareme_var[["R0_isole"]],
                          bareme_var[["R0_couple"]]),
                   ifelse(nb_enfants==1,
                          bareme_var[["R0_1pac"]],
                          ifelse(nb_enfants==2,
                                 bareme_var[["R0_2pac"]],
                                 ifelse(nb_enfants==3,
                                        bareme_var[["R0_3pac"]],
                                        ifelse(nb_enfants==4,
                                               bareme_var[["R0_4pac"]],
                                               ifelse(nb_enfants==5,
                                                      bareme_var[["R0_5pac"]],
                                                      ifelse(nb_enfants==6,
                                                             bareme_var[["R0_6pac"]],
                                                             bareme_var[["R0_6pac"]]+bareme_var[["R0_pers_sup"]]*(nb_enfants-6)
                                                      )
                                               )
                                        )
                                 )
                          )
                   )
  )
  
  al_RO <<- al_RO/12
  
  ### Prelevements sociaux (conjoint) ###
  
  # Cotisations sociales employeurs
  if (year>18){
    cs_emp <<- function(x,bareme){
      y <<- ifelse(x>0,bareme[["taux_cs_emp_t1"]]*pmin(x, bareme[["plafond_ss"]]),0)+
        ifelse(x>bareme[["plafond_ss"]],bareme[["taux_cs_emp_t2"]]*pmin(x-bareme[["plafond_ss"]], 2*bareme[["plafond_ss"]]),0)+
        ifelse(x>3*bareme[["plafond_ss"]],bareme[["taux_cs_emp_t3"]]*pmin(x-3*bareme[["plafond_ss"]], 1*bareme[["plafond_ss"]]),0)+
        ifelse(x>4*bareme[["plafond_ss"]],bareme[["taux_cs_emp_t4"]]*pmin(x-4*bareme[["plafond_ss"]], 4*bareme[["plafond_ss"]]),0)+
        ifelse(x>8*bareme[["plafond_ss"]],bareme[["taux_cs_emp_t5"]]*(x-8*bareme[["plafond_ss"]]),0)+
        ifelse(x>bareme[["plafond_ss"]],bareme[["emp_retraites_comp_cet"]]*pmin(x, 8*bareme[["plafond_ss"]]),0)  
      return(y)
    }
  } else {
    cs_emp <<- function(x,bareme){
      y <<- ifelse(x>0,bareme[["taux_cs_emp_t1"]]*pmin(x, bareme[["plafond_ss"]]),0)+
        ifelse(x>bareme[["plafond_ss"]],bareme[["taux_cs_emp_t2"]]*pmin(x-bareme[["plafond_ss"]], 2*bareme[["plafond_ss"]]),0)+
        ifelse(x>3*bareme[["plafond_ss"]],bareme[["taux_cs_emp_t3"]]*pmin(x-3*bareme[["plafond_ss"]], 1*bareme[["plafond_ss"]]),0)+
        ifelse(x>4*bareme[["plafond_ss"]],bareme[["taux_cs_emp_t4"]]*(x-4*bareme[["plafond_ss"]]),0)
      return(y)
    }
  }
  
 part_smic_conj <- (sal_brut_conjoint/bareme_var[["smic_b"]])*100
  if (year>14){
    if (year>18){
      cotis_emp_conj <<- as.numeric(cs_emp(sal_brut_conjoint,bareme_var)-SI(part_smic_conj<=bareme_var[["part_smic_exo"]]*100,bareme_var[["mod_cotis_fam"]]*sal_brut_conjoint,0)-SI(part_smic_conj<=bareme_var[["seuil_smic_pat_assmal"]]*100,bareme_var[["emp_maladie_t1"]]*sal_brut_conjoint,0)) # Cotisations sociales employeurs
    } else {
    cotis_emp_conj <<- as.numeric(cs_emp(sal_brut_conjoint,bareme_var)-SI(part_smic_conj<=bareme_var[["part_smic_exo"]]*100,bareme_var[["mod_cotis_fam"]]*sal_brut_conjoint,0)) # Cotisations sociales employeurs
    }
  } else {
    cotis_emp_conj <<- as.numeric(cs_emp(sal_brut_conjoint,bareme_var))
    }

  # Exoneration Fillon
  
  # NB : pas la formule qui se trouve dans la maquette, mais celle indiquee sur service public (https://www.service-public.fr/professionnels-entreprises/vosdroits/F24542)
  exo_fillon_conj <<- exo_fillon1(sal_brut_conjoint,bareme_var)  
  # NB : pas la mÃªme formule pour le conjoint et la personne mais je ne comprends pas celle dans la maquette
  
  # Cotisations sociales salaries
  
  if (year>18){
    cs_sal <<- function(x,bareme){
      y<<- (x>0)*bareme[["taux_cs_sal_t1"]]*pmin(x, bareme[["plafond_ss"]])+
        (x>bareme[["plafond_ss"]])*bareme[["taux_cs_sal_t2"]]*pmin(x-bareme[["plafond_ss"]], 2*bareme[["plafond_ss"]])+
        (x>3*bareme[["plafond_ss"]])*bareme[["taux_cs_sal_t3"]]*pmin(x-3*bareme[["plafond_ss"]], 1*bareme[["plafond_ss"]])+ 
        (x>4*bareme[["plafond_ss"]])*bareme[["taux_cs_sal_t4"]]*pmin(x-4*bareme[["plafond_ss"]], 4*bareme[["plafond_ss"]])+ 
        (x>8*bareme[["plafond_ss"]])*bareme[["taux_cs_sal_t5"]]*(x-8*bareme[["plafond_ss"]])+
        ifelse(x>bareme[["plafond_ss"]],bareme[["sal_retraites_comp_cet"]]*pmin(x, 8*bareme[["plafond_ss"]]),0)  
      return(y)
    }
  } else {
    cs_sal <<- function(x,bareme){
      y<<- (x>0)*bareme[["taux_cs_sal_t1"]]*pmin(x, bareme[["plafond_ss"]])+
        (x>bareme[["plafond_ss"]])*bareme[["taux_cs_sal_t2"]]*pmin(x-bareme[["plafond_ss"]], 2*bareme[["plafond_ss"]])+
        (x>3*bareme[["plafond_ss"]])*bareme[["taux_cs_sal_t3"]]*pmin(x-3*bareme[["plafond_ss"]], 1*bareme[["plafond_ss"]])+
        (x>4*bareme[["plafond_ss"]])*bareme[["taux_cs_sal_t4"]]*(x-4*bareme[["plafond_ss"]])
      return(y)
    }
  }

  cotis_sal_conj <<- cs_sal(sal_brut_conjoint,bareme_var)
  
  # CSG deductible
  csg_deduc_conj<<- csg_deduc(sal_brut_conjoint,bareme_var)
  
  # CSG non deductible / CRDS
  csg_non_deduc_conj <<- csg_non_deduc(sal_brut_conjoint,bareme_var)
  
  ### Salaires (conjoint) ###
  
  # Salaire mensuel declare du conjoint
  sal_declar_conj <<- (nb_adultes==2)*(sal_brut_conjoint-cotis_sal_conj-csg_deduc_conj)
  
  # Cout du travail du conjoint
  cout_trav_conj <<- (nb_adultes==2)*(sal_brut_conjoint+cotis_emp_conj-exo_fillon_conj)
  
  # Salaire mensuel net du conjoint
  sal_net_conj <<- ifelse(nb_adultes==2,sal_brut_conjoint-cotis_sal_conj-csg_deduc_conj-csg_non_deduc_conj,0)
  

  ### Quotient familial ###
  
  # Nombre de parts au sens de l'IR
  nb_part <<- nb_adultes+(nb_enfants*0.5)+(nb_enfants>2)*(nb_enfants-2)*0.5 + (nb_adultes==1)*(nb_enfants>0)*0.5
  
  # Avantage maximal lie au quotien familial
  max_qf <<- bareme_var[["plafond_qf"]]*(nb_part-nb_adultes)*2 + 
    ifelse((nb_adultes==1)&&(nb_enfants>0),bareme_var[["plaf_qf_monoparent"]]-2*bareme_var[["plafond_qf"]]  , 0)
  
  
  ### Taxe d'habitation ###
  
  # Montant
  if (year<23){
    montant_TH <<- (nb_adultes==1)*bareme_var[["montant_th_1pers"]] + (nb_adultes!=1)*bareme_var[["mont_th_couple"]]
  } else {
    montant_TH <<-0
  }
  
  # Abattement RFR
  if (year<20){
    abat_rfr <<- bareme_var[["abt_th_base"]]+bareme_var[["abt_th_4demipart"]]*pmin(2,nb_part-1)*2+bareme_var[["abt_th_demipartsup"]]*pmax(0,nb_part-3)*2
  } else {
    abat_rfr <<-0
  }
  
  # Plafond d'eligiblite
  if (year<20) {plaf_elig_th <<- bareme_var[["plaf_th_base"]]+bareme_var[["plaf_th_1demipart"]]*pmin(1.5,nb_part-1)*2+bareme_var[["plaf_th_demipartsup"]]*pmax(0,nb_part-1.5)*2}
  else {plaf_elig_th <<-0}
  
  
  #Seuil RFR dégrèvement total
  if (year>17 & year<23){
    seuil_rfr_degr_tot<<-bareme_var[["seuil_degrev_th_max_1p"]]+pmin(nb_part-1,2)*2*bareme_var[["supp_th_max_2demipart"]]+pmax(nb_part-2,0)*2*bareme_var[["supp_th_max_demipartsup"]]
  } else {
    seuil_rfr_degr_tot<<-0
  }
  
  #Seuil RFR dégrèvement dégressif
  if (year>17 & year<23){
    seuil_rfr_degr_deg<<-bareme_var[["seuil_degrev_th_deg_1p"]]+pmin(nb_part-1,2)*2*bareme_var[["supp_th_deg_2demipart"]]+pmax(nb_part-2,0)*2*bareme_var[["supp_th_deg_demipartsup"]]
  } else {
    seuil_rfr_degr_deg<<-0
  }
  
  
  ### CMU/ACS ###
  
  # Plafond CMUC
  if (year<23){
    plaf_cmuc <<- bareme_var[["plaf_cmuc_base"]] +
      pmin(1,nb_adultes+nb_enfants-1)*bareme_var[["plaf_cmuc_1pac"]]+
      pmax(0,pmin(1,nb_adultes+nb_enfants-2))*bareme_var[["plaf_cmuc_34pac"]]+
      pmax(0,pmin(1,nb_adultes+nb_enfants-3))*bareme_var[["plaf_cmuc_34pac"]]+
      pmax(0,(nb_adultes+nb_enfants-4))*bareme_var[["plaf_cmuc_5pluspac"]]
  } else {
    plaf_cmuc <<- 0
  }
  
  # Plafond ACS
  if (year>19){
    plaf_acs <<- bareme_var[["tx_majo_plafond_cssp"]]*plaf_cmuc
  } else {
    plaf_acs <<- bareme_var[["tx_majo_plafond_acs"]]*plaf_cmuc
  }
  
  
  ### Montant d'ASF ###
  
  if (year>13){
    # Montant total
    montant_asf <<- (eligible_asf==1)*(nb_adultes==1)*bareme_var[["mont_ASF_total"]]*nb_enfants
    # Entrant dans la base ressources du RSA
    asf_br_rsa <<- (eligible_asf==1)*(nb_adultes==1)*bareme_var[["mont_ASF_RSA"]]*nb_enfants
  } else {
    montant_asf <<- 0
    asf_br_rsa <<- 0
  }
  
  
  if (year>14){
    ### ARE ###
    #Salaire net de reference
    sal_ref_conj_are <<- (nb_adultes==2)*(are_nette_conjoint/bareme_var[["rap_salnet_are"]])
  } else {
    sal_ref_conj_are <<- 0
  }
  
  ### AAH ###
  #Plafond
  if (year>23){
    plaf_AAH <<- bareme_var[["AAH_plaf_seul"]] + bareme_var[["AAH_plaf_sup_pac"]]*nb_enfants
  } else { if (year>14){
    plaf_AAH <<- (nb_adultes==1)*bareme_var[["AAH_plaf_seul"]] + (nb_adultes!=1)*bareme_var[["AAH_plaf_couple"]] + bareme_var[["AAH_plaf_sup_pac"]]*nb_enfants
  } else { 
    plaf_AAH <<- 0
  }
  }
  
  ### Reduction d'IR ###
  if (year>16 & year<20){
    # 1er plafond
    prem_plaf_RI <<- (nb_adultes>1)*(bareme_var[["plaf1_RI_couple"]]+bareme_var[["RI_demipart"]]*(nb_part-2)) + (nb_adultes<=1)*(bareme_var[["plaf1_RI"]]+bareme_var[["RI_demipart"]]*(nb_part-1))
    #2e plafond
    deux_plaf_RI <<- (nb_adultes>1)*(bareme_var[["plaf2_RI_couple"]]+bareme_var[["RI_demipart"]]*(nb_part-2)) + (nb_adultes<=1)*(bareme_var[["plaf2_RI"]]+bareme_var[["RI_demipart"]]*(nb_part-1))
  } else {
    prem_plaf_RI <<- 0
    deux_plaf_RI <<- 0
  }
  
  
  
  ### Prime pour l'emploi (seulement de 2013 à 2015, disparait apres et devient PA) ###
  
  if (year<16){
    # Plafond RFR PPE
    plaf_rfr_ppe <<- ifelse(nb_adultes==1,
                            bareme_var[["plaf_ppe_seul"]],
                            bareme_var[["plaf_ppe_couple"]])+
      bareme_var[["plaf_ppe_suppl_enf"]]*pmin(2,nb_enfants)+2*bareme_var[["plaf_ppe_suppl_enf"]]*pmax(0,nb_enfants-2)
    
    # Temps de travail PPE conjoint
    if (tps_trav_conjoint < 0.5){
      tps_trav_conj_ppe <<- "Tps partielc"
    } else {
      if(tps_trav_conjoint < 1){
        tps_trav_conj_ppe <<- "Tps partiell"
      } else{
        tps_trav_conj_ppe <<- "Tps plein"
      }
    }
  } else {
    plaf_rfr_ppe <<- 0
    tps_trav_conj_ppe <<- ""
  }
  
  ### Unité de consommation pour chéque énergie ###
  uc_energie <<- (nb_adultes==1)*(1+(nb_enfants>0)*(0.5+(nb_enfants-1)*0.3)) + (nb_adultes>1)*(1.5+nb_enfants*0.3)
  
} # fin de la fonction choix_input


