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
#########           Calcul des transferts et construction               ########
#########            des tables de données et des graphiques            ########
#########                                                               ########  
################################################################################




#==================================================================================================#
#============================================= A LIRE =============================================#
#==================================================================================================#

# Dans ce script est definie la fonction castype() qui permet de passer d'un vecteur de revenus d'activite bruts à un dataframe contenant
# tous les transferts et le revenu disponible.
# Les fonctions castype1(), castype2(), castype3() genèrent les graphiques visibles dans le dernier onglet.
# Toutes ces fonctions sont appelées dans le pgm server.R

castype <- function(vecteur,bareme_var,year){
###vecteur est un vecteur de revenus vide dont la taille correspond aux paramètres d'échelle de revenus choisis par l'utilisateur
##bareme_var sont les paramètres législatifs
##year est l'année de législation sélectionnée

print(bareme_var[["seuil_D9"]])
  
  #le vecteur est rempli selon le pas choisi par l'utilisateur
  for (i in 2:length(vecteur)){
    vecteur[i]=vecteur[i-1]+pas_sal_brut
  }
  

  ###################################
  #### Revenus d'activite bruts #####
  ###################################
  
  percen_smic_tps_plein <- (vecteur/bareme_var[["smic_b"]])*100 #Salaire en % du Smic brut tps plein 
  tps_travail <- pmin(percen_smic_tps_plein,100) #temps de travail (en % du temps plein SMIC brut)
  
  ##################
  #### Chomage #####
  ##################

  #ARE nette
  if (year>14){
    ARE_net <- vector("numeric",nn) 
    for (i in 2:length(ARE_net)){
      ARE_net[i]=ARE_net[i-1]+pas_are
      }
    
    # Salaire de reference net (N-1 et N-2)
    sal_ref_net <- ARE_net/bareme_var[["rap_salnet_are"]]
      
  } else {
    ARE_net <- vector("numeric",nn)
    sal_ref_net <- vector("numeric",nn)
  }
    
  
  ###############################
  #### Prelevements sociaux #####
  ###############################
  
  if (year>14){
    if (year>18){
      cotis_emp <- cs_emp(vecteur,bareme_var)-SI(percen_smic_tps_plein<=bareme_var[["part_smic_exo"]]*100,bareme_var[["mod_cotis_fam"]]*vecteur,0)-SI(percen_smic_tps_plein<=bareme_var[["seuil_smic_pat_assmal"]]*100,bareme_var[["emp_maladie_t1"]]*vecteur,0) # Cotisations sociales employeurs
    } else {
      cotis_emp <- cs_emp(vecteur,bareme_var)-SI(percen_smic_tps_plein<=bareme_var[["part_smic_exo"]]*100,bareme_var[["mod_cotis_fam"]]*vecteur,0) # Cotisations sociales employeurs
    }
  } else {
    cotis_emp <- cs_emp(vecteur,bareme_var)
  }
  fillon_exo <- exo_fillon(vecteur,tps_travail,percen_smic_tps_plein,bareme_var)# Exoneration Fillon
  fillon_exo[1] <- 0
  cotis_sal <- cs_sal(vecteur,bareme_var) # Cotisations sociales salaries
  csg_ded <- csg_deduc(vecteur,bareme_var) # CSG deductible (sur les salaires)
  csg_non_ded <- csg_non_deduc(vecteur,bareme_var) # CSG non-deductible/CRDS
  

  #####################################################
  #### Revenus d'activite nets et cout du travail #####
  #####################################################
  
  rev_act_net <- vecteur-cotis_sal-csg_ded-csg_non_ded # Revenus d'activite annee N nets
  rev_act_dec <- vecteur-cotis_sal-csg_ded # Revenus d'activite annee N declares
  perc_smic_net_tpsplein <- (rev_act_net/bareme_var[["smic_n"]])*100 # Salaire en % du smic net temps plein
  tps_trav_net <- pmin(100,perc_smic_net_tpsplein) # Temps de trav. (en % du temps plein smic net)
  cout_travail <- vecteur+cotis_emp-fillon_exo #Cout travail annee N
  
  
  ####################################
  #### Prime pour l'emploi (PPE) #####
  ####################################
  # NB : la prime pour l'emploi disparaît en 2016, remplacee par la prime d'activite, ces colonnes ne sont donc valables que pour 2013-2015
  if (year<16){
    
    # Temps de travail PPE
    ppe_tps_trav <- vector("character",nn)
    ppe_tps_trav <- ifelse(perc_smic_net_tpsplein<50,"Tps partielc",ifelse(perc_smic_net_tpsplein<100,"Tps partiell","Tps plein"))

    # Eligibilite RFR
    ppe_elig_rfr <- vector("numeric",nn)
    # ppe_elig_rfr <- SI((0.9*(rev_act_dec+sal_declar_conj+autres_rev)/(1+bareme_var[["deflat"]]))>plaf_rfr_ppe,0,1)
    ppe_elig_rfr <- SI((0.9*(rev_act_dec+autres_rev+sal_ref_net+sal_declar_conj+sal_ref_conj_are)/(1+bareme_var[["deflat"]]))>plaf_rfr_ppe,0,1)

    # PPE declarant si temps plein 
     ppe_declar_tps_plein <- SI(ppe_elig_rfr==1,1,0)*SI(rev_act_dec/(1+bareme_var[["deflat"]])<bareme_var[["min_ppe"]],
                              0,
                              SI((rev_act_dec/((1+bareme_var[["deflat"]])*tps_trav_net/100))<=bareme_var[["sommet_ppe"]],
                                 bareme_var[["tx_ppe_avant"]]*(rev_act_dec/((1+bareme_var[["deflat"]])*tps_trav_net/100)),
                                 SI((rev_act_dec/((1+bareme_var[["deflat"]])*tps_trav_net/100))<=bareme_var[["max_ppe"]],
                                    (bareme_var[["max_ppe"]]-(rev_act_dec/((1+bareme_var[["deflat"]])*tps_trav_net/100)))*bareme_var[["tx_ppe_apres"]],
                                    0)))
     ppe_declar_tps_plein[1]<-0 #nécessaire car division par tps_trav_net = 0 sinon

     if (all(rev_act_dec==rep(0,nn))){ppe_declar_tps_plein<-0} #nécessaire pour les mm raisons (correspond au cas où l'individu est à l'ARE)

    # Prime individuelle
    
    ppe_prime_indiv <- SI(ppe_elig_rfr==1,1,0)*
      SI(
        ppe_tps_trav=="Tps partielc",
        (1+bareme_var[["majo_tp_ppe"]])*perc_smic_net_tpsplein/100,
        SI(
          ppe_tps_trav=="Tps partiell",
          (1-bareme_var[["majo_tp_ppe"]])*perc_smic_net_tpsplein/100+bareme_var[["majo_tp_ppe"]],
          1)
        )*ppe_declar_tps_plein

    # PPE conjoint si temps plein
    ppe_conj_tps_plein <- vector("numeric",nn)
    if (nb_adultes>1 && sal_brut_conjoint>0){
      ppe_conj_tps_plein <- (ppe_elig_rfr==1)*
        SI(sal_net_conj/(1+bareme_var[["deflat"]])<bareme_var[["min_ppe"]],
           0,
           SI((sal_net_conj/((1+bareme_var[["deflat"]])*tps_trav_conjoint))<=bareme_var[["sommet_ppe"]],
              bareme_var[["tx_ppe_avant"]]*(sal_net_conj/((1+bareme_var[["deflat"]])*tps_trav_conjoint)),
              SI((sal_net_conj/((1+bareme_var[["deflat"]])*tps_trav_conjoint))<=bareme_var[["max_ppe"]],
                 (bareme_var[["max_ppe"]]-(sal_net_conj/((1+bareme_var[["deflat"]])*tps_trav_conjoint)))*bareme_var[["tx_ppe_apres"]],
                 0)))
    } else {
      ppe_conj_tps_plein <- rep(0,nn)
    }
    
    # Prime individuelle conjoint
    

    ppe_prime_conj <- (SI(ppe_elig_rfr==1,1,0))*
      SI(
        tps_trav_conj_ppe=="Tps partielc",
        (1+bareme_var[["majo_tp_ppe"]])*tps_trav_conjoint,
        SI(
          tps_trav_conj_ppe=="Tps partiell",
          (1-bareme_var[["majo_tp_ppe"]])*tps_trav_conjoint+bareme_var[["majo_tp_ppe"]],
          1)
      )*ppe_conj_tps_plein
    
    # Majoration monoactif
    
    ppe_majo_monoact <- SI(ppe_elig_rfr==1,1,0)*
      SI(nb_adultes==1,
         0,
         SI((
           (sal_net_conj/(1+bareme_var[["deflat"]])<bareme_var[["min_ppe"]])*(bareme_var[["min_ppe"]]<rev_act_dec/(1+bareme_var[["deflat"]]))*(rev_act_dec/(1+bareme_var[["deflat"]])<bareme_var[["max_mono_ppe_inf"]])|
             (rev_act_dec/(1+bareme_var[["deflat"]])<bareme_var[["min_ppe"]])*(bareme_var[["min_ppe"]]<sal_net_conj/(1+bareme_var[["deflat"]]))*(sal_net_conj/(1+bareme_var[["deflat"]])<bareme_var[["max_mono_ppe_inf"]])),
            bareme_var[["majo_mono_act_ppe"]],
            0
            ))
    
    # Majoration PAC
    
    ppe_majo_PAC <- SI(ppe_elig_rfr==1,1,0)*(
      SI((nb_adultes==2)*(nb_enfants>0),
         SI((ppe_prime_indiv>0 | ppe_prime_conj>0),
            nb_enfants*bareme_var[["majo_enf_ppe"]],
            SI(
              (bareme_var[["min_ppe"]]<sal_net_conj)*(sal_net_conj<bareme_var[["max_mono_ppe"]]) | (bareme_var[["min_ppe"]]<rev_act_dec/(1+bareme_var[["deflat"]]))*(rev_act_dec/(1+bareme_var[["deflat"]])<bareme_var[["max_mono_ppe"]]),
               bareme_var[["majo_enf_ppe"]],
              0)),
         0)+
        SI((nb_adultes==1)*(nb_enfants>0),
           SI(ppe_prime_indiv > 0,
              bareme_var[["majo_monoparent_ppe"]] + (nb_enfants-1)*bareme_var[["majo_enf_ppe"]],
              SI((bareme_var[["min_ppe"]]< rev_act_dec/(1+bareme_var[["deflat"]]))*(rev_act_dec/(1+bareme_var[["deflat"]])<bareme_var[["max_mono_ppe"]]),
                 bareme_var[["majo_monoparent_ppe"]],
                 0)),
           0)
    )
    

    # PPE totale (avant imputation du RSA)
    ppe_tot_avRSA <- ppe_prime_indiv+ppe_prime_conj+ppe_majo_monoact+ppe_majo_PAC
      
} else {
    ppe_tps_trav <- vector("character",nn)
    ppe_elig_rfr <- vector("numeric",nn)
    ppe_declar_tps_plein <- vector("numeric",nn)
    ppe_prime_indiv <- vector("numeric",nn)
    ppe_conj_tps_plein <- vector("numeric",nn)
    ppe_prime_conj <- vector("numeric",nn)
    ppe_majo_monoact <- vector("numeric",nn)
    ppe_majo_PAC <- vector("numeric",nn)
    ppe_tot_avRSA <- vector("numeric",nn)
  }
   

  ########################
  #### ASS du conjoint ###
  ########################
  
    # Base ressource ASS (revenu imposable N-1 avant abattements)
    br_ASS <- (rev_act_dec+autres_rev+sal_declar_conj+sal_ref_net+sal_ref_conj_are)/(1+bareme_var[["deflat"]])
    
    
    # Montant ASS
    mont_ASS <- (nb_adultes==2)*recours_conjoint_ASS*(
      (br_ASS<bareme_var[["ASS_plaf_couple"]])*bareme_var[["ASS_mtt_forf"]]+
        (br_ASS>= bareme_var[["ASS_plaf_couple"]])*(bareme_var[["ASS_diff_plaf_couple"]]-br_ASS>=bareme_var[["seuil_vers_ASS"]])*(bareme_var[["ASS_diff_plaf_couple"]]-br_ASS)
    )
    
  
  #################################################
  #### Allocation adulte handicape (conjoint) #####
  #################################################
  
  # Toutes les cases presentes dans la maquette ne le sont pas dans cette section, 
  # elles ont ete remontees afin de pouvoir executer certaines cellules
    
    if (year<22){
      
      br_conj_AAH <- (rev_act_dec*bareme_var[["AAH_abatt_general"]]+autres_rev+are_nette_conjoint+ARE_net)*bareme_var[["abatt_rfr"]]+
        pmin(sal_declar_conj,bareme_var[["AAH_plafond_abatt_salaire"]])*bareme_var[["AAH_abatt_salaire_t1"]] +
        pmax(sal_declar_conj-bareme_var[["AAH_plafond_abatt_salaire"]],0)*bareme_var[["AAH_abatt_salaire_t2"]]
      
      #Montant (conjoint)
      mont_conj_AAH <- (handicap_conjoint>0)*SI(plaf_AAH-br_conj_AAH>=bareme_var[["AAH_montant"]],bareme_var[["AAH_montant"]],pmax(0,plaf_AAH-br_conj_AAH))
      
      #Max(AAH,ASS) du conjoint (non cumul)
      max_aah_ass_conj <- pmax(mont_ASS,mont_conj_AAH)
      
    } else {
    
      br_conj_AAH <- pmax(0,rev_act_dec*bareme_var[["abatt_rfr"]]-bareme_var[["AAH_abatt_general"]]-nb_enfants*bareme_var[["AAH_abatt_majo"]])
                             +(autres_rev+are_nette_conjoint+ARE_net)*bareme_var[["abatt_rfr"]]+
        pmin(sal_declar_conj,bareme_var[["AAH_plafond_abatt_salaire"]])*bareme_var[["AAH_abatt_salaire_t1"]] +
        pmax(sal_declar_conj-bareme_var[["AAH_plafond_abatt_salaire"]],0)*bareme_var[["AAH_abatt_salaire_t2"]]
      
      #Montant (conjoint)
      mont_conj_AAH <- (handicap_conjoint>0)*SI(plaf_AAH-br_conj_AAH>=bareme_var[["AAH_montant"]],bareme_var[["AAH_montant"]],pmax(0,plaf_AAH-br_conj_AAH))
      
      #Max(AAH,ASS) du conjoint (non cumul)
      max_aah_ass_conj <- pmax(mont_ASS,mont_conj_AAH)
      
    }
    

  ######################################
  #### ASS du conjoint Prime de Noel ###
  ######################################
  
    ASS_conj <- (mont_ASS>0)*SI(year>16,(mont_ASS==max_aah_ass_conj),1)*bareme_var[["noel_ASS"]]

  ######################################
  ### Revenu imposable en N-1 et N-2 ###
  ######################################
  
  
  
  if (year>14){
    
    # Revenu imposable (N-1)
    rev_impo_n_1 <- (rev_act_dec+autres_rev+sal_declar_conj+sal_ref_net+sal_ref_conj_are +SI(year<17,mont_ASS,SI(mont_conj_AAH==max_aah_ass_conj,0,mont_ASS)))/(1+bareme_var[["deflat"]])*bareme_var[["abatt_rfr"]] 
    
    
    # Revenu imposable (N-2)
    rev_impo_n_2 <- (rev_act_dec+autres_rev+sal_declar_conj+sal_ref_net+sal_ref_conj_are +SI(year<17,mont_ASS,SI(mont_conj_AAH==max_aah_ass_conj,0,mont_ASS)))/(1+bareme_var[["deflat_2"]])*bareme_var[["abatt_rfr"]] 
    
    # Revenu imposable (N-2), avec abatt. 30% pour les rev. act N-2 pour beneficiaires de l'ARE
    revimp_n_2_abatt <- (
      rev_act_dec+autres_rev+sal_declar_conj+SI(year<17,mont_ASS,SI(mont_conj_AAH==max_aah_ass_conj,0,mont_ASS))+(1-bareme_var[["abatt_rbg_are"]])*(sal_ref_net+sal_ref_conj_are)
    )/(1+bareme_var[["deflat_2"]])*bareme_var[["abatt_rfr"]]
    
  } else { 
    
    rev_impo_n_1 <- (rev_act_dec+autres_rev+sal_declar_conj)*0.9/(1+bareme_var[["deflat"]])
    
    rev_impo_n_2 <- (rev_act_dec+autres_rev+sal_declar_conj)*0.9/(1+bareme_var[["deflat_2"]])
    
    revimp_n_2_abatt <- vector("numeric",nn)
    
  }
    
  # Base ressource ASS (revenu imposable N-1 avant abattements)
  
  
  
  
  ##############################
  #### Allocations logement  ###
  ##############################
    
  if (year>20){
    
    # BR AL
    
    BR_AL <- ceiling_dec(12*((1+bareme_var[["deflat_2"]])*revimp_n_2_abatt-mont_ASS-(nb_adultes==2)*(rev_act_dec>bareme_var[["bmaf"]])*(sal_declar_conj>bareme_var[["bmaf"]])*bareme_var[["APL_abatt_biactifs"]]),-2)/12
    
    # AL
    
    AL <- floor((proprietaire==0)*(      
      pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO))-bareme_var[["Mfo_deduire"]])*(1-bareme_var[["tx_crds"]])>= bareme_var[["seuil_versement_AL"]]
    )*pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO))-bareme_var[["Mfo_deduire"]])*(1-bareme_var[["tx_crds"]]))
    
    
  }else{if (year>16){
    
  # BR AL
    
  BR_AL <- ceiling_dec(12*(revimp_n_2_abatt-mont_ASS - (nb_adultes==2)*(rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*bareme_var[["APL_abatt_biactifs"]]),-2)/12
    
  # AL
  
  AL <- floor((proprietaire==0)*(      
    pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO))-bareme_var[["Mfo_deduire"]])*(1-bareme_var[["tx_crds"]])>= bareme_var[["seuil_versement_AL"]]
    )*pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO))-bareme_var[["Mfo_deduire"]])*(1-bareme_var[["tx_crds"]]))
  
    } else { if (year>14){
    
    # BR AL
    
    BR_AL<-   ceiling_dec(12*(revimp_n_2_abatt-mont_ASS -(nb_adultes==2)*(rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*bareme_var[["APL_abatt_biactifs"]]),-2)/12
     
    # AL
    
    if (year==15){
      AL <- (proprietaire==0)*(
        pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO)))*(1-bareme_var[["tx_crds"]])>= bareme_var[["seuil_versement_AL"]]
      )*pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO)))*(1-bareme_var[["tx_crds"]])
    } else { 
      AL <- floor((proprietaire==0)*(
        pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO)))*(1-bareme_var[["tx_crds"]])>= bareme_var[["seuil_versement_AL"]]
      )*pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,BR_AL-al_RO)))*(1-bareme_var[["tx_crds"]]))
    }

    } else {
    
    # Abattements AL (abattements biactif non present dans les baremes de 2013 à 2014)
    BR_AL <- vector("numeric",nn)
    
    # AL
    AL <- SI(pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,(rev_act_dec+sal_declar_conj+autres_rev)*0.9/(1+bareme_var[["deflat_2"]])-al_RO)))*(1-bareme_var[["tx_crds"]])>= bareme_var[["seuil_versement_AL"]],
              pmax(0,al_LC-(al_PO+12*(al_TF+bareme_var[["al_taux_compl"]])*pmax(0,(rev_act_dec+sal_declar_conj+autres_rev)*0.9/(1+bareme_var[["deflat_2"]])-al_RO))),
               0)*(1-bareme_var[["tx_crds"]])
  }}
}
  ##############################
  #### Complement familial #####
  ##############################
  
  if (year > 13){
  # Plafond CF non majore
  plaf_CF <- SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
            bareme_var[["plafond_maj_CF"]]+pmax(nb_enfants-3,0)*bareme_var[["pers_sup_CF"]],
            bareme_var[["plafond_simple_CF"]] + pmax(nb_enfants-3,0)*bareme_var[["pers_sup_CF"]]
  )
  
  # Plafond CF majore
  
  
  plaf_CF_majo <- SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
            bareme_var[["plafond_maj_CF_majo"]]+pmax(nb_enfants-3,0)*bareme_var[["pers_sup_CF_majo"]],
            bareme_var[["plafond_simple_CF_majo"]] + pmax(nb_enfants-3,0)*bareme_var[["pers_sup_CF_majo"]]
  )
  
  
  # Montant
  
  mont_CF <- SI(((nb_enft_35+nb_enft_610+nb_enft_1113+nb_enft_14+nb_enft_1519+nb_enft_20)<3) | (nb_enft_3>=1),
            0,
            SI(revimp_n_2_abatt<plaf_CF_majo,
               bareme_var[["montant_CF_majo"]],
               SI(revimp_n_2_abatt<plaf_CF,
                  bareme_var[["montant_CF"]],
                  SI(revimp_n_2_abatt<(plaf_CF+bareme_var[["montant_CF"]]),
                     bareme_var[["montant_CF"]]+plaf_CF-revimp_n_2_abatt,
                     0)))
  )
  
  } else {
    
    # Plafond
    
    plaf_CF <- SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
              bareme_var[["plafond_maj_CF"]]+pmax(nb_enfants-3,0)*bareme_var[["pers_sup_CF"]],
              bareme_var[["plafond_simple_CF"]] + pmax(nb_enfants-3,0)*bareme_var[["pers_sup_CF"]]
    )
    
    plaf_CF_majo <- vector("numeric",nn)
    
    # Montant
    
    mont_CF <- SI(nb_enft_35+nb_enft_610+nb_enft_1113+nb_enft_14+nb_enft_1519+nb_enft_20<3,
              0,
              SI((rev_act_dec*0.9/(1+bareme_var[["deflat_2"]]))<plaf_CF,
                 bareme_var[["montant_CF"]],
                 SI((rev_act_dec*0.9/(1+bareme_var[["deflat_2"]]))<(plaf_CF+bareme_var[["montant_CF"]]),
                    bareme_var[["montant_CF"]]+plaf_CF-rev_act_dec*0.9/(1+bareme_var[["deflat_2"]]),
                    0)
                 )
              )

  }
  
  
  ########################################
  #### Allocation de base de la PAJE #####
  ########################################
  
  if (year>14){
    
    # Plafond de l'allocation de base à taux partiel
    
    plaf_AB_partiel <- SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
              bareme_var[["plafond_majore_paje"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje"]],
              bareme_var[["plaf_simple_paje"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje"]]
    )
    
    # Plafond de l'allocation de base à taux complet
    
    plaf_AB_plein <-  SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
               bareme_var[["plafond_majore_paje_plein"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje_plein"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje_plein"]],
               bareme_var[["plaf_simple_paje_plein"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje_plein"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje_plein"]]
    )
    
    
    # Montant
    
    mont_AB_paje <- SI(revimp_n_2_abatt<plaf_AB_plein,bareme_var[["montant_paje_plein"]]*(nb_enft_3>0),SI(revimp_n_2_abatt<plaf_AB_partiel,bareme_var[["montant_paje_partiel"]]*(nb_enft_3>0),0))
    
    
  } else { if (year==14){
    
    plaf_AB_partiel <- SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
              bareme_var[["plafond_majore_paje"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje"]],
              bareme_var[["plaf_simple_paje"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje"]]
    )
    
    plaf_AB_plein <-  SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
               bareme_var[["plafond_majore_paje_plein"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje_plein"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje_plein"]],
               bareme_var[["plaf_simple_paje_plein"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje_plein"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje_plein"]]
    )
    
    mont_AB_paje <- SI((rev_act_dec+sal_declar_conj+autres_rev)*0.9/(1+bareme_var[["deflat_2"]])<plaf_AB_plein,bareme_var[["montant_paje_plein"]]*(nb_enft_3>0),SI((rev_act_dec+sal_declar_conj+autres_rev)*0.9/(1+bareme_var[["deflat_2"]])<plaf_AB_partiel,bareme_var[["montant_paje_partiel"]]*(nb_enft_3>0),0))
    
  } else {
    # avant 2014, la distinction entre paje taux partiel/taux plein n'est pas faite
    
    plaf_AB_partiel <- SI((nb_adultes==1) | (rev_act_dec/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]])*(sal_declar_conj/(1+bareme_var[["deflat_2"]])>bareme_var[["bmaf_n_2"]]),
              bareme_var[["plafond_majore_paje_plein"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje_plein"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje_plein"]],
              bareme_var[["plaf_simple_paje_plein"]] + pmin(nb_enfants,2)*bareme_var[["plafond_sup_enf_12_paje_plein"]] + pmax(nb_enfants-3,0)*bareme_var[["plafond_sup_enf_3_paje_plein"]]
    )
    
    plaf_AB_plein <- vector("numeric",nn)
    
    mont_AB_paje <- SI((rev_act_dec*0.9/(1+bareme_var[["deflat_2"]]))<plaf_AB_partiel,
              bareme_var[["montant_paje_plein"]]*(nb_enft_3>0),
              0)
    }
 

  }
  

  
  ######################################
  #### Allocation adulte handicape #####
  ######################################
  
    
    if (year<22){
      # Base ressources
      br_AAH <- ((sal_declar_conj+ are_nette_conjoint)*bareme_var[["AAH_abatt_general"]] + autres_rev  + ARE_net +  SI(mont_conj_AAH==max_aah_ass_conj,0,mont_ASS))*bareme_var[["abatt_rfr"]]+
        pmin(rev_act_dec,bareme_var[["AAH_plafond_abatt_salaire"]])*bareme_var[["AAH_abatt_salaire_t1"]] +
        pmax(rev_act_dec-bareme_var[["AAH_plafond_abatt_salaire"]],0)*bareme_var[["AAH_abatt_salaire_t2"]]
      
      # Montant
      mont_AAH <- (handicap_personne>0)*SI(plaf_AAH-br_AAH>=bareme_var[["AAH_montant"]],bareme_var[["AAH_montant"]],pmax(0,plaf_AAH-br_AAH))
      
      # Majoration pour vie autonome
      mva <- (vecteur==0)*(handicap_personne==2)*(mont_AAH==bareme_var[["AAH_montant"]])*(AL>0)*bareme_var[["AAH_mva"]]
      
      #Majoration vie autonome (conjoint)
      mva_conj <- (sal_brut_conjoint==0)*(handicap_conjoint==2)*(mont_conj_AAH==bareme_var[["AAH_montant"]])*(AL>0)*bareme_var[["AAH_mva"]]
      
      #Total
      AAH_tot <- mont_AAH+mva+mont_conj_AAH+mva_conj
      
      #Max(AAH,ASS) du conjoint (non cumul)
      max_aah_ass_conj <- pmax(mont_ASS,mont_conj_AAH)
      
    } else {

      # Base ressources
      br_AAH <- pmax(0,(sal_declar_conj+ are_nette_conjoint)*bareme_var[["abatt_rfr"]]-bareme_var[["AAH_abatt_general"]]-nb_enfants*bareme_var[["AAH_abatt_majo"]])+ (autres_rev  + ARE_net +  SI(mont_conj_AAH==max_aah_ass_conj,0,mont_ASS))*bareme_var[["abatt_rfr"]]+
        pmin(rev_act_dec,bareme_var[["AAH_plafond_abatt_salaire"]])*bareme_var[["AAH_abatt_salaire_t1"]] +
        pmax(rev_act_dec-bareme_var[["AAH_plafond_abatt_salaire"]],0)*bareme_var[["AAH_abatt_salaire_t2"]]
      
      # Montant
      mont_AAH <- (handicap_personne>0)*SI(plaf_AAH-br_AAH>=bareme_var[["AAH_montant"]],bareme_var[["AAH_montant"]],pmax(0,plaf_AAH-br_AAH))
      
      # Majoration pour vie autonome
      mva <- (vecteur==0)*(handicap_personne==2)*(mont_AAH==bareme_var[["AAH_montant"]])*(AL>0)*bareme_var[["AAH_mva"]]
      
      #Majoration vie autonome (conjoint)
      mva_conj <- (sal_brut_conjoint==0)*(handicap_conjoint==2)*(mont_conj_AAH==bareme_var[["AAH_montant"]])*(AL>0)*bareme_var[["AAH_mva"]]
      
      #Total
      AAH_tot <- mont_AAH+mva+mont_conj_AAH+mva_conj
      
      #Max(AAH,ASS) du conjoint (non cumul)
      max_aah_ass_conj <- pmax(mont_ASS,mont_conj_AAH)
    }
    
  
  
  
 
  ###########################
  #### Calcul RSA socle #####
  ###########################
  
  if (year>15){
    
    # Base ressources
    br_rsa <- rev_act_net+sal_net_conj+AAH_tot+(AL>0)*pmin(AL,fl_RSA)+(proprietaire==1)*fl_RSA+montant_af-af_majo_age+
            pmin(mont_CF,bareme_var[["montant_CF"]])+mont_AB_paje+asf_br_rsa+ARE_net+are_nette_conjoint+
            autres_rev+(mont_conj_AAH!=max_aah_ass_conj)*mont_ASS

    # RSA socle
    mont_RSA <- (br_rsa+bareme_var[["seuil_versement_rsa"]]<= mf_RSA)*(mf_RSA-br_rsa)
 
    # Prime de Noel
    prime_noel <- (mont_RSA>0)*(
      ((nb_adultes+nb_enfants)==1)*bareme_var[["noel_1pers"]]+
        ((nb_adultes+nb_enfants)==2)*bareme_var[["noel_2pers"]]+
        ((nb_adultes+nb_enfants)==3)*bareme_var[["noel_3pers"]]+
        (nb_adultes==1)*((nb_adultes+nb_enfants)==4)*bareme_var[["noel_isole_4pers"]]+
        (nb_adultes==2)*((nb_adultes+nb_enfants)==4)*bareme_var[["noel_couple_4pers"]]+
        (nb_adultes==1)*((nb_adultes+nb_enfants)>4)*(bareme_var[["noel_isole_4pers"]]+bareme_var[["noel_pers_sup"]]*(nb_adultes+nb_enfants-4))+
        (nb_adultes==2)*((nb_adultes+nb_enfants)>4)*(bareme_var[["noel_couple_4pers"]]+bareme_var[["noel_pers_sup"]]*(nb_adultes+nb_enfants-4))
    )
    
    # Max (primes de Noel RSA, ASS)
    max_noel_ass_conj <- pmax(prime_noel,ASS_conj)
    
    } else {
    # Base ressources 
    br_rsa <- pmin(mont_CF,bareme_var[["montant_CF"]])+montant_af-af_majo_age+rev_act_net+pmin(AL,fl_RSA)*(AL>0)+(proprietaire==1)*fl_RSA+
            autres_rev+sal_net_conj+asf_br_rsa+ARE_net+are_nette_conjoint+AAH_tot+(year==15)*mont_AB_paje+sal_ref_net
    
    # RSA total
    RSA_tot <- SI((recours_PA==1 | br_rsa<mf_RSA),
              SI(mf_RSA-br_rsa+(1-bareme_var[["pente_rsa"]])*(rev_act_net+sal_net_conj)>=bareme_var[["seuil_versement_rsa"]],
                 mf_RSA-br_rsa+(1-bareme_var[["pente_rsa"]])*(rev_act_net+sal_net_conj),
                 0),
              SI(mf_RSA-br_rsa>=bareme_var[["seuil_versement_rsa"]],
                 mf_RSA-br_rsa,
                 0)
              )

  # RSA activite
  RSA_act <- SI(br_rsa<mf_RSA,pmax(0,br_rsa+RSA_tot-mf_RSA),recours_PA*RSA_tot)
    
  # RSA socle 
  RSA_soc <- RSA_tot-RSA_act
    
  # Prime de Noel
  prime_noel_av2016 <- (RSA_soc>0)*(
  ((nb_adultes+nb_enfants)==1)*bareme_var[["noel_1pers"]]+
    ((nb_adultes+nb_enfants)==2)*bareme_var[["noel_2pers"]]+
    ((nb_adultes+nb_enfants)==3)*bareme_var[["noel_3pers"]]+
    (nb_adultes==1)*((nb_adultes+nb_enfants)==4)*bareme_var[["noel_isole_4pers"]]+
    (nb_adultes==2)*((nb_adultes+nb_enfants)==4)*bareme_var[["noel_couple_4pers"]]+
    (nb_adultes==1)*((nb_adultes+nb_enfants)>4)*(bareme_var[["noel_isole_4pers"]]+bareme_var[["noel_pers_sup"]]*(nb_adultes+nb_enfants-4))+
    (nb_adultes==2)*((nb_adultes+nb_enfants)>4)*(bareme_var[["noel_couple_4pers"]]+bareme_var[["noel_pers_sup"]]*(nb_adultes+nb_enfants-4))
) 
  
max_noel_ass_conj <- pmax(prime_noel_av2016 ,ASS_conj)

# PPE residuelle
ppe_resid <- pmax(0,ppe_tot_avRSA-RSA_act)

# RSA act + PPE residuelle
RSA_act_ppe_resid <-RSA_act + ppe_resid

  }
  
  
  ###########################
  #### Prime d'activite #####
  ###########################
  
  if (year > 15){
    
    # Base ressources
    br_pa <- rev_act_net+sal_net_conj+SI(year==16,AAH_tot+mont_ASS,AAH_tot-mont_conj_AAH+max_aah_ass_conj)+(AL>0)*pmin(AL,fl_PA) +(proprietaire==1)*fl_PA+ montant_af-af_majo_age+pmin(mont_CF,bareme_var[["montant_CF"]])+mont_AB_paje+asf_br_rsa+ARE_net+are_nette_conjoint+autres_rev
    
    # Bonus d'activite "théorique"
    bonus_pa <- bareme_var[["PA_bonus"]]*(
      SI((rev_act_net+(rev_act_net>=bareme_var[["Sal_min_AAH_PA"]])*mont_AAH)>= bareme_var[["PA_tranche1smic"]]*bareme_var[["smic_n"]],
                                        (pmin(rev_act_net+(rev_act_net>=bareme_var[["Sal_min_AAH_PA"]])*mont_AAH,bareme_var[["PA_tranche2smic"]]*bareme_var[["smic_n"]])-bareme_var[["PA_tranche1smic"]]*bareme_var[["smic_n"]])/
                                        ((bareme_var[["PA_tranche2smic"]]-bareme_var[["PA_tranche1smic"]])*bareme_var[["smic_n"]]),
         0
    )+
      SI(sal_net_conj+(sal_net_conj>=bareme_var[["Sal_min_AAH_PA"]])*(mont_conj_AAH>mont_ASS)*mont_conj_AAH >= bareme_var[["PA_tranche1smic"]]*bareme_var[["smic_n"]],
         (pmin(sal_net_conj+(sal_net_conj>=bareme_var[["Sal_min_AAH_PA"]])*(mont_conj_AAH>mont_ASS)*mont_conj_AAH,bareme_var[["PA_tranche2smic"]]*bareme_var[["smic_n"]])-bareme_var[["PA_tranche1smic"]]*bareme_var[["smic_n"]])/
           ((bareme_var[["PA_tranche2smic"]]-bareme_var[["PA_tranche1smic"]])*bareme_var[["smic_n"]]),
         0
      )
    )

    # Prime d'activite
mont_PA <- (recours_PA==1 | mont_RSA>0)*SI((vecteur + sal_net_conj > 0)*((mf_PA+(1-bareme_var[["PA_pente"]])*(rev_act_net+sal_net_conj+(rev_act_net>=bareme_var[["Sal_min_AAH_PA"]])*mont_AAH+(sal_net_conj>=bareme_var[["Sal_min_AAH_PA"]])*(mont_conj_AAH>mont_ASS)*mont_conj_AAH)+bonus_pa-pmax(br_pa,mf_PA))*(1-bareme_var[["tx_crds"]])>=bareme_var[["seuil_versement_PA"]]),
                                      (mf_PA+(1-bareme_var[["PA_pente"]])*(rev_act_net+sal_net_conj+(rev_act_net>=bareme_var[["Sal_min_AAH_PA"]])*mont_AAH+(sal_net_conj>=bareme_var[["Sal_min_AAH_PA"]])*(mont_conj_AAH>mont_ASS)*mont_conj_AAH)+bonus_pa-pmax(br_pa,mf_PA))*(1-bareme_var[["tx_crds"]]),
                                      0)

    
# Bonus servi
    bonus_servi <- pmin(bonus_pa,mont_PA)
 
  } else {
    br_pa <- vector("numeric",nn)
    bonus_pa <- vector("numeric",nn)
    bonus_servi  <- vector("numeric",nn)
    mont_PA <- vector("numeric",nn)
  }
  
  ############################################
  #### Allocation rentree scolaire (ARS) #####
  ############################################
  if (year>14){
    
    ars <- SI(revimp_n_2_abatt < bareme_var[["plaf_ars"]]+bareme_var[["plaf_ars_enf"]]*nb_enfants,
              (bareme_var[["ars6_10"]]*nb_enft_610 + bareme_var[["ars_11_14"]]*(nb_enft_1113+nb_enft_14)+bareme_var[["ars_15_18"]]*nb_enft_1519)/12,
              SI(revimp_n_2_abatt<bareme_var[["plaf_ars"]]+bareme_var[["plaf_ars_enf"]]*nb_enfants+(bareme_var[["ars6_10"]]*nb_enft_610 + bareme_var[["ars_11_14"]]*(nb_enft_1113+nb_enft_14)
                                                                                       +bareme_var[["ars_15_18"]]*nb_enft_1519)/12,
                 (bareme_var[["plaf_ars"]]+bareme_var[["plaf_ars_enf"]]*nb_enfants+(bareme_var[["ars6_10"]]*nb_enft_610 + bareme_var[["ars_11_14"]]*(nb_enft_1113+nb_enft_14)+
                                                                                      bareme_var[["ars_15_18"]]*nb_enft_1519)/12)-revimp_n_2_abatt,
                 0
              )
    )
    
  } else {
    ars <- SI((revimp_n_2_abatt+rev_act_dec+sal_declar_conj+autres_rev)*0.9/(1+bareme_var[["deflat_2"]]) < bareme_var[["plaf_ars"]]+bareme_var[["plaf_ars_enf"]]*nb_enfants,
              (bareme_var[["ars6_10"]]*nb_enft_610 + bareme_var[["ars_11_14"]]*(nb_enft_1113+nb_enft_14)+bareme_var[["ars_15_18"]]*nb_enft_1519)/12,
              0)
  }
 
  ##############################
  #### Impot sur le revenu #####
  ##############################
    

    
    IR <<- function(rfr,nb_part,nb_adultes){
      
    rfr_par_part <- rfr/nb_part
    
    # Impot par part fiscale
    # suppression d'une tranche d'IR en 2014
    if (year>14){
      imp_par_part <- (rfr_par_part>0)*bareme_var[["taux_ir_t1"]]*pmin(rfr_par_part,bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t1"]]<rfr_par_part)*bareme_var[["taux_ir_t2"]]*pmin(rfr_par_part-bareme_var[["plafond_ir_t1"]],bareme_var[["plafond_ir_t2"]]-bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t2"]]<rfr_par_part)*bareme_var[["taux_ir_t3"]]*pmin(rfr_par_part-bareme_var[["plafond_ir_t2"]], bareme_var[["plafond_ir_t3"]]-bareme_var[["plafond_ir_t2"]])+
        (bareme_var[["plafond_ir_t3"]]<rfr_par_part)*bareme_var[["taux_ir_t4"]]*pmin(rfr_par_part-bareme_var[["plafond_ir_t3"]], bareme_var[["plafond_ir_t4"]]-bareme_var[["plafond_ir_t3"]])+
        (bareme_var[["plafond_ir_t4"]]<rfr_par_part)*bareme_var[["taux_ir_t5"]]*(rfr_par_part-bareme_var[["plafond_ir_t4"]])
      
    } else {
      imp_par_part <- (rfr_par_part>0)*bareme_var[["taux_ir_t1"]]*pmin(rfr_par_part,bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t1"]]<rfr_par_part)*bareme_var[["taux_ir_t2"]]*pmin(rfr_par_part-bareme_var[["plafond_ir_t1"]], bareme_var[["plafond_ir_t2"]]-bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t2"]]<rfr_par_part)*bareme_var[["taux_ir_t3"]]*pmin(rfr_par_part-bareme_var[["plafond_ir_t2"]], bareme_var[["plafond_ir_t3"]]-bareme_var[["plafond_ir_t2"]])+
        (bareme_var[["plafond_ir_t3"]]<rfr_par_part)*bareme_var[["taux_ir_t4"]]*pmin(rfr_par_part-bareme_var[["plafond_ir_t3"]], bareme_var[["plafond_ir_t4"]]-bareme_var[["plafond_ir_t3"]])+
        (bareme_var[["plafond_ir_t4"]]<rfr_par_part)*bareme_var[["taux_ir_t5"]]*pmin(rfr_par_part-bareme_var[["plafond_ir_t4"]], bareme_var[["plafond_ir_t5"]]-bareme_var[["plafond_ir_t4"]])+
        (bareme_var[["plafond_ir_t5"]]<rfr_par_part)*bareme_var[["taux_ir_t6"]]*(rfr_par_part-bareme_var[["plafond_ir_t5"]])
    }
    
    # Impot total 
    imp_tot <- imp_par_part*nb_part
    
    #Revenu imposable par part fiscale (sans demi-part suppl)
    rev_imp_part <- rfr/nb_adultes
    
    #Impot par part fiscale (sans demi-part suppl)
    if (year>14){
      
      imp_part_sansdemi<- (rev_imp_part>0)*bareme_var[["taux_ir_t1"]]*pmin(rev_imp_part,bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t1"]]<rev_imp_part)*bareme_var[["taux_ir_t2"]]*pmin(rev_imp_part-bareme_var[["plafond_ir_t1"]],bareme_var[["plafond_ir_t2"]]-bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t2"]]<rev_imp_part)*bareme_var[["taux_ir_t3"]]*pmin(rev_imp_part-bareme_var[["plafond_ir_t2"]], bareme_var[["plafond_ir_t3"]]-bareme_var[["plafond_ir_t2"]])+
        (bareme_var[["plafond_ir_t3"]]<rev_imp_part)*bareme_var[["taux_ir_t4"]]*pmin(rev_imp_part-bareme_var[["plafond_ir_t3"]], bareme_var[["plafond_ir_t4"]]-bareme_var[["plafond_ir_t3"]])+
        (bareme_var[["plafond_ir_t4"]]<rev_imp_part)*bareme_var[["taux_ir_t5"]]*(rev_imp_part-bareme_var[["plafond_ir_t4"]])
      
    } else {
      imp_part_sansdemi<- (rev_imp_part>0)*bareme_var[["taux_ir_t1"]]*pmin(rev_imp_part,bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t1"]]<rev_imp_part)*bareme_var[["taux_ir_t2"]]*pmin(rev_imp_part-bareme_var[["plafond_ir_t1"]],bareme_var[["plafond_ir_t2"]]-bareme_var[["plafond_ir_t1"]])+
        (bareme_var[["plafond_ir_t2"]]<rev_imp_part)*bareme_var[["taux_ir_t3"]]*pmin(rev_imp_part-bareme_var[["plafond_ir_t2"]], bareme_var[["plafond_ir_t3"]]-bareme_var[["plafond_ir_t2"]])+
        (bareme_var[["plafond_ir_t3"]]<rev_imp_part)*bareme_var[["taux_ir_t4"]]*pmin(rev_imp_part-bareme_var[["plafond_ir_t3"]], bareme_var[["plafond_ir_t4"]]-bareme_var[["plafond_ir_t3"]])+
        (bareme_var[["plafond_ir_t4"]]<rev_imp_part)*bareme_var[["taux_ir_t5"]]*pmin(rev_imp_part-bareme_var[["plafond_ir_t4"]], bareme_var[["plafond_ir_t5"]]-bareme_var[["plafond_ir_t4"]])+
        (bareme_var[["plafond_ir_t5"]]<rev_imp_part)*bareme_var[["taux_ir_t6"]]*(rev_imp_part-bareme_var[["plafond_ir_t5"]])
    }
    
    
    #Impot total (sans demi-part suppl)
    
    imp_tot_sansdemi <-imp_part_sansdemi*nb_adultes
    
    # Avantage lie au QF
    
    avantage_qf <- imp_tot_sansdemi - imp_tot
    
    # Impot apres plafonnement du QF
    
    imp_plaf_qf <- SI(avantage_qf<max_qf,imp_tot,imp_tot_sansdemi-max_qf)
    
    # Decote
    if (year>15){
      
      decote <- pmax(0,SI(nb_adultes==1,pmax(bareme_var[["Decote_taux"]]*bareme_var[["mont_decote_celib"]]-bareme_var[["Decote_taux"]]*imp_plaf_qf),pmax(bareme_var[["Decote_taux"]]*bareme_var[["mont_decote_couple"]]-bareme_var[["Decote_taux"]]*imp_plaf_qf)))
      
    } else { 
      if (year==15){
        
        decote <- SI(nb_adultes==1,
                     pmax(bareme_var[["mont_decote_celib"]]-imp_plaf_qf,0),
                     pmax(bareme_var[["mont_decote_couple"]]-imp_plaf_qf,0))
        
      } else {
        decote <- SI(imp_plaf_qf<bareme_var[["plafond_decote"]],
                     bareme_var[["mont_decote"]]-0.5*imp_plaf_qf,
                     0)
      }
      
      
    }
    
    # Reduction d'impot (mise en place en 2017 puis supprime en 2020)
    if (year>16 & year<20){
      
      RI_2017 <- pmax(0,
                      SI(nb_adultes>1,
                         SI(rfr<=prem_plaf_RI,
                            bareme_var[["tx_RI"]]*(imp_plaf_qf-decote),
                            0)+ SI((rfr>prem_plaf_RI)*(rfr<=deux_plaf_RI),
                                   (bareme_var[["tx_RI"]]*(deux_plaf_RI-rfr)*12/4000)*(imp_plaf_qf-decote),
                                   0),
                         0)
      ) + pmax(0,
               SI(nb_adultes==1,
                  SI(rfr<=prem_plaf_RI,
                     bareme_var[["tx_RI"]]*(imp_plaf_qf-decote),
                     0) + SI((rfr>prem_plaf_RI)*(rfr<=deux_plaf_RI),
                             (bareme_var[["tx_RI"]]*(deux_plaf_RI-rfr)*12/2000)*(imp_plaf_qf-decote),
                             0),
                  0
               ))
      
    } else {
      
      RI_2017 <- vector("numeric",length=nn)
    }
    
    # Impot apres decote et reduction d'impot
    
    imp_decote_RI <- pmax(0,imp_plaf_qf-decote-RI_2017)
    
    # Impot apres seuil de recouvrement
    if (year>13){
      imp_recouvr <- SI(imp_decote_RI>bareme_var[["seuil_recouvrement_IR"]],imp_decote_RI,0)
    } else {
      imp_recouvr <- imp_decote_RI
    }
    
    #Impot apres seuil de recouvrement et PPE (seulement avant 2015, et pas meme formule en 2013)
    if (year<16){
      if (year>13){
        imp_ppe_recouvr <- SI((imp_recouvr-ppe_resid>=bareme_var[["seuil_recouvrement_IR_apres_credits"]]) | (imp_recouvr-ppe_resid<0),
                              imp_recouvr-ppe_resid,
                              0)
      } else {
        imp_ppe_recouvr <- imp_recouvr-ppe_resid
      } 
      
    } else {
      imp_ppe_recouvr <- rep(0,nn)
    }
    
   return(c(rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,RI_2017,imp_decote_RI,imp_recouvr,imp_ppe_recouvr))
    }
    
 
    # Revenu imposable (RFR)
    # rfr sur revenus contemporains à partir de 2019 (mise en place du PAS)
    
    if (year>18){
      rfr1  <- (rev_act_dec+autres_rev+sal_ref_net)*bareme_var[["abatt_rfr"]]
      rfr2  <- (sal_declar_conj+sal_ref_conj_are+(mont_conj_AAH==max_aah_ass_conj)*0 + (mont_conj_AAH!=max_aah_ass_conj)*mont_ASS)*bareme_var[["abatt_rfr"]]
      rfr  <- rfr1+rfr2
      
    } else {
      rfr1  <- ((rev_act_dec+autres_rev+sal_ref_net)/(1+bareme_var[["deflat"]]))*bareme_var[["abatt_rfr"]]
      rfr2  <- ((sal_declar_conj+sal_ref_conj_are+(mont_conj_AAH==max_aah_ass_conj)*0 + (mont_conj_AAH!=max_aah_ass_conj)*mont_ASS)/(1+bareme_var[["deflat"]]))*bareme_var[["abatt_rfr"]] 
      rfr <- rfr1+rfr2
    }
    
    nb_part1<-(rfr1>=rfr2)*(1+(nb_enfants*0.5)+(nb_enfants>2)*(nb_enfants-2)*0.5 + (nb_enfants>0)*0.5)+(rfr1<rfr2)*1
    nb_part2<-(rfr1<rfr2)*(1+(nb_enfants*0.5)+(nb_enfants>2)*(nb_enfants-2)*0.5 + (nb_enfants>0)*0.5)+(rfr1>=rfr2)*1
    
    sorties_IR <<-IR(rfr,nb_part,nb_adultes)
    sorties_IR1 <<-IR(rfr1,nb_part1,1)
    sorties_IR2 <<-IR(rfr2,nb_part2,1)
    
    if (n0==3){
  
      rfr<-sorties_IR1[1:nn]+sorties_IR2[1:nn]
      rfr_par_part<-sorties_IR1[(nn+1):(2*nn)]+sorties_IR2[(nn+1):(2*nn)]
      imp_par_part<-sorties_IR1[(2*nn+1):(3*nn)]+sorties_IR2[(2*nn+1):(3*nn)]
      imp_tot<-sorties_IR1[(3*nn+1):(4*nn)]+sorties_IR2[(3*nn+1):(4*nn)]
      rev_imp_part<-sorties_IR1[(4*nn+1):(5*nn)]+sorties_IR2[(4*nn+1):(5*nn)]
      imp_part_sansdemi<-sorties_IR1[(5*nn+1):(6*nn)]+sorties_IR2[(5*nn+1):(6*nn)]
      imp_tot_sansdemi<-sorties_IR1[(6*nn+1):(7*nn)]+sorties_IR2[(6*nn+1):(7*nn)]
      avantage_qf<-sorties_IR1[(7*nn+1):(8*nn)]+sorties_IR2[(7*nn+1):(8*nn)]
      imp_plaf_qf<-sorties_IR1[(8*nn+1):(9*nn)]+sorties_IR2[(8*nn+1):(9*nn)]
      decote<-sorties_IR1[(9*nn+1):(10*nn)]+sorties_IR2[(9*nn+1):(10*nn)]
      RI_2017<-sorties_IR1[(10*nn+1):(11*nn)]+sorties_IR2[(10*nn+1):(11*nn)]
      imp_decote_RI<-sorties_IR1[(11*nn+1):(12*nn)]+sorties_IR2[(11*nn+1):(12*nn)]
      imp_recouvr<-sorties_IR1[(12*nn+1):(13*nn)]+sorties_IR2[(12*nn+1):(13*nn)]
      imp_ppe_recouvr<-sorties_IR1[(13*nn+1):(14*nn)]+sorties_IR2[(13*nn+1):(14*nn)]
      
      av_QC<-sorties_IR1[(12*nn+1):(13*nn)]+sorties_IR2[(12*nn+1):(13*nn)]-sorties_IR[(12*nn+1):(13*nn)]
      
    }else {
  
      rfr<-sorties_IR[1:nn]
      rfr_par_part<-sorties_IR[(nn+1):(2*nn)]
      imp_par_part<-sorties_IR[(2*nn+1):(3*nn)]
      imp_tot<-sorties_IR[(3*nn+1):(4*nn)]
      rev_imp_part<-sorties_IR[(4*nn+1):(5*nn)]
      imp_part_sansdemi<-sorties_IR[(5*nn+1):(6*nn)]
      imp_tot_sansdemi<-sorties_IR[(6*nn+1):(7*nn)]
      avantage_qf<-sorties_IR[(7*nn+1):(8*nn)]
      imp_plaf_qf<-sorties_IR[(8*nn+1):(9*nn)]
      decote<-sorties_IR[(9*nn+1):(10*nn)]
      RI_2017<-sorties_IR[(10*nn+1):(11*nn)]
      imp_decote_RI<-sorties_IR[(11*nn+1):(12*nn)]
      imp_recouvr<-sorties_IR[(12*nn+1):(13*nn)]
      imp_ppe_recouvr<-sorties_IR[(13*nn+1):(14*nn)]
      
      av_QC<-0
      
    }
  
  
  
  
  ############################
  #### Taxe d'habitation #####
  ############################
  
  # Eligible au plafonnement
    
if (year>19) {elig_plaf_th <- 0}
   else {elig_plaf_th <- as.numeric((revimp_n_2_abatt<plaf_elig_th))}
  
  if (year>14){
    # Plafond du montant de TH
    if (year>19) {
      plaf_mont_th <-0
      mont_TH_predegr <- montant_TH
    } else {
      plaf_mont_th <-pmax(0,revimp_n_2_abatt-abat_rfr)*bareme_var[["tx_degrevement_th"]]
      mont_TH_predegr<-(elig_plaf_th*pmin(plaf_mont_th,montant_TH) + (1-elig_plaf_th)*montant_TH)*(1-((imp_recouvr==0)*(handicap_personne>0 | handicap_conjoint>0)))
      }
    if (year>17 & year<23){
      # Montant de TH
      mont_TH <-mont_TH_predegr*
        ( (rev_impo_n_1<seuil_rfr_degr_tot)*(1-bareme_var[["degr_th_max"]])+
           SI((rev_impo_n_1>=seuil_rfr_degr_tot) & (rev_impo_n_1<seuil_rfr_degr_deg),(1-bareme_var[["degr_th_max"]]*(seuil_rfr_degr_deg-rev_impo_n_1)/(seuil_rfr_degr_deg-seuil_rfr_degr_tot)),0)+
           (rev_impo_n_1>=pmax(seuil_rfr_degr_tot,seuil_rfr_degr_deg))*1
          )
      if (year>20){
        #Montant TH apres degrev
        mont_TH <- mont_TH_predegr*
         ( (rev_impo_n_1<seuil_rfr_degr_tot)*0+
            SI ((rev_impo_n_1>=seuil_rfr_degr_tot) & (rev_impo_n_1<seuil_rfr_degr_deg),(1-(seuil_rfr_degr_deg-rev_impo_n_1)/(seuil_rfr_degr_deg-seuil_rfr_degr_tot))*(1-bareme_var[["degr_th_partiel"]]),0)+
             (rev_impo_n_1>=pmax(seuil_rfr_degr_tot,seuil_rfr_degr_deg))*(1-bareme_var[["degr_th_partiel"]])
          )
      }
   } else {
      # Montant de TH
      mont_TH <- mont_TH_predegr
   } 
    
  } else {
    plaf_mont_th <- pmax(0,(rev_act_dec+sal_declar_conj+autres_rev)/(1+bareme_var[["deflat_2"]])-abat_rfr)*bareme_var[["tx_degrevement_th"]]
    mont_TH <- pmin(plaf_mont_th,montant_TH)
  }
  
  ################################
  #### Allocations familiales ####
  ################################

  # Montant des AF (module selon les revenus à partir de 2015)
  if (year>14){
  mont_AF <- SI(revimp_n_2_abatt<=plaf_1tranch_AF,
            montant_af,
            SI(revimp_n_2_abatt<=plaf_1tranch_AF+montant_af/2,
               montant_af+plaf_1tranch_AF-revimp_n_2_abatt,
               SI(revimp_n_2_abatt<=plaf_2tranch_AF,
                  montant_af/2,
                  SI(revimp_n_2_abatt<=plaf_2tranch_AF+montant_af/4,
                     plaf_2tranch_AF-revimp_n_2_abatt,
                     montant_af/4))) 
  )
    } else {
     mont_AF <- rep(montant_af,times=nn)
    }

  
  ########################
  #### Revenus totaux ####
  ########################
  
  # Revenu primaire total du ménage
  rev_primaire <- rev_act_net+sal_net_conj+ARE_net+are_nette_conjoint+autres_rev
  
  # Revenus du travail nets 
  rev_trav_net <- rev_act_net+sal_net_conj
  
  # Revenus hors travail
  if (year<16){
    rev_hors_trav <- mont_AF+ppe_resid+RSA_tot+ars+max_noel_ass_conj+AAH_tot+mont_ASS+mont_AB_paje+mont_CF+AL+ARE_net+are_nette_conjoint+autres_rev+montant_asf
  } else {
    if (year==16){
      rev_hors_trav <- mont_AF+mont_RSA+mont_PA+ars+max_noel_ass_conj+AAH_tot+mont_ASS+mont_AB_paje+mont_CF+AL+ARE_net+are_nette_conjoint+autres_rev+montant_asf
    } else {
      rev_hors_trav <- mont_AF+mont_RSA+mont_PA+ars+max_noel_ass_conj+max_aah_ass_conj+AAH_tot-mont_conj_AAH+mont_AB_paje+mont_CF+AL+ARE_net+are_nette_conjoint+autres_rev+montant_asf 
    }
  }
  
  # Somme des prestations incluses dans le revenu disponible
  if (year<16){
    presta <- mont_AF+ppe_resid+RSA_tot+ars+max_noel_ass_conj+AAH_tot+mont_ASS+mont_AB_paje+mont_CF+AL+montant_asf
  } else {
    if (year==16){
      presta <- mont_AF+mont_RSA+mont_PA+ars+max_noel_ass_conj+AAH_tot+mont_ASS+mont_AB_paje+mont_CF+AL+montant_asf
    } else {
      presta <- mont_AF+mont_RSA+mont_PA+ars+max_noel_ass_conj+max_aah_ass_conj+AAH_tot-mont_conj_AAH+mont_AB_paje+mont_CF+AL+montant_asf 
    }
    }
  
  # Somme des prélèvements (en fait impôts) inclus dans le revenu disponible
  prelev <- imp_recouvr+mont_TH
  
  # Revenu disponible (y.c impots, TH, AF et AL)
  rev_disp <- rev_hors_trav+rev_trav_net-imp_recouvr-mont_TH
  
  # Niveau de vie
  nv_vie <- rev_disp/(1+(nb_adultes-1+nb_enft_14+nb_enft_1519+nb_enft_20)*0.5 + (nb_enft_3+nb_enft_35+nb_enft_610+nb_enft_1113)*0.3)
  
  # Taux d'imposition marginal implicite (sur le net)
  # Je m'écarte de la formule des maquettes excel car j'envisage le cas ARE
  TMI_net <- vector("numeric",nn+1)
  for (i in 1:(length(vecteur))){
    TMI_net[i] <- (1-((rev_disp[i+1]-rev_disp[i])/(rev_act_net[i+1]+ARE_net[i+1]-rev_act_net[i]-ARE_net[i])))
  }
  TMI_net <- TMI_net [1:nn]
  
  # AJOUT : Taux d'effet marginal=1-Taux d'imposition marginal implicite (sur le net)
  EM_net <- 1-TMI_net
  
  # Taux d'imposition marginal implicite (sur le superbrut)
  TMI_superbrut <- vector("numeric",nn+1)
  for (i in 1:(length(vecteur))){
    TMI_superbrut[i] <- 1-((rev_disp[i+1]-rev_disp[i])/(cout_travail[i+1]-cout_travail[i]))
  }
  TMI_superbrut <- TMI_superbrut [1:nn]

  
  #################################
  #### Decile de niveau de vie ####
  #################################
  if (year>13){
    decile <- 1+SI(bareme_var[["seuil_D1"]]<nv_vie*12,1,0)+SI(bareme_var[["seuil_D2"]]<nv_vie*12,1,0)+SI(bareme_var[["seuil_D3"]]<nv_vie*12,1,0)+SI(bareme_var[["seuil_D4"]]<nv_vie*12,1,0)+
    SI(bareme_var[["seuil_D5"]]<nv_vie*12,1,0)+SI(bareme_var[["seuil_D6"]]<nv_vie*12,1,0)+SI(bareme_var[["seuil_D7"]]<nv_vie*12,1,0)+SI(bareme_var[["seuil_D8"]]<nv_vie*12,1,0)+
    SI(bareme_var[["seuil_D9"]]<nv_vie*12,1,0)
  } else {
    decile <- vector("numeric",nn)
  }
  
  #############################################
  #### CMUC-ACS-droits sociaux electricite ####
  #############################################
  
  # CMUC
  if (year>15){
    
    CMUc <- as.numeric((recours_CMU_ACS*SI(mont_RSA==0,
                              (br_rsa<plaf_cmuc)*(nb_adultes*bareme_var[["montant_cmuc_16_49"]]+(nb_enfants-nb_enft_1519-nb_enft_20)*bareme_var[["montant_cmuc_0_15"]]+
                                           (nb_enft_1519+nb_enft_20)*bareme_var[["montant_cmuc_16_49"]]),
                              (nb_adultes*bareme_var[["montant_cmuc_16_49"]]+(nb_enfants-nb_enft_1519-nb_enft_20)*bareme_var[["montant_cmuc_0_15"]]+
                                 (nb_enft_1519+nb_enft_20)*bareme_var[["montant_cmuc_16_49"]])
                              ))>0)
    if (year>19){
      
      CMUc <- as.numeric((recours_CMU_ACS*SI(mont_RSA==0,
                                             (br_rsa<plaf_cmuc),1))>0)
    }
  } else {
    CMUc <- as.numeric((recours_CMU_ACS*SI(RSA_soc==0,
                              (br_rsa<plaf_cmuc)*(nb_adultes*bareme_var[["montant_cmuc_16_49"]]+(nb_enfants-nb_enft_1519-nb_enft_20)*bareme_var[["montant_cmuc_0_15"]]+
                                           (nb_enft_1519+nb_enft_20)*bareme_var[["montant_cmuc_16_49"]]),
                              (nb_adultes*bareme_var[["montant_cmuc_16_49"]]+(nb_enfants-nb_enft_1519-nb_enft_20)*bareme_var[["montant_cmuc_0_15"]]+
                                 (nb_enft_1519+nb_enft_20)*bareme_var[["montant_cmuc_16_49"]])
                              ))>0)
}
  
  # ACS
  #Bien que l'on change le nom (et les tranches d'âge) des paramètres importés depuis excel, on garde l'appellation ACS pour la CSS payante
  #Pour le moment, je laisse "recours_CMU_ACS" tel quel (provient de base.R)

  if (year>19){
  
    ACS <- as.numeric((recours_CMU_ACS*(plaf_cmuc<br_rsa)*(br_rsa<plaf_acs))>0)
    
  } else {

    ACS <- as.numeric((recours_CMU_ACS*(plaf_cmuc<br_rsa)*(br_rsa<plaf_acs)*(nb_adultes*bareme_var[["montant_acs_16_49"]]
                                                                             +(nb_enfants-nb_enft_1519-nb_enft_20)*bareme_var[["montant_acs_0_15"]]+
                                                                               (nb_enft_1519+nb_enft_20)*bareme_var[["montant_acs_16_49"]]))>0)
    
  }
  
  
  
  # Electricite (avant 2018) - chèque énergie
  if (year<18){
    energie <- recours_CMU_ACS*(CMUc>0 | ACS>0)*(
      ((nb_adultes+nb_enfants)==1)*bareme_var[["montant_elec_1p"]]+
        ((nb_adultes+nb_enfants)==2 | (nb_adultes+nb_enfants)==3)*bareme_var[["montant_elec_23p"]]+
        ((nb_adultes+nb_enfants)>3)*bareme_var[["montant_elec_4plus"]]
    )
  } else { if (year==18){
    energie <- ((rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_1tranch"]])*
      ((uc_energie==1)*(bareme_var[["ener_1tranch_1UC"]])+
      SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_1tranch_2UC"]],0)+
      (uc_energie>=2)*(bareme_var[["ener_1tranch_3UC"]]))+
      SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_1tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_2tranch"]]),(uc_energie==1)*bareme_var[["ener_2tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_2tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_2tranch_3UC"]],0)+
      SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_2tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_3tranch"]]),(uc_energie==1)*bareme_var[["ener_3tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_3tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_3tranch_3UC"]],0))/12
  }else { if (year==22){
    energie <- ((rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_1tranch"]])*((uc_energie==1)*(bareme_var[["ener_1tranch_1UC"]])+
                                                                                  SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_1tranch_2UC"]],0)+
                                                                                  (uc_energie>=2)*(bareme_var[["ener_1tranch_3UC"]]))+
                  SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_1tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_2tranch"]]),(uc_energie==1)*bareme_var[["ener_2tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_2tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_2tranch_3UC"]],0)+
                  SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_2tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_3tranch"]]),(uc_energie==1)*bareme_var[["ener_3tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_3tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_3tranch_3UC"]],0)+
                  SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_3tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_4tranch"]]),(uc_energie==1)*bareme_var[["ener_4tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_4tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_4tranch_3UC"]],0)+
                  SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_4tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_5tranch"]]),bareme_var[["ener_bonus2022"]],0))/12
  } else {
    energie <- ((rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_1tranch"]])*((uc_energie==1)*(bareme_var[["ener_1tranch_1UC"]])+
         SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_1tranch_2UC"]],0)+
      (uc_energie>=2)*(bareme_var[["ener_1tranch_3UC"]]))+
      SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_1tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_2tranch"]]),(uc_energie==1)*bareme_var[["ener_2tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_2tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_2tranch_3UC"]],0)+
      SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_2tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_3tranch"]]),(uc_energie==1)*bareme_var[["ener_3tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_3tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_3tranch_3UC"]],0)+
      SI((rev_impo_n_1/uc_energie>=bareme_var[["plafond_ener_3tranch"]]) & (rev_impo_n_1/uc_energie<bareme_var[["plafond_ener_4tranch"]]),(uc_energie==1)*bareme_var[["ener_4tranch_1UC"]]+SI((uc_energie>1) & (uc_energie<2),bareme_var[["ener_4tranch_2UC"]],0)+(uc_energie>=2)*bareme_var[["ener_4tranch_3UC"]],0))/12
  }
  } 
  }
  
  ##############################################################################
  #### Revenu ajuste (revenu dispo + electricite/cheque energie) ####
  ##############################################################################
  
  rev_ajust <- rev_disp+energie
  
  #################################################
  #### Allocation de soutien familiale #########
  #################################################
  
  if (year>13){
    
    ASF <- SI((eligible_asf==1)&&(nb_adultes==1),
              bareme_var[["mont_ASF_total"]]*nb_enfants,
              rep(0,nn))
  } else {
    
    ASF <- rep(0,nn)
  }

  
  ##########################################################################
  ########################## Création des totaux ###########################
  ##########################################################################
  
  
  
  #Total des prestations familiales
  total_pf <- montant_af + mont_CF + ASF + mont_AB_paje + ars

  
  #Total des minima sociaux et prime d'activité
  #Total sans chèque énergie 
  
  if (year>15){
    total_minima_soc <- mont_ASS + AAH_tot + mont_RSA + prime_noel + mont_PA
  } else {
    total_minima_soc <-  RSA_soc+ mont_ASS + AAH_tot + RSA_act_ppe_resid + prime_noel_av2016
  }
  
  #Total avec chèque énergie 
  if (year>15){
    total_minima_soc_cheque_energie <- mont_ASS + AAH_tot + mont_RSA + prime_noel + mont_PA + energie 
  } else {
    total_minima_soc_cheque_energie <- mont_ASS + AAH_tot + RSA_act_ppe_resid + prime_noel_av2016 + energie
  }
  
  #Total des prélèvements sociaux
  total_ps <- cotis_emp - fillon_exo + cotis_sal + csg_ded + csg_non_ded

  
  ##################################################################################################
  ##### Attention /!\ L'ordre dans les dataframes suivants doit être le même dans les colnames #####
  ############################################ de global.R #########################################
  ##################################################################################################
  
  if (year==23){
    df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                     cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                     total_ps,
                     cout_travail,
                     rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                     sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                     br_ASS,rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                     mont_ASS,ASS_conj,BR_AL,AL,mont_AF,plaf_CF,plaf_CF_majo,mont_CF,ASF,plaf_AB_partiel,plaf_AB_plein,
                     mont_AB_paje,br_AAH,mont_AAH,mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,max_aah_ass_conj,
                     fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,bonus_pa,bonus_servi,mont_PA,
                     total_minima_soc,total_minima_soc_cheque_energie,
                     ars,total_pf,
                     rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,
                     imp_decote_RI,imp_recouvr,av_QC,mont_TH_predegr,mont_TH,rev_trav_net,rev_hors_trav,
                     presta,prelev,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                     rev_ajust,check.names=F)
    
    colnames(df) <- colnames23
    for (i in 1:ncol(df)){
      label(df[,i]) <- labels23[i]
    }
  } else {  
    
  if (year==22){
    df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                     cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                     total_ps,
                     cout_travail,
                     rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                     sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                     br_ASS,rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                     mont_ASS,ASS_conj,BR_AL,AL,mont_AF,plaf_CF,plaf_CF_majo,mont_CF,ASF,plaf_AB_partiel,plaf_AB_plein,
                     mont_AB_paje,br_AAH,mont_AAH,mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,max_aah_ass_conj,
                     fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,bonus_pa,bonus_servi,mont_PA,
                     total_minima_soc,total_minima_soc_cheque_energie,
                     ars,total_pf,
                     rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,
                     imp_decote_RI,imp_recouvr,av_QC,mont_TH_predegr,mont_TH,rev_trav_net,rev_hors_trav,
                     presta,prelev,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                     rev_ajust,check.names=F)
    
    colnames(df) <- colnames22
    for (i in 1:ncol(df)){
      label(df[,i]) <- labels22[i]
    }
  } else {  
  if (year==21){
    df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                     cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                     total_ps,
                     cout_travail,
                     rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                     sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                     br_ASS,rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                     mont_ASS,ASS_conj,BR_AL,AL,mont_AF,plaf_CF,plaf_CF_majo,mont_CF,ASF,plaf_AB_partiel,plaf_AB_plein,
                     mont_AB_paje,br_AAH,mont_AAH,mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,max_aah_ass_conj,
                     fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,bonus_pa,bonus_servi,mont_PA,
                     total_minima_soc,total_minima_soc_cheque_energie,
                     ars,total_pf,
                     rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,
                     imp_decote_RI,imp_recouvr,av_QC,mont_TH_predegr,mont_TH,rev_trav_net,rev_hors_trav,
                     presta,prelev,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                     rev_ajust,check.names=F)
    
    colnames(df) <- colnames21
    for (i in 1:ncol(df)){
      label(df[,i]) <- labels21[i]
    }
  } else {
  if (year==20){
    df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                     cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                     total_ps,
                     cout_travail,
                     rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                     sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                     br_ASS,rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                     mont_ASS,ASS_conj,BR_AL,AL,mont_AF,plaf_CF,plaf_CF_majo,mont_CF,ASF,plaf_AB_partiel,plaf_AB_plein,
                     mont_AB_paje,br_AAH,mont_AAH,mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,max_aah_ass_conj,
                     fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,bonus_pa,bonus_servi,mont_PA,
                     total_minima_soc,total_minima_soc_cheque_energie,
                     ars,total_pf,
                     rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,
                     imp_decote_RI,imp_recouvr,av_QC,mont_TH_predegr,mont_TH,rev_trav_net,rev_hors_trav,
                     presta,prelev,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                     rev_ajust,check.names=F)
    
    colnames(df) <- colnames20
    for (i in 1:ncol(df)){
      label(df[,i]) <- labels20[i]
    }
  } else {
    if (year==19){
      df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                       cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                       total_ps,
                       cout_travail,
                       rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                       sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                       br_ASS,rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                       mont_ASS,ASS_conj,BR_AL,AL,mont_AF,plaf_CF,plaf_CF_majo,mont_CF,ASF,plaf_AB_partiel,plaf_AB_plein,
                       mont_AB_paje,br_AAH,mont_AAH,mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,max_aah_ass_conj,
                       fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,bonus_pa,bonus_servi,mont_PA,total_minima_soc,total_minima_soc_cheque_energie,
                       ars,total_pf,rfr,
                       rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,
                       RI_2017,imp_decote_RI,imp_recouvr,av_QC,elig_plaf_th,plaf_mont_th,mont_TH_predegr,mont_TH,
                       rev_trav_net,rev_hors_trav,
                       presta,prelev,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                       rev_ajust,check.names=F)
      
      colnames(df) <- colnames19
      for (i in 1:ncol(df)){
        label(df[,i]) <- labels19[i]
      }
    } else {
    if (year==18){
      df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                     cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                     total_ps,
                     cout_travail,
                     rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                     sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                     br_ASS,rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                     mont_ASS,ASS_conj,BR_AL,AL,mont_AF,plaf_CF,plaf_CF_majo,mont_CF,ASF,plaf_AB_partiel,plaf_AB_plein,
                     mont_AB_paje,br_AAH,mont_AAH,mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,max_aah_ass_conj,
                     fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,bonus_pa,bonus_servi,mont_PA,total_minima_soc,total_minima_soc_cheque_energie,
                     ars,total_pf,rfr,
                     rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,
                     RI_2017,imp_decote_RI,imp_recouvr,av_QC,elig_plaf_th,plaf_mont_th,mont_TH_predegr,mont_TH,
                     rev_trav_net,rev_hors_trav,
                     presta,prelev,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                     rev_ajust,check.names=F)
    
      colnames(df) <- colnames18
      for (i in 1:ncol(df)){
        label(df[,i]) <- labels18[i]
    }
  } else {
    if (year==17){
      df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                       cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                       total_ps,
                       cout_travail,
                     rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                     sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                     br_ASS,rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                     mont_ASS,ASS_conj,BR_AL,AL,mont_AF,plaf_CF,plaf_CF_majo,mont_CF,ASF,plaf_AB_partiel,plaf_AB_plein,
                     mont_AB_paje,br_AAH,mont_AAH,mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,max_aah_ass_conj,
                     fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,bonus_pa,bonus_servi,mont_PA,total_minima_soc,total_minima_soc_cheque_energie,
                     ars,total_pf,rfr,
                     rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,
                     RI_2017,imp_decote_RI,imp_recouvr,av_QC,elig_plaf_th,plaf_mont_th,mont_TH,
                     rev_trav_net,rev_hors_trav,presta,prelev,
                     rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                     rev_ajust,check.names=F)
  
      colnames(df) <- colnames17
      for (i in 1:ncol(df)){
        label(df[,i]) <- labels17[i]
    }
  } else {
    if (year==16){
      df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                       cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                       total_ps,
                       cout_travail,
                       rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                       sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                       rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                       mont_ASS,ASS_conj,BR_AL,
                       AL,plaf_CF,plaf_CF_majo,mont_CF,plaf_AB_partiel,plaf_AB_plein,mont_AB_paje,br_AAH,mont_AAH,
                       mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,fl_RSA,br_rsa,mont_RSA,prime_noel,max_noel_ass_conj,fl_PA,br_pa,
                       bonus_pa,bonus_servi,mont_PA,total_minima_soc,total_minima_soc_cheque_energie,
                       ars,total_pf,rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,imp_part_sansdemi,
                       imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,imp_recouvr,av_QC,elig_plaf_th,plaf_mont_th,mont_TH,mont_AF,
                       rev_trav_net,rev_hors_trav,presta,prelev,
                       ASF,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,
                       ACS,energie,rev_ajust,check.names=F)
      
        colnames(df) <- colnames16
        for (i in 1:ncol(df)){
          label(df[,i]) <- labels16[i]
      }
    } else {
        if (year==15){
          df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,ARE_net,sal_ref_net,
                           cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                           total_ps,
                           cout_travail,
                           rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                           sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                           rev_impo_n_1,rev_impo_n_2,revimp_n_2_abatt,
                           mont_ASS,ASS_conj,BR_AL,
                           AL,ppe_tps_trav,ppe_elig_rfr,ppe_declar_tps_plein,ppe_prime_indiv,ppe_conj_tps_plein, ppe_prime_conj,ppe_majo_monoact,ppe_majo_PAC,
                           ppe_tot_avRSA,plaf_CF,plaf_CF_majo,mont_CF,plaf_AB_partiel,plaf_AB_plein,mont_AB_paje,br_AAH,mont_AAH,
                           mva,br_conj_AAH,mont_conj_AAH,mva_conj,AAH_tot,fl_RSA,br_rsa,RSA_tot,RSA_act,RSA_soc,
                           prime_noel_av2016,max_noel_ass_conj,ppe_resid,RSA_act_ppe_resid,total_minima_soc,total_minima_soc_cheque_energie,
                           ars,total_pf,rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,
                           imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,imp_recouvr,imp_ppe_recouvr,av_QC,elig_plaf_th,plaf_mont_th,
                           mont_TH,mont_AF,
                           rev_trav_net,rev_hors_trav,presta,prelev,
                           ASF,rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,
                           decile,CMUc,ACS,energie,rev_ajust,check.names=F)
          
          colnames(df) <- colnames15
          for (i in 1:ncol(df)){
            label(df[,i]) <- labels15[i]
          }
        } else{ 
          if (year==14){
            df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,
                             cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                             total_ps,
                             cout_travail,
                             rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                             sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                             AL,ppe_tps_trav,
                             ppe_elig_rfr,ppe_declar_tps_plein,ppe_prime_indiv,ppe_conj_tps_plein, ppe_prime_conj,ppe_majo_monoact,ppe_majo_PAC,
                             ppe_tot_avRSA,plaf_CF,plaf_CF_majo,mont_CF,plaf_AB_partiel,plaf_AB_plein,mont_AB_paje,
                             fl_RSA,br_rsa,RSA_tot, RSA_act,RSA_soc, prime_noel_av2016, ppe_resid,
                             RSA_act_ppe_resid,total_minima_soc,total_minima_soc_cheque_energie,
                             ars,total_pf,rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,
                             imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,imp_recouvr,imp_ppe_recouvr,av_QC,
                             elig_plaf_th,plaf_mont_th,mont_TH,mont_AF,
                             rev_trav_net,rev_hors_trav,presta,prelev,ASF,
                             rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,decile,CMUc,ACS,energie,
                             rev_ajust,check.names=F)
            
            colnames(df) <- colnames14
            for (i in 1:ncol(df)){
              label(df[,i]) <- labels14[i]
            }
          } else {
            if (year==13)
              df <- data.frame(vecteur,tps_travail,percen_smic_tps_plein,
                               cotis_emp,fillon_exo,cotis_sal,csg_ded,csg_non_ded,
                               total_ps,
                               cout_travail,
                               rev_act_net,tps_trav_net,perc_smic_net_tpsplein,rev_act_dec,
                               sal_net_conj,are_nette_conjoint,autres_rev,rev_primaire,
                               AL,ppe_tps_trav,
                               ppe_elig_rfr,ppe_declar_tps_plein,ppe_prime_indiv,ppe_conj_tps_plein, ppe_prime_conj,ppe_majo_monoact,
                               ppe_majo_PAC,ppe_tot_avRSA,plaf_CF,mont_CF,plaf_AB_partiel,mont_AB_paje,
                               fl_RSA,br_rsa,RSA_tot, RSA_act,RSA_soc, prime_noel_av2016, ppe_resid,
                               RSA_act_ppe_resid,total_minima_soc,total_minima_soc_cheque_energie,
                               ars,total_pf,rfr,rfr_par_part,imp_par_part,imp_tot,rev_imp_part,
                               imp_part_sansdemi,imp_tot_sansdemi,avantage_qf,imp_plaf_qf,decote,imp_recouvr,imp_ppe_recouvr,av_QC,
                               elig_plaf_th,plaf_mont_th,mont_TH,mont_AF,
                               rev_trav_net,rev_hors_trav,presta,prelev,ASF,
                               rev_disp,nv_vie,TMI_net,EM_net,TMI_superbrut,CMUc,ACS,energie,
                               rev_ajust,check.names=F)
            
            colnames(df) <- colnames13
            for (i in 1:ncol(df)){
              label(df[,i]) <- labels13[i]}
          }
        }
    }
  }
  }
  }
  }
  } 
  }
  }# fin du else
  
  return(df)
  

  
                }# fin de la fonction castype

#==================================================================================================================
# La fonction castype1() génère le graphique représentant l'effet marginal d'une augmentation de revenu net
# 2 versions du code selon que le type de revenus sélectionné soit salaire ou ARE
#==================================================================================================================

castype1 <- function(data,leg,bareme_var,year,n2000){
##data est la table des données représentées 
##leg est la légende qui suit le graphique 
##bareme_var sont les paramètres législatifs
##year est l'année de législation sélectionnée
##n2000 est le type de revenu sélectionné
  if (n2000==0){
    
    fig <- plot_ly(data, x = ~rev_act_net, y = ~(1-TMI_net))
    p1 <-  fig %>% 
      add_trace(y = ~(1-TMI_net), type = 'scatter', name="Effet marginal", mode = 'lines+markers', marker = list(size = 5),text=paste('Salaire net: ', round(data$rev_act_net,0),'euros \n Effet marginal: ', round(100*(1-data$TMI_net),0),'%'),hoverinfo = 'text')  %>%
      add_trace(x =c(bareme_var[["smic_n"]]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='black',dash="dash"),name = 'SMIC',text=paste('SMIC: ',round(bareme_var[["smic_n"]]),'euros'),hoverinfo = 'text') %>%
      add_trace(x =c(data[floor((dim(data)[1]-1)/2),"rev_act_net"]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='grey',dash="dash"),name = 'Note de lecture',text='Note de lecture',hoverinfo = 'text') %>%
      layout(title = "Effet marginal sur le revenu disponible \n d'une augmentation de salaire net",
             xaxis = list(title = "Salaire net (en euros)"),
             yaxis = list(tickformat = ".0%",title = "Effet marginal sur le revenu disponible"),
             margin=list(l=20, r=20, t=40, b=140),
             legend=list(orientation = "v", x = 0.7, y = 0.8),
             annotations=list(x = 1, y = -0.2, text = leg, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='top',align="right",
                              font=list(size=10)))
  } 
  
  if (n2000==1){
    
    fig <- plot_ly(data, x = ~ARE_net, y = ~(1-TMI_net))
    p1 <-  fig %>% 
      add_trace(y = ~(1-TMI_net), type = 'scatter', name="Effet marginal", mode = 'lines+markers', marker = list(size = 5),text=paste('ARE nette: ', round(data$ARE_net,0),'euros \n Effet marginal: ', round(100*(1-data$TMI_net),0),'%'),hoverinfo = 'text')  %>%
      add_trace(x =c(bareme_var[["smic_n"]]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='black',dash="dash"),name = 'SMIC',text=paste('SMIC: ',round(bareme_var[["smic_n"]]),'euros'),hoverinfo = 'text') %>%
      add_trace(x =c(data[floor((dim(data)[1]-1)/2),"ARE_net"]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='grey',dash="dash"),name = 'Note de lecture',text='Note de lecture',hoverinfo = 'text') %>%
      layout(title = "Effet marginal sur le revenu disponible \n d'une augmentation de salaire net",
             xaxis = list(title = "ARE nette (en euros)"),
             yaxis = list(tickformat = ".0%",title = "Effet marginal sur le revenu disponible"),
             margin=list(l=20, r=20, t=40, b=140),
             legend=list(orientation = "v", x = 0.7, y = 0.8),
             annotations=list(x = 1, y = -0.2, text = leg, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='top',align="right",
                              font=list(size=10)))
    
  } 
  
  print(p1)
}


#==================================================================================================================
# La fonction castype2() génère le graphique représentant les montants de RSA/PA/PPE
# 4 versions du code selon que le type de revenus sélectionné soit salaire ou ARE et selon que l'année sélectionnée soit 2015 ou non
#==================================================================================================================

castype2 <- function(data,leg,bareme_var,year,n2000){
##data est la table des données représentées 
##leg est la légende qui suit le graphique 
##bareme_var sont les paramètres législatifs
##year est l'année de législation sélectionnée
##n2000 est le type de revenu sélectionné
  if (year>15 & n2000==0){

    fig <- plot_ly(data, x = ~rev_act_net, y = ~mont_RSA)
    p2 <-  fig %>% 
      add_trace(y = ~mont_RSA, name = 'RSA', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('Salaire net: ', round(data$rev_act_net,0),'euros \n RSA: ', round(data$mont_RSA,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(y = ~mont_PA, name = 'PA', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('Salaire net: ', round(data$rev_act_net,0),'euros \n PA: ', round(data$mont_PA,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(x =c(bareme_var[["smic_n"]]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='black',dash="dash"),name = 'SMIC',text=paste('SMIC: ',round(bareme_var[["smic_n"]]),'euros'),hoverinfo = 'text') %>%
      layout(title = "Montant du RSA et de la PA \n selon le salaire net",
             xaxis = list(title = "Salaire net (en euros)"),
             yaxis = list(title = "PA et RSA (en euros)"),
             margin=list(l=20, r=20, t=40, b=100),
             legend=list(orientation = "v", x = 0.7, y = 0.8),
             annotations=list(x = 1, y = -0.1, text = leg, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='top',align="right",
                              font=list(size=10)))
    
  } 
  if (year<=15 & n2000==0) {

    fig <- plot_ly(data, x = ~rev_act_net, y = ~RSA_act)
    p2 <-  fig %>% 
      add_trace(y = ~RSA_act, name = 'RSA', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('Salaire net: ', round(data$rev_act_net,0),'euros \n RSA: ', round(data$RSA_act,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(y = ~ppe_tot_avRSA, name = 'PPE', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('Salaire net: ', round(data$rev_act_net,0),'euros \n PPE: ', round(data$ppe_tot_avRSA,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(x =c(bareme_var[["smic_n"]]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='black',dash="dash"),name = 'SMIC',text=paste('SMIC: ',round(bareme_var[["smic_n"]]),'euros'),hoverinfo = 'text') %>%
      layout(title = "Montant du RSA activité et de la PPE \n selon le salaire net",
             xaxis = list(title = "Salaire net (en euros)"),
             yaxis = list(title = "PPE et RSA (en euros)"),
             margin=list(l=20, r=20, t=40, b=100),
             legend=list(orientation = "v", x = 0.7, y = 0.8),
             annotations=list(x = 1, y = -0.1, text = leg, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='top',align="right",
                              font=list(size=10)))
    
  }
  
  if (year>15 & n2000==1){
    
    fig <- plot_ly(data, x = ~ARE_net, y = ~mont_RSA)
    p2 <-  fig %>% 
      add_trace(y = ~mont_RSA, name = 'RSA', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('ARE nette: ', round(data$ARE_net,0),'euros \n RSA: ', round(data$mont_RSA,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(y = ~mont_PA, name = 'PA', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('ARE nette: ', round(data$ARE_net,0),'euros \n PA: ', round(data$mont_PA,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(x =c(bareme_var[["smic_n"]]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='black',dash="dash"),name = 'SMIC',text=paste('SMIC: ',round(bareme_var[["smic_n"]]),'euros'),hoverinfo = 'text') %>%
      layout(title = "Montant du RSA et de la PA \n selon l'ARE nette",
             xaxis = list(title = "ARE nette (en euros)"),
             yaxis = list(title = "PA et RSA (en euros)"),
             margin=list(l=20, r=20, t=40, b=100),
             legend=list(orientation = "v", x = 0.7, y = 0.8),
             annotations=list(x = 1, y = -0.1, text = leg, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='top',align="right",
                              font=list(size=10)))
  
  } 
  if (year<=15 & n2000==1) {
    
    fig <- plot_ly(data, x = ~ARE_net, y = ~RSA_act)
    p2 <-  fig %>% 
      add_trace(y = ~RSA_act, name = 'RSA', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('ARE nette: ', round(data$ARE_net,0),'euros \n RSA: ', round(data$RSA_act,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(y = ~ppe_tot_avRSA, name = 'PPE', type = 'scatter',mode = 'lines+markers', marker = list(size = 5),text=paste('ARE nette: ', round(data$ARE_net,0),'euros \n PPE: ', round(data$ppe_tot_avRSA,0),'euros'),hoverinfo = 'text')  %>%
      add_trace(x =c(bareme_var[["smic_n"]]),type = 'scatter', mode = 'lines',line = list(shape = 'linear',  color='black',dash="dash"),name = 'SMIC',text=paste('SMIC: ',round(bareme_var[["smic_n"]]),'euros'),hoverinfo = 'text') %>%
      layout(title = "Montant du RSA activité et de la PPE \n selon l'ARE nette",
             xaxis = list(title = "ARE nette (en euros)"),
             yaxis = list(title = "PPE et RSA (en euros)"),
             margin=list(l=20, r=20, t=40, b=100),
             legend=list(orientation = "v", x = 0.7, y = 0.8),
             annotations=list(x = 1, y = -0.1, text = leg, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='top',align="right",
                              font=list(size=10)))
    
  }
  print(p2)
  }

#==================================================================================================================
# La fonction castype3() génère le graphique empilé
# 2 versions du code selon que le type de revenus sélectionné soit salaire ou ARE
#==================================================================================================================

castype3 <- function(data,leg,bareme_var,year,mv,n2000){
  ##data est la table des données représentées 
  ##leg est la légende qui suit le graphique 
  ##bareme_var sont les paramètres législatifs
  ##year est l'année de législation sélectionnée
  ##mv est la liste des prestations et revenus primaires sélectionnés pour la représentation en graphique empilé
  ##n2000 est le type de revenu sélectionné
  if (n2000==0) {
    print(paste("melt castype4",system.time({
      tmp <- data %>%
        select(
          c("rev_act_net","rev_disp","are_nette_conjoint","sal_net_conj","autres_rev"),
          all_of(mv)) %>%
        mutate(across(all_of(mv), as.numeric)) %>% mutate(rev_act_net_=rev_act_net,
                                                         sal_net_conj_=sal_net_conj,
                                                         are_nette_conjoint_=are_nette_conjoint,
                                                         autres_rev_=autres_rev)
      data2<- tidyr::pivot_longer(
        tmp,
        cols = all_of(mv),
        names_to = "variable") %>% rename(rev_act_net=rev_act_net_,
                                          sal_net_conj=sal_net_conj_,
                                          are_nette_conjoint=are_nette_conjoint_,
                                          autres_rev=autres_rev_) %>% mutate(variable=factor(variable,levels=mv))
      }
     )))
    
    # realisation du graphique en aires empilees
    getPalette = colorRampPalette(brewer.pal(9, "Spectral"))
    
    max_revenu_net=max(data2$rev_act_net,na.rm=T)
    max_revtot_net=max(data2$rev_act_net+data2$are_nette_conjoint+data2$sal_net_conj+data2$autres_rev,na.rm=T)
    min_revtot_net=min(data2$rev_act_net+data2$are_nette_conjoint+data2$sal_net_conj+data2$autres_rev,na.rm=T)
    
    p3 <- ggplot(data2, aes(x = rev_act_net,  y=value))+
      geom_area(position='stack', aes(fill=variable))+
      scale_fill_manual(values=getPalette(15)
                        ,labels=labels(libelles[libelles %in% mv])
      )+
      labs(title="Composition du revenu disponible \n en fonction du salaire net",x="Salaire net (en euros)", y="Ressources finales (en euros)",fill="Prestations et \nrevenus primaires",caption=leg)+
      geom_line(data=data2,aes(rev_act_net, rev_disp, color='Revenu disponible'),linewidth=1)+
      geom_line(data=data.frame(x=c(0,max_revenu_net),y=c(min_revtot_net,max_revtot_net)),aes(x=x, y=y,color="Revenus primaires"), 
                linewidth=1,linetype="dashed")+
      geom_vline(xintercept=c(bareme_var[["smic_n"]]), linetype="dotted")+ 
      scale_colour_manual("", 
                          breaks = c("Revenu disponible", "Revenus primaires"),
                          values = c("Revenu disponible"="blue", "Revenus primaires"="red")) + 
      theme(panel.background = element_rect(fill = "white",colour = "white",linewidth = 0.5, linetype = "solid"),
            panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',colour = "grey"), 
            panel.border = element_blank(),
            panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',colour = "grey"),
            plot.title = element_text(face="bold", size=12,hjust=0.5),
            plot.caption = element_text(size=10))+
      geom_text(mapping = aes(x = bareme_var[["smic_n"]],
                              y =0, vjust=1.2,
                              label = "SMIC"))
    #p3<-girafe(p3, height = 5, width = 6)
    
  }
  if (n2000==1) {
    print(paste("melt castype4",system.time({
      tmp <- data %>%
        select(
          c("ARE_net","rev_disp","are_nette_conjoint","sal_net_conj","autres_rev"),
          all_of(mv)) %>%
        mutate(across(all_of(mv), as.numeric)) %>% mutate(ARE_net_=ARE_net,
                                                          sal_net_conj_=sal_net_conj,
                                                          are_nette_conjoint_=are_nette_conjoint,
                                                          autres_rev_=autres_rev)
      data2<- tidyr::pivot_longer(
        tmp,
        cols = all_of(mv),
        names_to = "variable") %>% rename(ARE_net=ARE_net_,
                                          sal_net_conj=sal_net_conj_,
                                          are_nette_conjoint=are_nette_conjoint_,
                                          autres_rev=autres_rev_) %>% mutate(variable=factor(variable,levels=mv))
      }
      )))
    
    
    # Realisation du graphique en aires empilees
    getPalette = colorRampPalette(brewer.pal(9, "Spectral"))
    
    max_revenu_net=max(data2$ARE_net,na.rm=T)
    max_revtot_net=max(data2$ARE_net+data2$are_nette_conjoint+data2$sal_net_conj+data2$autres_rev,na.rm=T)
    min_revtot_net=min(data2$ARE_net+data2$are_nette_conjoint+data2$sal_net_conj+data2$autres_rev,na.rm=T)
    
    p3 <- ggplot(data2, aes(x = ARE_net,  y=value))+
      geom_area(position='stack', aes(fill=variable))+
      scale_fill_manual(values=getPalette(15)
                        ,labels=labels(libelles[libelles %in% mv])
      )+
      labs(title="Composition du revenu disponible \n en fonction de l'ARE nette",x="ARE nette (en euros)", y="Ressources finales (en euros)",fill="Prestations et \nrevenus primaires",caption=leg)+
      geom_line(data=data2,aes(ARE_net, rev_disp, color='Revenu disponible'),linewidth=1)+
      geom_line(data=data.frame(x=c(0,max_revenu_net),y=c(min_revtot_net,max_revtot_net)),aes(x=x, y=y,color="Revenus primaires"), 
                linewidth=1,linetype="dashed")+geom_vline(xintercept=c(bareme_var[["smic_n"]]), linetype="dotted")+ 
      scale_colour_manual("", 
                          breaks = c("Revenu disponible", "Revenus primaires"),
                          values = c("Revenu disponible"="blue", "Revenus primaires"="red")) + 
      theme(panel.background = element_rect(fill = "white",colour = "white",linewidth = 0.5, linetype = "solid"),
            panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',colour = "grey"), 
            panel.border = element_blank(),
            panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',colour = "grey"),
            plot.title = element_text(face="bold", size=12,hjust=0.5),
            plot.caption = element_text(size=10))+
      geom_text(mapping = aes(x = bareme_var[["smic_n"]],
                              y =0, vjust=1.2,
                              label = "SMIC"))
  }
  print(p3)
}

