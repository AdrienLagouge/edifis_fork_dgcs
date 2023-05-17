################################################################################
#
# Copyright (C) 2022. Logiciel élaboré par l'État, via la Drees.
#
# Nom du dernier auteur : Camille Dufour, Drees.
#
# Noms des co-auteurs : Simon Fredon et Chloé Pariset
#
# Ce programme informatique a été développé par la Drees. Il permet de de reproduire l'application R-Shiny "Edifis". 
#
# Ce programme a été exécuté le 14/10/2022 avec la version 4.1.2 de R.
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
#########      Définition des commandes qui s’opèrent côté serveur      ########
#########                                                               ########  
################################################################################

options(encoding = "UTF-8")

server <- function(session,input,output){
  
  ######################### 2e onglet #########################
  
  # fonction réactive year() retourne les 2 derniers caractères de l'année choisie par l'utilisateur
  year <- reactive({
    year <- as.numeric(substr(input$year,3,4))
    return(year)
  })
  
  # fonction réactive bareme_var() retourne le barème de l'année choisie par l'utilisateur
  bareme_var <- reactive({
    # import du bareme depuis les maquettes excel selon l'annee de legislation choisie 
    print(paste("bareme_var readxl",system.time(assign(paste("bareme",sep=''),read_excel(paste("data/Maquette_cas_types_20",year(),".xls",sep=''),sheet=2)))))
    bareme_var <- modif_df(bareme,var_names[[as.character(year())]])
    return(bareme_var)
  })
  
  # fonction réactive n2000() retourne le type de revenus choisi par l'utilisateur en format numérique
  n2000 <- reactive({
    n2000 <- as.numeric(input$n2000)
    return(n2000)
  })
  
  # fonctions réactives smic_b() et smic_n() retournent les valeurs en euros des smics brut et net de l'année choisie par l'utilisateur
  smic_brut <- reactive({
    bareme_var <- bareme_var()
    smic_b <- bareme_var[["smic_b"]]
    return(smic_b)
  })
  
  smic_net <- reactive({
    bareme_var <- bareme_var()
    smic_n <- bareme_var[["smic_n"]]
    return(smic_n)
  })
  
  # fonctions réactives rev_max_b() et rev_max_n() retournent les valeurs en euros des salaires max brut et net choisis par l'utilisateur
  rev_max_brut <- reactive({
    bareme_var <- bareme_var()
    rev_max_b <- bareme_var[["smic_b"]]*input$n21/100
    return(rev_max_b)
  })
  
  rev_max_net <- reactive({
    bareme_var <- bareme_var()
    rev_max_n <- bareme_var[["smic_n"]]*input$n21/100
    return(rev_max_n)
  })
  
  # fonctions réactives tr_rev_b() et tr_rev_n() retournent les valeurs en euros des variations de salaires brut et net choisies par l'utilisateur
  tr_rev_brut <- reactive({
    bareme_var <- bareme_var()
    tr_rev_b <- bareme_var[["smic_b"]]*input$n22/100
    return(tr_rev_b)
  })
  
  tr_rev_net <- reactive({
    bareme_var <- bareme_var()
    tr_rev_n <- bareme_var[["smic_n"]]*input$n22/100
    return(tr_rev_n)
  })
  
  #idem pour l'ARE
  are_max_brut <- reactive({
    bareme_var <- bareme_var()
    are_max_b <- bareme_var[["smic_b"]]*input$n13/100
    return(are_max_b)
  })
  
  are_max_net <- reactive({
    bareme_var <- bareme_var()
    are_max_n <- bareme_var[["smic_n"]]*input$n13/100
    return(are_max_n)
  })
  
  tr_are_brut <- reactive({
    bareme_var <- bareme_var()
    tr_are_b <- bareme_var[["smic_b"]]*input$n12/100
    return(tr_are_b)
  })
  
  tr_are_net <- reactive({
    bareme_var <- bareme_var()
    tr_are_n <- bareme_var[["smic_n"]]*input$n12/100
    return(tr_are_n)
  })
  
  # fonctions réactives rev_conj_b() et rev_conj_n() retournent les valeurs en euros des salaires du conjoint brut et net choisis par l'utilisateur
  rev_conj_brut <- reactive({
    bareme_var <- bareme_var()
    rev_conj_b <- bareme_var[["smic_b"]]*input$n9/100
    return(rev_conj_b)
  })
  
  rev_conj_net <- reactive({
    bareme_var <- bareme_var()
    rev_conj_n <- bareme_var[["smic_n"]]*input$n9/100
    return(rev_conj_n)
  })
  
  # idem pour l'ARE du conjoint
  are_conj_brut <- reactive({
    bareme_var <- bareme_var()
    are_conj_b <- bareme_var[["smic_b"]]*input$n10/100
    return(are_conj_b)
  })
  
  are_conj_net <- reactive({
    bareme_var <- bareme_var()
    are_conj_n <- bareme_var[["smic_n"]]*input$n10/100
    return(are_conj_n)
  })
  
  # éléments de texte dépendants des paramètres choisis par l'utilisateur (les fonctions réactives y sont appelées)
  output$valeur_smic <- renderUI({
    mylist <- c(paste('<i class="fa fa-info-circle" style = "color:#0253a3;"></i>  <strong>SMIC temps plein</strong>'),paste("En brut :", round(smic_brut())," euros"), paste("En net :", round(smic_net())," euros"))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  output$valeur_rev_max <- renderUI({
    mylist <- c(paste("En brut :", round(rev_max_brut())," euros"), paste("En net :", round(rev_max_net())," euros"))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  output$valeur_tr_rev <- renderUI({
    mylist <- c(paste("En brut :", round(tr_rev_brut())," euros"), paste("En net :", round(tr_rev_net())," euros"))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })

  output$valeur_are_max <- renderUI({
    mylist <- c(paste("En brut :", round(are_max_brut())," euros"), paste("En net :", round(are_max_net())," euros"))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  output$valeur_tr_are <- renderUI({
    mylist <- c(paste("En brut :", round(tr_are_brut())," euros"), paste("En net :", round(tr_are_net())," euros"))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  output$message_echelle_rev<- renderUI({
    mylist <- paste("Avec les paramètres choisis,",ifelse(input$n2000==0,"le salaire net","l'allocation chômage (ARE) nette"),
                    "de la personne de référence varie de 0 à",strong(ifelse(input$n2000==0,rev_max_net(),are_max_net())),"euros par tranche de",
                    strong(ifelse(input$n2000==0,round(tr_rev_net()),round(tr_are_net()))),"euros, soit un revenu qui varie de 0 à",
                    strong(ifelse(input$n2000==0,input$n21,input$n13)),strong("%")," du SMIC par tranche de",
                    strong(ifelse(input$n2000==0,input$n22,input$n12)),strong("%")," du SMIC.")
    HTML(mylist)
  })
  
  output$valeur_rev_conj <- renderUI({
    mylist <- c(paste("En brut :", round(rev_conj_brut())," euros"), paste("En net :", round(rev_conj_net())," euros"))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  output$valeur_are_conj <- renderUI({
    mylist <- c(paste("En brut :", round(are_conj_brut())," euros"), paste("En net :", round(are_conj_net())," euros"))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  # bulles explicatives
  addPopover(session,"n2000","Type de revenu", 
             content="<p> Les deux types sont exclusifs, ils ne se cumulent pas.")
  addPopover(session,"year","Année de législation", 
             content="<p> Législation au 01/07 de l'année")
  addPopover(session,"n11","Autres revenus imposables du ménage", placement="up",
             content="<p> Montant mensuel fixe de revenus autres que les salaires et l'ARE (pensions alimentaires notamment) avant abattement de 10% pour frais professionnels, en &#8364 par mois </p>")
  addPopover(session,"n22","Variation de salaire (en % du SMIC)", 
             content="<p> Incrément de salaire en % de SMIC par mois, fixé à 2.5% par défaut </p>")
  addPopover(session,"n12","Variation d'allocation chômage (ARE) (en % du SMIC)", 
             content="<p> Incrément d'allocation en % de SMIC par mois, fixé à 2.5% par défaut </p>")
  addPopover(session,"n21","Salaire maximum (en % du SMIC)",
             content="<p> Ce paramètre permet de choisir la borne supérieure des salaires pour 
             lesquels on calcule un revenu disponible, en % de SMIC par mois. Par défaut, il est fixé à 300%. </p>")
  addPopover(session,"n13","Allocation chômage (ARE) maximum (en % du SMIC)",
             content="<p> Ce paramètre permet de choisir la borne supérieure du niveau d'allocation chômage (ARE) pour 
             laquelle on calcule un revenu disponible, en % de SMIC par mois. Par défaut, il est fixé à 300%. </p>")
  addPopover(session,"n1","Situation conjugale",placement="up",content="<p> Contrairement aux marié(e)s ou pacsé(e)s, les concubins sont imposés séparément. Dans ce cas, les 'autres revenus imposables du ménage' sont attribués à la personne de référence et les parts fiscales des enfants
             sont attribuées au membre du couple ayant les revenus les plus élevés. </p>")
  addPopover(session,"n16","Handicap au sens de l'AAH",placement="up",content="<p> Un taux d'incapacité au sens de l'AAH supérieur à 80% est un des critères déterminant le droit à la Majoration pour la Vie Autonome (MVA). </p>")
  addPopover(session,"n18","Parent isolé au sens du RSA et de la PA",placement="up",content="<p> La personne de référence a droit à une majoration temporaire du RSA et de la PA en tant que parent isolé, 
            pour une durée maximum de 12 mois après la séparation ou jusqu’au troisième anniversaire de l’enfant le plus jeune. </p>")
  addPopover(session,"n19","Droit à l'ASF",placement="up",content="<p> La personne de référence a droit à l'ASF si elle a au moins 1 enfant pour lequel l'autre parent ne participe plus à l'entretien depuis au moins 1 mois 
             ou verse une faible pension alimentaire. </p>")
  addPopover(session,"n20","Statut d'occupation du logement",placement="up",content="<p> Le statut d'occupation du ménage a un impact sur les aides au logement (AL) d'une part et sur le RSA et la PA via le forfait logement d'autre part.
             Les ménages accédants sont hors du champ de cette maquette, les AL accession n'étant pas simulées. </p>")
  
  # valeurs par défaut des valeurs max et des variations de revenu net (salaire ou ARE)
  observeEvent(input$n2000,{
    req(input$n2000)
    if(input$n2000==0) {
      updateNumericInput(session,"n12",value=0)
      updateNumericInput(session,"n13",value=0)
      updateNumericInput(session,"n21",value=300)
      updateNumericInput(session,"n22",value=100/40)
    }
    if(input$n2000==1) {
      updateNumericInput(session,"n21",value=0)
      updateNumericInput(session,"n22",value=0)
      updateNumericInput(session,"n12",value=100/40)
      updateNumericInput(session,"n13",value=300)
    }
  })
  
  #valeur par défaut du paramètre "Parent isolé au sens du RSA et de la PA" en fonction de la config familiale choisie
  observeEvent(c(input$n1,input$n100,input$n200,input$n300,input$n400,input$n500),{
    req(c(input$n1,input$n100,input$n200,input$n300,input$n400,input$n500))
    if(input$n1==1 & (input$n100==1 | input$n200==1 | input$n300==1 | input$n400==1 | input$n500==1)) {
      updateNumericInput(session,"n18",value=1)
    }
    if(input$n1>1 | (input$n100!=1 & input$n200!=1 & input$n300!=1 & input$n400!=1 & input$n500!=1)) {
      updateNumericInput(session,"n18",value=0)
    }
  }) 
  
  #valeur par défaut du paramètre "Droit à l'allocation de soutien familial (ASF)" en fonction de la config familiale choisie
  observeEvent(c(input$n1,input$nbenfants),{
    req(c(input$n1,input$nbenfants))
    if(input$n1==1  & input$nbenfants>0) {
      updateNumericInput(session,"n19",value=1)
    }
    if(input$n1!=1  | input$nbenfants==0) {
      updateNumericInput(session,"n19",value=0)
    }
  }) 
  
  # valeur par défaut des âges des enfants en fonction du nb d'enfants choisi
  observeEvent(input$nbenfants,{
    req(input$nbenfants)
    if(input$nbenfants ==0) {
      updateNumericInput(session,"n100",value=NULL)
      updateNumericInput(session,"n200",value=NULL)
      updateNumericInput(session,"n300",value=NULL)
      updateNumericInput(session,"n400",value=NULL)
      updateNumericInput(session,"n500",value=NULL)
    }
    if(input$nbenfants ==1) {
      updateNumericInput(session,"n100",value=3)
      updateNumericInput(session,"n200",value=NULL)
      updateNumericInput(session,"n300",value=NULL)
      updateNumericInput(session,"n400",value=NULL)
      updateNumericInput(session,"n500",value=NULL)
    }
    if(input$nbenfants ==2) {
      updateNumericInput(session,"n100",value=3)
      updateNumericInput(session,"n200",value=3)
      updateNumericInput(session,"n300",value=NULL)
      updateNumericInput(session,"n400",value=NULL)
      updateNumericInput(session,"n500",value=NULL)
    }
    if(input$nbenfants ==3) {
      updateNumericInput(session,"n100",value=3)
      updateNumericInput(session,"n200",value=3)
      updateNumericInput(session,"n300",value=3)
      updateNumericInput(session,"n400",value=NULL)
      updateNumericInput(session,"n500",value=NULL)
    }
    if(input$nbenfants ==4) {
      updateNumericInput(session,"n100",value=3)
      updateNumericInput(session,"n200",value=3)
      updateNumericInput(session,"n300",value=3)
      updateNumericInput(session,"n400",value=3)
      updateNumericInput(session,"n500",value=NULL)
    }
    if(input$nbenfants ==5) {
      updateNumericInput(session,"n100",value=3)
      updateNumericInput(session,"n200",value=3)
      updateNumericInput(session,"n300",value=3)
      updateNumericInput(session,"n400",value=3)
      updateNumericInput(session,"n500",value=3)
    }
  })

    # affichage de l'onglet 3 en fonction du clic sur le bouton "J'ai choisi mes paramètres, voir les résultats"
    observeEvent(input$jumpToP3,{
      updateTabsetPanel(session,"maquette",selected="panel2")
    })

  ######################### 3e onglet #########################
  
  # fonction réactive max_rows() retourne le nombre de lignes du tableau de données en fonction des paramètres d'échelle de revenus choisis
  max_rows <- reactive({
    n21 <- input$n21*smic_brut()/100
    n22 <- input$n22*smic_brut()/100
    n13 <- input$n13*smic_net()/100
    n12 <- input$n12*smic_net()/100
    
    return((max(c(n21,n13))%/%max(c(n22,n12)))+1)
  })

  # fonction write_parameters() restitue les paramètres choisis par l'utilisateur sous forme de liste (utile pour l'export dans une version précédente du code)
  write_parameters <- reactive({
    
    conv_age <- function(x) {
      if (x==0){y<-" "}
      if (x==1){y<-"Moins de 3 ans"}
      if (x==2){y<-"De 3 à 5 ans"}
      if (x==3){y<-"De 6 à 10 ans"}
      if (x==4){y<-"De 11 à 13 ans"}
      if (x==5){y<-"14 ans"}
      if (x==6){y<-"De 15 à 19 ans"}
      if (x==7){y<-"20 ans"}
      return(y)
    }
    
    vect_param <- c("Année de législation"=input$year,"Type de revenu"=ifelse(input$n2000==0,"salaire","Allocation chômage (ARE)"),"Nombre d'adultes"=input$n1, "Nombre d'enfants"=input$nbenfants,
                    "Âge du premier enfant"=conv_age(input$n100),"Âge du second enfant"=conv_age(input$n200),"Âge du troisième enfant"=conv_age(input$n300),
                    "Âge du quatrième enfant"=conv_age(input$n400),"Âge du cinquième enfant"=conv_age(input$n500),"Salaire du conjoint (en % du SMIC)"=input$n9,
                    "Allocation chômage (ARE) du conjoint (en % du SMIC)"=input$n10, "Autres revenus imposables du ménage (en euros)"=input$n11,
                    "Droit à l'allocation de solidarité spécifique (ASS) du conjoint"=ifelse(input$n15==0,"Non","Oui"),
                    "Handicap au sens de l'AAH"=ifelse(input$n16==0,"Non","Oui"),"Handicap du conjoint au sens de l'AAH"=ifelse(input$n17==0,"Non","Oui"),
                    "Parent isolé au sens du RSA et de la PA"=ifelse(input$n18==0,"Non","Oui"),"Droit à l'allocation de soutien familial (ASF)"=ifelse(input$n19==0,"Non","Oui"),
                    "Statut d'occupation du logement"=ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement"))
    df_param <- data.frame(x1=vect_param)
    return(df_param)
  }) 
  
  # élément de texte qui restitue les paramètres choisis par l'utilisateur sous forme de liste (affiché en tête d'onglet 3)
  output$parameters1 <- renderUI({
    if (input$n1==1){
      mylist <- c(paste("Année de législation : ",input$year),
                  paste("Type de revenu de la personne de référence : ",ifelse(input$n2000==0,"Salaire","Allocation chômage (ARE)")),
                  paste(ifelse(input$n2000==0,"Son salaire net","Son allocation chômage (ARE) nette")," varie de 0 à",input$n24,"euros par tranche de",ifelse(input$n2000==0,round(tr_rev_net(),0),round(tr_are_net(),0)),"euros."),
                  paste("Situation conjugale : ",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"En couple (marié(e)s ou pacsé(e)s)","En couple (en concubinage)"))),
                  paste("Nombre d'enfants : ",input$nbenfants),
                  paste("Autres revenus imposables du ménage (en euros) : ",input$n11),
                  paste("Handicap : ",ifelse(input$n16==0,"Non","Oui")),
                  paste("Statut d'occupation du logement : ",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
      
    } else {
      mylist <- c(paste("Année de législation : ",input$year),
                  paste("Type de revenu de la personne de référence : ",ifelse(input$n2000==0,"Salaire","Allocation chômage (ARE)")),
                  paste(ifelse(input$n2000==0,"Son salaire net","Son allocation chômage (ARE) nette")," varie de 0 à",input$n24,"euros par tranche de",ifelse(input$n2000==0,round(tr_rev_net(),0),round(tr_are_net(),0)),"euros."),
                  paste("Situation conjugale : ",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"En couple (marié(e)s ou pacsé(e)s)","En couple (en concubinage)"))),
                  paste("Nombre d'enfants : ",input$nbenfants),
                  paste("Salaire du conjoint (en % du SMIC) : ",input$n9),
                  paste("ARE du conjoint (en % du SMIC) : ",input$n10),
                  paste("Autres revenus imposables du ménage (en euros) : ",input$n11),
                  paste("Handicap : ",ifelse(input$n16==0,"Non","Oui")),
                  paste("Handicap du conjoint : ",ifelse(input$n17==0,"Non","Oui")),
                  paste("Statut d'occupation du logement : ",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
    }
    
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  # fonction réactive create_data() crée la table de données totale en fonction des paramètres choisis dans l'onglet 2
  create_data <- reactive({
    n1 <- as.numeric(input$n1)
    n2 <- (input$nbenfants>0)*as.numeric((input$nbenfants>0)*(input$n100==1)+(input$nbenfants>1)*(input$n200==1)+(input$nbenfants>2)*(input$n300==1)+(input$nbenfants>3)*(input$n400==1)+(input$nbenfants>4)*(input$n500==1))
    n3 <- (input$nbenfants>0)*as.numeric((input$nbenfants>0)*(input$n100==2)+(input$nbenfants>1)*(input$n200==2)+(input$nbenfants>2)*(input$n300==2)+(input$nbenfants>3)*(input$n400==2)+(input$nbenfants>4)*(input$n500==2))
    n4 <- (input$nbenfants>0)*as.numeric((input$nbenfants>0)*(input$n100==3)+(input$nbenfants>1)*(input$n200==3)+(input$nbenfants>2)*(input$n300==3)+(input$nbenfants>3)*(input$n400==3)+(input$nbenfants>4)*(input$n500==3))
    n5 <- (input$nbenfants>0)*as.numeric((input$nbenfants>0)*(input$n100==4)+(input$nbenfants>1)*(input$n200==4)+(input$nbenfants>2)*(input$n300==4)+(input$nbenfants>3)*(input$n400==4)+(input$nbenfants>4)*(input$n500==4))
    n6 <- (input$nbenfants>0)*as.numeric((input$nbenfants>0)*(input$n100==5)+(input$nbenfants>1)*(input$n200==5)+(input$nbenfants>2)*(input$n300==5)+(input$nbenfants>3)*(input$n400==5)+(input$nbenfants>4)*(input$n500==5))
    n7 <- (input$nbenfants>0)*as.numeric((input$nbenfants>0)*(input$n100==6)+(input$nbenfants>1)*(input$n200==6)+(input$nbenfants>2)*(input$n300==6)+(input$nbenfants>3)*(input$n400==6)+(input$nbenfants>4)*(input$n500==6))
    n8 <- (input$nbenfants>0)*as.numeric((input$nbenfants>0)*(input$n100==7)+(input$nbenfants>1)*(input$n200==7)+(input$nbenfants>2)*(input$n300==7)+(input$nbenfants>3)*(input$n400==7)+(input$nbenfants>4)*(input$n500==7))
    n9 <- as.numeric(input$n9*smic_brut()/100)
    n10 <- as.numeric(input$n10*smic_net()/100)
    n21 <- as.numeric(input$n21*smic_brut()/100)
    n22 <- as.numeric(input$n22*smic_brut()/100)
    n13 <- as.numeric(input$n13*smic_net()/100)
    n12 <- as.numeric(input$n12*smic_net()/100)
    
    rev_act <- vector("numeric",(max(c(n21,n13))%/%max(c(n22,n12)))+2)
    print(paste("RE3 choix_input",system.time(choix_input(n1,input$nbenfants,n2,n3,n4,n5,n6,n7,n8,n9,n10,input$n11,
                                                          as.numeric(input$n15),as.numeric(input$n16),as.numeric(input$n17),as.numeric(input$n18),as.numeric(input$n19),as.numeric(input$n20),n21,n22,n13,n12,bareme_var(),year(),n2000()))))
    print(paste("RE castype",system.time(data <- castype(rev_act,bareme_var(),year()))))
    return(data)
  })
  
  # fonction réactive RE() retourne la table de données qui s'affiche dans l'onglet 3 (selon les colonnes choisies par l'utilisateur)
  RE <- reactive({
    
    rev <- input[[paste0("rev",year(),n2000())]]
    prevsoc <- input[[paste0("prevsoc",year())]]
    impotax <- input[[paste0("impotax",year())]]
    minsoc <- input[[paste0("minsoc",year())]]
    pf <- input[[paste0("pf",year())]]
    alloclog <- input[[paste0("alloclog",year())]]
    revdisp <- input[[paste0("revdisp",year())]]
    
    show_vars <- c(rev,prevsoc,impotax,minsoc,pf,alloclog,revdisp)
    data <- create_data()
    colnames(data) <- label(data)
    return(head(round(data[, show_vars],2), n=(max_rows()),drop = FALSE))
    
  }) # fin de RE()
  
  # création et formatage de l'objet table qui s'affiche dans l'onglet 3
  output$base <- renderDataTable({RE()}, 
                                 rownames = FALSE,extensions = list('FixedHeader','Scroller'),
                                 options = list( scrollY = 400,scroller = TRUE, scrollX=TRUE,autoWidth = TRUE,
                                                 fixedHeader = TRUE,pageLength = (max_rows()),
                                                 sDom  = '<"top">lrt<"bottom">ip', 
                                                 lengthChange = FALSE)
                                 ) #fin de output$base
  

  # création de l'objet table téléchargeable dans l'onglet 3
  output$tab.csv <- downloadHandler(
    filename = function(){
      paste("tab_", input$year,".csv")
    },
    content = function(file){
      write.csv2(RE(),file,fileEncoding="latin1",row.names=F)
    }
  )
  
  ######################### 4e onglet ##########################

  # élément de texte qui restitue les paramètres choisis par l'utilisateur sous forme de liste (affiché en tête d'onglet 4) 
  # identique à output$parameters1, mais bugg si on appelle deux fois le mm objet dans ui.R
  output$parameters <- renderUI({
    
    if (input$n1==1){
      mylist <- c(paste("Année de législation : ",input$year),
                  paste("Type de revenu de la personne de référence : ",ifelse(input$n2000==0,"Salaire","Allocation chômage (ARE)")),
                  paste(ifelse(input$n2000==0,"Son salaire net","Son allocation chômage (ARE) nette")," varie de 0 à",input$n24,"euros par tranche de",ifelse(input$n2000==0,round(tr_rev_net(),0),round(tr_are_net(),0)),"euros."),
                  paste("Situation conjugale : ",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"En couple (marié(e)s ou pacsé(e)s)","En couple (en concubinage)"))),
                  paste("Nombre d'enfants : ",input$nbenfants),
                  paste("Autres revenus imposables du ménage (en euros) : ",input$n11),
                  paste("Handicap : ",ifelse(input$n16==0,"Non","Oui")),
                  paste("Statut d'occupation du logement : ",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
      
    } else {
      mylist <- c(paste("Année de législation : ",input$year),
                  paste("Type de revenu de la personne de référence : ",ifelse(input$n2000==0,"Salaire","Allocation chômage (ARE)")),
                  paste(ifelse(input$n2000==0,"Son salaire net","Son allocation chômage (ARE) nette")," varie de 0 à",input$n24,"euros par tranche de",ifelse(input$n2000==0,round(tr_rev_net(),0),round(tr_are_net(),0)),"euros."),
                  paste("Situation conjugale : ",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"En couple (marié(e)s ou pacsé(e)s)","En couple (en concubinage)"))),
                  paste("Nombre d'enfants : ",input$nbenfants),
                  paste("Salaire du conjoint (en % du SMIC) : ",input$n9),
                  paste("ARE du conjoint (en % du SMIC) : ",input$n10),
                  paste("Autres revenus imposables du ménage (en euros) : ",input$n11),
                  paste("Handicap : ",ifelse(input$n16==0,"Non","Oui")),
                  paste("Handicap du conjoint : ",ifelse(input$n17==0,"Non","Oui")),
                  paste("Statut d'occupation du logement : ",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
    }
    
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  # valeur par défaut du revenu net maximum des graphiques de l'onglet 4 en fonction des paramètres choisis dans l'onglet 2
  observe({
    x <- smic_net()
    n13 <- as.numeric(input$n13/100)
    n21 <- as.numeric(input$n21/100)
    
    if (is.null(x))
      x <- character(0)

    updateNumericInput(session, "n24",
                       label = "Revenu net maximum",
                       value = round(max(c(n13,n21))*x),
                       min = 1,
                       max = round(max(c(n13,n21))*x))
  })
  
  # bulle explicative
  addPopover(session,"n24","Revenu net maximum",
             content="<p> Ce paramètre permet de choisir la borne supérieure de l'axe des abscisses sur les graphiques, en &#8364 par mois. 
             Par défaut, il est fixé au revenu net maximum choisi dans l'onglet de choix des paramètres. Seul un ajustement à la baisse par 
             rapport à cette valeur par défaut aura un effet. </p>")
  
  ################## 2 graphiques simples #######################
  
  # fonction réactive RE1() qui crée le graphique représentant l'effet marginal d'une augmentation de revenu net (appel de la fonction castype1())
  RE1 <- reactive({
    
    n22 <- as.numeric(input$n22*smic_net()/100)
    n12 <- as.numeric(input$n12*smic_net()/100)
    n25 <- (input$n24%/%max(c(n22,n12)))
    data <- head(create_data(),n=n25+1)
    
    mylist_TMI<-c(paste("Source : EDIFIS Maquette au 1er juillet",input$year,"de cas-types Drees-BRE\n"),
                  paste("Note de lecture : quand",ifelse(input$n2000==0,"le salaire net","l'ARE nette"),"égal à",round(data[floor(n25/2),ifelse(input$n2000==0,"rev_act_net","ARE_net")]),"augmente de",round(data[floor(n25/2)+1,ifelse(input$n2000==0,"rev_act_net","ARE_net")]-data[floor(n25/2),ifelse(input$n2000==0,"rev_act_net","ARE_net")]),"euros,"),
                  paste("le revenu disponible après prestations et impôts varie de",round(data[floor(n25/2)+1,"rev_disp"]-data[floor(n25/2),"rev_disp"]),"euros.\n"),
                  paste("Ainsi",round(100-100*data[floor(n25/2),"TMI_net"],0),"% de l'augmentation",ifelse(input$n2000==0,"de salaire net","d'ARE nette"),"revient au ménage in fine. Cette part correspond à l'effet marginal sur le revenu disponible d'une augmentation",ifelse(input$n2000==0,"de salaire net","d'ARE nette"),".\n"), 
                  paste("Principaux paramètres retenus : Situation conjugale =",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"Marié(e)s ou pacsé(e)s","En concubinage")),"; Nombre d'enfants =",input$nbenfants,";\n"),
                  paste("Salaire du conjoint (en % du SMIC) =",input$n9,"; ARE du conjoint (en % du SMIC) =",input$n10,";\n"),
                  paste("Autres revenus imposables du ménage (en euros) =",input$n11,"; Handicap =",ifelse(input$n16==0,"Non","Oui"),"; Handicap du conjoint =",ifelse(input$n17==0,"Non","Oui"),";\n"),
                  paste("Statut d'occupation du logement =",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
    leg<-HTML(paste(mylist_TMI))
    
    return(castype1(data,leg,bareme_var(),year(),n2000()))
  })   #fin de RE1
  
  #plotlysation du graphique
  output$graph1 <- renderPlotly({RE1()})
  
  #bulle explicative
  addPopover(session,"graph1","Effet marginal sur le revenu disponible d'une augmentation de revenu net",placement="up",content="<p> Variation de revenu disponible après prestations et impôts
             induite par une augmentation de revenu net (en % de cette augmentation).</p>")
  
  
  #####################
  
  # fonction réactive RE2() qui crée le graphique représentant les montants de RSA/PA/PPE (appel de la fonction castype2())
  RE2 <- reactive({

     n22 <- as.numeric(input$n22*smic_net()/100)
     n12 <- as.numeric(input$n12*smic_net()/100)
     n25 <- (input$n24%/%max(c(n22,n12)))
     data <- head(create_data(),n=n25+1)
     
     mylist<-c(paste("Source : EDIFIS Maquette au 1er juillet",input$year,"de cas-types Drees-BRE\n"),
               paste("Principaux paramètres retenus : Situation conjugale =",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"Marié(e)s ou pacsé(e)s","En concubinage")),"; Nombre d'enfants =",input$nbenfants,";\n"),
               paste("Salaire du conjoint (en % du SMIC) =",input$n9,"; ARE du conjoint (en % du SMIC) =",input$n10,";\n"),
               paste("Autres revenus imposables du ménage (en euros) =",input$n11,"; Handicap =",ifelse(input$n16==0,"Non","Oui"),"; Handicap du conjoint =",ifelse(input$n17==0,"Non","Oui"),";\n"),
               paste("Statut d'occupation du logement =",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
     leg<-HTML(paste(mylist))
     
     #print(paste("RE2 castype2",system.time(castype2(data,leg,bareme_var(),year(),n2000()))))
     return(castype2(data,leg,bareme_var(),year(),n2000()))
   })   #fin de RE2
  
  #plotlysation du graphique
  output$graph2 <- renderPlotly({RE2()})
  
  
  ################## Graphique empilé #######################
  
  # utile pour boucler sur l'année à terme :
  # output$year_00 <- renderUI({
  #   mylist <- substr(input$year,3,4)
  #   print(mylist)
  # })

  # fonction réactive RE3() qui crée le graphique empilé (appel de la fonction castype3())  
   RE3 <- reactive({
     mylist<-c(paste("Source : EDIFIS Maquette au 1er juillet",input$year,"de cas-types Drees-BRE\n"),
               paste("Principaux paramètres retenus : Situation conjugale =",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"Marié(e)s ou pacsé(e)s","En concubinage")),"; Nombre d'enfants =",input$nbenfants,";\n"),
               paste("Salaire du conjoint (en % du SMIC) =",input$n9,"; ARE du conjoint (en % du SMIC) =",input$n10,";\n"),
               paste("Autres revenus imposables du ménage (en euros) =",input$n11,"; Handicap =",ifelse(input$n16==0,"Non","Oui"),"; Handicap du conjoint =",ifelse(input$n17==0,"Non","Oui"),";\n"),
               paste("Statut d'occupation du logement =",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
     leg<-HTML(paste(mylist))

     n22 <- as.numeric(input$n22*smic_net()/100)
     n12 <- as.numeric(input$n12*smic_net()/100)
     n25 <- input$n24%/%max(c(n22,n12))
     data <- head(create_data(),n=n25+1)
     mvars <- input[[paste0("show_area",year(),n2000())]]
     print(paste("RE3 castype3",system.time(castype3(data,leg,bareme_var(),year(),mvars,n2000()))))
   })  #fin de RE3
   output$graph3 <- renderPlot({RE3()})
   
   # création du graphique téléchargeable 
   output$graph_emp.png <- downloadHandler(
     filename = function(){
       paste("graph_emp_", input$year,".png")
     },
     content = function(file){
       png(file, width =800, height = 500)
       mylist<-c(paste("Source : EDIFIS Maquette au 1er juillet",input$year,"de cas-types Drees-BRE\n"),
                 paste("Principaux paramètres retenus : Situation conjugale =",ifelse(input$n1==1,"Seul(e)",ifelse(input$n1==2,"Marié(e)s ou pacsé(e)s","En concubinage")),"; Nombre d'enfants =",input$nbenfants,";\n"),
                 paste("Salaire du conjoint (en % du SMIC) =",input$n9,"; ARE du conjoint (en % du SMIC)=",input$n10,";\n"),
                 paste("Autres revenus imposables du ménage (en euros) =",input$n11,"; Handicap=",ifelse(input$n16==0,"Non","Oui"),"; Handicap du conjoint =",ifelse(input$n17==0,"Non","Oui"),";\n"),
                 paste("Statut d'occupation du logement =",ifelse(input$n20==0,"Locataire en zone 2","Propriétaire non accédant ou logé gratuitement")))
       leg<-HTML(paste(mylist))
       n22 <- as.numeric(input$n22*smic_net()/100)
       n12 <- as.numeric(input$n12*smic_net()/100)
       n25 <- input$n24%/%max(c(n22,n12))
       data <- head(create_data(),n=n25+1)
       mvars <- input[[paste0("show_area",year(),n2000())]]
       print(paste("RE3 castype3",system.time(castype3(data,leg,bareme_var(),year(),mvars,n2000()))))
       dev.off()
     }
   )
   
}





