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
#########    Définition de l'interface utilisateur de l'application     ########
#########                                                               ########  
################################################################################

ui <- 
  fluidPage(
    
    tags$head(
      
      #Google analytics
      HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
           <script async src='https://www.googletagmanager.com/gtag/js?id=UA-119893698-9x'></script>
           <script>
           window.dataLayer = window.dataLayer || [];
           function gtag(){dataLayer.push(arguments);}
           gtag('js', new Date());
           
           gtag('config', 'UA-119893698-9x');
           </script>"
      ),
     
      # Footer
      tags$style(type = 'text/css',
                 "footer{background: #0253a3;  min-height:100px}
                 .titlebox {
                 position: relative;
                 height: 100%;
                 }
                 .titlebox-img {
                 position: absolute;
                 left: 0;
                 bottom: 0;
                 }
                 table{
                 margin: 0 auto;
                 width: 97%;
                 clear: both;
                 table-layout: fixed;
                 word-wrap:break-word;
                 }
                 
                 .legend {
                 padding: 5px 5px 5px 5px;
                 }"),
      
      HTML("<nav class='navbar' style='background-color: #0253a3;'>
         <div class='col-sm-2'>
         <div class='pull-right'>"),
      tags$a(
        img(src="img/Logo_ministere-150.jpg"), #width = 90, height = round(0.69*90)), 
        href="http://solidarites-sante.gouv.fr/", 
        target="_blank"),
      HTML("</div> 
         </div> 
         <div class='col-sm-2'>
         <div class='titlebox'>
         <br> "),
      tags$a(img(src="img/Logo_Drees.jpg", width = 90, height = round(0.69*90)), 
             href="http://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/", 
             target="_blank"),
      HTML("</div>
         </div>
         <div class='container-fluid col-md-8'>
         <div> 
         <h3 class='pull-left' style='color: white'> Direction de la recherche, des études, de l'évaluation et des statistiques</h3>
         </div>
         </div>
         </nav>"),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrapv5.css")
      )
      ), 
    
  
    fluidPage(useShinydashboard(),infoBox(title=HTML("<br>"),subtitle="Vous êtes invités à adresser vos retours d'utilisation à l'adresse DREES-REDISTRIBUTION-INES@sante.gouv.fr",icon = icon("users"),color = "red",width=11,fill=TRUE )),
    navbarPage( title = "EDIFIS - maquette d’Evaluation des DIspositifs FIscaux et Sociaux sur cas-types",
             id="maquette",
             windowTitle="windowtitle",
             footer =  HTML("<footer>
                              <div class='col-sm-2'>
                            <div class='pull-right'>
                            <br> 
                            <a href='http://solidarites-sante.gouv.fr/' target='_blank'>
                            <img src='img/Marianne.png' height='70'/>
                            </a>
                            <br> <br>
                            </div>
                            </div>
                            <div class='col-sm-2'>
                            <div class='pull-left'>
                            <br> 
                            <a href='http://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/' target='_blank'>
                            <img src='img/Logo_Drees.jpg' height='70'/>
                            </a>
                            <br> <br> <br>
                            </div>
                            </div>
                            <span style='font-size: 11pt; line-height:10pt; color: white'>
                            <br>
                            Direction de la recherche, des études, de l'évaluation et des statistiques
                            <br>
                            Ministère des Solidarités et de la Santé - 14 avenue Duquesne - 75 350 Paris 07 SP
                            <br>
                            Retrouvez toutes nos publications sur<strong> <a href = 'http://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/' target='_blank'; style='color: #ffffff'>
                            drees.solidarites-sante.gouv.fr
                            
                            </a></strong> et nos données sur <strong> <a href = 'http://www.data.drees.sante.gouv.fr/' target='_blank'; style='color: #ffffff'>
                            data.drees.sante.gouv.fr
                            </a> </strong> <br>
                            <strong>
                            <a href='mailto:drees-infos@sante.gouv.fr' style='color: #ffffff'>   Contact  </a> </strong></span>
                            <div> <br> </div> </footer>"),
             
             ######################### 1er onglet #########################
                 tabPanel("Guide d'utilisation",icon = icon("circle-info"),
                          
                          fluidPage(
                            useShinydashboard(),
                            column(4),
                            column(10,align="justify",
                                   h1("Guide d'usage de la maquette EDIFIS"),
                                   h3("Objectif"),
                                   p(HTML("L'objectif est d'estimer le revenu disponible mensuel d'un ménage-type en fonction du salaire de la personne de référence. 
                                      La maquette présente au choix les résultats de la législation en vigueur au 01/07 des années 2015 à 2022 sur le champ des prélèvements et des prestations légales. 
                                      <br/>La maquette met en évidence les effets redistributifs stylisés du système socio-fiscal par type de ménage. 
                                          Attention, elle ne prend pas en compte le poids des types de ménage dans la population totale  donc elle ne permet pas d'en déduire directement les effets redistributifs agrégés 
                                          sur la population totale, ni de simuler des droits individuels.")),
                                   h3("Procédure pour générer des cas-types"),
                                   p("La première étape consiste à choisir les paramètres d'année législative et de caractéristiques du ménage dans l'onglet 'Choix des paramètres'."),
                                   p("Une fois les paramètres choisis, l'onglet 'Table de données' restitue
                                      un tableau dont chaque ligne correspond à un niveau de salaire et dont les colonnes présentent les revenus et 
                                     transferts associés pour le type de ménage défini dans l'onglet précédent. L'exportation du tableau vers un fichier csv est possible en cliquant sur le 
                                     bouton 'Télécharger le tableau'."),
                                   p("Enfin, le dernier onglet 'Sorties graphiques' permet de visualiser le montant de certaines prestations en fonction des revenus nets de la personne de référence. Les graphiques sont exportables en cliquant sur les boutons de téléchargement."),
                                   h3("Socle d'hypothèses"),
                                   p("Le ménage conserve les mêmes revenus au cours des trois années précédentes (en euros constants). La personne de référence et son éventuel conjoint sont par hypothèse d’âge actif entre 25 et 65 ans. Le cas des enfants de plus de 20 ans est exclu. Lorsqu'ils travaillent, les individus sont salariés en milieu ordinaire."),
                                   p("Par souci de simplification, certains paramètres de la législation ne sont pas pris en compte. Les principales hypothèses simplificatrices sont listées ici :"),
                                   p("- Les cotisations sociales : le barème d'un salarié non cadre en CDI est appliqué."),
                                   p("- L'impôt sur le revenu : l’ensemble des revenus paramétrés dans la maquette sont soumis au barème (pas d’impôt à taux proportionnel, ni de réductions ou crédits d’impôt). A partir de 2019, année de mise en place du prélèvement à la source, l'impôt simulé est celui dû au titre des revenus contemporains."),
                                   p("- La taxe d'habitation : les montants avant dégrèvement sont des montants fixes moyens par situation conjugale."),
                                   p("- Les transitions de revenus : du fait de l'absence de transitions, les mécanismes de neutralisations/abattements associés pour les prestations familiales et les aides au logement ne s'appliquent pas."),
                                   p("- Les prestations familiales : celles dédiées à la garde des enfants ne sont pas modélisées (ni le complément de libre choix du mode de garde (Cmg), ni la prestation partagée d’éducation de l’enfant (PreParE))."),
                                   p("- Les aides au logement : les ménages sont locataires du parc privé en zone 2 au loyer plafond et donc éligibles aux aides au logement sous réserve de satisfaire aux conditions de ressources. Les « aides au logement accession » sont exclues de l'analyse."),
                                   p("- Le recours : le recours est automatique pour toutes les prestations."),
                                   p("- A partir de 2020 : les revalorisations exceptionnelles d'ARS, de RSA, d'ASS et d'AL en 2020 puis l'indemnité inflation en 2021 et 2022 versées ponctuellement ne sont pas incluses."),
                                   h3("Le code en libre accès"),
                                   a(href = "https://gitlab.com/DREES_code/public/outils", "Disponible ici."),
                                   HTML("<br><br>")
                                   ),
                            column(4)
                            )
                          ),
             
             ######################### 2e onglet #########################
                 tabPanel(title="Choix des paramètres",value="panel1",icon = icon("gear"),
                          fluidPage(
                            column(4,
                                   sliderInput("year","Année de législation",min=2015,max=2022,value=2022,step=1,sep=""),
                                   h3("Echelle de revenus de la personne de référence (PR)"),
                                   p("La maquette calcule le revenu disponible d'un ménage-type, pour des revenus de la personne de référence allant de 
                                     0 à une valeur maximale suivant un pas à paramétrer."),
                                   p(""),
                                   
                                   fluidRow(
                                     column(6,
                                            conditionalPanel(condition="input.year>2014",radioButtons(
                                              "n2000",
                                              "Type de revenu",
                                              choices = c("Salaire"=0,"Allocation chômage (ARE)"=1),
                                              selected =0 ))
                                            ),
                                             column(6,
                                            box(htmlOutput("valeur_smic"),width=14)
                                            
                                     )
                                     ),
                                   fluidRow(
                                     column(6,
                                            conditionalPanel(condition="input.n2000==0",
                                                             numericInput("n21",HTML("Salaire maximum <br/> (en % du SMIC)"),min=0,max=1000,value=300),
                                                             box(htmlOutput("valeur_rev_max"),width=14)),
                                            conditionalPanel(condition="input.n2000==1",
                                                             numericInput("n13",HTML("Allocation chômage (ARE) maximum <br/> (en % du SMIC)"),min=0,max=10000,value=300),
                                                             box(htmlOutput("valeur_are_max"),width=14))
                                            ),
                                     column(6,
                                            conditionalPanel(condition="input.n2000==0",
                                                             numericInput("n22",HTML("Variation de salaire <br/> (en % du SMIC)"),min=0,max=100,value=100/40),
                                                             box(htmlOutput("valeur_tr_rev"),width=14)),
                                            conditionalPanel(condition="input.n2000==1",
                                                             numericInput("n12",HTML("Variation d'allocation chômage (ARE) <br/> (en % du SMIC)"),min=0,max=100,value=100/40),
                                                             box(htmlOutput("valeur_tr_are"),width=14))
                                     )
                                   ),
                                   box(htmlOutput("message_echelle_rev"),background = "blue",width=14)
                                   ),
                            column(3,
                                   h3("Composition familiale"),
                                   selectInput(inputId="n1",label="Situation conjugale", choices=c("Seul(e)"=1,"En couple (marié(e)s ou pacsé(e)s)"=2,"En couple (en concubinage)"=3),selected=1),
                                   numericInput("nbenfants",label="Nombre d'enfants de 0 à 5",0,min=0, max=5,step=1),
                                   conditionalPanel(condition="input.nbenfants>0",
                                                    selectInput(inputId="n100",label="Âge du premier enfant", choices=c(" "=0,"Moins de 3 ans"=1,"De 3 à 5 ans"=2,
                                                                                                                        "De 6 à 10 ans"=3,"De 11 à 13 ans"=4,
                                                                                                                        "14 ans"=5,"De 15 à 19 ans"=6,
                                                                                                                        "20 ans"=7),selected=NULL)),
                                   conditionalPanel(condition="input.nbenfants>1",
                                                    selectInput(inputId="n200",label="Âge du second enfant", choices=c(" "=0,"Moins de 3 ans"=1,"De 3 à 5 ans"=2,
                                                                                                                        "De 6 à 10 ans"=3,"De 11 à 13 ans"=4,
                                                                                                                        "14 ans"=5,"De 15 à 19 ans"=6,
                                                                                                                        "20 ans"=7),selected=NULL)),
                                   conditionalPanel(condition="input.nbenfants>2",
                                                    selectInput(inputId="n300",label="Âge du troisième enfant", choices=c(" "=0,"Moins de 3 ans"=1,"De 3 à 5 ans"=2,
                                                                                                                        "De 6 à 10 ans"=3,"De 11 à 13 ans"=4,
                                                                                                                        "14 ans"=5,"De 15 à 19 ans"=6,
                                                                                                                        "20 ans"=7),selected=NULL)),
                                   conditionalPanel(condition="input.nbenfants>3",
                                                    selectInput(inputId="n400",label="Âge du quatrième enfant", choices=c(" "=0,"Moins de 3 ans"=1,"De 3 à 5 ans"=2,
                                                                                                                       "De 6 à 10 ans"=3,"De 11 à 13 ans"=4,
                                                                                                                       "14 ans"=5,"De 15 à 19 ans"=6,
                                                                                                                       "20 ans"=7),selected=NULL)),
                                   conditionalPanel(condition="input.nbenfants>4",
                                                    selectInput(inputId="n500",label="Âge du cinquième enfant", choices=c(" "=0,"Moins de 3 ans"=1,"De 3 à 5 ans"=2,
                                                                                                                        "De 6 à 10 ans"=3,"De 11 à 13 ans"=4,
                                                                                                                        "14 ans"=5,"De 15 à 19 ans"=6,
                                                                                                                        "20 ans"=7),selected=NULL)),
                                   
                                   h3("Revenus supplémentaires"),
                                   p(""),
                                   conditionalPanel(condition="input.n1>1", 
                                                    numericInput("n9",HTML("Salaire du conjoint fixé à <br/> (en % du SMIC)"),0,min=0,step=1),
                                                    box(htmlOutput("valeur_rev_conj"),width=14)),
                                                    
                                   conditionalPanel(condition="input.n1>1", 
                                                    numericInput("n10",HTML("Allocation chômage (ARE) du conjoint fixée à (en % du SMIC)"),0,min=0,step=1),
                                                    box(htmlOutput("valeur_are_conj"),width=14)),
                                   
                                   conditionalPanel(condition="input.n1>1", 
                                                    selectInput(inputId="n15",label="Droit à l'allocation de solidarité spécifique (ASS) du conjoint", choices=c("Non"=0,"Oui"=1),selected=0)),
                                   
                                   numericInput("n11",HTML("Autres revenus imposables du ménage fixés à (en euros par mois)"),0,min=0,step=1)
                                   ),
                            column(3,
                                   h3("Handicap"),
                                   selectInput(inputId="n16",label="Handicap au sens de l'AAH", choices=c("Non"=0,"Oui avec un taux d'incapacité compris entre 50% et 80%"=1,"Oui avec un taux d'incapacité supérieur à 80%"=2),selected=0),
                                   conditionalPanel(condition="input.n1>1",
                                                    selectInput(inputId="n17",label="Handicap du conjoint au sens de l'AAH", choices=c("Non"=0,"Oui avec un taux d'incapacité compris entre 50% et 80%"=1,"Oui avec un taux d'incapacité supérieur à 80%"=2),selected=0)),
                                   h3("Autres"),
                                   selectInput(inputId="n18",label="Parent isolé au sens du RSA et de la PA", choices=c("Non"=0,"Oui"=1),selected=0),
                                   selectInput(inputId="n19",label="Droit à l'allocation de soutien familial (ASF)", choices=c("Non"=0,"Oui"=1),selected=0),
                                   selectInput(inputId="n20",label="Statut d'occupation du logement", choices=c("Locataire en zone 2"=0,"Propriétaire non accédant ou logé gratuitement"=1),selected=0),
                                   HTML("<br>"),
                                   actionButton("jumpToP3",HTML("J'ai choisi mes paramètres, <br> voir les résultats"), 
                                               style="color: #fff; background-color:  #0253a3;padding:4px; font-size:80%"),
                                   HTML("<br><br>")
                                   )
                            )

                          ),
     
             ######################### 3e onglet #########################              
                 tabPanel(title="Table de données",value="panel2",icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              h3("Choix des colonnes à afficher"),
                              conditionalPanel(condition="input.year==2013",
                                               pickerInput("rev130","Revenus primaires",rev13,
                                                           selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                      "Total des revenus primaires du ménage",
                                                                      `selected-text-format` = "count > 3"),
                                                           multiple = TRUE),
                                               pickerInput("prevsoc13","Prélèvements sociaux",prelevements_soc13,
                                                           multiple = TRUE),
                                               pickerInput("impotax13","Impôts",impot_tax13,
                                                           multiple = TRUE),
                                               pickerInput("minsoc13","Minima sociaux et prime pour l'emploi",min_soc13,
                                                           multiple = TRUE),
                                               pickerInput("pf13",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_13,
                                                           multiple = TRUE),
                                               pickerInput("alloclog13",HTML("Allocations logement <br/> des locataires"),alloc_log13,
                                                           multiple = TRUE),
                                               pickerInput("revdisp13","Revenus disponibles",rev_disp13,
                                                           selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                           multiple = TRUE)
                              ),
                              conditionalPanel(condition="input.year==2014",
                                               pickerInput("rev140","Revenus primaires",rev14,
                                                           selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                      "Total des revenus primaires du ménage"
                                                                      ,`selected-text-format` = "count > 3"),
                                                           multiple = TRUE),
                                               pickerInput("prevsoc14","Prélèvements sociaux",prelevements_soc14,
                                                           multiple = TRUE),
                                               pickerInput("impotax14","Impôts",impot_tax14,
                                                           multiple = TRUE),
                                               pickerInput("minsoc14","Minima sociaux et prime pour l'emploi",min_soc14,
                                                           multiple = TRUE),
                                               pickerInput("pf14",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_14,
                                                           multiple = TRUE),
                                               pickerInput("alloclog14",HTML("Allocations logement <br/> des locataires"),alloc_log14,
                                                           multiple = TRUE),
                                               pickerInput("revdisp14","Revenus disponibles",rev_disp14,
                                                           selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                           multiple = TRUE)
                              ),
                              conditionalPanel(condition="input.year==2015 & input.n2000==0",
                                               pickerInput("rev150","Revenus primaires",rev15,
                                                           selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                      "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                           multiple = TRUE)),
                              conditionalPanel(condition="input.year==2015 & input.n2000==1",
                                               pickerInput("rev151","Revenus primaires",rev15,
                                                           selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                           multiple = TRUE)),
                              conditionalPanel(condition="input.year==2015",
                                               pickerInput("prevsoc15","Prélèvements sociaux",prelevements_soc15,
                                                           multiple = TRUE),
                                               pickerInput("impotax15","Impôts",impot_tax15,
                                                           multiple = TRUE),
                                               pickerInput("minsoc15","Minima sociaux et prime pour l'emploi",min_soc15,
                                                           multiple = TRUE),
                                               pickerInput("pf15",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_15,
                                                           multiple = TRUE),
                                               pickerInput("alloclog15",HTML("Allocations logement <br/> des locataires"),alloc_log15,
                                                           multiple = TRUE),
                                               pickerInput("revdisp15","Revenus disponibles",rev_disp15,
                                                           selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                           multiple = TRUE)
                              ),
                              conditionalPanel(condition="input.year==2016 & input.n2000==0",
                                               pickerInput("rev160","Revenus primaires",rev16,
                                                           selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                      "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                           multiple = TRUE)),
                              conditionalPanel(condition="input.year==2016 & input.n2000==1",
                                               pickerInput("rev161","Revenus primaires",rev16,
                                                           selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                           multiple = TRUE)),
                              conditionalPanel(condition="input.year==2016",
                                               pickerInput("prevsoc16","Prélèvements sociaux",prelevements_soc16,
                                                           multiple = TRUE),
                                               pickerInput("impotax16","Impôts",impot_tax16,
                                                           multiple = TRUE),
                                               pickerInput("minsoc16","Minima sociaux et prime d'activité",min_soc16,
                                                           multiple = TRUE),
                                               pickerInput("pf16",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_16,
                                                           multiple = TRUE),
                                               pickerInput("alloclog16",HTML("Allocations logement <br/> des locataires"),alloc_log16,
                                                           multiple = TRUE),
                                               pickerInput("revdisp16","Revenus disponibles",rev_disp16,
                                                           selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                           multiple = TRUE)
                              ),
                              conditionalPanel(condition="input.year==2017 & input.n2000==0",
                                               pickerInput("rev170","Revenus primaires",rev17,
                                                           selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                      "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                           multiple = TRUE)),
                              conditionalPanel(condition="input.year==2017 & input.n2000==1",
                                               pickerInput("rev171","Revenus primaires",rev17,
                                                           selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                           multiple = TRUE)),
                              conditionalPanel(condition="input.year==2017",
                                               pickerInput("prevsoc17","Prélèvements sociaux",prelevements_soc17,
                                                           multiple = TRUE),
                                               pickerInput("impotax17","Impôts",impot_tax17,
                                                           multiple = TRUE),
                                               pickerInput("minsoc17","Minima sociaux et prime d'activité",min_soc17,
                                                           multiple = TRUE),
                                               pickerInput("pf17",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_17,
                                                           multiple = TRUE),
                                               pickerInput("alloclog17",HTML("Allocations logement <br/> des locataires"),alloc_log17,
                                                           multiple = TRUE),
                                               pickerInput("revdisp17","Revenus disponibles",rev_disp17,
                                                           selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                           multiple = TRUE)
                            ),
                            conditionalPanel(condition="input.year==2018 & input.n2000==0",
                                             pickerInput("rev180","Revenus primaires",rev18,
                                                         selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                    "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2018 & input.n2000==1",
                                             pickerInput("rev181","Revenus primaires",rev18,
                                                         selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2018",
                                             pickerInput("prevsoc18","Prélèvements sociaux",prelevements_soc18,
                                                         multiple = TRUE),
                                             pickerInput("impotax18","Impôts",impot_tax18,
                                                         multiple = TRUE),
                                             pickerInput("minsoc18","Minima sociaux et prime d'activité",min_soc18,
                                                         multiple = TRUE),
                                             pickerInput("pf18",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_18,
                                                         multiple = TRUE),
                                             pickerInput("alloclog18",HTML("Allocations logement <br/> des locataires"),alloc_log18,
                                                         multiple = TRUE),
                                             pickerInput("revdisp18","Revenus disponibles",rev_disp18,
                                                         selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                         multiple = TRUE)
                            ),
                            conditionalPanel(condition="input.year==2019 & input.n2000==0",
                                             pickerInput("rev190","Revenus primaires",rev19,
                                                         selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                    "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2019 & input.n2000==1",
                                             pickerInput("rev191","Revenus primaires",rev19,
                                                         selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2019",
                                             pickerInput("prevsoc19","Prélèvements sociaux",prelevements_soc19,
                                                         multiple = TRUE),
                                             pickerInput("impotax19","Impôts",impot_tax19,
                                                         multiple = TRUE),
                                             pickerInput("minsoc19","Minima sociaux et prime d'activité",min_soc19,
                                                         multiple = TRUE),
                                             pickerInput("pf19",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_19,
                                                         multiple = TRUE),
                                             pickerInput("alloclog19",HTML("Allocations logement <br/> des locataires"),alloc_log19,
                                                         multiple = TRUE),
                                             pickerInput("revdisp19","Revenus disponibles",rev_disp19,
                                                         selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                         multiple = TRUE)
                            ),
                            conditionalPanel(condition="input.year==2020 & input.n2000==0",
                                             pickerInput("rev200","Revenus primaires",rev20,
                                                         selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                    "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2020 & input.n2000==1",
                                             pickerInput("rev201","Revenus primaires",rev20,
                                                         selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2020",
                                             pickerInput("prevsoc20","Prélèvements sociaux",prelevements_soc20,
                                                         multiple = TRUE),
                                             pickerInput("impotax20","Impôts",impot_tax20,
                                                         multiple = TRUE),
                                             pickerInput("minsoc20","Minima sociaux et prime d'activité",min_soc20,
                                                         multiple = TRUE),
                                             pickerInput("pf20",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_20,
                                                         multiple = TRUE),
                                             pickerInput("alloclog20",HTML("Allocations logement <br/> des locataires"),alloc_log20,
                                                         multiple = TRUE),
                                             pickerInput("revdisp20","Revenus disponibles",rev_disp20,
                                                         selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                         multiple = TRUE)
                            ),
                            conditionalPanel(condition="input.year==2021 & input.n2000==0",
                                             pickerInput("rev210","Revenus primaires",rev21,
                                                         selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                    "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2021 & input.n2000==1",
                                             pickerInput("rev211","Revenus primaires",rev21,
                                                         selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2021",
                                             pickerInput("prevsoc21","Prélèvements sociaux",prelevements_soc21,
                                                         multiple = TRUE),
                                             pickerInput("impotax21","Impôts",impot_tax21,
                                                         multiple = TRUE),
                                             pickerInput("minsoc21","Minima sociaux et prime d'activité",min_soc21,
                                                         multiple = TRUE),
                                             pickerInput("pf21",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_21,
                                                         multiple = TRUE),
                                             pickerInput("alloclog21",HTML("Allocations logement <br/> des locataires"),alloc_log21,
                                                         multiple = TRUE),
                                             pickerInput("revdisp21","Revenus disponibles",rev_disp21,
                                                         selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                         multiple = TRUE)
                            ),
                            conditionalPanel(condition="input.year==2022 & input.n2000==0",
                                             pickerInput("rev220","Revenus primaires",rev22,
                                                         selected=c("Salaire brut en % du Smic brut temps plein (PR)","Salaire net (PR)",
                                                                    "Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2022 & input.n2000==1",
                                             pickerInput("rev221","Revenus primaires",rev22,
                                                         selected=c("ARE nette (PR)","Total des revenus primaires du ménage",`selected-text-format` = "count > 3"),
                                                         multiple = TRUE)),
                            conditionalPanel(condition="input.year==2022",
                                             pickerInput("prevsoc22","Prélèvements sociaux",prelevements_soc22,
                                                         multiple = TRUE),
                                             pickerInput("impotax22","Impôts",impot_tax22,
                                                         multiple = TRUE),
                                             pickerInput("minsoc22","Minima sociaux et prime d'activité",min_soc22,
                                                         multiple = TRUE),
                                             pickerInput("pf22",HTML("Prestations familiales <br/> (hors aides à la garde)"),pf_22,
                                                         multiple = TRUE),
                                             pickerInput("alloclog22",HTML("Allocations logement <br/> des locataires"),alloc_log22,
                                                         multiple = TRUE),
                                             pickerInput("revdisp22","Revenus disponibles",rev_disp22,
                                                         selected=c("Total des prestations","Total des impôts","Revenu disponible"),
                                                         multiple = TRUE)
                            ),
                            
                            
                          width = 3),
                            mainPanel(
                              div(style="display: inline-block; vertical-align:top; width: 700px;",infoBox(title="Rappel des principaux paramètres", subtitle=htmlOutput("parameters1"),
                                                                                                          icon =icon("gear"),
                                                                                                          color ="blue" ,
                                                                                                          width=14,
                                                                                                          fill=FALSE)),
                              HTML("<br><br>"),
                              div(style="display: inline-block;vertical-align:top; width: 600px;",HTML("<br>")),
                              div(style="display: inline-block;vertical-align:bottom;  width: 300px;",downloadButton('tab.csv', 'Télécharger le tableau')),
                              HTML("<br><br>"),
                              div(style="display: inline-block; width: 900px;",dataTableOutput("base"),  style = "font-size:70%"),
                              HTML("<br><br>")
                              ))
                          
                 ),
                 
             ######################### 4e onglet ########################## 
                      tabPanel("Sorties graphiques",
                      icon = icon("square-poll-vertical"),
                      fluidPage(
                        useShinydashboard(),
                          fluidRow(
                            
                            sidebarLayout(
                              sidebarPanel(
                                           h3("Choix de la représentation graphique"),
                                           numericInput("n24","Revenu net maximum",min=0,max=100000,value=5107,step=1),
                                           conditionalPanel(condition="input.year==2015 & input.n2000==0",
                                                            pickerInput("show_area150",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["15"]],
                                                                        selected=measure_vars0[["15"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2015 & input.n2000==1",
                                                            pickerInput("show_area151",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["15"]],
                                                                        selected=measure_vars1[["15"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2016 & input.n2000==0",
                                                            pickerInput("show_area160",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["16"]],
                                                                        selected=measure_vars0[["16"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2016 & input.n2000==1",
                                                            pickerInput("show_area161",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["16"]],
                                                                        selected=measure_vars1[["16"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2017 & input.n2000==0",
                                                            pickerInput("show_area170",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["17"]],
                                                                        selected=measure_vars0[["17"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2017 & input.n2000==1",
                                                            pickerInput("show_area171",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["17"]],
                                                                        selected=measure_vars1[["17"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2018 & input.n2000==0",
                                                            pickerInput("show_area180",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["18"]],
                                                                        selected=measure_vars0[["18"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2018 & input.n2000==1",
                                                            pickerInput("show_area181",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["18"]],
                                                                        selected=measure_vars1[["18"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2019 & input.n2000==0",
                                                            pickerInput("show_area190",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["19"]],
                                                                        selected=measure_vars0[["19"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2019 & input.n2000==1",
                                                            pickerInput("show_area191",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["19"]],
                                                                        selected=measure_vars1[["19"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2020 & input.n2000==0",
                                                            pickerInput("show_area200",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["20"]],
                                                                        selected=measure_vars0[["20"]],
                                                                        multiple = TRUE)                 
                                           ),
                                           conditionalPanel(condition="input.year==2020 & input.n2000==1",
                                                            pickerInput("show_area201",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["20"]],
                                                                        selected=measure_vars1[["20"]],
                                                                        multiple = TRUE)                
                                           ),
                                           conditionalPanel(condition="input.year==2021 & input.n2000==0",
                                                            pickerInput("show_area210",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["21"]],
                                                                        selected=measure_vars0[["21"]],
                                                                        multiple = TRUE)                 
                                           ),
                                           conditionalPanel(condition="input.year==2021 & input.n2000==1",
                                                            pickerInput("show_area211",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["21"]],
                                                                        selected=measure_vars1[["21"]],
                                                                        multiple = TRUE)
                                           ),
                                           conditionalPanel(condition="input.year==2022 & input.n2000==0",
                                                            pickerInput("show_area220",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars0[["22"]],
                                                                        selected=measure_vars0[["22"]],
                                                                        multiple = TRUE)                 
                                           ),
                                           conditionalPanel(condition="input.year==2022 & input.n2000==1",
                                                            pickerInput("show_area221",HTML("Prestations et revenus primaires <br/>dans le graphique empilé"),
                                                                        choices=measure_vars1[["22"]],
                                                                        selected=measure_vars1[["22"]],
                                                                        multiple = TRUE)),
        
                               width = 3),
                              
                              mainPanel(
                                div(style="display: inline-block;vertical-align:top; width: 700px;",infoBox(title="Rappel des principaux paramètres", subtitle=htmlOutput("parameters"),
                                                                                                            icon =icon("gear"),
                                                                                                            color ="blue" ,
                                                                                                            width=14,
                                                                                                            fill=FALSE))
                                        )
                              )
                              )
                        
                          ),
                          
                          fluidRow(
                              HTML("<br>"),
                              withSpinner(plotOutput("graph3", height = "500px")),
                              downloadButton(outputId = "graph_emp.png", label = "Télécharger le graphique empilé"),
                              HTML("<br><br><br>"),
                              withSpinner(plotlyOutput("graph2", width = "83%", height = "400px")),
                              HTML("<br><br><br>"),
                              withSpinner(plotlyOutput("graph1", width = "83%", height = "400px")),
                              bsPopover("graph1","Effet marginal sur le revenu disponible d'une augmentation de revenu net",content = "<p>Variation de revenu disponible après prestations et impôts induite par une augmentation de revenu net (en % de cette augmentation).</p>"),
                              HTML("<br><br>")
                        )
                      
                      )
                 )
   
   ) # fin de la fonction Ui
   


