#install.packages("shiny")
#install.packages("ggplot2")
# install.packages("shinythemes")
# install.packages("rintrojs")

library(shiny)
library(ggplot2)
library(shinythemes)
library(rintrojs)
library(tidyverse)

setwd("C:/Users/luffy/Desktop/Projet_datascience/DS-BD26/App")
source("./scripts/chargement_donnees_team.R")
source("./scripts/chargement_donnees_player.R")
source("./scripts/chargement_Player.R")
source("./scripts/chargement_Team.R")
source("./scripts/chargement_ListMatch.R")
source("./scripts/chargement_Match_PL.R")
source("./scripts/traitement_donnees.R")
setwd("C:/Users/luffy/Desktop/Projet_datascience/DS-BD26")


choiceschampionnat<-c("championnat 1",
                      "championnat 2",
                      "championnat 3")

choicesteam <- names(table(unique(stat_player_saison_actu$team_long_name)))

### fabrication de la table qui permettra de mettre les attributs des équipes sur la dernière saison
# table_transi=
#   Team %>% filter(team_long_name %in% names(table(unique(ListMatch$home_team_long_name)))) %>% 
#   select(c(4,2)) %>% 
#   arrange(.$team_api_id)
# 
# 
# table_attribut_team=
#   Team_Attributes_L %>% filter(team_api_id %in% table_transi$team_api_id) %>% 
#   filter(Saison =="2016/2017") %>% 
#   arrange(.$team_api_id) %>% 
#   right_join(table_transi,.)



#### pas vu comment faire le lien entre teams et équipe
#### probablement un passage à faire pour mettre dans l'ui pour en faire des choix interactifs. 
#### ajout d'un potentiel choix "autre" après ça qui arrive sur un recherche en fonction de ce que tape l'utilisateur
choicesjoueurdom <- names(table(unique(Player$player_name)))

choicesjoueurext <- names(table(unique(Player$player_name)))

ui =  function(request) {
     fluidPage(introjsUI(),
               theme = shinytheme("united"),
               headerPanel('Maquette shiny v2'),
               br(),
               p("Vous pouvez utiliser ce bouton pour voir comment le shiny fonctionne"),
               actionButton("tour","Apperçue", class = "btn btn-primary btn-lg"),
               hr(),
               
               tabsetPanel(
                    tabPanel(
                         "Prédiction",
                         
                         #### Panel choix de l'equipe dom
                         fluidPage(
                              fluidRow(
                                   column(4,
                                          fluidRow(
                                               column(8,
                                                      introBox(
                                                           uiOutput('Allchoicesteam1'),
                                                           data.step = 1,
                                                           data.intro = "Utilisez cette partie pour faire vos choix généraux sur l'équipe domicile"
                                                      )
                                               ),
                                               
                                               #### Panel display des joueurs de l'equipe dom
                                               column(4,
                                                      introBox(
                                                           uiOutput('displayteam1'),
                                                           data.step = 4,
                                                           data.intro = "Cette partie vous montre les joueurs au sein de l'équipe domicile. Vous pouvez aussi selectionner vos joueurs"
                                                      )
                                               )
                                          )
                                   ),
                                   
                                   ####################
                                   ######################## Panel centrale
                                   ####################
                                   column(4,
                                          
                                          fluidRow(
                                               p("Ce sera le main panel"),
                                               img(src="Terrain.png"),
                                               br(),
                                               fluidRow(
                                                    column(6,offset=3,actionButton("match", "Simulation de match")))
                                          ),
                                   ),
                              
                              
                              
                              #### Panel choix de l'equipe exter
                              column(4,
                                     fluidRow(
                                          column(4,
                                                 introBox(
                                                      uiOutput('displayteam2'),
                                                      data.step = 6,
                                                      data.intro = "Cette partie vous montre les joueurs au sein de l'équipe exterieur. Vous pouvez aussi selectionner vos joueurs"
                                                 )
                                          ),
                                          
                                          #### Panel display des joueurs de l'equipe exter
                                          column(8,
                                                 introBox(
                                                      uiOutput('Allchoicesteam2'),
                                                      data.step = 7,
                                                      data.intro = "Utilisez cette partie pour faire vos choix généraux sur l'équipe extérieur"
                                                 )
                                          )
                                     )
                              )
                         )
                    )
                    
                    
               )
          )
     )

}

tible_joueur_dom=data.frame()
tible_joueur_dom<-reactiveValues(temp_name_equipe=tible_joueur_dom)

server = function(input, output, session){
     
     
     #Tour 
     observeEvent(input$tour,
                  introjs(session, options = list("nextLabel"="Next",
                                                  "prevLabel"="Back"
                  )))
     
     
     ############################
     ############################
     ############################
     #Tab championnat et équipe domicile
     output$Allchoicesteam1 <- renderUI(
          tabPanel(h3("Choix de championnat"),
                   # introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
                   #          data.step = 2,
                   #          data.intro = "Vous pouvez choisir le championnat de l'équipe à domicile"),
                   # hr(),
                   h3("Choix équipe domicile",align="center"),
                   introBox(fluidRow(column(12,selectInput("Equipe_dom", "Equipe", choicesteam))),
                            data.step = 2,
                            data.intro = "Vous pouvez choisir l'équipe à domicile"),
                   # fluidRow(column(6,offset=3,h3("logo1"))),
                   # fluidRow(column(9,offset=1,fluidRow(
                   #      column(4,textInput(" ", "Att", "84",width='400px')),
                   #      column(4,textInput(" ", "Mid", "84",width='400px')),
                   #      column(4,textInput(" ", "Def", "84",width='400px'))
                   # ))),
                   
          )
     ) 
     
     
     observeEvent(input$Equipe_dom,{
          
          tible_joueur_dom=
               temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,tible_joueur_dom[[i]])
          }
          tible_joueur_dom=temp
          
          for (i in 1:11){
               updateSelectInput(session, paste0("Joueur_dom_",i),label =paste0("Joueur ",i),choices = c(tible_joueur_dom,"Autre équipe"),selected=tible_joueur_dom[i])
          }
          
     })
     
     #Tab joueurs de l'équipe domicile
     output$displayteam1 <- renderUI(
          tabPanel(br(),
                   br(),
                   br(),
                   br(),
                   introBox(fluidRow(column(12,
                                            selectInput( "Joueur_dom_1",label ="Joueur 1",choices = ""))),
                            data.step = 5,
                            data.intro = "Si vous voulez changer vos joueurs, vous pouvez le faire ici"),
                   fluidRow(column(12,selectInput("Joueur_dom_2", "Joueur 2", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_3", "Joueur 3", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_4", "Joueur 4", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_5", "Joueur 5", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_6", "Joueur 6", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_7", "Joueur 7", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_8", "Joueur 8", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_9", "Joueur 9", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_10", "Joueur 10", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_dom_11", "Joueur 11", choices = "")))
          )
     ) 
     
     # ############################
     # ############################
     # ############################
     #Tab championnat et équipe exterieur
     output$Allchoicesteam2 <- renderUI(
          tabPanel(h3("Choix de championnat"),
                   # introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
                   #          data.step = 6,
                   #          data.intro = "Vous pouvez choisir le championnat de l'équipe à l'extérieur"),
                   # hr(),
                   h3("Choix équipe l'extérieur",align="center"),
                   introBox(fluidRow(column(12,selectInput("Equipe_ext", "Equipe", choicesteam))),
                            data.step = 6,
                            data.intro = "Vous pouvez choisir l'équipe à l'exterieur"),
                   # fluidRow(column(6,offset=3,h3("logo2"))),
                   # fluidRow(column(9,offset=1,fluidRow(
                   #      column(4,textInput(" ", "Att", "84",width='400px')),
                   #      column(4,textInput(" ", "Mid", "84",width='400px')),
                   #      column(4,textInput(" ", "Def", "84",width='400px'))
                   # ))),
                   
          )
     )
     
     
     observeEvent(input$Equipe_ext,{
          
          tible_joueur_dom=
               temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,tible_joueur_dom[[i]])
          }
          tible_joueur_dom=temp
          
          for (i in 1:11){
               updateSelectInput(session, paste0("Joueur_ext_",i),label =paste0("Joueur ",i),choices = c(tible_joueur_dom,"Autre équipe"),selected=tible_joueur_dom[i])
          }
          
     })
     
     
     #Tab joueurs de l'équipe exterieur
     output$displayteam2 <- renderUI(
          tabPanel(br(),
                   br(),
                   br(),
                   br(),
                   introBox(fluidRow(column(12,selectInput( "Joueur_ext_1",label ="Joueur 1",choices = ""))),
                            data.step = 8,
                            data.intro = "Si vous voulez changer vos joueurs, vous pouvez le faire ici"),
                   
                   fluidRow(column(12,selectInput("Joueur_ext_2", "Joueur 2", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_3", "Joueur 3", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_4", "Joueur 4", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_5", "Joueur 5", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_6", "Joueur 6", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_7", "Joueur 7", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_8", "Joueur 8", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_9", "Joueur 9", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_10", "Joueur 10", choices = ""))),
                   fluidRow(column(12,selectInput("Joueur_ext_11", "Joueur 11", choices = "")))
          )
     )
     
     
     ######################################################################################################
     ######################################################################################################
     ################################# Copier coller en folie pour ajouter du choix #######################
     #######################################pour les joueurs à domicile####################################
     ######################################################################################################
     ##### test d'ajout de tout les obseveEvents autres équipes
     # Return the UI for a modal dialog with data selection input. If 'failed' is
     # TRUE, then display a message that the previous value was invalid.
     # reactiveValues object for storing current data set.
     vals <- reactiveValues(data = "")
     list_joueur_dom_actu<- reactiveValues(data="")
     
     
     dataModal <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_1, {
          if (input$Joueur_dom_1=="Autre équipe"){
               showModal(dataModal())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal
          updateSelectInput(session,"Joueur_dom_1", label="Joueur 1",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal2 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal2", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok2", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_2, {
          if (input$Joueur_dom_2=="Autre équipe"){
               showModal(dataModal2())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok2, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal2,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal2
          updateSelectInput(session,"Joueur_dom_2", label="Joueur 2",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal3 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal3", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok3", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_3, {
          if (input$Joueur_dom_3=="Autre équipe"){
               showModal(dataModal3())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok3, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal3,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal3
          updateSelectInput(session,"Joueur_dom_3", label="Joueur 3",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal4 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal4", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok4", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_4, {
          if (input$Joueur_dom_4=="Autre équipe"){
               showModal(dataModal4())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok4, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal4,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal4
          updateSelectInput(session,"Joueur_dom_4", label="Joueur 4",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal5 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal5", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok5", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_5, {
          if (input$Joueur_dom_5=="Autre équipe"){
               showModal(dataModal5())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok5, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal5,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal5
          updateSelectInput(session,"Joueur_dom_5", label="Joueur 5",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal6 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal6", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok6", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_6, {
          if (input$Joueur_dom_6=="Autre équipe"){
               showModal(dataModal6())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok6, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal6,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal6
          updateSelectInput(session,"Joueur_dom_6", label="Joueur 6",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal7 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal7", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok7", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_7, {
          if (input$Joueur_dom_7=="Autre équipe"){
               showModal(dataModal7())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok7, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal7,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal7
          updateSelectInput(session,"Joueur_dom_7", label="Joueur 7",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal8 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal8", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok8", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_8, {
          if (input$Joueur_dom_8=="Autre équipe"){
               showModal(dataModal8())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok8, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal8,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal8
          updateSelectInput(session,"Joueur_dom_8", label="Joueur 8",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal9 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal9", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok9", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_9, {
          if (input$Joueur_dom_9=="Autre équipe"){
               showModal(dataModal9())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok9, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal9,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal9
          updateSelectInput(session,"Joueur_dom_9", label="Joueur 9",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     
     dataModal10 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal10", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok10", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_10, {
          if (input$Joueur_dom_10=="Autre équipe"){
               showModal(dataModal10())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok10, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal10,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal10
          updateSelectInput(session,"Joueur_dom_10", label="Joueur 10",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal11 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal11", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok11", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_dom_11, {
          if (input$Joueur_dom_11=="Autre équipe"){
               showModal(dataModal11())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok11, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal11,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal11
          updateSelectInput(session,"Joueur_dom_11", label="Joueur 11",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     ######################################################################################################
     ######################################################################################################
     ################################# Copier coller en folie pour ajouter du choix #######################
     ######################################pour les joueurs à l'extérieur##################################
     ######################################################################################################
     ##### test d'ajout de tout les obseveEvents autres équipes
     # Return the UI for a modal dialog with data selection input. If 'failed' is
     # TRUE, then display a message that the previous value was invalid.
     # reactiveValues object for storing current data set.
     
     dataModal200 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal200", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok200", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_1, {
          if (input$Joueur_ext_1=="Autre équipe"){
               showModal(dataModal200())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok200, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal200,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal200
          updateSelectInput(session,"Joueur_ext_1", label="Joueur 1",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal202 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal202", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok202", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_2, {
          if (input$Joueur_ext_2=="Autre équipe"){
               showModal(dataModal202())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok202, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal202,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal202
          updateSelectInput(session,"Joueur_ext_2", label="Joueur 2",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal203 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal203", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok203", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_3, {
          if (input$Joueur_ext_3=="Autre équipe"){
               showModal(dataModal203())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok203, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal203,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal203
          updateSelectInput(session,"Joueur_ext_3", label="Joueur 3",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal204 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal204", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok204", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_4, {
          if (input$Joueur_ext_4=="Autre équipe"){
               showModal(dataModal204())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok204, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal204,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal204
          updateSelectInput(session,"Joueur_ext_4", label="Joueur 4",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal205 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal205", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok205", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_5, {
          if (input$Joueur_ext_5=="Autre équipe"){
               showModal(dataModal205())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok205, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal205,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal205
          updateSelectInput(session,"Joueur_ext_5", label="Joueur 5",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal206 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal206", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok206", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_6, {
          if (input$Joueur_ext_6=="Autre équipe"){
               showModal(dataModal206())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok206, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal206,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal206
          updateSelectInput(session,"Joueur_ext_6", label="Joueur 6",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal207 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal207", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok207", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_7, {
          if (input$Joueur_ext_7=="Autre équipe"){
               showModal(dataModal207())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok207, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal207,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal207
          updateSelectInput(session,"Joueur_ext_7", label="Joueur 7",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal208 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal208", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok208", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_8, {
          if (input$Joueur_ext_8=="Autre équipe"){
               showModal(dataModal208())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok208, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal208,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal208
          updateSelectInput(session,"Joueur_ext_8", label="Joueur 8",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal209 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal209", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok209", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_9, {
          if (input$Joueur_ext_9=="Autre équipe"){
               showModal(dataModal209())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok209, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal209,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal209
          updateSelectInput(session,"Joueur_ext_9", label="Joueur 9",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     
     dataModal210 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal210", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok210", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_10, {
          if (input$Joueur_ext_10=="Autre équipe"){
               showModal(dataModal210())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok210, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal210,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal210
          updateSelectInput(session,"Joueur_ext_10", label="Joueur 10",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     
     dataModal211 <- function(failed = FALSE) {
          modalDialog(
               selectInput("modalglobal211", "Choose data set",
                           choices = stat_player_saison_actu$player_name),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok211", "OK")
               )
          )
     }
     # 
     # #choices autres
     observeEvent(input$Joueur_ext_11, {
          if (input$Joueur_ext_11=="Autre équipe"){
               showModal(dataModal211())
          }
          
     },ignoreInit = T)
     
     # When OK button is pressed, attempt to load the data set. If successful,
     # remove the modal. If not show another modal, but this time with a failure
     # message.
     observeEvent(input$ok211, {
          # Check that data object exists and is data frame.
          init_list_joueur=temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
          
          temp=c()
          for (i in 1:11){
               temp=append(temp,init_list_joueur[[i]])
          }
          init_list_joueur=temp
          init_list_joueur=append(init_list_joueur,"Autre équipe")
          
          list_joueur_dom_actu$data=init_list_joueur
          
          list_joueur_dom_actu$data=append(list_joueur_dom_actu$data,input$modalglobal210,after=length(list_joueur_dom_actu$data)-1)
          vals$data <- input$modalglobal211
          updateSelectInput(session,"Joueur_ext_11", label="Joueur 11",choices = list_joueur_dom_actu$data , selected = vals$data)
          removeModal()
     })
     #######################################
     #######################################
     # #choices autres
     resultat=c("Win domicile","nul","Win Exterieur")
     dataModal_res <- function(failed = FALSE) {
          modalDialog(
               p(sample(resultat,1)),
               footer = tagList(
                    modalButton("Cancel")
               )
          )
     }
     # 
     
     
     observeEvent(input$match, {
               showModal(dataModal_res())
          
     },ignoreInit = T)
     
     
     }

shinyApp(ui = ui, server = server)
