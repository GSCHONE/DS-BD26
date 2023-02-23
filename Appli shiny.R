#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("shinythemes")
#install.packages("rintrojs")

library(shiny)
library(ggplot2)
library(shinythemes)
library(rintrojs)

choiceschampionnat<-c("championnat 1",
           "championnat 2",
           "championnat 3")

choicesteam <- names(table(unique(ListMatch$home_team_long_name)))

#### creation de table intermediaire
table_inter_team= 
     Team %>% filter(.$team_long_name %in% names(table(unique(ListMatch$home_team_long_name)))) %>% 
     arrange(.$team_api_id) %>% 
     select(c(4,2))

table_attibute_team=
     Team_Attributes_L[which(Team_Attributes_L$Saison=="2016/2017"),] %>% 
     left_join(table_inter_team,.,by="team_api_id")

# Team_Attributes_L %>% 
#      filter(.$Saison =="2016/2017")

### début pour recuperer les données plus spécifiques aux joueurs
res_final=c()
colname=c("team_api_id","player_1","player_2","player_3","player_4","player_5","player_6",
          "player_7","player_8","player_9","player_10","player_11")

for (n in table_inter_team$team_api_id){
     #creation de la table temporaire
     selecteur=table_inter_team %>% filter(.$team_api_id==n) %>% select(c(2))
     team1_part1=
          Match_PL %>% filter(.$home_team_api_id == selecteur$team_api_id) %>%
          select(c(8,12:22))
     team1_part2=
          Match_PL %>% filter(.$away_team_api_id == selecteur$team_api_id) %>%
          select(c(9,23:33))
     
     colnames(team1_part1)=colname
     colnames(team1_part2)=colname
     team1_final=rbind(team1_part1,team1_part2)
     
     res=c()
     for (i in names(team1_final)){
          temp=as.data.frame(table(team1_final[,i]))
          temp2=as.integer(as.character(temp[which(temp$Freq==max(temp$Freq)),][1,1]))
          print(temp2)
          res=cbind(res,temp2)
     }
     colnames(res)=colname
     
     res=as.data.frame(res)
     
     if (n==names(team1_final)[1]){
          res_final=res
     }
     else{
          res_final=rbind(res_final,res)
     }
}
###à la fin de ce code là on a récuperer les id des équipes avec leur onze fort
###on cherche maitenant a recuperer leur nom ainsi que leur score de base et leur potentiel

temp_table=res_final %>% select(c(1))

for (p in 1:11){
     stat_play=Player_Attributes_L
     stat_play=stat_play[which(stat_play$Saison=="2016/2017"),]
     names(stat_play)<-paste0("player_",p,"_",names(stat_play))
     name_play=Player
     names(name_play)<-paste0("player_",p,"_",names(name_play))
     
     name_play=
          name_play %>% select(c(2,3))
     
     res_final_temp=res_final
     names(res_final_temp)<-paste0(names(res_final_temp),"_player_api_id")
     
     # var_tmp_tab1=paste0("player_",p)
     var_tmp_tab2=paste0("player_",p,"_player_api_id")
     # vec_tmp_join=c(var_tmp_tab1,var_tmp_tab2)
     
     t1=res_final_temp %>% 
          select(c(p+1)) %>% 
          left_join(.,stat_play,
                    by=var_tmp_tab2) %>% 
          select(-c(2)) %>% 
          left_join(.,name_play,by=var_tmp_tab2)
     
     temp_table=cbind(temp_table,t1)
}

res_final=temp_table

###certaines donnees sont manquantes. On va mettre les score manquant par la moyenne des joueurs

table_attribute_team_joueur=temp_table
table_attribute_team_joueur=table_attribute_team_joueur %>% select(ends_with(c("potential","rating")))
table_attribute_team_joueur_temp=temp_table
for (y in 1:dim(table_attribute_team_joueur)[2]){
     
     varname_save=names(table_attribute_team_joueur[y])
     t1=table_attribute_team_joueur[y]
     colnames(t1)="norm"
     
     t1_na=is.na(table_attribute_team_joueur[y])
     colnames(t1_na)="bool"
     t1=cbind(t1,t1_na)
     t1[which(t1$bool),1]=as.integer(mean(t1$norm,na.rm = TRUE))
     
     colnames(t1)=c(paste0(varname_save,"_fin"),"bool")
     
     table_attribute_team_joueur_temp=cbind(table_attribute_team_joueur_temp,t1[1])
}
table_attribute_team_joueur = table_attribute_team_joueur_temp %>% select(-ends_with(c("potential","rating")))
###ici on a donc récupéré l'ensemble des nom des joueurs avec quelques attributs


#ajouter une partie pour mettre plus de la réactiviter à ce niveau là avec probablement passage
#de cela au sein de l'UI
choicesjoueurdom <- names(table(unique(Player$player_name)))

choicesjoueurext <- names(table(unique(Player$player_name)))




ui =  function(request) {
  fluidPage(introjsUI(),
            theme = shinytheme("united"),
            headerPanel('Maquette shiny v2'),
            br(),
            p("Vous pouvez utiliser ce bouton pour voir comment le shiny fonctionne"),
            actionButton("tour","ApperÃ§ue", class = "btn btn-primary btn-lg"),
            hr(),
            
            #### Panel choix de l'equipe dom
            fluidPage(
              fluidRow(
                column(4,
                fluidRow(
                  column(8,
                    introBox(
                      uiOutput('Allchoicesteam1'),
                      data.step = 1,
                      data.intro = "Utilisez cette partie pour faire vos choix gÃ©nÃ©raux sur l'Ã©quipe domicile"
                    )
                  ),
              
              #### Panel display des joueurs de l'equipe dom
                  column(4,
                    introBox(
                      uiOutput('displayteam1'),
                      data.step = 4,
                      data.intro = "Cette partie vous montre les joueurs au sein de l'Ã©quipe domicile. Vous pouvez aussi selectionner vos joueurs"
                    )
                  )
                )
              ),
              
              ####################
              ######################## Panel centrale
              ####################
              column(4,
                
                fluidRow(
                    p("Ce sera le main panel")
                  )
              ),
              
            
            
            #### Panel choix de l'equipe exter
              column(4,
                fluidRow(
                  column(4,
                         introBox(
                           uiOutput('displayteam2'),
                           data.step = 6,
                           data.intro = "Cette partie vous montre les joueurs au sein de l'Ã©quipe exterieur. Vous pouvez aussi selectionner vos joueurs"
                         )
                  ),
                  
                  #### Panel display des joueurs de l'equipe exter
                  column(8,
                         introBox(
                           uiOutput('Allchoicesteam2'),
                           data.step = 7,
                           data.intro = "Utilisez cette partie pour faire vos choix gÃ©nÃ©raux sur l'Ã©quipe extÃ©rieur"
                         )
                  )
                )
              )
            )
          )
        )
}
#Server
server = function(input, output, session){
  
  
  #Tour 
  observeEvent(input$tour,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back"
               )))
  
  
  ############################
  ############################
  ############################
  #Tab championnat et Ã©quipe domicile
  output$Allchoicesteam1 <- renderUI(
      tabPanel(h3("Choix de championnat"),
               introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
                        data.step = 2,
                        data.intro = "Vous pouvez choisir le championnat de l'Ã©quipe Ã  domicile"),
               hr(),
               h3("Choix Ã©quipe domicile",align="center"),
               introBox(fluidRow(column(12,selectInput("Equipe", "Equipe", choicesteam))),
                        data.step = 3,
                        data.intro = "Vous pouvez choisir l'Ã©quipe Ã  domicile"),
               fluidRow(column(6,offset=3,h3("logo1"))),
               fluidRow(column(9,offset=1,fluidRow(
                          column(4,textInput(" ", "Att", "84",width='400px')),
                          column(4,textInput(" ", "Mid", "84",width='400px')),
                          column(4,textInput(" ", "Def", "84",width='400px'))
                        ))),

      )
  ) 
  
  #Tab joueurs de l'Ã©quipe domicile
  output$displayteam1 <- renderUI(
    tabPanel(br(),
             br(),
             br(),
             br(),
             introBox(fluidRow(column(12,selectInput("Joueur dom 1", "Joueur 1", choicesjoueurdom, selected = choicesjoueurdom[1]))),
                      data.step = 5,
                      data.intro = "Si vous voulez changer vos joueurs, vous pouvez le faire ici"),
             
             fluidRow(column(12,selectInput("Joueur dom 2", "Joueur 2", choicesjoueurdom, selected = choicesjoueurdom[2]))),
             fluidRow(column(12,selectInput("Joueur dom 3", "Joueur 3", choicesjoueurdom, selected = choicesjoueurdom[3]))),
             fluidRow(column(12,selectInput("Joueur dom 4", "Joueur 4", choicesjoueurdom, selected = choicesjoueurdom[4]))),
             fluidRow(column(12,selectInput("Joueur dom 5", "Joueur 5", choicesjoueurdom, selected = choicesjoueurdom[5]))),
             fluidRow(column(12,selectInput("Joueur dom 6", "Joueur 6", choicesjoueurdom, selected = choicesjoueurdom[6]))),
             fluidRow(column(12,selectInput("Joueur dom 7", "Joueur 7", choicesjoueurdom, selected = choicesjoueurdom[7]))),
             fluidRow(column(12,selectInput("Joueur dom 8", "Joueur 8", choicesjoueurdom, selected = choicesjoueurdom[8]))),
             fluidRow(column(12,selectInput("Joueur dom 9", "Joueur 9", choicesjoueurdom, selected = choicesjoueurdom[9]))),
             fluidRow(column(12,selectInput("Joueur dom 10", "Joueur 10", choicesjoueurdom, selected = choicesjoueurdom[10]))),
             fluidRow(column(12,selectInput("Joueur dom 11", "Joueur 11", choicesjoueurdom, selected = choicesjoueurdom[11])))
    )
  ) 
  
  ############################
  ############################
  ############################
  #Tab championnat et Ã©quipe exterieur
  output$Allchoicesteam2 <- renderUI(
    tabPanel(h3("Choix de championnat"),
             introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
                      data.step = 6,
                      data.intro = "Vous pouvez choisir le championnat de l'Ã©quipe Ã  l'extÃ©rieur"),
             hr(),
             h3("Choix Ã©quipe l'extÃ©rieur",align="center"),
             introBox(fluidRow(column(12,selectInput("Equipe", "Equipe", choicesteam))),
                      data.step = 7,
                      data.intro = "Vous pouvez choisir l'Ã©quipe Ã  l'exterieur"),
             fluidRow(column(6,offset=3,h3("logo2"))),
             fluidRow(column(9,offset=1,fluidRow(
               column(4,textInput(" ", "Att", "84",width='400px')),
               column(4,textInput(" ", "Mid", "84",width='400px')),
               column(4,textInput(" ", "Def", "84",width='400px'))
             ))),
             
    )
  ) 
  
  #Tab joueurs de l'Ã©quipe exterieur
  output$displayteam2 <- renderUI(
    tabPanel(br(),
             br(),
             br(),
             br(),
             introBox(fluidRow(column(12,selectInput("Joueur ext 1", "Joueur 1", choicesjoueurext, selected = choicesjoueurext[1]))),
                      data.step = 8,
                      data.intro = "Si vous voulez changer vos joueurs, vous pouvez le faire ici"),
             
             fluidRow(column(12,selectInput("Joueur ext 2", "Joueur 2", choicesjoueurext, selected = choicesjoueurext[2]))),
             fluidRow(column(12,selectInput("Joueur ext 3", "Joueur 3", choicesjoueurext, selected = choicesjoueurext[3]))),
             fluidRow(column(12,selectInput("Joueur ext 4", "Joueur 4", choicesjoueurext, selected = choicesjoueurext[4]))),
             fluidRow(column(12,selectInput("Joueur ext 5", "Joueur 5", choicesjoueurext, selected = choicesjoueurext[5]))),
             fluidRow(column(12,selectInput("Joueur ext 6", "Joueur 6", choicesjoueurext, selected = choicesjoueurext[6]))),
             fluidRow(column(12,selectInput("Joueur ext 7", "Joueur 7", choicesjoueurext, selected = choicesjoueurext[7]))),
             fluidRow(column(12,selectInput("Joueur ext 8", "Joueur 8", choicesjoueurext, selected = choicesjoueurext[8]))),
             fluidRow(column(12,selectInput("Joueur ext 9", "Joueur 9", choicesjoueurext, selected = choicesjoueurext[9]))),
             fluidRow(column(12,selectInput("Joueur ext 10", "Joueur 10", choicesjoueurext, selected = choicesjoueurext[10]))),
             fluidRow(column(12,selectInput("Joueur ext 11", "Joueur 11", choicesjoueurext, selected = choicesjoueurext[11])))
    )
  ) 
  
}
#Run

shinyApp(ui=ui, server=server, enableBookmarking = "url")



























