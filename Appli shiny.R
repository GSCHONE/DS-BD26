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

choicesteam <- c("equipe 1",
                 "equipe 2",
                 "equipe 3")

choicesjoueurdom <- c("joueur 1",
                      "joueur 2",
                      "joueur 3",
                      "joueur 4",
                      "joueur 5",
                      "joueur 6",
                      "joueur 7",
                      "joueur 8",
                      "joueur 9",
                      "joueur 10",
                      "joueur 11")

choicesjoueurext <- c("joueur 1",
                      "joueur 2",
                      "joueur 3",
                      "joueur 4",
                      "joueur 5",
                      "joueur 6",
                      "joueur 7",
                      "joueur 8",
                      "joueur 9",
                      "joueur 10",
                      "joueur 11")

ui =  function(request) {
  fluidPage(introjsUI(),
            theme = shinytheme("united"),
            headerPanel('Maquette shiny v2'),
            br(),
            p("Vous pouvez utiliser ce bouton pour voir comment le shiny fonctionne"),
            actionButton("tour","Apperçue", class = "btn btn-primary btn-lg"),
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
  #Tab championnat et équipe domicile
  output$Allchoicesteam1 <- renderUI(
      tabPanel(h3("Choix de championnat"),
               introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
                        data.step = 2,
                        data.intro = "Vous pouvez choisir le championnat de l'équipe à domicile"),
               hr(),
               h3("Choix equipe domicile",align="center"),
               introBox(fluidRow(column(12,selectInput("Equipe", "Equipe", choicesteam))),
                        data.step = 3,
                        data.intro = "Vous pouvez choisir l'équipe à domicile"),
               fluidRow(column(6,offset=3,h3("logo1"))),
               fluidRow(column(9,offset=1,fluidRow(
                          column(4,textInput(" ", "Att", "84",width='400px')),
                          column(4,textInput(" ", "Mid", "84",width='400px')),
                          column(4,textInput(" ", "Def", "84",width='400px'))
                        ))),

      )
  ) 
  
  #Tab joueurs de l'équipe domicile
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
  #Tab championnat et équipe exterieur
  output$Allchoicesteam2 <- renderUI(
    tabPanel(h3("Choix de championnat"),
             introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
                      data.step = 6,
                      data.intro = "Vous pouvez choisir le championnat de l'équipe à l'extérieur"),
             hr(),
             h3("Choix équipe l'extérieur",align="center"),
             introBox(fluidRow(column(12,selectInput("Equipe", "Equipe", choicesteam))),
                      data.step = 7,
                      data.intro = "Vous pouvez choisir l'équipe à l'exterieur"),
             fluidRow(column(6,offset=3,h3("logo2"))),
             fluidRow(column(9,offset=1,fluidRow(
               column(4,textInput(" ", "Att", "84",width='400px')),
               column(4,textInput(" ", "Mid", "84",width='400px')),
               column(4,textInput(" ", "Def", "84",width='400px'))
             ))),
             
    )
  ) 
  
  #Tab joueurs de l'équipe exterieur
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



























