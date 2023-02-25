tible_joueur_dom=data.frame()
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
             h3("Choix équipe domicile",align="center"),
             introBox(fluidRow(column(12,selectInput("Equipe_dom", "Equipe", choicesteam))),
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
  
  
  tible_joueur_dom<-reactiveValues(tible_joueur_dom=tible_joueur_dom)
  
  
  observeEvent(input$equipe_dom,{
       print("yolo")
       print(c(tible_joueur_dom,"Autres"))
       tible_joueur_dom=
            temp_name_equipe %>% filter(team_long_name==input$equipe_dom) %>% select(c(14:24))
       
       temp=c()
       for (i in 1:11){
            temp=append(temp,tible_joueur_dom[[i]])
       }
       tible_joueur_dom=temp
       print(c(tible_joueur_dom,"Autres"))
  })
  
  #Tab joueurs de l'équipe domicile
  output$displayteam1 <- renderUI(
    tabPanel(br(),
             br(),
             br(),
             br(),
             introBox(fluidRow(column(12,
                      updateSelectInput(session, "Joueur_dom_1",label ="Joueur 1",choices = c(tible_joueur_dom,"Autres"),selected=tible_joueur_dom[1]))),
                      data.step = 5,
                      data.intro = "Si vous voulez changer vos joueurs, vous pouvez le faire ici"),

             # fluidRow(column(12,selectInput("Joueur dom 2", "Joueur 2", choicesjoueurdom, selected = choicesjoueurdom[2]))),
             # fluidRow(column(12,selectInput("Joueur dom 3", "Joueur 3", choicesjoueurdom, selected = choicesjoueurdom[3]))),
             # fluidRow(column(12,selectInput("Joueur dom 4", "Joueur 4", choicesjoueurdom, selected = choicesjoueurdom[4]))),
             # fluidRow(column(12,selectInput("Joueur dom 5", "Joueur 5", choicesjoueurdom, selected = choicesjoueurdom[5]))),
             # fluidRow(column(12,selectInput("Joueur dom 6", "Joueur 6", choicesjoueurdom, selected = choicesjoueurdom[6]))),
             # fluidRow(column(12,selectInput("Joueur dom 7", "Joueur 7", choicesjoueurdom, selected = choicesjoueurdom[7]))),
             # fluidRow(column(12,selectInput("Joueur dom 8", "Joueur 8", choicesjoueurdom, selected = choicesjoueurdom[8]))),
             # fluidRow(column(12,selectInput("Joueur dom 9", "Joueur 9", choicesjoueurdom, selected = choicesjoueurdom[9]))),
             # fluidRow(column(12,selectInput("Joueur dom 10", "Joueur 10", choicesjoueurdom, selected = choicesjoueurdom[10]))),
             # fluidRow(column(12,selectInput("Joueur dom 11", "Joueur 11", choicesjoueurdom, selected = choicesjoueurdom[11])))
    )
  ) 
  
  # ############################
  # ############################
  # ############################
  # #Tab championnat et équipe exterieur
  # output$Allchoicesteam2 <- renderUI(
  #   tabPanel(h3("Choix de championnat"),
  #            introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
  #                     data.step = 6,
  #                     data.intro = "Vous pouvez choisir le championnat de l'équipe à l'extérieur"),
  #            hr(),
  #            h3("Choix équipe l'extérieur",align="center"),
  #            introBox(fluidRow(column(12,selectInput("Equipe", "Equipe", choicesteam))),
  #                     data.step = 7,
  #                     data.intro = "Vous pouvez choisir l'équipe à l'exterieur"),
  #            fluidRow(column(6,offset=3,h3("logo2"))),
  #            fluidRow(column(9,offset=1,fluidRow(
  #              column(4,textInput(" ", "Att", "84",width='400px')),
  #              column(4,textInput(" ", "Mid", "84",width='400px')),
  #              column(4,textInput(" ", "Def", "84",width='400px'))
  #            ))),
  #            
  #   )
  # ) 
  # 
  # #Tab joueurs de l'équipe exterieur
  # output$displayteam2 <- renderUI(
  #   tabPanel(br(),
  #            br(),
  #            br(),
  #            br(),
  #            introBox(fluidRow(column(12,selectInput("Joueur ext 1", "Joueur 1", choicesjoueurext, selected = choicesjoueurext[1]))),
  #                     data.step = 8,
  #                     data.intro = "Si vous voulez changer vos joueurs, vous pouvez le faire ici"),
  #            
  #            fluidRow(column(12,selectInput("Joueur ext 2", "Joueur 2", choicesjoueurext, selected = choicesjoueurext[2]))),
  #            fluidRow(column(12,selectInput("Joueur ext 3", "Joueur 3", choicesjoueurext, selected = choicesjoueurext[3]))),
  #            fluidRow(column(12,selectInput("Joueur ext 4", "Joueur 4", choicesjoueurext, selected = choicesjoueurext[4]))),
  #            fluidRow(column(12,selectInput("Joueur ext 5", "Joueur 5", choicesjoueurext, selected = choicesjoueurext[5]))),
  #            fluidRow(column(12,selectInput("Joueur ext 6", "Joueur 6", choicesjoueurext, selected = choicesjoueurext[6]))),
  #            fluidRow(column(12,selectInput("Joueur ext 7", "Joueur 7", choicesjoueurext, selected = choicesjoueurext[7]))),
  #            fluidRow(column(12,selectInput("Joueur ext 8", "Joueur 8", choicesjoueurext, selected = choicesjoueurext[8]))),
  #            fluidRow(column(12,selectInput("Joueur ext 9", "Joueur 9", choicesjoueurext, selected = choicesjoueurext[9]))),
  #            fluidRow(column(12,selectInput("Joueur ext 10", "Joueur 10", choicesjoueurext, selected = choicesjoueurext[10]))),
  #            fluidRow(column(12,selectInput("Joueur ext 11", "Joueur 11", choicesjoueurext, selected = choicesjoueurext[11])))
  #   )
  # ) 
  
}