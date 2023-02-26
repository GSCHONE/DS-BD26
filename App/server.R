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
  

  observeEvent(input$Equipe_dom,{
       
       tible_joueur_dom=
            temp_name_equipe %>% filter(team_long_name==input$Equipe_dom) %>% select(c(14:24))
       
       temp=c()
       for (i in 1:11){
            temp=append(temp,tible_joueur_dom[[i]])
       }
       tible_joueur_dom=temp
       
       for (i in 1:11){
            updateSelectInput(session, paste0("Joueur_dom_",i),label =paste0("Joueur ",i),choices = c(tible_joueur_dom,"Autres"),selected=tible_joueur_dom[i])
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
             introBox(fluidRow(column(12,selectInput("Championnat", "Championnat", choiceschampionnat))),
                      data.step = 6,
                      data.intro = "Vous pouvez choisir le championnat de l'équipe à l'extérieur"),
             hr(),
             h3("Choix équipe l'extérieur",align="center"),
             introBox(fluidRow(column(12,selectInput("Equipe_ext", "Equipe", choicesteam))),
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

  
  observeEvent(input$Equipe_ext,{
       
       tible_joueur_dom=
            temp_name_equipe %>% filter(team_long_name==input$Equipe_ext) %>% select(c(14:24))
       
       temp=c()
       for (i in 1:11){
            temp=append(temp,tible_joueur_dom[[i]])
       }
       tible_joueur_dom=temp
       
       for (i in 1:11){
            updateSelectInput(session, paste0("Joueur_ext_",i),label =paste0("Joueur ",i),choices = c(tible_joueur_dom,"Autres"),selected=tible_joueur_dom[i])
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
  
}