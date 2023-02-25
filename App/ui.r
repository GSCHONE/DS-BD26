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
            #     column(4,
            #            fluidRow(
            #              column(4,
            #                     introBox(
            #                       uiOutput('displayteam2'),
            #                       data.step = 6,
            #                       data.intro = "Cette partie vous montre les joueurs au sein de l'équipe exterieur. Vous pouvez aussi selectionner vos joueurs"
            #                     )
            #              ),
            #              
            #              #### Panel display des joueurs de l'equipe exter
            #              column(8,
            #                     introBox(
            #                       uiOutput('Allchoicesteam2'),
            #                       data.step = 7,
            #                       data.intro = "Utilisez cette partie pour faire vos choix généraux sur l'équipe extérieur"
            #                     )
            #              )
            #            )
            #     )
              )
            )
  )
}