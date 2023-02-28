if (interactive()) {

  # Display a modal that requires valid input before continuing.
  shinyApp(
    ui = basicPage(
      actionButton("show", "Show modal dialog"),
      selectInput("test", "Choose data set",
                  choices = "")
    ),
    
    server = function(input, output,session) {
      # reactiveValues object for storing current data set.
      vals <- reactiveValues(data = "")
      
      # Return the UI for a modal dialog with data selection input. If 'failed' is
      # TRUE, then display a message that the previous value was invalid.
      dataModal <- function(failed = FALSE) {
        modalDialog(
          selectInput("test1", "Choose data set",
                   choices = stat_player_saison_actu$player_name),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          )
        )
      }
      
      # Show modal when button is clicked.
      observeEvent(input$show, {
        showModal(dataModal())
      })
      
      # When OK button is pressed, attempt to load the data set. If successful,
      # remove the modal. If not show another modal, but this time with a failure
      # message.
      observeEvent(input$ok, {
        # Check that data object exists and is data frame.
        
        vals$data <- input$test1
        updateSelectInput(session,"test", label="Choose data set",choices = stat_player_saison_actu$player_name , selected = vals$data)
        removeModal()
        

      })
      
      # Display information about selected data
      output$yolo <- renderUI(
        
      )
    }
  )
}
