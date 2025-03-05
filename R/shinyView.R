##' Function to visualice the outcome of a remRats analysis
##'
##' This function open a shiny app where the different output tables and time series plots generated are shown. This Shiny app is interactive so the end user can decider project or no the model into nearby future to avaluate the expectations of future field effort. This function shouldf be called from inside the call of remRats.
##' @title shinyView
##' @param popEst an object of class 'fittedRemMLERec'.
##' @return open a shiny app in a web browser to visualize the data analysis and projections
##' @author Fer Arce
##' @export
shinyView <- function(popEst) {
  # Define UI
  ui <- fluidPage(
    titlePanel("Summary of Removal Trapping Experiment"),
    tags$div(
      style = 'max-width: 800px; margin: auto;',
      tabsetPanel(
        tabPanel("Raw data",
                 DT::dataTableOutput("table1"),
                 plotOutput("plot1")
                 ),
        tabPanel("Population estimate",
                 DT::dataTableOutput("table2"),
                 plotOutput("plot2")
                 ),
        tabPanel("% of population trapped",
                 DT::dataTableOutput("table3"),
                 plotOutput("plot3")
                 ),
        tabPanel("Population projection",
                 numericInput("num_input", "Select a number (0 to 3):", value = 0, min = 0, max = 3),
                 actionButton("run_btn", "Run"),
                 textOutput("result"),
                 DT::dataTableOutput("table4"),
                 plotOutput("plot4")
                 )
      )
    )
  )

  server <- function(input, output, session) {
                                        ## Raw data tab
      output$table1 <- DT::renderDataTable({
          DT::datatable(popEst$Nraw, colnames = c('Trapping occasion', 'Captures per occasion', 'Captures (cumulative)'))
      })
      output$plot1 <- renderPlot({
        df <- popEst$Nraw
        ggplot(df, aes(x = n.occ, y = Ncumu)) +
            geom_point(color = "blue", size = 3) +
            geom_line(color = "blue") +
            labs(x = "Trapping occasion", y = "Cumulative number of Catches") +
            theme_minimal() +
            theme(axis.title = element_text(size = rel(2)))
    })
    
    # Population estimate tab
    output$table2 <- DT::renderDataTable({
      DT::datatable(popEst$Nprops, colnames = c("Trapping occasion", "Population size estimate", "Standard error", "% (Lower ci)", "% (upper ci)"))
    })
    output$plot2 <- renderPlot({
      df <- popEst$Nest
      ggplot(df, aes(x = n.occ, y = Nestimate)) +
                        geom_ribbon(aes(ymin = Nlcl, ymax = Nucl), fill = "lightblue", alpha = 0.3) +
          geom_point(color = "darkgreen", size = 3) +
          geom_line(color = "darkgreen") +
          labs(x = "Trapping occasion", y = "Population estimate") +
            theme_minimal() +
            theme(axis.title = element_text(size = rel(2)))
    })
      ## % of population trapped tab
    output$table3 <- DT::renderDataTable({
      DT::datatable(popEst$Nprops, colnames = c("Trapping occasion", "Cumulative number of individuals", "% of the population sampled", "% (Lower ci)", "% (upper ci)"))
    })
    output$plot3 <- renderPlot({
      df <- popEst$Nprops
      ggplot(df, aes(x = n.occ, y = Pestimate)) +
                                  geom_ribbon(aes(ymin = Plcl, ymax = Pucl), fill = "lightblue", alpha = 0.3) +
          geom_point(color = "purple", size = 3) +
          geom_line(color = "purple") +
          labs(x = "Trapping occasion", y = "% of the population Sampled") +
            theme_minimal() +
            theme(axis.title = element_text(size = rel(2)))
    })
      ## Population proejction
      observeEvent(input$run_btn, {
                                        # Pass the selected number to the external function
          result <- projCatches(popEst, input$num_input)
          ## output$result <- renderText({
          ##     paste("Result of Projection: ", result)
          ## })
          output$table4 <- DT::renderDataTable({
              DT::datatable(result, colnames = c("projection","s.e "))
          })     
          output$plot4 <- renderPlot({
              df <- result
              ggplot(df, aes(x = n.occ, y = fit)) +
                  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "lightblue", alpha = 0.3) +
                  geom_point(color = "purple", size = 3) +
                  geom_line(color = "purple") +
                  labs(x = "Trapping occasion", y = "% of the population Sampled") +
                  theme_minimal() +
                  theme(axis.title = element_text(size = rel(2)))
          })
      })
      
  }
    
    shinyApp(ui = ui, server = server)
}
