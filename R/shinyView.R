##' Function to visualice the outcome of a remRats analysis
##'
##' This function open a shiny app where the different output tables and time series plots generated are shown.
##' @title shinyView
##' @param popEst an object of class ****
##' @return open a shiny app in a web browser with the data analysis
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
    # % of population trapped tab
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
  }
  
  shinyApp(ui = ui, server = server)
}
