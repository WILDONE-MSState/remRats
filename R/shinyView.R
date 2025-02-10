##' Function to visualice the outcome of a remRats analysis
##'
##' This function open a shiny app where the different output tables recursively generated are shown.
##' @title shinyView
##' @param popEst 
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
    # Raw data tab
    output$table1 <- DT::renderDataTable({
    })
    output$plot1 <- renderPlot({
      df <- popEst$Nraw
      if(all(c("n.occ", "Ncumu") %in% names(df))) {
        ggplot(df, aes(x = n.occ, y = Ncumu)) +
          geom_point(color = "blue", size = 3) +
          geom_line(color = "blue") +
          labs(x = "n.occ", y = "Ncumu", title = "n.occ vs Ncumu (Raw Data)") +
          theme_minimal()
      } else {
        # If the required columns are missing, show a message
        plot.new()
        text(0.5, 0.5, "Columns 'n.occ' and/or 'Ncumu' not available", cex = 1.5)
      }
    })
    
    # Population estimate tab
    output$table2 <- DT::renderDataTable({
      popEst$Nest
    })
    output$plot2 <- renderPlot({
      df <- popEst$Nest
      if(all(c("n.occ", "Nestimate") %in% names(df))) {
        ggplot(df, aes(x = n.occ, y = Nestimate)) +
          geom_point(color = "darkgreen", size = 3) +
          geom_line(color = "darkgreen") +
          labs(x = "n.occ", y = "Nestimate", title = "n.occ vs Ncumu (Population Estimate)") +
          theme_minimal()
      } else {
        plot.new()
        text(0.5, 0.5, "Columns 'n.occ' and/or 'Ncumu' not available", cex = 1.5)
      }
    })
    # % of population trapped tab
    output$table3 <- DT::renderDataTable({
      DT::datatable(popEst$Nprops, colnames = c("Trapping occasion", "Cumulative number of individuals", "% of the population sampled", "% (Lower ci)", "% (upper ci)"))

    })
    output$plot3 <- renderPlot({
      df <- popEst$Nprops
      if(all(c("n.occ", "Pestimate") %in% names(df))) {
        ggplot(df, aes(x = n.occ, y = Pestimate)) +
          geom_point(color = "purple", size = 3) +
          geom_line(color = "purple") +
          labs(x = "n.occ", y = "Pestimate", title = "n.occ vs Ncumu (% of population trapped)") +
          theme_minimal()
      } else {
        plot.new()
        text(0.5, 0.5, "Columns 'n.occ' and/or 'Ncumu' not available", cex = 1.5)
      }
    })
  }
  
  shinyApp(ui = ui, server = server)
}



## shinyView <- function(popEst){
##                                         # Define UI
##     ui <- fluidPage(
##                      titlePanel("summary of Removal Trapping Experiment"),
##                      tags$div(
##                                      style = 'max-width: 800px; margin: auto;',
##                                      tabsetPanel(
##                                                 tabPanel("Raw data",
##                                                                 DT::dataTableOutput("table1")),
##                                                 tabPanel("Population estimate",
##                                                                 DT::dataTableOutput("table2")),
##                                                 tabPanel("% of population trapped",
##                                                                 DT::dataTableOutput("table3"))
##                                             )
##                                  )
##                  )
    
    
    
##     server <- function(input, output, session) {
##         output$table1 <- DT::renderDataTable({
##             popEst$Nraw
##         })  
##         output$table2 <- DT::renderDataTable({
##             popEst$Nest
##         })
##         output$table3 <- DT::renderDataTable({
##             popEst$Nprops
##         })
##     }
    
    
##     shinyApp(ui = ui, server = server)
## }

