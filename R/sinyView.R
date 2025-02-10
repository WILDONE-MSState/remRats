##' Function to visualice the outcome of a remRats analysis
##'
##' This function open a shiny app where the different output tables are shown
##' @title shinyView
##' @param popEst 
##' @return open a shiny app in a web browser
##' @author Fer Arce
shinyView <- function(popEst){
# Define UI
ui <- fluidPage(
    titlePanel("summary of Removal Trapping Experiment"),
    tags$div(
             style = 'max-width: 800px; margin: auto;',
             tabsetPanel(
                 tabPanel("Raw data", DT::dataTableOutput("table1")),
                 tabPanel("Population estimate", DT::dataTableOutput("table2")),
                 tabPanel("% of population trapped", DT::dataTableOutput("table3"))
             )
         )
)



server <- function(input, output, session) {
  output$table1 <- DT::renderDataTable({
    popEst$Nraw
  })  
  output$table2 <- DT::renderDataTable({
    popEst$Nest
  })
  output$table3 <- DT::renderDataTable({
    popEst$Nprops
  })
}


shinyApp(ui = ui, server = server)
}

shinyView(popEst)
