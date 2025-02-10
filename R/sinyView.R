##' Function to visualice the outcome of a remRats analysis
##'
##' This function open a shiny app where the different output tables are shown
##' @title shinyView
##' @param popEst 
##' @return open a shiny app in a web browser
##' @author Fer Arce
shinyView <- function(popEst){
# Define UI
ui <- shiny::fluidPage(
    shiny::titlePanel("summary of Removal Trapping Experiment"),
    shiny::tags$div(
             style = 'max-width: 800px; margin: auto;',
             shiny::tabsetPanel(
                 shiny::tabPanel("Raw data", DT::dataTableOutput("table1")),
                 shiny::tabPanel("Population estimate", DT::dataTableOutput("table2")),
                 shiny::tabPanel("% of population trapped", DT::dataTableOutput("table3"))
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
    
    
    shiny::shinyApp(ui = ui, server = server)
}

##shinyView(popEst)
