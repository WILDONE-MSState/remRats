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


          par(mar = c(5, 5, 2, 2))

plot(df$n.occ, df$Ncumu, 
     type = "n",
     xlab = "Trapping occasion", 
     ylab = "Cumulative number of Catches", 
     cex.lab = 1.5,
     bty = "n", 
     xaxt = "n", yaxt = "n"
     )
          axis(1, cex.axis = 1.2)
          axis(2, cex.axis = 1.2)\
          abline(h = pretty(df$Ncumu), col = "grey90", lwd = 1)
          abline(v = pretty(df$n.occ), col = "grey90", lwd = 1)
          lines(df$n.occ, df$Ncumu, col = "blue", lwd = 2)
          points(df$n.occ, df$Ncumu, col = "blue", pch = 16, cex = 1.5)

    })
    
    # Population estimate tab
    output$table2 <- DT::renderDataTable({
      DT::datatable(popEst$Nprops, colnames = c("Trapping occasion", "Population size estimate", "Standard error", "% (Lower ci)", "% (upper ci)"))
    })
    output$plot2 <- renderPlot({
      df <- popEst$Nest
      par(mar = c(5, 5, 2, 2))
      plot(df$n.occ, df$Nestimate,
           type = "n",
           xlab = "Trapping occasion",
           ylab = "Population estimate",
           cex.lab = 1.5,
           bty = "n",
           xaxt = "n", yaxt = "n",
           ylim = range(df$Nlcl, df$Nucl, na.rm = TRUE))
      axis(1, cex.axis = 1.2)
      axis(2, cex.axis = 1.2)
      abline(h = pretty(df$Nestimate), col = "grey90", lwd = 1)
      abline(v = pretty(df$n.occ), col = "grey90", lwd = 1)

      polygon(c(df$n.occ, rev(df$n.occ)),
              c(df$Nlcl, rev(df$Nucl)),
              col = adjustcolor("lightblue", alpha.f = 0.3),
              border = NA)
      
      lines(df$n.occ, df$Nestimate, col = "darkgreen", lwd = 2)
      points(df$n.occ, df$Nestimate, col = "darkgreen", pch = 16, cex = 1.5)

    })
      ## % of population trapped tab
    output$table3 <- DT::renderDataTable({
      DT::datatable(popEst$Nprops, colnames = c("Trapping occasion", "Cumulative number of individuals", "% of the population sampled", "% (Lower ci)", "% (upper ci)"))
    })
    output$plot3 <- renderPlot({
        df <- popEst$Nprops

# Set up plotting area
        par(mar = c(5, 5, 2, 2))

# Empty plot to lay out grid and ribbon
        plot(df$n.occ, df$Pestimate,
             type = "n",
             xlab = "Trapping occasion",
             ylab = "% of the population Sampled",
             cex.lab = 1.5,
             bty = "n",
             xaxt = "n", yaxt = "n",
             ylim = range(df$Plcl, df$Pucl, na.rm = TRUE))

# Axes
        axis(1, cex.axis = 1.2)
        axis(2, cex.axis = 1.2)

# Grid lines
        abline(h = pretty(df$Pestimate), col = "grey90", lwd = 1)
        abline(v = pretty(df$n.occ), col = "grey90", lwd = 1)

# Confidence ribbon
        polygon(c(df$n.occ, rev(df$n.occ)),
                c(df$Plcl, rev(df$Pucl)),
                col = adjustcolor("lightblue", alpha.f = 0.3),
                border = NA)

# Add points and line
        lines(df$n.occ, df$Pestimate, col = "purple", lwd = 2)
        points(df$n.occ, df$Pestimate, col = "purple", pch = 16, cex = 1.5)
        
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
              
                                        # Set up margins for large axis labels
              par(mar = c(5, 5, 2, 2))
              
                                        # Empty plot with appropriate y-axis range
              plot(df$n.occ, df$fit,
                   type = "n",
                   xlab = "Trapping occasion",
                   ylab = "% of the population Sampled",
                   cex.lab = 1.5,
                   bty = "n",
                   xaxt = "n", yaxt = "n",
                   ylim = range(df$lcl, df$ucl, na.rm = TRUE))
              
                                        # Add axes
              axis(1, cex.axis = 1.2)
              
                                        # Y-axis with % labels
              yticks <- pretty(df$fit)
              axis(2, at = yticks, labels = paste0(round(yticks * 100), "%"), cex.axis = 1.2)
              
                                        # Add grid lines
              abline(h = yticks, col = "grey90", lwd = 1)
              abline(v = pretty(df$n.occ), col = "grey90", lwd = 1)
              
                                        # Add confidence ribbon
              polygon(c(df$n.occ, rev(df$n.occ)),
                      c(df$lcl, rev(df$ucl)),
                      col = adjustcolor("lightblue", alpha.f = 0.3),
                      border = NA)
              
                                        # Add points and line
              lines(df$n.occ, df$fit, col = "purple", lwd = 2)
              points(df$n.occ, df$fit, col = "purple", pch = 16, cex = 1.5)
              
          })
      })
      
  }
    
    shinyApp(ui = ui, server = server)
}
