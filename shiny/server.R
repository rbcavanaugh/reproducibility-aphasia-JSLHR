#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    values = reactiveValues()
    
    observe({
      values$collapse = input$collapse
      values$outcome = input$outcome
      values$participant1 = input$participant1
      values$participant2 = input$participant2
      values$condition1 = input$condition1
      values$condition2 = input$condition2
      values$itemType1 = input$itemType1
      values$itemType2 = input$itemType2
      values$ok = "ok"
    })
    

    
    output$plot1 <- renderPlot({
      req(values$ok)
       plotdat1 = get_plotDat(v=values, "1")
       sced_plot(plotdat1)
    })
    
    output$plot2 <- renderPlot({
      plotdat2 = get_plotDat(v=values, "2")
      sced_plot(plotdat2)
    })
    
    output$t1 <- renderUI({
      get_table(values$participant1, values$condition1, values$itemType1)
    })
    
    output$t2 <- renderUI({
      get_table(values$participant2, values$condition2, values$itemType2)
    })
    

})
