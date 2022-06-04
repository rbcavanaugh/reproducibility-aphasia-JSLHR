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
      
      values$outcome = ifelse(input$outcome, "count", "accuracy")
      values$participant1 = input$participant1
      values$participant2 = input$participant2
      values$condition1 = input$condition1
      values$condition2 = input$condition2
      values$itemType1 = input$itemType1
      values$itemType2 = input$itemType2
      values$adjust = ifelse(input$adjust=="t", TRUE, FALSE)
      values$phoneme = ifelse(input$phoneme=="across", TRUE, FALSE)
      values$collapse = ifelse(input$phoneme=="across", TRUE, FALSE)
      values$all = ifelse(input$all=="t", TRUE, FALSE)
      values$tau = input$tau
      values$ok = "ok"
    })
    

    
    output$plot1 <- renderPlot({
      req(values$ok)
       plotdat1 = get_plotDat(v=values, "1")
       sced_plot(plotdat1, cap = FALSE, all = values$all)
    })
    
    output$plot2 <- renderPlot({
      plotdat2 = get_plotDat(v=values, "2")
      sced_plot(plotdat2, cap = TRUE, all = values$all)
    })
    
    output$t1 <- function(){
      get_table(values$participant1,
                values$condition1,
                values$itemType1,
                values$adjust,
                values$all,
                values$tau,
                values$phoneme)
    }
    
    output$t2 <- function(){
      get_table(values$participant2,
                values$condition2,
                values$itemType2,
                values$adjust,
                values$all,
                values$tau,
                values$phoneme)
    }
    
    showModal(modalDialog(
      title = "Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial",
      includeMarkdown("details.md"),
      easyClose = TRUE,
      size = "xl", 
      footer = modalButton("Get Started"),
    ))
    
    observeEvent(input$about, {
      showModal(modalDialog(
        title = "Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial",
        includeMarkdown("details.md"),
        easyClose = TRUE,
        size = "xl"
      ))
    })
    
    observeEvent(input$matrix, {
      showModal(modalDialog(
        title = "Scatterplots and correlations bewteen effect sizes",
        div(tags$img(src="p3.png", width = "90%"),style="text-align: center;"),
        easyClose = TRUE,
        size = "l"
      ))
    })
    
    observeEvent(input$shuffle,{
      updateSelectInput(session=session, "participant1", selected = paste0("P", sample(seq(1, 20, 1), 1)))
      updateRadioButtons(session=session,"condition1", selected = sample(c("blocked", "random"), 1))
      updateSelectInput(session=session,"participant2", selected = paste0("P", sample(seq(1, 20, 1), 1)))
      updateRadioButtons(session=session,"condition2", selected = sample(c("blocked", "random"), 1))
    })
    

})
