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
    
    modal = modalDialog(
      title = "Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial",
      includeMarkdown("details.md"),
      easyClose = TRUE,
      size = "l", 
      footer = modalButton("Get Started"),
    )
    
    showModal(modal)
    
    observeEvent(input$about, {
      showModal(modal)
    })
    
    observeEvent(input$matrix, {
      showModal(modalDialog(
        title = "Scatterplots and correlations bewteen effect sizes",
       # div(tags$img(src="p3.png", width = "90%"),style="text-align: center;"),
       # div(style="width:100%;text-align:center;height:60vh;",
       #   plotOutput("scatterplot", height = "100%")
       #   ),
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
    
    
    output$scatterplot <- renderPlot({
      
      if(values$adjust){
        col_glmm_percent = es_raw$glmm_percent_a
        col_glmm_logit = es_raw$glmm_logit_a
      } else {
        col_glmm_percent = es_raw$glmm_percent
        col_glmm_logit = es_raw$glmm_logit
      }
      
      # If using all baseline observations
      if(values$all){
          # SMD
          if(values$phoneme){
            col_smd = es_raw$SMD_phoneme_all
          } else {
            col_smd = es_raw$SMD_all
          }
        
          # TAU
          if(values$tau==0.33){
            col_tau = es_raw$Tau
          } else {
            col_tau = es_raw$Tau_4
          }
        
        col_pmg = es_raw$PMG_all
        
        # if not using all baseline observations
      } else {
          # SMD
          if(values$phoneme){
            col_smd = es_raw$SMD_phoneme
          } else {
            col_smd = es_raw$SMD
          }
        
          # TAU
          if(values$tau==0.33){
            col_tau = es_raw$Tau_last5
          } else {
            col_tau = es_raw$Tau_last5_4
          }
        
        col_pmg = es_raw$PMG
      }
      
      dat = tibble(
        participant = es_raw$participant,
        condition = es_raw$condition,
        itemType = es_raw$itemType,
        smd = col_smd,
        pmg = col_pmg,
        tau = col_tau,
        glmm_logit = col_glmm_logit,
        glmm_percent = col_glmm_percent
      ) %>% mutate(
        
      )
      
      glimpse(dat)
      p3 = ggpairs(dat,
                   columns = 4:8,
                   mapping = aes(fill = itemType, color = itemType),
                   columnLabels = c("d[BR]", "PMG", "T~au-U", "GLMM~Logit", "GLMM~Percent"),
                   labeller = "label_parsed",
                   diag = list(discrete="barDiag",
                               continuous = wrap("densityDiag", alpha=0.5, color = "grey50" )),
                   upper = list(#combo = wrap("box_no_facet", alpha=0.5),
                     continuous = wrap(ggally_cor, stars = F, digits = 2, size = 5)),
                   lower = list(continuous = wrap("points", alpha = 0.7, pch=21, color="grey40", size=1.5))) +
        theme(strip.text = element_text(size = 12))
      
      dropLeadingZero <- function(l){
        lnew <- c()
        for(i in l){
          if(i==0){ #zeros stay zero
            lnew <- c(lnew,"0")
          } else if (i>1){ #above one stays the same
            lnew <- c(lnew, as.character(i))
          } else
            lnew <- c(lnew, gsub("(?<![0-9])0+", "", i, perl = TRUE))
        }
        as.character(lnew)
      }
      
      # fix the x and y axis scales in each scatterplot and density plot
      p3[2,1] = p3[2,1] + scale_x_continuous(breaks = seq(0, 35, 5))
      p3[3,1] = p3[3,1] + scale_x_continuous(breaks = seq(0, 35, 5))
      p3[4,1] = p3[4,1] + scale_x_continuous(breaks = seq(0, 35, 5)) +
        scale_y_continuous(breaks = seq(0,10,2.5), limits = c(0,10))
      p3[5,1] = p3[5,1] + scale_x_continuous(breaks = seq(0, 35, 5)) +
        scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1))
      
      p3[3,2] = p3[3,2] + scale_x_continuous(labels = dropLeadingZero, breaks = seq(0, 1, 0.25))
      p3[4,2] = p3[4,2] + scale_x_continuous(labels = dropLeadingZero, breaks = seq(0, 1, 0.25))
      p3[5,2] = p3[5,2] + scale_x_continuous(labels = dropLeadingZero, breaks = seq(0, 1, 0.25))
      
      p3[4,3] = p3[4,3] + scale_x_continuous(labels = dropLeadingZero, breaks = seq(0, 1, 0.25))
      p3[5,3] = p3[5,3] + scale_x_continuous(labels = dropLeadingZero, breaks = seq(0, 1, 0.25))
      
      p3[5,4] = p3[5,4] + scale_x_continuous(breaks = seq(0, 10, 2))
      
      p3[5,5] = p3[5,5] + scale_x_continuous(labels = dropLeadingZero, breaks = seq(0, 1, 0.25), limits = c(0,1))
      
      return(p3)
    })
    
    
    
})
