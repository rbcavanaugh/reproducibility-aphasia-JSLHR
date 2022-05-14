#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(bslib)
library(bsutils)

# Define UI for application that draws a histogra
shinyUI(fluidPage(
  title = "Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial",
  titlePanel("Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial"),
  
    theme = bs_theme(version=5, font_scale = 0.9),
    useShinyjs(),
    br(),
    sidebarLayout(
        
        sidebarPanel(width = 3,
            
            selectInput("participant1", "Participant 1:", choices = unique(data$participant)),
            radioButtons("condition1", "Condition", choices = c("blocked",  "random"), inline = TRUE),
            radioButtons("itemType1", "Item Type", choices = c("tx",  "gx"), inline = TRUE),
          
            selectInput("participant2", "Participant 2:", choices = unique(data$participant), selected = "P2"),
            radioButtons("condition2", "Condition", choices = c("blocked",  "random"), inline = TRUE),
            radioButtons("itemType2", "Item Type", choices = c("tx",  "gx"), inline = TRUE),
            
            tags$hr(style="border-color: black;"),
            
            tags$h4("Plot options"),
            
            checkboxInput("collapse", label = "Collapse across phonemes", value = TRUE),
            radioButtons("outcome", choices = c("accuracy", "count"), label = "DV Scale", inline = TRUE)
        ),
        mainPanel(width = 9,
          fluidRow(align = "middle",
            column(width = 10, plotOutput("plot1", height = "40vh")),
            column(width = 2, uiOutput("t1"))
            ),
          fluidRow(
            column(width = 10, plotOutput("plot2", height = "40vh")),
            column(width = 2, uiOutput("t2"))
          )
        )
      )
    ) 
)
