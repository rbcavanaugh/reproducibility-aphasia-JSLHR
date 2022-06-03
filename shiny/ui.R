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
library(knitr)
library(kableExtra)
library(formattable)



# Define UI for application that draws a histogra
shinyUI(page_navbar(title = "Reproducibility in small-N treatment research in aphasia and related disorders: a tutorial",
  theme = bs_theme(version=5, font_scale = 0.8),
  tabPanel("Tab1",
  #titlePanel("Comparing effect sizes in small-N research in aphasia & relatd disorders" style = "siz"),
  tags$head(
    tags$style(HTML(".form-group {margin-bottom: .5rem;}
                    .control-label{font-weight: bold;}
                    #tau-label{margin-bottom: 0.2rem;}
                    .navbar-brand{color:black!important;}
                    .navbar.navbar-default {background-color:rgba(0,0,0,0.03)!important;}
                    .navbar {padding-top: 0; padding-bottom: 0;}"))
    ),
    
    useShinyjs(),
    sidebarLayout(
        
        sidebarPanel(width = 3,
            selectInput("participant1", "Participant", choices = unique(data$participant),
                        selected = paste0("P", sample(seq(1, 20, 1), 1))),
            radioButtons("condition1", "Condition", choices = c("blocked",  "random"), inline = TRUE,
                         selected = sample(c("blocked", "random"), 1)),
            radioButtons("itemType1", "Item Type", choices = c("tx",  "gx"), inline = TRUE),
            
            tags$hr(style="border-color: black;"),
          
            selectInput("participant2", "Participant", choices = unique(data$participant),
                        selected = paste0("P", sample(seq(1, 20, 1), 1))),
            radioButtons("condition2", "Condition", choices = c("blocked",  "random"), inline = TRUE,
                         selected = sample(c("blocked", "random"), 1)),
            radioButtons("itemType2", "Item Type", choices = c("tx",  "gx"), inline = TRUE),
            
            tags$hr(style="border-color: black;"),
        
            tags$h5("Researcher degrees of freedom"),
            radioButtons("all",
                         label = tags$span(HTML("<em>d</em><sub>BR</sub>")," & PMG", "baseline data"), 
                         inline = TRUE,
                         choices = c("All observations" = "t", "Last 5 observations" = "f")),
            radioButtons("tau", label = "Tau-U trend cutoff", choices = c(0.33, 0.4), inline = TRUE),
            radioButtons("adjust",
                         label = "GLMM baseline trend adjustment", inline = TRUE,
                         choices = c("Extrapolate trend" = "t", "Don't extrapolate" = "f")),
            
            tags$hr(style="border-color: black;"),

            div(align = "center",
              actionButton("shuffle", "Shuffle Participants")
            )
        ),
        mainPanel(width = 9,
          fluidRow(#align = "middle",
            column(width = 7, plotOutput("plot1", height = "38vh")),
            column(width = 5, tableOutput("t1"))
            ),
          hr(),
          fluidRow(#align = "middle",
            column(width = 7, plotOutput("plot2", height = "38vh")),
            column(width = 5, tableOutput("t2"))
          ),
          hidden(
            br(),
          fluidRow(
            column(width = 3, offset = 3, checkboxInput("collapse", label = "Collapse phonemes", value = TRUE)),
            column(width = 3, offset = 0, checkboxInput("outcome", label = "Count y-axis", value = FALSE))
            
          )))
        )
      ),
      !!!list(bslib::nav_spacer(),
              bslib::nav_item(
                actionButton("about", "App Details", style = "margin-right:10px; border-color:transparent; background-color:transparent;"),
              ),
              bslib::nav_item(
                
                tags$a(icon("github"),
                       href = "https://github.com/rbcavanaugh/reproducibility-small-N",
                       target = "_blank",
                       style = "color:black;font-size:1.2rem;")
              )
      )
    ) 
)
