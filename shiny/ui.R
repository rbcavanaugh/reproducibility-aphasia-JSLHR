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
library(shinyWidgets)





# Define UI for application that draws a histogra
shinyUI(page_navbar(title = "Reproducibility in small-N treatment research: a tutorial through the lens of aphasiology",
  theme = bs_theme(version=4, font_scale = 0.85),
  tabPanelBody("Tab1",
  #titlePanel("Comparing effect sizes in small-N research in aphasia & relatd disorders" style = "siz"),
  # tags$head(
  #   tags$style(HTML(""))
  #   ),
  shiny::includeCSS(here("shiny", "www", "style.css")),
  #shiny::includeCSS(here("www", "style.css")),
  
    useShinyjs(),
    sidebarLayout(
        
        sidebarPanel(width = 3,
                     tags$h5("Participant 1", style="font-style:italic;"),
            pickerInput("participant1",
                        options = list(style = "btn-default", size = 10),
                        label = NULL, #"Participant",
                        choices = paste("P", 1:20, sep = ""), #vals, #paste("P", 1:20, sep = ""),
                        selected = sample(vals, 1)), #paste0("P", sample(seq(1, 20, 1), 1))),
            radioGroupButtons("condition1", label = NULL,
                              #"Condition:",
                              justified = TRUE,
                              choices = list("Blocked" = "blocked",  "Random" = "random"),
                              size = "sm",
                              #inline = TRUE,
                         selected = sample(c("blocked", "random"), 1)), #div(style="height:2px;"),

            radioGroupButtons("itemType1", label = NULL,
                             # "Item Type:",
                              choices = list("Treated" = "tx", "Generalization"= "gx"),
                              size = "sm",
                              justified = TRUE,
                              #inline = TRUE
                              ),
            
            tags$hr(style="border-color: black;"),
            tags$h5("Participant 2", style="font-style:italic;"),
            pickerInput("participant2",
                        options = list(style = "btn-default", size = 10),
                        label = NULL, #"Participant",
                        choices = paste("P", 1:20, sep = ""), #vals, #paste("P", 1:20, sep = ""),
                        selected = sample(vals, 1)), #paste0("P", sample(seq(1, 20, 1), 1))),
            radioGroupButtons("condition2",  label = NULL,
                              #"Condition:",
                              choices = list("Blocked" = "blocked",  "Random" = "random"),
                              size = "sm",
                              justified = TRUE,
                              #inline = TRUE,
                         selected = sample(c("blocked", "random"), 1)), #div(style="height:2px;"),
            radioGroupButtons("itemType2", label = NULL,
                             # "Item Type:",
                              list("Treated" = "tx", "Generalization"= "gx"),
                              size = "sm",
                              #inline = TRUE
                              justified = TRUE
                              ),
            
            tags$hr(style="border-color: black;"),
        
            tags$h5("Researcher degrees of freedom", style="font-style:italic;"),
            radioGroupButtons("all",
                         label = tags$span(HTML("<em>d</em><sub>BR</sub>")," & PMG", "baseline data"), 
                         #inline = TRUE,
                         size = "sm",justified = TRUE,
                         choices = c("All obs" = "t", "Last 5 obs" = "f")),
            radioGroupButtons("phoneme",
                         label = tags$span(HTML("<em>d</em><sub>BR</sub>"), "calculation"), 
                         #inline = TRUE,
                         size = "sm",
                         justified = TRUE,
                         choices = c("Across phonemes" = "across", "Within phonemes" = "within")),
            radioGroupButtons("tau", label = "Tau-U trend cutoff",
                             # inline = TRUE,
                             size = "sm",
                             justified = TRUE,
                              choices = c(0.33, 0.4)
                              
                              ),
            radioGroupButtons("adjust",
                         label = "GLMM trend adjustment",
                         #inline = TRUE,
                         justified = TRUE,
                         size = "sm",
                         choices = c("Extend trend" = "t", "Don't extend" = "f")),
            
            tags$hr(style="border-color: black;"),

            div(align = "center",
              actionButton("shuffle", "Shuffle Participants")
            )
        ),
        mainPanel(width = 9,
          tabsetPanel(#type = "pills",
            tabPanel(title = "Participant plots",
             div(style="margin-top:10px;",
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
                  
                ))
            )),
            tabPanel(title = "Correlation Matrix",
              div(align="center",
                  style="height:75vh;padding-left:7.5%;padding-right:7.5%;padding-top:2.5%;margin-top:10px;",
                  plotOutput("scatterplot", height = "100%")
              ),
              div(align = "right",style="padding-left:5%;padding-right:5%;padding-top:2.5%;",
                  tags$em("Relationships between individual effect size measures typically used in aphasia small-N studies."))
            )
          )
          
          )
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
