############################################################################################
### Jose A Espinosa. NPMMD/CB-CRG Group. Dec 2015                                        ###
############################################################################################
### Shiny app to show pergola data                                                       ###
### ui.R                                                                                 ###
############################################################################################
### TODO                                                                                 ###
### Change color scheme with a color blind friendly scheme, some ideas here:             ###
### http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/#a-colorblind-friendly-palette ###
############################################################################################

# Running the app
# runApp ("/Users/jespinosa/git/shinyPergola/apps/minPhenBrowser")

library(shiny)

# shinyUI(pageWithSidebar(
#   # Application title
#   headerPanel("Simple Genome Browser"),
#   # Sidebar with a slider inputs and selection boxes
#   sidebarPanel(
#     uiOutput("idSelect")
#   )
#   # Show a plot of the region
#   mainPanel("main panel")
# #     plotOutput("mismatchPlot", height=800)
#   
# ))

shinyUI(pageWithSidebar(
#   headerPanel("Behavioral browser"),
  headerPanel("Behavioral browser",
              tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
                        tags$style(type="text/css", "select { max-width: 200px; }"),
                        tags$style(type="text/css", "textarea { max-width: 185px; }"),
                        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
                        tags$style(type='text/css', ".well { max-width: 330px; }"),
                        tags$style(type='text/css', ".span4 { max-width: 330px; }")) 
  ),

  sidebarPanel(
    conditionalPanel(condition="input.tabs_p=='About'",
                     h4("Introduction")
    ),
    conditionalPanel(condition="input.tabs_p=='Browser'",
                     h4("Introductiowwwn"),
                     fileInput('filePhases', 'Phases CSV File', multiple=TRUE, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.bed')),
                     tags$hr(),
                     checkboxInput('header', 'Header', FALSE),
                     radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
                     radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"') 
    ),
    conditionalPanel(condition="input.tabs_p=='Data upload'",
                    sliderInput("windowsize", 
                                "Windowsize:", 
                                min = 1000,
                                max = 1000000,
                                value =1000,
                                step = 300),
                    uiOutput("bedGraphRange"),
                    uiOutput("idSelect"),
                    uiOutput("genomicPositionSelect"),
    
      fileInput('fileEnv', 'Choose CSV File', multiple=TRUE, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.bed')),
      tags$hr(),
      checkboxInput('header', 'Header', FALSE),
      radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
#       radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
      radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"')
#       fileInput('filePhases', 'Phases CSV File', multiple=TRUE, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.bed')),
#       tags$hr(),
#       checkboxInput('header', 'Header', FALSE),
#       radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
#       radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"') 
      )
    ),
  
  mainPanel(
#     textOutput("text1"),
    tabsetPanel(
      tabPanel("Browser",
               HTML('<p>Browser</p>')),
      tabPanel("About",
              HTML('<p>Pergola</p>')),
      tabPanel("Data upload",               
              plotOutput("intervals", height=800),
              plotOutput("envInfo", height=20),
              plotOutput("barPlotValue", height=400),
              plotOutput("barPlotDuration", height=400),
              plotOutput("barPlotN", height=400),
              plotOutput("barPlotRate", height=400),
              tableOutput('bed')),
      id="tabs_p"
#               tableOutput('bedgraph'),
#               tableOutput('fileEnv')

  )
)
))