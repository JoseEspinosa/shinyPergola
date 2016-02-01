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
  headerPanel("Behavioral browser"),
  
  sidebarPanel(
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
    radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"') 
  ),
  
  mainPanel(
#     textOutput("text1"),
    plotOutput("intervals", height=800),
    plotOutput("envInfo", height=20),
    plotOutput("barPlotValue", height=400),
    plotOutput("barPlotDuration", height=400),
    plotOutput("barPlotN", height=400),
    plotOutput("barPlotRate", height=400),
    tableOutput('bed')
#     tableOutput('bedgraph'),
#     tableOutput('fileEnv')
  )
))