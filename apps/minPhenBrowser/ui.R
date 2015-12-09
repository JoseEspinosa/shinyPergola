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
    uiOutput("idSelect"),
    uiOutput("genomicPositionSelect")
  ),
  
  mainPanel(
    textOutput("text1"),
    plotOutput("intervals", height=400),
    plotOutput("bedgraph", height=400),
    plotOutput("barPlotValue", height=400),
    plotOutput("barPlotDuration", height=400),
    plotOutput("barPlotN", height=400),
    plotOutput("barPlotRate", height=400)    
  )
))