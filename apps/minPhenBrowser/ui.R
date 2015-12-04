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
  headerPanel("Simple Genome Browser"),
  
  sidebarPanel(
    sliderInput("windowsize", 
                "Windowsize:", 
                min = 10,
                max = 200,
                value = 50,
                step = 5),
    uiOutput("idSelect")
  ),
  
  mainPanel("plot")
))