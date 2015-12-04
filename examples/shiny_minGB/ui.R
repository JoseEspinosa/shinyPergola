library(shiny)
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Simple Genome Browser"),
  # Sidebar with a slider inputs and selection boxes
  sidebarPanel(
    sliderInput("windowsize", 
                "Windowsize:", 
                min = 10,
                max = 200,
                value = 50,
                step = 5),
    uiOutput("chromSelect"),
    uiOutput("genomicPositionSelect")
  ),
  # Show a plot of the region
  mainPanel(
    plotOutput("mismatchPlot", height=800)
  )
))
