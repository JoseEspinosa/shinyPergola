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


shinyUI(fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("windowsize"),
      uiOutput("bedGraphRange"),
      uiOutput("idSelect"),
      uiOutput("genomicPositionSelect")
    ),
    
    mainPanel(
      plotOutput("intervals"),
      textOutput("text1")
    )
  )
))
