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
  headerPanel(HTML('Behavioral browser
              <a href="http://cbcrg.github.io/pergola/" target="_blank"><img align="right" alt="Pergola logo" 
                   src="https://cloud.githubusercontent.com/assets/6224346/12887167/dcf80b24-ce72-11e5-8389-90122fd6c84e.png" /></a>'),
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
    conditionalPanel(condition="input.tabs_p=='Upload data'",
                    h3("Additional information upload"),
                    
                    h4("File 2"),
                    fileInput('fileEnv', 'Choose Additional File', multiple=TRUE, 
                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.bed')),
                    tags$hr(),
                    checkboxInput('header', 'Header', FALSE),
                    radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
                    #       radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
                    radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
                    #       fileInput('filePhases', 'Phases CSV File', multiple=TRUE, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.bed')),
                    #       tags$hr(),
                    #       checkboxInput('header', 'Header', FALSE),
                    #       radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
                    #       radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"')  
                    
                    h4("File 1"),
                    fileInput('filePhases', 'Phases CSV File', multiple=TRUE, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.bed')),
                    tags$hr(),
                    checkboxInput('header', 'Header', FALSE),
                    radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
                    radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"')
                     
                                     
                  ),
    # Load info such as experimental info
    conditionalPanel(condition="input.tabs_p=='Browser'",
                    sliderInput("windowsize", 
                               "Windowsize:", 
                               min = 1000,
                               max = 1000000,
                               value =1000,
                               step = 300),
                               uiOutput("bedGraphRange"),
                               uiOutput("idSelect"),
                               uiOutput("genomicPositionSelect")
                    )
  ),
  
  mainPanel(
#     textOutput("text1"),
    tabsetPanel(
      tabPanel("Browser",
               plotOutput("intervals", height=800),
               plotOutput("envInfo", height=20),
               plotOutput("barPlotValue", height=400),
               plotOutput("barPlotDuration", height=400),
               plotOutput("barPlotN", height=400),
               plotOutput("barPlotRate", height=400)),#,
#                tableOutput('bed')),                 
      tabPanel("Upload data",
               HTML('<p>Additional data</p>'),  
               tableOutput('bed'),     
               HTML('<p>Phases data</p>'),  
               tableOutput('fileEnv'),
               HTML('<p>End data</p>')),    
      tabPanel("About",
               HTML('<p>Pergola</p>')),
      id="tabs_p"
#               tableOutput('bedgraph'),
#               tableOutput('fileEnv')

    )
  )
)
)