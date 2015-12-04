library(shiny)
library(datasets)

# runApp ("/Users/jespinosa/git/shinyPergola/apps/minPhenBrowser")
# path_files <- "/Users/jespinosa/git/shinyPergola/data/bed4test"
# 
# setwd(path_files)
# list_files <-list.files(path=path_files ,pattern = ".bed$")
# 
# data_bed = do.call (rbind, lapply (list_files, y <- function (x) { data <- read.table (x)
#                                                                    id <- gsub("(^tr_)(\\d+)(_.+$)", "\\2", x)
#                                                                    data$id <- id 
#                                                                    print (id)
#                                                                    return (data) }))
# 
# choices_id <- unique (data_bed$id)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
# mpgData <- mtcars
# mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  output$idSelect <- renderUI({
    selectInput( "id", "Id", choices = c(1:4))
  })
})
  
