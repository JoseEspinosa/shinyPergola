library(shiny)
# library(datasets)
library (plotrix) #std.err # mirar si la utilizo
library (ggplot2)

source ("/Users/jespinosa/git/phecomp/lib/R/plotParamPublication.R")

# runApp ("/Users/jespinosa/git/shinyPergola/apps/minPhenBrowser")
path_files <- "/Users/jespinosa/git/shinyPergola/data/bed4test"

colours_v <- c("darkgreen", "red", "magenta", "black") 

setwd(path_files)
list_files <-list.files(path=path_files ,pattern = ".bed$")

caseGroupLabel <- "case"
controlGroupLabel <- "control"
nAnimals <- 4

#Label by experimental group (control, free choice, force diet...)
id <- c (1 : nAnimals)
group <- c (rep (controlGroupLabel, nAnimals/2), rep (caseGroupLabel, nAnimals/2))
df.id_group <- data.frame (id, group)
df.id_group$group [which (id %% 2 != 0)] <- controlGroupLabel
df.id_group$group [which (id %% 2 == 0)] <- caseGroupLabel

data_bed = do.call (rbind, lapply (list_files, y <- function (x) { data <- read.table (x)
                                                                   id <- gsub("(^tr_)(\\d+)(_.+$)", "\\2", x)
                                                                   data$id <- id                                                                   
                                                                   return (data) }))

df.data_bed <- merge (data_bed, df.id_group , by.x= "id", by.y = "id")

colnames (df.data_bed) <- c("id", "chrom", "startChrom", "endChrom", "V4", "value", "strand", "V7", "V8", "V9", "group")
df.data_bed$duration <- df.data_bed$endChrom - df.data_bed$startChrom
tail(df.data_bed)

df.data_bed$group <- factor(df.data_bed$group , levels=c("control", "case"), 
                            labels=c("control", "case"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  output$idSelect <- renderUI({
    selectInput( "id", "Id", choices = choices_id)
  })
  
  output$genomicPositionSelect <- renderUI({
#     sliderInput( "gpos", "Genomic Position:", min = 10, max = chromlengths[input$chrom] - 10, value = 200 )
    sliderInput( "tpos", "Time Point:", min = 10, max = max(df.data_bed$endChrom) - 10, value = 569984 )
    
  })
  
  pos <-  reactive({
    min( max( input$windowsize + 1, input$tpos ), max(df.data_bed$endChrom) - input$windowsize - 1 )
  })
  
  output$text1 <- renderText({ 
#    as.character (pos())
     paste (as.character (input$windowsize), as.character (pos()))
  })

  data <- reactive({    
    df.data_f <- df.data_bed [which (df.data_bed$startChrom > max( pos() - input$windowsize, 0 ) & 
                         df.data_bed$endChrom < min( pos() + input$windowsize, max(df.data_bed$endChrom))),]   
    df.data_f
  })
  
  df_mean  <- reactive({
#     with (data() , aggregate (cbind (value), list (group=group), mean))
    df_t <- with (data(), aggregate (cbind (value, duration), list (group=group),FUN=function (x) c (mean=mean(x), std.error=std.error(x), length(x))))
    df_t$meanValue <- df_t$value [,1]
    df_t$std.errorValue <- df_t$value [,2]
    df_t$number <- df_t$value [,3]
    
    df_t$meanDuration <- df_t$duration [,1]
    df_t$std.errorDuration <- df_t$duration [,2]
    
    df_t
  }) 
  
  output$barPlotValue <- renderPlot({  
    p = ggplot(data = df_mean(), aes(x=group, y=meanValue, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=meanValue-std.errorValue, ymax=meanValue+std.errorValue), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Mean value\n") + 
      labs (x = "\nMean value", y = "Group\n")
    print(p) 
  })

  output$barPlotDuration <- renderPlot({  
    p = ggplot(data = df_mean(), aes(x=group, y=meanDuration, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=meanDuration-std.errorDuration, ymax=meanDuration+std.errorDuration), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Length\n") + 
      labs (x = "\nLength", y = "Group\n")
  print(p) 
  })  
  
  # Number of events
  output$barPlotN <- renderPlot({  
    p = ggplot(data = df_mean(), aes(x=group, y=number, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Number of events\n") + 
      labs (x = "\nNumber", y = "Group\n")
  print(p) 
  })  

  # Rate
  output$barPlotLength <- renderPlot({  
    p = ggplot(data = df_mean(), aes(x=group, y=number, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Number of events\n") + 
      labs (x = "\nNumber", y = "Group\n")
    print(p) 
})  
})
  
