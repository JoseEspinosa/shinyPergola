library(shiny)
# library(datasets)
library (plotrix) #std.err # mirar si la utilizo
library (ggplot2)
library(grid)

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

colnames (df.data_bed) <- c("id", "chr", "start", "end", "V4", "value", "strand", "V7", "V8", "V9", "group")
choices_id <- unique (df.data_bed$id)

n_tracks <- length(unique(df.data_bed$id))

df.data_bed$group_id <- paste (df.data_bed$id, df.data_bed$group, sep="_")
df.data_bed <- df.data_bed [with(df.data_bed, order(group, id)), ]

# Create a new Id that follows the order case 1,2,3/ control 1,2,3 
df.data_bed <- transform(df.data_bed, new_id=match(group_id, unique(group_id)))

df.data_bed$duration <- df.data_bed$end - df.data_bed$start
df.data_bed$rate <- df.data_bed$value / df.data_bed$duration 

df.data_bed$group <- factor(df.data_bed$group , levels=c("control", "case"), 
                            labels=c("control", "case"))

df.data_bed$id <- as.numeric (df.data_bed$id)
min_tr <- min(df.data_bed$id)

## Bedgraph files
list_files_bedGr <- list.files (path=path_files ,pattern = ".bedGraph$")

nAnimals <- 4

#Label by experimental group (control, free choice, force diet...)
id <- c (1 : nAnimals)
group <- c (rep (controlGroupLabel, nAnimals/2), rep (caseGroupLabel, nAnimals/2))
df.id_group <- data.frame (id, group)
df.id_group$group [which (id %% 2 != 0)] <- controlGroupLabel
df.id_group$group [which (id %% 2 == 0)] <- caseGroupLabel

data_bedGr = do.call (rbind, lapply (list_files_bedGr, y <- function (x) { data <- read.table (x)
                                                                           id <- gsub("(^tr_)(\\d+)(_.+$)", "\\2", x)
                                                                           data$id <- id                                                                   
                                                                           return (data) }))

data_bedGr <- merge (data_bedGr, df.id_group, by.x= "id", by.y = "id")
colnames (data_bedGr) <- c("id", "chrom", "start", "end", "value", "group")

data_bedGr$group <- factor(data_bedGr$group , levels=c("control", "case"), 
                           labels=c("control", "case"))


data_bedGr$n_group <- c(1:length (data_bedGr$group))
data_bedGr$n_group [data_bedGr$group == controlGroupLabel] <- 1
data_bedGr$n_group [data_bedGr$group == caseGroupLabel] <- 2
data_bedGr$group_id <- paste (data_bedGr$n_group, data_bedGr$id, sep="_")


data_bedGr$id <- as.numeric (data_bedGr$id)
min_v <- floor (min (data_bedGr$value))
max_v <- ceiling (max(data_bedGr$value))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  output$idSelect <- renderUI({
    selectInput( "id", "Id", choices = choices_id)
  })
  
  output$genomicPositionSelect <- renderUI({
    #     sliderInput( "gpos", "Genomic Position:", min = 10, max = chromlengths[input$chrom] - 10, value = 200 )
    sliderInput( "tpos", "Time Point:", min = 10, max = max(df.data_bed$end) - 10, value = 569984 )
  })
  
  output$bedGraphRange <- renderUI({
    sliderInput("bedGraphRange", "Range bedgraph:", 
                min = min_v, max = max_v, value = c(0, 0.5), step= 0.1)
  }) 
  
  pos <-  reactive({
    min( max( input$windowsize + 1, input$tpos ), max(df.data_bed$end) - input$windowsize - 1 )
  })
  
  # Hacer un reactive que lo meta en un table data!!!
  dfFileEnv <- reactive ({
    
    # input$fileEnv will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$fileEnv
    
    if (is.null(inFile)) return(NULL)
    
    df <- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    df$id <- as.factor (c(1: length(df [,1])))
    df
  })
  
  output$text1 <- renderText({ 
    #    as.character (pos())
    paste (as.character (input$windowsize), as.character (pos()))
  })
  
  data <- reactive({    
    df.data_f <- df.data_bed [which (df.data_bed$start > max( pos() - input$windowsize, 0 ) & 
                                       df.data_bed$end < min( pos() + input$windowsize, max(df.data_bed$end))),]
    df.data_f
  })
  
  dataBedgraph <- reactive({    
    df.dataBedgraph_f <- data_bedGr [which (data_bedGr$start > max( pos() - input$windowsize, 0 ) & 
                                              data_bedGr$end < min( pos() + input$windowsize, max(data_bedGr$end))),]
    df.dataBedgraph_f [which (df.dataBedgraph_f$value < input$bedGraphRange [1]), "value"] <- input$bedGraphRange [1] + 0.001
    df.dataBedgraph_f [which (df.dataBedgraph_f$value > input$bedGraphRange [2]), "value"] <- input$bedGraphRange [2] - 0.001
    
    df.dataBedgraph_f
  })
  
  range_x <- reactive({
    c (max( pos() - input$windowsize, 0 ), min( pos() + input$windowsize, max(as.numeric(data()$end))))
    #     rbind(c (max( pos() - input$windowsize, 0 ), min( pos() + input$windowsize, max(as.numeric(data()$end)))),
    #           c (max( pos() - input$windowsize, 0 ), min( pos() + input$windowsize, max(as.numeric(dataBedgraph()$end)))),
    #           c (min(data()$start), max(data()$end)),
    #           c (min(dataBedgraph()$start), max(dataBedgraph()$end)),
    #           c (min(data()$start, dataBedgraph()$start), max(data()$end, dataBedgraph()$end)))
  })  
  
  dfFileEnv_range <- reactive({    
    if (is.null(dfFileEnv())) return(NULL)
    
    #     range_win <- c (max( pos() - input$windowsize, 0 ), min( pos() + input$windowsize, max(as.numeric(dfFileEnv()$V3))))
    range_win <- range_x()                                                    
    range_r <- dfFileEnv()[1,] 
    
    range_r$V2 <- range_win[1] 
    range_r$V3 <- range_win[2]
    
    ranges <- merge(dfFileEnv(), range_r, by="V1",suffixes=c("A","B"))
    ranges_i <- ranges [with(ranges, V2A >= V2B & V3A <= V3B),][,c(0:10)] 
    
    # left thresholding
    ranges_l <- ranges [with(ranges, V2A < V2B & V3A > V2B),][,c(0:10)]
    
    # right thresholding
    # range_win <- c(568984.00, 570925.00)
    ranges_r <- ranges [with(ranges, V2A < V3B & V3A > V3B),][,c(0:10)]
    
    if (all.equal (ranges_l,ranges_r) == TRUE ) {
      ranges_r <- data.frame()
      ranges_l$V2A <- range_win[1]+0.001
      ranges_l$V3A <- range_win[2]-0.001
    }
    
    if (nrow (ranges_l) != 0) {
      ranges_l$V2A <- range_win[1]+0.001
    }
    
    if (nrow (ranges_r) != 0) {
      ranges_r$V3A <- range_win[2]-0.001
    }
    
    ranges_p <- rbind (ranges_l, ranges_i, ranges_r)
    
    ranges_p 
  })
  
  output$bed <- renderTable({
    as.data.frame(dfFileEnv_range())
  })
  #   output$bedgraph <- renderTable({
  #     dataBedgraph()
  #   })
  #   output$fileEnv <- renderTable({
  #     dfFileEnv_range()
  #   })
  
  # Intervals plot
  interv_p <- reactive({ 
    ggplot(data = data()) +
      geom_rect(aes(xmin = start, xmax = end, ymin = new_id, ymax = new_id + 0.9, fill=group)) +
      scale_fill_manual(values=colours_v) +
      scale_y_continuous(limits=c(min_tr, n_tracks + 1), breaks=unique(data()$new_id) + 0.5, labels=unique(data()$id)) +
      scale_x_continuous(limits=range_x(), breaks =NULL) +
      theme(legend.position="none", axis.line.x=element_blank())
  })
  
  #bedGraph plot
  bedgraph_p <- reactive({ 
    ggplot (data = dataBedgraph()) + 
      geom_rect (aes(xmin = start, xmax = end, ymin = 0, ymax = value, fill=group)) +
      scale_fill_manual(values=colours_v) +
      scale_y_continuous(limits=input$bedGraphRange, breaks=input$bedGraphRange, labels=input$bedGraphRange) + 
      scale_x_continuous(limits=range_x(), breaks=NULL) +
      facet_wrap(~group_id, ncol= 1) + 
#       theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.y = element_text(size=10), 
#             legend.position="none", axis.line.x=element_blank()) 
  theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.y = element_text(size=10)) 
  })
  
  # Environmental info plot
  env_p <- reactive ({ 
    #   env_p <- reactive({ 
    if (is.null (dfFileEnv_range())) {
      return (NULL)
    }
    else {
      ggplot (data = dfFileEnv_range()) + 
        geom_rect (aes(xmin = V2A, xmax = V3A, ymin = 0, ymax = 1, fill=idA)) +
        scale_y_continuous(breaks=NULL) +
        scale_x_continuous(limits=range_x()) +
        #       scale_fill_manual(values=colours_v) +
        #         scale_y_continuous(limits=input$bedGraphRange, breaks=input$bedGraphRange, labels=input$bedGraphRange) + 
        #       facet_wrap(~group_id, ncol= 1) + 
        theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.y = element_text(size=10), 
              legend.position="none", axis.text.y=element_blank())
      #               axis.text.y = element_text(size=10), axis.line.y=element_blank(),axis.text.y=element_blank()) 
    }
  }) 
  
  output$intervals <- renderPlot({ 
    if (is.null (env_p())) {
      p1 <- ggplot_gtable(ggplot_build(interv_p()))
      p2 <- ggplot_gtable(ggplot_build(bedgraph_p()))
      #     p3 <- ggplot_gtable(ggplot_build(env_p()))
      
      maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
      #     maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], p3$widths[2:3])
      
      p1$widths[2:3] <- maxWidth
      p2$widths[2:3] <- maxWidth
      #     p3$widths[2:3] <- maxWidth
      
      #     grid.arrange(p1, p2, p3, heights = c(2, 2))
      grid.arrange(p1, p2, heights = c(2, 2)) 
    }
    else {
      p1 <- ggplot_gtable(ggplot_build(interv_p()))
      p2 <- ggplot_gtable(ggplot_build(bedgraph_p()))
      p3 <- ggplot_gtable(ggplot_build(env_p()))
      
      maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], p3$widths[2:3])
      
      p1$widths[2:3] <- maxWidth
      p2$widths[2:3] <- maxWidth
      p3$widths[2:3] <- maxWidth
      
      grid.arrange(p1, p2, p3, heights = c(2, 2, 1))
    }
  })
  
  df_mean  <- reactive({
    #     with (data() , aggregate (cbind (value), list (group=group), mean))
    df_t <- with (data(), aggregate (cbind (value, duration, rate), list (group=group),FUN=function (x) c (mean=mean(x), std.error=std.error(x), length(x))))
    df_t$meanValue <- df_t$value [,1]
    df_t$std.errorValue <- df_t$value [,2]
    
    df_t$number <- df_t$value [,3]
    
    df_t$meanDuration <- df_t$duration [,1]
    df_t$std.errorDuration <- df_t$duration [,2]
    
    df_t$meanRate <- df_t$rate [,1]
    df_t$std.errorRate <- df_t$rate [,2]
    
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
  output$barPlotRate <- renderPlot({  
    p = ggplot(data = df_mean(), aes(x=group, y=meanRate, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=meanRate-std.errorRate, ymax=meanRate+std.errorRate), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Rate\n") + 
      labs (x = "\nRate", y = "Group\n")
    print(p) 
  })  
})