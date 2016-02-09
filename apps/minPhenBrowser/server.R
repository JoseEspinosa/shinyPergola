############################################################################################
### Jose A Espinosa. NPMMD/CB-CRG Group. Dec 2015                                        ###
############################################################################################
### Shiny app to show pergola data                                                       ###
### server.R                                                                             ### 
############################################################################################
### TODO                                                                                 ###
### Change color scheme with a color blind friendly scheme, some ideas here:             ###
### http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/#a-colorblind-friendly-palette ###
############################################################################################

# runApp ("/Users/jespinosa/git/shinyPergola/apps/minPhenBrowser")

library(shiny)
# library(datasets)
# library(grid)
library (gridExtra)
library (plotrix) #std.err # mirar si la utilizo
library (ggplot2)

source ("/Users/jespinosa/git/phecomp/lib/R/plotParamPublication.R")

# path_files <- "/Users/jespinosa/git/shinyPergola/data/bed4test"
path_files <- "/Users/jespinosa/git/shinyPergola/data/bed4test_all"

# colours_v <- c("red", "darkblue", "magenta", "black") 
# colours_v <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colours_v <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#56B4E9", "#009E73", "#F0E442")
setwd(path_files)
# list_files <-list.files(path=path_files ,pattern = ".bed$")
list_files <-list.files(path=path_files ,pattern = "^tr.*.bed$")

caseGroupLabel <- "case"
controlGroupLabel <- "control"
nAnimals <- 18

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

df.data_bed$n_group <- c(1:length (df.data_bed$group))
df.data_bed$n_group [df.data_bed$group == controlGroupLabel] <- 1
df.data_bed$n_group [df.data_bed$group == caseGroupLabel] <- 2
df.data_bed$group_id <- paste (df.data_bed$n_group, df.data_bed$id, sep="_")

# df.data_bed$group_id <- factor(df.data_bed$group_id, levels=df.data_bed$group_id 
#                                [order(as.numeric(df.data_bed$n_group), as.numeric(df.data_bed$id))], ordered=TRUE)
df.data_bed$group_id <- factor(df.data_bed$group_id, 
                               levels=unique(as.character(df.data_bed$group_id[order(df.data_bed$n_group, df.data_bed$id)])))


# df.data_bed$id <- as.numeric (df.data_bed$id)
# min_tr <- min(df.data_bed$id)

## Bedgraph files
list_files <-list.files(path=path_files ,pattern = "^tr.*.bed$")
list_files_bedGr <- list.files (path=path_files ,pattern = "^tr.*.bedGraph$")
nAnimals <- 18

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

min_t <- floor (min (df.data_bed$start))
max_t <- ceiling (max(df.data_bed$end))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  output$windowsize <- renderUI({
    sliderInput("windowsize", "Windowsize:", min = min(max_t, 1000), max = min(max_t, 1000000), 
                value =min(max_t, 1000), step = min(max_t, 300))
  })
  
  output$idSelect <- renderUI({
    selectInput( "id", "Id", choices = choices_id)
  })
  
  output$genomicPositionSelect <- renderUI({
#     sliderInput( "gpos", "Genomic Position:", min = 10, max = chromlengths[input$chrom] - 10, value = 200 )
#     sliderInput( "tpos", "Time Point:", min = 10, max = max(df.data_bed$end) - 10, value = 569984 )
    sliderInput( "tpos", "Time Point:", min = 10, max = max(df.data_bed$end) - 10, value = min(df.data_bed$start)+10 )
  })
  
  output$bedGraphRange <- renderUI({
    sliderInput("bedGraphRange", "Range bedgraph:", 
                min = min_v, max = max_v, value = c(0, 0.5), step= 0.1)
  }) 
  
  pos <-  reactive({
    min( max( input$windowsize + 1, input$tpos ), max(df.data_bed$end) - input$windowsize - 1 )
#     min( max( output$windowsize + 1, input$tpos ), max(df.data_bed$end) - input$windowsize - 1 )
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

  dfFilePhases <- reactive ({
    # input$fileEnv will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFilePh <- input$filePhases
    
    if (is.null(inFilePh)) return(NULL)
    
    df_ph <- read.table(inFilePh$datapath, header=input$header, sep=input$sep, quote=input$quote)
    df_ph$id <- as.factor (c(1: length(df_ph [,1])))
    df_ph
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
    levels(ranges_p$V1) <- c("Env1")
    ranges_p 
  })
  
  dfFilePhases_range <- reactive({    
    if (is.null(dfFilePhases())) return(NULL)
    
    #     range_win <- c (max( pos() - input$windowsize, 0 ), min( pos() + input$windowsize, max(as.numeric(dfFileEnv()$V3))))
    range_win <- range_x()                                                    
    range_r <- dfFilePhases()[1,] 
    
    range_r$V2 <- range_win[1] 
    range_r$V3 <- range_win[2]
    
    ranges <- merge(dfFilePhases(), range_r, by="V1",suffixes=c("A","B"))
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
    levels(ranges_p$V1) <- c("Env1")
    
    cb_palette <- c("lightblue", "darkblue","#999999", "#E69F00", "#56B4E9",
                   "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    colors_p <- cb_palette [1:length(unique(ranges_p$V4A))]
    list(ranges_p=ranges_p, colors_p=colors_p) 
  })

  output$bed <- renderTable({
    as.data.frame(dfFileEnv_range())
  })
#   output$bedgraph <- renderTable({
#     dataBedgraph()
#   })
  output$fileEnv <- renderTable({
#     as.data.frame(dfFileEnv_range())
#     as.data.frame(dfFilePhases_range())
    as.data.frame(dfFilePhases_range()$ranges_p)    
  })

  # Intervals plot
  interv_p <- reactive({ 
    ggplot(data = data()) +
#       geom_rect(aes(xmin = start, xmax = end, ymin = new_id, ymax = new_id + 0.9, fill=group)) +
      geom_rect (aes(xmin = start, xmax = end, ymin = 0, ymax = 1, fill=group)) +
#       geom_text (aes(x=min(start), y=max(end), label=id), size=4) +
#       geom_text (aes(x=-10, y=0.5, label=id), size=4) +
      scale_fill_manual(values=colours_v) +
#       scale_y_continuous(limits=c(min_tr, n_tracks + 1), breaks=unique(data()$new_id) + 0.5, labels=unique(data()$id)) +
      scale_y_continuous(limits=c(0,1), breaks=NULL, labels=unique(data()$id))  +
      scale_x_continuous(limits=range_x(), breaks =NULL) +
      theme(axis.text.y = element_text(size=10), axis.line.x=element_blank(), #strip.background = element_blank(),
            legend.position="none", strip.text.x = element_blank(), strip.text.y = element_text(size=8)) +
#       facet_wrap(~ group_id, ncol= 1)
#       facet_grid(group_id ~ .)
      facet_grid(group_id ~ .)
  })

  #bedGraph plot
  bedgraph_p <- reactive({ 
    ggplot (data = dataBedgraph()) + 
    geom_rect (aes(xmin = start, xmax = end, ymin = 0, ymax = value, fill=group)) +
    scale_fill_manual(values=colours_v) +
    scale_y_continuous(limits=input$bedGraphRange, breaks=input$bedGraphRange, labels=input$bedGraphRange) + 
#     scale_x_continuous(limits=range_x(), breaks=NULL) +
    scale_x_continuous(limits=range_x()) +
#     facet_wrap(~group_id, ncol= 1) + 
    facet_grid(group_id ~ .) +
    theme(axis.text.y = element_text(size=10), #strip.background = element_blank(), axis.line.x=element_blank(),
          legend.position="none", strip.text.x = element_blank(), strip.text.y = element_text(size=8)) 
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
        scale_x_continuous(limits=range_x(), breaks=NULL) +
        facet_grid(V1 ~ .) +
  #       scale_fill_manual(values=colours_v) +
  #         scale_y_continuous(limits=input$bedGraphRange, breaks=input$bedGraphRange, labels=input$bedGraphRange) + 
  #       facet_wrap(~group_id, ncol= 1) + 
      theme(strip.text.x = element_blank(), axis.line.x=element_blank(), strip.text.y = element_text(size=6), #strip.background = element_blank(),
            legend.position="none", axis.text.y=element_blank())
#               axis.text.y = element_text(size=10), axis.line.y=element_blank(),axis.text.y=element_blank()) 
    }
  }) 
  
  # Environmental phases plot
  phases_p <- reactive ({ 
    #   env_p <- reactive({ 
    if (is.null (dfFilePhases_range()$ranges_p)) {
      return (NULL)
    }
    else {
      ggplot (data = dfFilePhases_range()$ranges_p) + 
        geom_rect (aes(xmin = V2A, xmax = V3A, ymin = 0, ymax = 1, fill=V4A)) +
#         scale_fill_manual (values = c("lightblue", "darkblue")) +
        scale_fill_manual (values = dfFilePhases_range()$colors_p) +
        scale_y_continuous(breaks=NULL) +
        scale_x_continuous(limits=range_x(), breaks=NULL) +
        facet_grid(V1 ~ .) +
        theme(strip.text.x = element_blank(), axis.line.x=element_blank(), strip.text.y = element_text(size=6), #strip.background = element_blank(),
              legend.position="none", axis.text.y=element_blank())
    }
  })

  output$intervals <- renderPlot({ 
    if (is.null (env_p()) & is.null(phases_p())) {
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
    else if (is.null (env_p())) {
      p1 <- ggplot_gtable(ggplot_build(interv_p()))
      p2 <- ggplot_gtable(ggplot_build(bedgraph_p()))
      p3 <- ggplot_gtable(ggplot_build(phases_p()))
      
      maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], p3$widths[2:3])
      
      p1$widths[2:3] <- maxWidth
      p2$widths[2:3] <- maxWidth
      p3$widths[2:3] <- maxWidth
      
      grid.arrange(p3, p1, p2, heights = c(0.25, 2, 2))
    }
    else if (is.null (phases_p())) {
      p1 <- ggplot_gtable(ggplot_build(interv_p()))
      p2 <- ggplot_gtable(ggplot_build(bedgraph_p()))
      p3 <- ggplot_gtable(ggplot_build(env_p()))
      
      maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], p3$widths[2:3])
      
      p1$widths[2:3] <- maxWidth
      p2$widths[2:3] <- maxWidth
      p3$widths[2:3] <- maxWidth
      
      grid.arrange(p3, p1, p2, heights = c(0.25, 2, 2))
    }
    else {
      p1 <- ggplot_gtable(ggplot_build(interv_p()))
      p2 <- ggplot_gtable(ggplot_build(bedgraph_p()))
      p3 <- ggplot_gtable(ggplot_build(env_p()))
      p4 <- ggplot_gtable(ggplot_build(phases_p()))
      
      maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], p3$widths[2:3], p4$widths[2:3])
      
      p1$widths[2:3] <- maxWidth
      p2$widths[2:3] <- maxWidth
      p3$widths[2:3] <- maxWidth
      p4$widths[2:3] <- maxWidth
        
      grid.arrange(p3, p4, p1, p2, heights = c(0.25, 0.25, 2, 2))
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
  
  value_plot <- reactive({
    ggplot(data = df_mean(), aes(x=group, y=meanValue, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=meanValue-std.errorValue, ymax=meanValue+std.errorValue), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Mean value\n") + 
      labs (x = "\nMean value", y = "Group\n")
  })


  output$barPlotValue <- renderPlot({  
    print(value_plot()) 
  })

  duration_plot <- reactive({  
    ggplot(data = df_mean(), aes(x=group, y=meanDuration, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=meanDuration-std.errorDuration, ymax=meanDuration+std.errorDuration), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Length\n") + 
      labs (x = "\nLength", y = "Group\n")
  })  
  
  output$barPlotDuration <- renderPlot({  
    print(duration_plot())
  })

  # Number of events
  number_plot <- reactive({  
    ggplot(data = df_mean(), aes(x=group, y=number, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Number of events\n") + 
      labs (x = "\nNumber", y = "Group\n")
  })  
  
  output$barPlotN <- renderPlot({  
    print(number_plot())
  })

  # Rate
  rate_plot <- reactive({  
    ggplot(data = df_mean(), aes(x=group, y=meanRate, fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=meanRate-std.errorRate, ymax=meanRate+std.errorRate), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=colours_v) +
      labs (title = "Rate\n") + 
      labs (x = "\nRate", y = "Group\n")
  })
  
  output$barPlotRate <- renderPlot({  
    print(rate_plot())
  })
  
  # Download value plot
  output$barPlotValueTiff <- downloadHandler(
    filename <- function() { paste('value.tiff') },
    content <- function(file) {
    ggsave (file, plot = value_plot(), width = 15, height = 10)
  
    },
    contentType = 'application/tiff'
  )
  
  # Download duration plot
  output$barPlotDurationTiff <- downloadHandler(
    filename <- function() { paste('duration.tiff') },
    content <- function(file) {
    ggsave (file, plot = duration_plot(), width = 15, height = 10)
    },
    contentType = 'application/tiff'
  )
  
  # Download n_events plot
  output$barPlotNumberTiff <- downloadHandler(
    filename <- function() { paste('n_events.tiff') },
    content <- function(file) {
      ggsave (file, plot = number_plot(), width = 15, height = 10)
    },
    contentType = 'application/tiff'
  )

  # Download rate plot
  output$barPlotRateTiff <- downloadHandler(
    filename <- function() { paste('rate.tiff') },
    content <- function(file) {
      ggsave (file, plot = rate_plot(), width = 15, height = 10)
    },
    contentType = 'application/tiff'
  )
})
  
