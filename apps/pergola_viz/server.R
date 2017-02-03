
col_gr_1 <- "darkblue"
col_gr_2 <- "brown"
col_ctrl <- col_gr_1
col_case <- col_gr_2
cb_palette <- c("#999999", "#E69F00", "#56B4E9",
                "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# "lightblue", "darkblue",

base_dir <- "/Users/jespinosa/git/shinyPergola/data"

# data_dir <- dir(file.path(base_dir,"bed4test"))
# data_dir <- file.path(base_dir,"bed4test")
# exp_design_f <- "exp_info_test.txt"
data_dir <- file.path(base_dir,"bed4test_all")
exp_design_f <- "exp_info.txt"

b2v <- s2c <- exp_info <- read.table(file.path(base_dir, exp_design_f), header = TRUE, stringsAsFactors=FALSE)

# exp_info$sample
kal_dirs <- perg_bed_files <- sapply(exp_info$sample, function(id) file.path(data_dir, paste(id, ".bed", sep="")))

# s2c <- exp_info <- read.table(file.path(base_dir, "exp_info_test.txt"), header = TRUE, stringsAsFactors=FALSE)
b2v <- s2c <- dplyr::mutate(b2v, path = perg_bed_files)
# s2c

kal_dirs <- perg_bedg_files <- sapply(exp_info$sample, function(id) file.path(data_dir, paste(id, ".bedGraph", sep="")))

bg2v <- s2c <- exp_info <- read.table(file.path(base_dir, exp_design_f), header = TRUE, stringsAsFactors=FALSE)
bg2v <- s2c <- dplyr::mutate(bg2v, path = perg_bedg_files)

unique(exp_info$condition)
# grps
g_min_start <- 100000000
g_max_end <- -100000000
min_v <- 0
max_v <- 0

bed2pergViz <- function (df, gr_df, format_f="BED") {
  grps <- as.character(gr_df[[setdiff(colnames(gr_df), 'sample')]])
  
  r <- lapply(unique(grps),
         function(g) {
           gr_samps <- grps %in% g
           gr_files <- df$path[gr_samps]
           
           lapply(gr_files, function (bed) {             
             id <- gsub(".+tr_(\\d+)(_.+$)", "\\1", bed)
             bed_GR <- import(bed, format = format_f)
             min_start <- min(start(bed_GR))
             max_end <- max(end(bed_GR))
             
             if (format_f == "BED") {                              
               tr <- AnnotationTrack(bed_GR, name = paste ("", id, sep=""))#, fill=col_ctrl, background.title = col_ctrl)               
             }
             
             if (format_f == "bedGraph") {
               min_v <<- floor (min(bed_GR$score))
               max_v <<- ceiling (max(bed_GR$score))
#                scores <- as.vector(mcols(bed_GR))
#                tr <- DataTrack(bed_GR, name = paste ("", id, sep=""))#, fill=col_ctrl, background.title = col_ctrl)               
               tr <- bed_GR
             }

             if (g_min_start > min_start) { g_min_start <<- min_start }
             if (g_max_end < max_end) { g_max_end <<- max_end }

             return (tr) })
         })
  
  names(r) <- unique(grps)
  return (r)
}

l_gr_annotation_tr_bed <- bed2pergViz (b2v, exp_info)

list_all <- list()

for (i in 1:length(l_gr_annotation_tr_bed)){
  list_gr <- lapply (l_gr_annotation_tr_bed[[i]], function (l, color=cb_palette[i]) { 
    displayPars(l) <- list(fill=color, background.title = color, col=NULL) # coll null for boxes lines
    return (l)
  })
  
  list_all <- append(list_all, list_gr)  
}

l_gr_annotation_tr_bg <- bed2pergViz (bg2v, exp_info, "bedGraph") 


# setdiff(l_gr_annotation_tr_bg[[1]][[1]], l_gr_annotation_tr_bg[[1]][[2]])
# subsetByOverlaps (l_gr_annotation_tr_bg[[1]][[1]], l_gr_annotation_tr_bg[[1]][[2]])
list_all_bg <- list()
group_lab <- c()
color_by_tr <- c()

for (i in 1:length(l_gr_annotation_tr_bg)){
  group_lab <- append(group_lab, rep (names(l_gr_annotation_tr_bg)[i], length(l_gr_annotation_tr_bg[[i]])))
  color_by_tr <- append(color_by_tr, cb_palette[i], length(l_gr_annotation_tr_bg[[i]]))
  
  for (j in 1:length(l_gr_annotation_tr_bg[[i]])){
    GR <- l_gr_annotation_tr_bg[[i]][[j]]

    id <- gsub(".+tr_(\\d+)(_.+$)", "\\1", names (l_gr_annotation_tr_bg[[i]][j]))
    d_tr <- DataTrack(GR, name = id, background.title = cb_palette[i],
                      type="heatmap", ylim = c(0, 0.5),
                      gradient=c('white','blue'))#, fill=col_ctrl, background.title = col_ctrl) 
    
    list_all_bg <- append (list_all_bg, d_tr)
  }
  
}

# list_gr <- list()
# vector_gr <- c()
# group_lab <- c()
# color_by_tr <- c()
# # GR_all <- GRanges()
# 
# for (i in 1:length(l_gr_annotation_tr_bg)){
#   GR <- GRanges()
#   
#   group_lab <- append(group_lab, rep (names(l_gr_annotation_tr_bg)[i], length(l_gr_annotation_tr_bg[[i]])))
#   color_by_tr <- append(color_by_tr, cb_palette[i], length(l_gr_annotation_tr_bg[[i]]))
#   
#   for (j in 1:length(l_gr_annotation_tr_bg[[i]])){
#     GR <- append(GR, l_gr_annotation_tr_bg[[i]][[j]])
# #     GR_all <- append(GR_all, l_gr_annotation_tr_bg[[i]][[j]])
#   }
#   
#   dt <- DataTrack(GR, name = "mean intake (mg)", type=c("a"), #, "p"))
#             col=cb_palette[i],
#             ylim = c(0, 1), legend=FALSE)
# #   
#   list_gr[[i]] <- dt
#   vector_gr <- c(vector_gr, GR)
# }
# 
# # GR_all
# names(list_gr) <- names(l_gr_annotation_tr_bg)
# list_gr <- rev(list_gr)
# 
# for (i in 1:length(list_gr)){
# #   displayPars(list_gr[[i]]) <- list(type=c("boxplot"), fill=cb_palette[i])
#   displayPars(list_gr[[i]]) <- list(type=c("a"), col=cb_palette[i])
# }
#  
# # o_tr_boxplot <-OverlayTrack(list_gr)
# # plotTracks  (o_tr_boxplot, name = "mean intake (mg)", #type=c("boxplot"), #, "p"))
# #                                 col=c("red", "blue"),
# #                                ylim = c(0, 1), legend=TRUE, from=1, to=5000)
# 
# 
# o_tr <-OverlayTrack(list_gr)

### Same starting and ending coordinates to create a single GRanges object with all the
### bedgraph files as metadatacolumns 
## with GRanges. 
## To obtain the values of the bedgraph files they have to be first in subsetByOverlaps
## and common intervals the second
# mcols(subsetByOverlaps (l_gr_annotation_tr_bg[[1]][[2]], common_intervals))

l_all_common_int <- list() 
common_intervals <- Reduce(subsetByOverlaps, c(unlist (l_gr_annotation_tr_bg))) 

for (i in 1:length(l_gr_annotation_tr_bg)){  
  l_gr_common_int <- sapply (l_gr_annotation_tr_bg[[i]], function (l, common_GR=common_intervals) { 
    mcol <- mcols(subsetByOverlaps (l, common_intervals)) 
    return (mcol)
#     return (data.frame(mcol))
  })
  
  l_all_common_int <- cbind(l_all_common_int, l_gr_common_int)  
}

df <- as.data.frame (unlist(l_all_common_int))

# This was not working problably because number of rows was not correctly set
# df <- data.frame(matrix(unlist(l_all_common_int), nrow=length(common_intervals), byrow=T))
names(df) <- paste ("id_", gsub(".+tr_(\\d+)(_.+$)", "\\1", names (unlist(l_gr_annotation_tr_bg))), sep="")

gr_common_intervals <- GRanges()
gr_common_intervals <- common_intervals
mcols(gr_common_intervals) <- df

common_bedg_dt <- DataTrack(gr_common_intervals, name = "mean intake (mg)", type="a",
                                    showSampleNames = TRUE, #ylim = c(0, 0.5),                                     
                                    groups = group_lab, col=color_by_tr,
                                    legend=FALSE)

# common_bedg_dt_boxPlot <- DataTrack(gr_common_intervals, name = "mean intake (mg)", type="a",
#                   showSampleNames = TRUE, #ylim = c(0, 0.5),                                     
#                   groups = group_lab, col=color_by_tr,
#                   legend=FALSE)

g_tr <- GenomeAxisTrack()

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
 
  
  output$genomicPositionSelect <- renderUI({
#     sliderInput( "tpos", "Time Point:", min = 10, max = g_max_end - 10, value = g_min_start + 10 )
    sliderInput( "tpos", "Time Point:", min = 0, max = g_max_end - 10, value = 0 )
  })
  
#   pos <-  reactive({
#     min( max( input$windowsize + 1, input$tpos ), max(g_max_end) - input$windowsize - 1 )    
#   })
  
  output$windowsize <- renderUI({                                                             
    sliderInput("windowsize", "Window size:", min = min(g_min_start, 1000), max = max(g_max_end, 1000000), 
                value =min(g_max_end, 3000), step = min(g_max_end, 300))
  })

  output$bedGraphRange <- renderUI({
    sliderInput("bedGraphRange", "Range bedgraph:", 
                min = min_v, max = max_v, value = c(0, 0.5), step= 0.1)
  }) 
  
  output$boxplot <- renderUI({                                                             
    checkboxInput("boxplot", "Add boxplot:", FALSE)
  })
  
  #  boxplot datatrack
  # it is overlap and then is very difficult to see anything
  boxplot_dt <- reactive({
    if(!is.null(input$boxplot) && input$boxplot == TRUE) {
      common_bedg_dt_boxplot <- common_bedg_dt 
      displayPars(common_bedg_dt_boxplot) <- list(type=c("boxplot"))
        
#       for (i in 1:length(list_gr)){
#         displayPars(list_gr[[i]]) <- list(type=c("boxplot"), fill=cb_palette[i])
# #           list(type=c("boxplot"), fill=cb_palette[i])
#       }
#       
#       o_tr_boxplot <-OverlayTrack(list_gr)
#       
#       IdeogramTrack(genome=input$ucscgen, chromosome=input$chr,
#                     showId=TRUE, showBandId=TRUE)
#       o_tr_boxplot
      common_bedg_dt_boxplot
    }
  })

  output$text1 <- renderText({ 
    paste (as.character (input$boxplot))
  })

  output$plotbed <- renderPlot({
    if(length(input$windowsize)==0){
      return(NULL)
    }
    else{
      if (input$boxplot==FALSE){
        pt <- plotTracks(c(g_tr, list_all, list_all_bg, common_bedg_dt), 
                         #       pt <- plotTracks(c(g_tr, list_all, o_tr),
                         #                          from=pos(), to=pos() + input$windowsize,
                         from=input$tpos, to=input$tpos+ input$windowsize,
                         ylim=c(input$bedGraphRange[1], input$bedGraphRange[2]),
                         shape = "box", stacking = "dense")        
      }
      else {
        
        pt <- plotTracks(c(g_tr, list_all, list_all_bg, common_bedg_dt, boxplot_dt()), 
                         #       pt <- plotTracks(c(g_tr, list_all, o_tr),
                         #                          from=pos(), to=pos() + input$windowsize,
                         from=input$tpos, to=input$tpos+ input$windowsize,
                         ylim=c(input$bedGraphRange[1], input$bedGraphRange[2]),
                         shape = "box", stacking = "dense")
      }
       
      return(pt)
    }
  })
})





# m iuygrpswqrv tgb fvv b   vb  n
# 
# scores_bedgr_n2 <- sapply (ctrl.worms.bedg.files, y <- function (x) { data <- read.table (x)                                                      
#                                                                       return (data$V4)
# })
# 
# bed2pergViz <- function (df) {
#   sapply(unique(grps),
#          function(g) {
#            gr_samps <- grps %in% g
#            gr_files <- s2c$path[gr_samps]
#            
#            lapply(gr_files, print)                                 
#            #            apply(gr_files, 1, print)
#            
#          })
# }
# 
# culo <- bed2pergViz (s2c)
# culo
# gr_sampsO <- grps %in% "control"
# gr_sampsO <- s2c$sample[gr_sampsO]
# class (s2c)
# s2c[s2c$condition=="control",]
# 
# bed2pergViz (s2c)
# 
# 
# 
# 
# 
# samps_gr <- 
# s2c [, valid_samps
# unique(exp_info$condition)
# # Here I obtain the ordered list of assignations
# grps <- as.character(exp_info[[setdiff(colnames(exp_info), 'sample')]])
# 
# grps %in% "control"
# 
# sapply(unique(grps),
#        function(g) {
#         gr_samps <- grps %in% g
#         gr_samps <- s2c$sample[gr_samps]
#         
#        })
# culo
# s2c$sample[gr_samps]
# 
# at_ctrl <- lapply(ctrl.mice.bed.files, function (bed) { 
#   mouse.id <- gsub(".+tr_(\\d+)(_.+$)", "\\1", bed)
#   at_bed <- AnnotationTrack(import(bed, format = "BED"), name = paste ("", mouse.id, sep=""), fill=col_ctrl, background.title = col_ctrl)
#   return (at_bed) })
# 
# 
