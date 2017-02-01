
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

bed2pergViz <- function (df, gr_df, format_f="BED") {
  grps <- as.character(gr_df[[setdiff(colnames(gr_df), 'sample')]])
  
  r <- lapply(unique(grps),
         function(g) {
           gr_samps <- grps %in% g
           gr_files <- df$path[gr_samps]
           
           lapply(gr_files, function (bed) {             
             id <- gsub(".+tr_(\\d+)(_.+$)", "\\1", bed)
             bed_GR <- import(bed, format = format_f)
             
             if (format_f == "BED") {
               min_start <- min(start(bed_GR))
               max_end <- max(end(bed_GR))
               
               tr <- AnnotationTrack(bed_GR, name = paste ("", id, sep=""))#, fill=col_ctrl, background.title = col_ctrl)               
             }
             
             if (format_f == "bedGraph") {
               min_start <- min(bed_GR$score)
               max_end <- max(bed_GR$score)
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
    displayPars(l) <- list(fill=color, background.title = color, col=NULL) 
    return (l)
  })
  
  list_all <- append(list_all, list_gr)  
}

l_gr_annotation_tr_bg <- bed2pergViz (bg2v, exp_info, "bedGraph") 
list_all_bg <- list()

# min(l_gr_annotation_tr_bg[[1]][[1]]$score)

for (i in 1:length(l_gr_annotation_tr_bg)){
#   list_gr_bg <- lapply (l_gr_annotation_tr_bg[[i]], function (l, color=cb_palette[i]) { 
    #     displayPars(l) <- list(col=color, type="l", ylim = c(0, 0.5)) 
    for (j in 1:length(l_gr_annotation_tr_bg[[i]])){
      GR <- l_gr_annotation_tr_bg[[i]][[j]]
#       print (names (l_gr_annotation_tr_bg[[i]][j]))
      id <- gsub(".+tr_(\\d+)(_.+$)", "\\1", names (l_gr_annotation_tr_bg[[i]][j]))
      d_tr <- DataTrack(GR, name = id, background.title = cb_palette[i],
                        type="heatmap", ylim = c(0, 0.5),
                        gradient=c('white','blue'))#, fill=col_ctrl, background.title = col_ctrl) 
      
#       list_all_bg <- append(list_all_bg, list_gr_bg)   
      list_all_bg <- append (list_all_bg, d_tr)
    }
    
    
#     displayPars(l) <- list(type="heatmap", ylim = c(0, 0.5),
#                            gradient=c('white','blue')) 
   
  #   displayPars(annotation_tr) <- list(fill=cb_palette[i], background.title = cb_palette[i])
  
}


# bedg2pergViz <- function (df, gr_df, format_f="bedGraph") {
#   grps <- as.character(gr_df[[setdiff(colnames(gr_df), 'sample')]])
#   
#   r <- lapply(unique(grps),
#               function(g) {
#                 gr_samps <- grps %in% g
#                 gr_files <- df$path[gr_samps]
#                 
#                 lapply(gr_files, function (bedg) {             
#                   id <- gsub(".+tr_(\\d+)(_.+$)", "\\1", bedg)
#                   bed_GR <- import(bedg, format = format_f)
#                   data <- read.table (bedg)
#                   min_start <- min(data$V2)
#                   max_end <- max(data$V3)
# 
#                   if (g_min_start > min_start) { g_min_start <<- min_start }
#                   if (g_max_end < max_end) { g_max_end <<- max_end }
# 
# #                   return (data$V4) })
#                 return (bed_GR) })
#               })
#   
#   names(r) <- unique(grps)
#   return (r)
# }

# l_gr_annotation_tr_bg <- bed2pergViz (bg2v, exp_info, "bedGraph") 

list_gr <- list()

for (i in 1:length(l_gr_annotation_tr_bg)){
  GR <- GRanges()
  
  for (j in 1:length(l_gr_annotation_tr_bg[[i]])){
    GR <- append(GR, l_gr_annotation_tr_bg[[i]][[j]])
  }
  
  dt <- DataTrack(GR, name = "mean intake (mg)", type=c("a"), #, "p"))
            col=cb_palette[i],
            ylim = c(0, 1), legend=FALSE)
  
  list_gr[[i]] <- dt
}

names(list_gr) <- names(l_gr_annotation_tr_bg)
list_gr <- rev(list_gr)
o_tr <-OverlayTrack(list_gr)

# 
# # l_gr_annotation_tr_bg$control$tr_1_dt_food_sc
# 
# list_all_bg <- list()
# 
# for (i in 1:length(l_gr_annotation_tr_bg)){
# #   print (names (l_gr_annotation_tr_bg[[i]]))
#   
#   list_gr_bg <- sapply (l_gr_annotation_tr_bg[[i]], y <-function (l, color=cb_palette[i]) { 
# #         displayPars(l) <- list(col=color, type="l", ylim = c(0, 0.5)) 
#     
#     displayPars(l) <- list(type="heatmap", ylim = c(0, 0.5),
#                            gradient=c('white','blue')) 
# #     return (as.vector(l))
#     return (l)
#   })

#   print (list_gr_bg)
#   df_all_bg <- as.data.frame(list_gr_bg) # diferente medida
#   print(df_all_bg)
#   print(head (list_gr_bg))
  #   displayPars(annotation_tr) <- list(fill=cb_palette[i], background.title = cb_palette[i])
#   list_all_bg <- append(list_all_bg, list_gr_bg)  
# }

# names(list_all_bg)
# as.data.frame(list_all_bg)
# list_all_bg[[1]]
# 
# df_bedGr_case
# gr <- as(cov, "GRanges")
# new_cv <- coverage(gr, weight="score", width=your_seqinfo)
# coverage(df_bedGr_case$m10, weight="score", width=your_seqinfo)
# 
# coverage(l_gr_annotation_tr_bg[[1]][[2]],weight="score", width=NULL)
# length (l_gr_annotation_tr_bg[[1]][[2]])
# length (l_gr_annotation_tr_bg[[1]][[1]])
# setdiff(l_gr_annotation_tr_bg[[1]][[1]],l_gr_annotation_tr_bg[[1]][[2]] )
# 
# l_gr_annotation_tr_bg[[2]][[2]][!(l_gr_annotation_tr_bg[[2]][[2]] %over% l_gr_annotation_tr_bg[[2]][[1]])]
# 
# joinGranges<- c(l_gr_annotation_tr_bg[[1]][[1]],l_gr_annotation_tr_bg[[1]][[2]])
# 
# joinDataTrack<-DataTrack(joinGranges, name = "midbody speed (microns/s)", type="heatmap",  
#                     gradient=c('blue', 'white','red'),
# #                     groups = c("ctrl", "case"), col=c(col_case, col_ctrl), 
#                     legend=FALSE)
# 
# plotTracks(joinDataTrack, from =1, to =2000)
# head (list_all)
# class(l_gr_annotation_tr_bg[[1]][[2]])
# 
# ######del
# scores_bedGr_ctrl <- sapply (ctrl.mice.bedGraph.files, y <- function (x) { data <- read.table (x)                                                      
#                                                                            id <- gsub("(^tr_)([^.]+)(_dt.+$)", "\\2", x)                                                                   
#                                                                            return (data$V4)
# })
# 
# df_bedGr_ctrl <- data.frame(scores_bedGr_ctrl)
# head (df_bedGr_ctrl)
# names(df_bedGr_ctrl) <- paste ("m", gsub(".+tr_(\\d+)(_.+$)", "\\1", ctrl.mice.bedGraph.files), sep="")
# ## Reverse order to make them appear from 1 to 18
# df_bedGr_ctrl_ordered <- df_bedGr_ctrl[ , rev(mixedsort(names(df_bedGr_ctrl)))]
# colnames(df_bedGr_ctrl_ordered)
# ctrl_bedg_GR <- GRanges()
# ctrl_bedg_GR <- import(ctrl.mice.bedGraph.files[[1]], format = "bedGraph")
# mcols(ctrl_bedg_GR) <- df_bedGr_ctrl_ordered
###########################del



# unlist (l_gr_annotation_tr_bg[[1]])
# o_tr<- OverlayTrack(list_all_bg)

# plotTracks(list_all_bg[[1]], from=900,
#                       to=1500)
# class (l_gr_annotation_tr_bedg)
# class(list_all)
# list_all
# warnings()
# plotTracks(list_all, from=1,
#            to=10000, shape = "box")

# min_t <- floor (min (df.data_bed$start))
# max_t <- ceiling (max(df.data_bed$end))

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
    sliderInput("windowsize", "Windowsize:", min = min(g_max_end, 1000), max = max(g_max_end, 1000000), 
                value =min(g_max_end, 1000), step = min(g_max_end, 300))
  })
  
  output$plotbed <- renderPlot({
    if(length(input$windowsize)==0){
      return(NULL)
    }
    else{

        pt <- plotTracks(c(g_tr, list_all, list_all_bg, o_tr), 
#       pt <- plotTracks(c(g_tr, list_all, o_tr),
#                          from=pos(), to=pos() + input$windowsize,
                         from=input$tpos, to=input$tpos+ input$windowsize,
                         shape = "box", stacking = "dense")

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
