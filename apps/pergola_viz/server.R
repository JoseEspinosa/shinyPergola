
col_gr_1 <- "darkblue"
col_gr_2 <- "brown"
col_ctrl <- col_gr_1
col_case <- col_gr_2

base_dir <- "/Users/jespinosa/git/shinyPergola/data"

# data_dir <- dir(file.path(base_dir,"bed4test"))
data_dir <- file.path(base_dir,"bed4test")

# exp_info$sample
kal_dirs <- perg_bed_files <- sapply(exp_info$sample, function(id) file.path(data_dir, paste(id, ".bed", sep="")))


s2c <- exp_info <- read.table(file.path(base_dir, "exp_info.txt"), header = TRUE, stringsAsFactors=FALSE)
s2c <- dplyr::mutate(s2c, path = kal_dirs)
s2c

unique(exp_info$condition)
grps
g_min_start <- 100000000
g_max_end <- -100000000
bed2pergViz <- function (df, gr_df) {
  grps <- as.character(gr_df[[setdiff(colnames(gr_df), 'sample')]])
  
  r <- lapply(unique(grps),
         function(g) {
           gr_samps <- grps %in% g
           gr_files <- s2c$path[gr_samps]
           
           lapply(gr_files, function (bed) {             
             id <- gsub(".+tr_(\\d+)(_.+$)", "\\1", bed)
             bed_GR <- import(bed, format = "BED")
             min_start <- min(start(bed_GR))
             max_end <- max(end(bed_GR))

             if (g_min_start > min_start) { g_min_start <<- min_start }
             if (g_max_end < max_end) { g_max_end <<- max_end }

             at_bed <- AnnotationTrack(bed_GR, name = paste ("", id, sep=""))#, fill=col_ctrl, background.title = col_ctrl)
             return (annot_tr=at_bed) })
         })
  
  names(r) <- unique(grps)
  return (r)
}

l_gr_annotation_tr <- bed2pergViz (s2c, exp_info)

list_all <- list()
cb_palette <- c("lightblue", "darkblue","#999999", "#E69F00", "#56B4E9",
                "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

for (i in 1:length(l_gr_annotation_tr)){
#   print (i)
#   annotation_tr <- l_gr_annotation_tr[[i]]
  
  list_gr <- lapply (l_gr_annotation_tr[[i]], function (l, color=cb_palette[i]) { 
    displayPars(l) <- list(fill=color, background.title = color) 
    return (l)
  })
  
  
#   displayPars(annotation_tr) <- list(fill=cb_palette[i], background.title = cb_palette[i])
  list_all <- append(list_all, list_gr)  
}

# plotTracks(list_all, from=1,
#            to=10000, shape = "box")

# min_t <- floor (min (df.data_bed$start))
# max_t <- ceiling (max(df.data_bed$end))

g_tr <- GRanges()

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  output$genomicPositionSelect <- renderUI({
    sliderInput( "tpos", "Time Point:", min = 10, max = g_max_end - 10, value = g_min_start + 10 )
  })
  
  pos <-  reactive({
    min( max( input$windowsize + 1, input$tpos ), max(g_max_end) - input$windowsize - 1 )    
  })
  
  output$windowsize <- renderUI({
    sliderInput("windowsize", "Windowsize:", min = min(g_max_end, 1000), max = min(g_max_end, 1000000), 
                value =min(g_max_end, 1000), step = min(g_max_end, 300))
  })
  
  output$plotbed <- renderPlot({
    if(length(input$windowsize)==0){
      return(NULL)
    }
    else{

        pt <- plotTracks(list_all, 
                         from=pos(), to=pos() + input$windowsize,
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
