#############################################################
### Jose A Espinosa. CSN/CB-CRG Group. Dec 2015           ###
#############################################################
### Script to develop the data frame and plots that are   ###
### going to be shown in the behavioral browser           ###
#############################################################
library (plotrix) #std.err # mirar si la utilizo
# library (ggplot2)
# library(plyr)
# library("GenomicRanges")
# source("http://bioconductor.org/biocLite.R") #biocLite("BiocUpgrade")  
# biocLite("ggbio") # load ggplot function that allows to use Granges 
# library("ggbio") # load ggplot function that allows Granges

source ("/Users/jespinosa/git/phecomp/lib/R/plotParamPublication.R")

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
tail(data_bed)

df.data_bed <- merge (data_bed, df.id_group , by.x= "id", by.y = "id")
# head (df.data_bed [which (df.data_bed$id==2),] )
 
colnames (df.data_bed) <- c("id", "chr", "start", "end", "V4", "value", "strand", "V7", "V8", "V9", "group")
df.data_bed$id <- as.numeric (df.data_bed$id)
df.data_bed$duration <- df.data_bed$end - df.data_bed$start
df.data_bed$duration <- df.data_bed$end - df.data_bed$start
df.data_bed$rate <- df.data_bed$value / df.data_bed$duration 
  
tail(df.data_bed)

df.data_bed$group <- factor(df.data_bed$group , levels=c("control", "case"), 
                            labels=c("control", "case"))

# Probably I have to do something with height of tracks, and always shown as many tracks as available in the data,
# even if the are empty, unique (id) --> ylim
df.data_bed$group_id <- paste(df.data_bed$id, df.data_bed$group, sep="_")
df.data_bed <- df.data_bed [with(df.data_bed, order(group, id)), ]
# df.data_bed_filt  <- transform (df.data_bed_filt [which (df.data_bed_filt$group == controlGroupLabel), ], new_id=match(group_id, unique(group_id)))
# transform (df.data_bed_filt [which (df.data_bed_filt$group == caseGroupLabel), ], new_id=match(group_id, unique(group_id)))

df.data_bed <- transform (df.data_bed, new_id=match(group_id, unique(group_id)))

length (df.data_bed[,1])

ini_window <- 1000000
end_window <- 1600000
n_tracks <- length(unique(df.data_bed$id))

df.data_bed_filt <- df.data_bed [which (df.data_bed$start > ini_window & df.data_bed$end < end_window),]
# pos <- 1000
# pos <- 1000000
pos <- 569000
# input_windowsize <- 1001
input_windowsize <- 100100

df.data_bed_filt <- df.data_bed [which (df.data_bed$start > max( pos - input_windowsize, 0 ) & 
                      df.data_bed$end < min( pos + input_windowsize, max(df.data_bed$end))),]
head (df.data_bed_filt)
tail (df.data_bed_filt)
# df_t <- with (df.data_bed_filt , aggregate (cbind (value), list (group=group), mean))
# df_t <- with (df.data_bed_filt , aggregate (cbind (value), list (group=group),FUN=function (x) c (mean=mean(x), std.error=std.error(x))))
df_t <- with (df.data_bed_filt, aggregate (cbind (value, duration, rate), list (group=group),FUN=function (x) c (mean=mean(x), std.error=std.error(x), length(x))))

df_t$meanValue <- df_t$value [,1]
df_t$std.errorValue <- df_t$value [,2]
df_t$number <- df_t$value [,3]

df_t$meanDuration <- df_t$duration [,1]
df_t$std.errorDuration <- df_t$duration [,2]

df_t$meanRate <- df_t$rate [,1]
df_t$std.errorRate <- df_t$rate [,2]

df.mean_bad <- df_t

p = ggplot(data = df.mean_bad, aes(x=group, y=meanValue, fill=group)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_errorbar(aes(ymin=meanValue-std.errorValue, ymax=meanValue+std.errorValue), width=.2, position=position_dodge(.9))

p  


# Example extracted from another script
# ggplot(data=tbl_stat_mean, aes(x=index, y=mean, fill=group2)) + 
#   #        geom_bar(stat="identity", position=position_dodge()) +
#   geom_bar(stat="identity", position=position_dodge(), show_guide = FALSE) +
#   geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   #               scale_x_continuous(breaks=1:9, limits=c(0.6,9.5))+
#   #               scale_x_continuous(breaks=1:28, limits=c(0.6,28.5))+
#   scale_x_continuous(breaks=seq(2,24,3), labels=c(1:8), limits=c(0,24.5))+
#   scale_y_continuous(limits=c(0, max(tbl_stat_mean$mean, na.rm=TRUE) + max(tbl_stat_mean$std.error, na.rm=TRUE))) +                
#   labs (title = title_plot) +  
#   labs (x = "\nDevelopment week\n", y=y_lab, fill = NULL) +
#   scale_fill_manual(values=cols, labels=c("Ctrl 24h before", "Ctrl after cleaning", "Ctrl 24h after", 
#                                           "HF 24h before", "HF after cleaning", "HF 24h after"))

# I finally used this version
library(ggplot2)
df.data_bed_filt
# I have to show always 10 bins or something meaningful depending of the length of the data I am going to show

unique(df.data_bed_filt$new_id)
unique(df.data_bed_filt$new_id) + 0.5
p_bed<- ggplot(df.data_bed_filt) + 
  geom_rect(aes(xmin = start, xmax = end, ymin = new_id, ymax = new_id + 0.9, fill=group)) 
#   geom_bar(aes(xmin = start, xmax = end, ymin = group_id, ymax = group_id + 0.9)) +
#   scale_y_continuous(limits=c(1, n_tracks+1), labels=new_id)
+
  theme_bw()

ggplot(df.data_bed_filt) + 
  geom_linerange(aes(x =  new_id, ymin = start, ymax = end), size =30) + coord_flip()


##########################
### Reading BEDGRAPH files
path_files <- "/Users/jespinosa/git/shinyPergola/data/bed4test"

colours_v <- c("darkgreen", "red", "magenta", "black") 

setwd(path_files)
list_files_bedGr <-list.files(path=path_files ,pattern = ".bedGraph$")
# list_files_bedGr

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

head (data_bedGr)
data_bedGr <- merge (data_bedGr, df.id_group, by.x= "id", by.y = "id")
colnames (data_bedGr) <- c("id", "chrom", "start", "end", "value", "group")
head (data_bedGr)
data_bedGr$id <- as.numeric (data_bedGr$id)
data_bedGr$group <- factor(data_bedGr$group , levels=c("case", "control"), 
                            labels=c("case", "control"))

data_bedGr$n_group <- c(1:length (data_bedGr$group))
data_bedGr$n_group [data_bedGr$group == controlGroupLabel] <- 1
data_bedGr$n_group [data_bedGr$group == caseGroupLabel] <- 2

data_bedGr$group_id <- paste (data_bedGr$n_group, data_bedGr$id, sep="_")
# data_bedGr <- data_bedGr [with(data_bedGr, order(-group, id)), ]

# data_bedGr <- transform(data_bedGr, new_id=match(group_id, unique(group_id)))
# data_bedGr$new_id <-  with(data_bedGr, factor(new_id, levels = rev(levels(new_id))))

head(data_bedGr)

# data_bedGr$group_id <- paste (data_bedGr$group, data_bedGr$id, sep="_")
data_bedGr$value[1]<-  -3.5
# if (any (data_bedGr$value < 0))
min_v <- floor (min (data_bedGr$value))
max_v <- ceiling (max(data_bedGr$value))

# source("http://bioconductor.org/biocLite.R")
# biocLite("Sushi")
# library("Sushi")


df.dataBedgraph_f <- data_bedGr[which (data_bedGr$start > max( pos - input_windowsize, 0 ) & 
                                         data_bedGr$end < min( pos + input_windowsize, max(data_bedGr$end))),]

tr_1 <- data_bedGr [which (data_bedGr$id == 1),]
head (tr_1)
# library("Sushi")
# plotBedgraph(tr_1, "chr1", 100,915000)

p <- ggplot (data = tr_1, fill=group) + 
#      geom_line(data = tr_1, aes(x = , y = Percent.Change, color = "red"))
     geom_rect (aes(xmin = start, xmax = end, ymin = id, ymax = id + value)) 
p
library("gridExtra")
grid.arrange(p,p, heights = c(5/10, 5/10)) 

# input_bedGraphRange <- c(-4, 0.5)
input_bedGraphRange <- c(-2, 0.5)
input_bedGraphRange <- c(-1.7, 0.5)

# All values over the threshold are not shown becuase they are truncated, I set all to the value of the threshold
df.dataBedgraph_f [df.dataBedgraph_f$value < input_bedGraphRange [1], "value"] <- input_bedGraphRange[1]+0.001
df.dataBedgraph_f [which (df.dataBedgraph_f$value > input_bedGraphRange [2]), "value"] <- input_bedGraphRange [2] - 0.001

# Using facet to do vertical ploting of bedgraph files
df.dataBedgraph_f$group <- factor(df.dataBedgraph_f$group , levels=c("control","case"), 
                           labels=c("control", "case"))

p <- ggplot (data = df.dataBedgraph_f) + 
  #      geom_line(data = tr_1, aes(x = , y = Percent.Change, color = "red"))
  geom_rect (aes(xmin = start, xmax = end, ymin = 0, ymax = value, fill=group)) +
  scale_y_continuous(limits=input_bedGraphRange, breaks=input_bedGraphRange, labels=input_bedGraphRange)  + 
  facet_wrap(~ group_id, ncol= 1)
p

df.dataBedgraph_f$group_id
p_bedGraph <- p + theme(strip.background = element_blank(),
       strip.text.x = element_blank())
p_bedGraph
# http://www.r-bloggers.com/r-recipe-aligning-axes-in-ggplot2/
library(gridExtra)
p1 <- ggplot_gtable(ggplot_build(p_bed))
p2 <- ggplot_gtable(ggplot_build(p_bedGraph))
maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
p1$widths[2:3] <- maxWidth
p2$widths[2:3] <- maxWidth

grid.arrange(p1, p2, heights = c(2, 2)) 


############
## Ploting environmental info
inFile_datapath <- "/Users/jespinosa/git/shinyPergola/data/bed4test/tr_1_3_2_5_4_7_6_9_8all_data_types.bed"
input_header <- FALSE
df_env <- read.table(inFile_datapath, header=input_header, sep="\t")
df_env$id <- as.factor (c(1: length(df [,1])))
df_env
# df_env <- df_env [which (df_env$V2 > max( pos - input_windowsize, 0 ) & 
#                                      df_env$V3 < min( pos + input_windowsize, max(df_env$V3))),]

df_env
x_axis_r <- c (max( pos - input_windowsize, 0 ), min( pos + input_windowsize, max(df_env$V3)))

# From:
# http://stackoverflow.com/questions/3916195/finding-overlap-in-ranges-with-r

range_r <- df_env[1,] 
# x_axis_r <- c(1000,570000)
range_r$V2 <- x_axis_r[1] 
range_r$V3 <- x_axis_r[2]

ranges <- merge(df_env, range_r, by="V1",suffixes=c("A","B"))
ranges_i <- ranges [with(ranges, V2A >= V2B & V3A <= V3B),][,c(0:9)] 

# left thresholding
ranges_l <- ranges [with(ranges, V2A < V2B & V3A < V3B),][,c(0:9)]
ranges_l$V2A <- x_axis_r[1]+0.001

# right thresholding
ranges_r <- ranges [with(ranges, V2A < V3B & V3A > V3B),][,c(0:9)]
ranges_r$V3A <- x_axis_r[2]-0.001
ranges_p <- rbind (ranges_l, ranges_i, ranges_r)
ranges_p$id <- as.factor (c(1:length(ranges_p[,1])))
p = ggplot (data = ranges_p) + 
  geom_rect (aes(xmin = V2A, xmax = V3A, ymin = 0, ymax = 1, fill=id)) +
  scale_x_continuous(limits=x_axis_r)
  #       scale_fill_manual(values=colours_v) +
  #         scale_y_continuous(limits=input$bedGraphRange, breaks=input$bedGraphRange, labels=input$bedGraphRange) + 
  #       facet_wrap(~group_id, ncol= 1) + 
#   theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.y = element_text(size=10)) 
print(p)

## Miscelanea
# Create and IRanges object from a data frame coming from a bed file
# bedRanges <- with(df.data_bed_filt, GRanges(chr, IRanges(start+1, end), strand, value, duration, rate, group,  id=id))
# eventually use makeGRangesFromDataFrame 
# ??makeGRangesFromDataFrame

# str(bedRanges)
# ir <- ranges(bedRanges)
# dat <- cbind(as.data.frame(ir), bin = bins)
# class(dat$bin)

# df.data_bed_filt$id <- as.numeric (df.data_bed_filt$id)


#############
#### http://stackoverflow.com/questions/21506724/how-to-plot-overlapping-ranges-with-ggplot2
# biocLite("IRanges") #plotRanges is inside the vignette
ir <- IRanges(c(3, 8, 14, 15, 19, 34, 40),
              width = c(12, 6, 6, 15, 6, 2, 7))
bins <- disjointBins(IRanges(start(ir), end(ir) + 1))

dat <- cbind(as.data.frame(ir), bin = bins)

library(ggplot2)
ggplot(dat) + 
  geom_rect(aes(xmin = start, xmax = end,
                ymin = bin, ymax = bin + 0.9)) +
  theme_bw()

library(GenomicRanges)
# biocLite("trackViewer")
library("trackViewer")
gir = GRanges(seqnames="chr1", ir, strand=c(rep("+", 4), rep("-",3)))
plotGRanges(bedRanges, xlim=c(0,60))

set.seed(123)
gr.b <- GRanges(seqnames = "chr1", IRanges(start = seq(1, 100, by = 10),
                                           width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100), id = rep(1:4, each = 3, len = 10)) 
gr.b 

## bar 

ggplot(gr.b) + geom_bar(aes(fill = value)) + geom_segment(stat = "identity", aes(y = score + 2))

## line
ggplot(gr.b) +  geom_rect(aes(xmin = start, xmax = end,
                              ymin = bin, ymax = bin + 0.9))

# geom_rect(aes(xmin = start, xmax = end,
#               ymin = bin, ymax = bin + 0.9)) +


par(mfrow=c(4,1), mar=c(4,2,2,2))

plotRanges(ir, xlim=c(0,60))
plotRanges(reduce(ir), xlim=c(0,60))
plotRanges(disjoin(ir), xlim=c(0,60))
plotRanges(gaps(ir), xlim=c(0,60))

library(GenomicRanges)
gir = GRanges(seqnames="chr1", ir, strand=c(rep("+", 4), rep("-",3)))

par(mfrow=c(4,1), mar=c(4,2,2,2))
class (gir)
plotGRanges(gir, xlim=c(0,60), range=c(0,60))
plotGRanges(resize(gir,1), xlim=c(0,60))
plotGRanges(flank(gir,3), xlim=c(0,60), col="purple")
plotGRanges(flank(gir,2,start=FALSE), xlim=c(0,60), col="brown")

#####
# http://bioconductor.org/packages/release/bioc/manuals/trackViewer/man/trackViewer.pdf
gr1 <- GRanges("chr1", IRanges(1:50, 51:100))
gr2 <- GRanges("chr1", IRanges(seq(from=10, to=80, by=5),
                               seq(from=20, to=90, by=5)))
vp <- plotGRanges(gr1, gr2, range=GRanges("chr1", IRanges(1, 100)))
addGuideLine(guideLine=c(5, 10, 50, 90), col=2:5, vp=vp)
gr <- GRanges("chr1", IRanges(c(1, 11, 21, 31), width=9),
              score=c(5, 10, 5, 1))
plotGRanges(gr, range=GRanges("chr1", IRanges(1, 50)))

######
### http://davetang.org/muse/2013/10/03/using-gviz/
# biocLite("Gviz")

#load the package
library(Gviz)
ref <- GRanges('chr', IRanges(1, 500))
ref_track <- GenomeAxisTrack(ref, lwd=4, fontsize=20)

data <- data.frame(chr=c('chr1','chr1','chr1'),
                   start=c(50,200,400),
                   end=c(75, 250, 500),
                   id=c('one', 'two', 'three'),
                   strand=c('+','-','-'))

data_g <- with(data, GRanges(chr, IRanges(start, end), strand, id=id))
data_g
data_track <- AnnotationTrack(data_g, name = "Features", width = 15, showFeatureId = T, min.height=2)
plotTracks(c(ref_track, data_track))

####
# https://github.com/kasperdanielhansen/genbioconductor/blob/master/Rmd/IRanges_Basic.Rmd
# http://kasperdanielhansen.github.io/genbioconductor/
source("http://www.bioconductor.org/biocLite.R")
biocLite(c("IRanges"))
library(IRanges)
ir <- IRanges(start = c(1,3,7,9), end = c(4,4,8,10))

#####
plotRanges <- function(x, xlim = x, main = deparse(substitute(x)), col = "black", sep = 0.5, ...)
    {
        height <- 1
        if (is(xlim, "Ranges"))
            xlim <- c(min(start(xlim)), max(end(xlim)))
        bins <- disjointBins(IRanges(start(x), end(x) + 1))
        plot.new()
        plot.window(xlim, c(0, max(bins)*(height + sep)))
        ybottom <- bins * (sep + height) - height
        rect(start(x)-0.5, ybottom, end(x)+0.5, ybottom + height, col = col, ...)
        title(main)
        axis(1)
      }
plotRanges(ir)
plotRanges(reduce(ir))

ir
reduce(ir)

disjoin(ir1)

plotRanges(ir)

plotRanges(disjoin(ir))
