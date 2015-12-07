#############################################################
### Jose A Espinosa. CSN/CB-CRG Group. Dec 2015           ###
#############################################################
### Script to develop the data frame and plots that are   ###
### going to be shown in the behavioral browser           ###
#############################################################

library (plotrix) #std.err # mirar si la utilizo
library (ggplot2)
# library(plyr)

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
# 
colnames (df.data_bed) <- c("id", "chrom", "startChrom", "endChrom", "V4", "value", "strand", "V7", "V8", "V9", "group")
df.data_bed$duration <- df.data_bed$endChrom - df.data_bed$startChrom
df.data_bed$duration <- df.data_bed$endChrom - df.data_bed$startChrom
df.data_bed$rate <- df.data_bed$value / df.data_bed$duration 
  
tail(df.data_bed)

df.data_bed$group <- factor(df.data_bed$group , levels=c("control", "case"), 
                            labels=c("control", "case"))

length (df.data_bed[,1])

ini_window <- 1000000
end_window <- 1600000

df.data_bed [which (df.data_bed$startChrom > max( 300 - input$windowsize, 0 ) & 
                      df.data_bed$endChrom < min( 300 + input$windowsize, max(df.data_bed$endChrom))),]

df.data_bed_filt <- df.data_bed [which (df.data_bed$startChrom > ini_window & df.data_bed$endChrom < end_window),]
pos <- 569984
input_windowsize <- 1000
# 
df.data_bed_filt <- df.data_bed [which (df.data_bed$startChrom > max( pos - input_windowsize, 0 ) & 
                      df.data_bed$endChrom < min( pos + input_windowsize, max(df.data_bed$endChrom))),]
head (df.data_bed_filt)
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

# df.mean_bad$mean <- df.mean_bad$value [,1]
# df.mean_bad$std.error <- df.mean_bad$value [,2]
# df.mean_bad$ymax <- df.mean_bad$mean + df.mean_bad$value [,2]
# df.mean_bad$ymin <- df.mean_bad$mean - df.mean_bad$value [,2]

# with (data() , aggregate (cbind (value), list (group=group), mean))
# df.mean_bad$group

# p = ggplot(data = df.mean_bad, aes(x=group, y=mean, fill=group)) +
#     geom_bar(stat="identity", position=position_dodge()) +
#     geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.2, position=position_dodge(.9))

# p  


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

# df.mean_bad$mean <- df.mean_bad$value [,1]
# df.mean_bad$std.error <- df.mean_bad$value [,2]

# head (df.data_bed_filt)
# choices_id <- unique (data_bed$id)
# data_bed data_bed$id == 1
# chromlengths <- as.numeric(subset( chromosomes, otype == "H5I_DATASET" & name == "Reference" )$dim)
# subset(data_bed, id=1)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
# mpgData <- mtcars
# mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))