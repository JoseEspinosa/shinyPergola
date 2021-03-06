# https://www.bioconductor.org/packages/devel/bioc/vignettes/h5vc/inst/doc/h5vc.tour.html
# biocLite("h5vcData")
suppressPackageStartupMessages(library(h5vc))
suppressPackageStartupMessages(library(rhdf5))

H5close()


tallyFile <- system.file( "extdata", "example.tally.hfs5", package = "h5vcData" )


#############
#h5ls(tallyFile)
chromosomes  <- h5ls( tallyFile )
sampleData <- getSampleData( tallyFile, "/ExampleStudy/16" )

sampleData

sampleData$ClinicalVariable <- rnorm(nrow(sampleData))
setSampleData( tallyFile, "/ExampleStudy/16", sampleData )
sampleData

data <- h5readBlock(
  filename = tallyFile,
  group = "/ExampleStudy/16",
  names = c( "Coverages", "Counts" ),
  range = c(29000000,29001000)
)
str(data)

suppressPackageStartupMessages(require(GenomicRanges))
data <- h5dapply(
  filename = tallyFile,
  group = "/ExampleStudy",
  names = c( "Coverages" ),
  dims = c(3),
  range = GRanges("16", ranges = IRanges(start = seq(29e6, 30e6, 5e6), width = 1000))
)
str(data)

rangeA <- GRanges("16", ranges = IRanges(start = seq(29e6, 29.5e6, 1e5), width = 1000))
rangeB <- GRanges("22", ranges = IRanges(start = seq(39e6, 39.5e6, 1e5), width = 1000))
coverages <- h5dapply(
  filename = tallyFile,
  group = "/ExampleStudy",
  names = c( "Coverages" ),
  dims = c(3),
  range = c(rangeA, rangeB),
  FUN = binnedCoverage,
  sampledata = sampleData
)
#options(scipen=10)
coverages <- do.call( rbind, lapply( coverages, function(x) do.call(rbind, x) ))
#rownames(coverages) <- NULL #remove block-ids used as row-names
coverages

variants <- h5dapply(
  filename = tallyFile,
  group = "/ExampleStudy/16",
  names = c( "Coverages", "Counts", "Deletions", "Reference" ),
  range = c(29950000,30000000),
  blocksize = 10000,
  FUN = callVariantsPaired,
  sampledata = sampleData,
  cl = vcConfParams(returnDataPoints = TRUE)
)
variants <- do.call( rbind, variants )
variants$AF <- (variants$caseCountFwd + variants$caseCountRev) / (variants$caseCoverageFwd + variants$caseCoverageRev)
variants <- variants[variants$AF > 0.2,]
rownames(variants) <- NULL # remove rownames to save some space on output :D
variants

windowsize <- 35
position <- variants$Start[2]
data <- h5readBlock(
  filename = tallyFile,
  group = paste( "/ExampleStudy", variants$Chrom[2], sep="/" ),
  names = c("Coverages","Counts","Deletions", "Reference"),
  range = c( position - windowsize, position + windowsize)
)



p = mismatchPlot(
  data_2,
  sampleData,
  #       samples = c("s288c","YB210"),
  samples = c("PT5PrimaryDNA", "PT5RelapseDNA", "PT5ControlDNA"),
  input_windowsize,
  pos
)

# data <- h5dapply(
#   filename = tallyFile,
#   group = paste( "/ExampleStudy", variants$Chrom[2], sep="/" ),
#   blocksize = 200*3,
#   names = c("Coverages","Counts","Deletions"),
#   range = c( position - windowsize, position + windowsize))[[1]]

patient <- sampleData$Patient[sampleData$Sample == variants$Sample[2]]
samples <- sampleData$Sample[sampleData$Patient == patient]
p <- mismatchPlot(
  data = data,
  sampledata = sampleData,
  samples = samples,
  windowsize = windowsize,
  position = position
)
print(p)

#############
chromlengths <- as.numeric(subset( chromosomes, otype == "H5I_DATASET" & name == "Reference" )$dim)
# chromosomes  <- subset( chromosomes, otype == "H5I_GROUP" & name != "yeast" )$name
chromosomes  <- subset( chromosomes, otype == "H5I_GROUP" & name != "ExampleStudy" )$name
names(chromlengths) = chromosomes
input_chrom <- 16
input_chrom_index <- 1
input_windowsize <- 35
study <- "/ExampleStudy"
group <-  paste( study, input_chrom, sep="/" ) 
input_gpos <- 50
#   input_gpos <- 29950746
# sampleData <- { sd = getSampleData( tallyFile, group ); sd$Sample = c("PT5PrimaryDNA", "PT5RelapseDNA", "PT5ControlDNA"); sd }
pos <- {
  min( max( input_windowsize + 1, input_gpos ), chromlengths[input_chrom_index] - input_windowsize - 1 )
}
############

data_2 <- {
  h5dapply(
    tallyFile,
    group,
    blocksize = input_windowsize*3,
    names = c("Coverages","Counts","Deletions"),
    #       range = c(10000,40000))
    #           range = c( max( pos - input_windowsize, 0 ), min( pos + input_windowsize, chromlengths[input_chrom_index] )  ))
    range =c( position - windowsize, position + windowsize) )
}[[1]]

str(data_2)
data_2$h5dapplyInfo
pos=29983010
p = mismatchPlot(
  data_2,
  sampleData,
  #       samples = c("s288c","YB210"),
  samples = c("PT5PrimaryDNA", "PT5RelapseDNA", "PT5ControlDNA"),
  input_windowsize,
  pos
)
print(p)
