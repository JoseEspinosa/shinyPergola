library(shiny)
library(h5vc)
library(rhdf5)
# tallyFile <- "/Library/Frameworks/R.framework/Versions/3.1/Resources/library/h5vcData/extdata/example.tally.hfs5"
tallyFile <- system.file( "extdata", "example.tally.hfs5", package = "h5vcData" )
# study <- "/yeast"
study <- "/ExampleStudy"
H5close()

# runApp("/Users/jespinosa/git/shinyPergola/examples/shiny_minGB")

h5ls( tallyFile )
chromosomes  <- h5ls( tallyFile )
chromlengths <- as.numeric(subset( chromosomes, otype == "H5I_DATASET" & name == "Reference" )$dim)
# chromosomes  <- subset( chromosomes, otype == "H5I_GROUP" & name != "yeast" )$name
chromosomes  <- subset( chromosomes, otype == "H5I_GROUP" & name != "ExampleStudy" )$name
names(chromlengths) = chromosomes

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  options(shiny.fullstacktrace = TRUE)
  
  output$chromSelect <- renderUI({
#     selectInput( "chrom", "Chromosome", choices = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","XIII","XIV","XV","XVI", "Mito"))
    selectInput( "chrom", "Chromosome", choices = c(16,22))
  })
  
  output$genomicPositionSelect <- renderUI({
    sliderInput( "gpos", "Genomic Position:", min = 10, max = chromlengths[input$chrom] - 10, value = 200 )
  })


  group <- reactive({ paste( study, input$chrom, sep="/" ) })
  
#   sampleData <- reactive({ sd = getSampleData( tallyFile, group() ); sd$Sample = c("YB210","s288c"); sd })
  sampleData <- reactive({ sd = getSampleData( tallyFile, group() ); sd$Sample = c("PT5PrimaryDNA", "PT5RelapseDNA", "PT5ControlDNA"); sd })
  pos <- reactive({
    min( max( input$windowsize + 1, input$gpos ), chromlengths[input$chrom] - input$windowsize - 1 )
  })
  
  data <- reactive({
    h5dapply(
      tallyFile,
      group(),
      blocksize = input$windowsize*3,
      names = c("Coverages","Counts","Deletions"),
      range = c( max( pos() - input$windowsize, 0 ), min( pos() + input$windowsize, chromlengths[input$chrom] )  )
    )[[1]]
#     data <- reactive({
#       h5readBlock(
#         tallyFile,
#         group(),
#         names = c("Coverages","Counts","Deletions"),
#         range = c( pos() - input$windowsize, pos() + windowsize))[[1]]
  })
  
  output$mismatchPlot <- renderPlot({
    p = mismatchPlot(
      data(),
      sampleData(),
#       samples = c("s288c","YB210"),
      samples = c("PT5PrimaryDNA", "PT5RelapseDNA", "PT5ControlDNA"),
      # This lines should be commented from the tutorial
#       input$windowsize,
#       pos()
    )
    print(p)
  })
})


################
###############
# chromlengths[3]
# 
# min( max( input$windowsize + 1, input$gpos ), chromlengths[input$chrom] - input$windowsize - 1 )
# data <- h5readBlock(
#     filename = tallyFile,
#     group = paste( "/ExampleStudy", 16, sep="/" ),
#     blocksize = 200*3,
#     names = c("Coverages","Counts","Deletions"),
#     range = c( max( 10000 - 200, 0 ), min( 10000) + 200, chromlengths[1] ) )[[1]]
#   


# data <- h5readBlock(
#   filename = tallyFile,
#   group = paste( "/ExampleStudy", variants$Chrom[2], sep="/" ),
#   names = c("Coverages","Counts","Deletions", "Reference"),
#   range = c( position - windowsize, position + windowsize))[[1]]
# 
# data <- h5dapply(
#   filename = tallyFile,
#   group = paste( "/ExampleStudy", variants$Chrom[2], sep="/" ),
#   blocksize = 200*3,
#   names = c("Coverages","Counts","Deletions"),
#   range = c( position - windowsize, position + windowsize))[[1]]
