library(shiny)
library(h5vc)
library(rhdf5)

# tallyFile <- "yeast.hfs5"
# study <- "/yeast"
# tallyFile <- "/Users/jespinosa/shinny_minGB/NRAS.tally.hfs5"
# study <- "/NRAS"
tallyFile <- "/Library/Frameworks/R.framework/Versions/3.1/Resources/library/h5vcData/extdata/example.tally.hfs5"
# setwd("/Users/jespinosa/shinny_minGB")
# tallyFile <- "example.tally.hfs5"
study <- "/ExampleStudy"
h5ls( tallyFile )
chromosomes  <- h5ls( tallyFile )
chromlengths <- as.numeric(subset( chromosomes, otype == "H5I_DATASET" & name == "Reference" )$dim)
# chromosomes  <- subset( chromosomes, otype == "H5I_GROUP" & name != "yeast" )$name
chromosomes  <- subset( chromosomes, otype == "H5I_GROUP" & name != "ExampleStudy")$name
names(chromlengths) = chromosomes
# subset( chromosomes, otype == "H5I_GROUP")$name
# chromosomes  <- subset( chromosomes, otype == "H5I_GROUP" & name != "yeast" )$name

# getSampleData( tallyFile, "/NRAS/1" )
# chrom <- 22
# group <- paste( study, chrom, sep="/" )
# getSampleData( tallyFile, group )
# { sd = getSampleData( tallyFile, group ); sd$Sample = c("PT5PrimaryDNA", "PT5ControlDNA"); sd }
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$chromSelect <- renderUI({
#     selectInput( "chrom", "Chromosome", choices = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","XIII","XIV","XV","XVI", "Mito"))
#     selectInput( "chrom", "Chromosome", choices = c("1"))
    selectInput( "chrom", "Chromosome", choices = c(16,22))
  })
  
  output$genomicPositionSelect <- renderUI({
#     sliderInput( "gpos", "Genomic Position:", min = 10, max = chromlengths[1] - 10, value = 200 )
    sliderInput( "gpos", "Genomic Position:", min = 10, max = chromlengths[input$chrom] - 10, value = 200 )
  })
  
#   group <- reactive({ paste( study, input$chrom, sep="/" ) })
#     group <- paste( study, 1, sep="/")
#   sd = getSampleData( tallyFile, group ); 
#   sd$Sample = c("AML","Control")
#   sd$Sample = c("YB210","s288c")
#   sd
# "YB210","s288c"
  sampleData <- reactive({ sd = getSampleData( tallyFile, group() ); sd$Sample = c("PT5PrimaryDNA", "PT5RelapseDNA", "PT5ControlDNA"); sd })
#   getSampleData( tallyFile, "/NRAS/1" )


  pos <- reactive({
    min( max( input$windowsize + 1, input$gpos ), chromlengths[input$chrom] - input$windowsize - 1 )
  })
# h5dapply(
#   tallyFile,
#   "/NRAS/1" ,
#   blocksize = 20*3,
#   names = c("Coverages","Counts","Deletions"),
#   range = c( max( 1000 - 20, 0 ), min( 1000 + 20, chromlengths["1"] )  )
# )[[1]]
  data <- reactive({
    h5dapply(
      tallyFile,
      group(),
      blocksize = input$windowsize*3,
#       names = c("Coverages","Counts","Deletions"),
      names = c("Coverages","Counts","Deletions"),
      range = c( max( pos() - input$windowsize, 0 ), min( pos() + input$windowsize, chromlengths[input$chrom] )  )
    )[[1]]
    
  })
  
  output$mismatchPlot <- renderPlot({
    p = mismatchPlot(
      data(),
      sampleData(),
#       samples = c("s288c","YB210"),
#       samples = c("AML","Control"),
#       samples = c("PT8PrimaryDNA", "PT5PrimaryDNA", "PT5RelapseDNA", "PT8EarlyStageDNA", "PT5ControlDNA", "PT8ControlDNA"),
      samples =c("PT5PrimaryDNA", "PT5RelapseDNA", "PT5ControlDNA"),
#       samples = c("PT8PrimaryDNA", "PT5PrimaryDNA"),
      input$windowsize,
      pos()
    )
    print(p)
  })
})
# H5close()
# getSampleData( "/Library/Frameworks/R.framework/Versions/3.1/Resources/library/h5vcData/extdata/example.tally.hfs5", "/ExampleStudy/22" )

