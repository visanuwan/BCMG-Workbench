library(Biostrings)

stats_sequence <- function(input) {
  # Calculate a statistics of sequence.
  #
  # Args:
  #   input: Nucleic sequence.
  #
  # Returns:
  #   Dataframe of stats
  print("Usage")
}

setGeneric("stats_sequence")

setMethod("stats_sequence",
          c(input="DNAString"),
          stats_sequence <- function(input) {
            # Calculate a statistics of sequence.
            #
            # Args:
            #   input: Nucleic sequence.
            #
            # Returns:
            #   Dataframe of stats

            freq <- alphabetFrequency(input)
            gcCount <- freq["C"] + freq["G"]
            allCount <- freq["A"] + freq["C"] + freq["G"] + freq["T"]

            # Calculate codon usage
            codon <- trinucleotideFrequency(input, step=3)
            normalizedCodon <- codon  # TODO
            # Convert to AA usage
            aaUsage <- rep(codon)
            names(aaUsage) <- GENETIC_CODE[names(codon)]
            aaUsage <- sapply(split(aaUsage, names(aaUsage)), sum)
            # normalizeCodon <- codon
            #
            ret <- list(length=length(input),
                        gcContent=gcCount/allCount,
                        codonUsage=normalizedCodon,
                        aaUsage=aaUsage)
            return(ret)
          })

setMethod("stats_sequence",
          c(input="DNAStringSet"),
          stats_sequence <- function(input) {
            # Calculate a statistics of sequence.
            #
            # Args:
            #   input: Nucleic sequence.
            #
            # Returns:
            #   Dataframe of stats

            # Calculate frequency and codon usage.
            freq <- colSums(alphabetFrequency(input)) # Different here...
            codon <- colSums(trinucleotideFrequency(input, step=3))

            gcCount <- freq["C"] + freq["G"]
            allCount <- freq["A"] + freq["C"] + freq["G"] + freq["T"]

            # Calculate codon usage
            normalizedCodon <- codon  # TODO
            # Convert to AA usage
            aaUsage <- rep(codon)
            names(aaUsage) <- GENETIC_CODE[names(codon)]
            aaUsage <- sapply(split(aaUsage, names(aaUsage)), sum)
            # normalizeCodon <- codon
            #
            ret <- list(length=length(input),
                        gcContent=gcCount/allCount,
                        codonUsage=normalizedCodon,
                        aaUsage=aaUsage)
            return(ret)
          })


  ##### Visualization of Codon/Aa usage #####
  #
  # Args:
  #   Data frame of stats of Codon/Aa usage:
  #   --------------------------------------
  #   |   codon_aa   |   type   |   value   |
  #   --------------------------------------
  #   |    codon     |    AAA   |  3.35984  |
  #   |    codon     |    CAA   |  1.58354  |
  #   |    codon     |    ...   |  .......  |
  #   |      aa      |     G    |  7.09010  |
  #   |      aa      |     A    |  9.51250  |
  #   |      aa      |     .    |  .......  |
  #   --------------------------------------
  #
  # Returns:
  #   Radar plots

codonUsage <- function(DF_CodonAaUsage) {

  # select a codon column
  x <- subset(DF_CodonAaUsage[DF_CodonAaUsage$codon_aa == 'codon',], select=c('type','value'))
  rownames(x) <- 1:nrow(x)
  x_min <- min(x$value)
  x_max <- max(x$value)

  # transpose from column to rows *** required by a fmsb package
  datax=as.data.frame(matrix( x$value , ncol=length(x$value)))
  colnames(datax) <- x$type
  datax <- rbind(rep(round(x_max, digits = 2),length(x$value)) ,
    rep(round(x_min, digits = 2),length(x$value)) , datax)

  # plot a radar chart from fmsb package
  radarchart( datax  , axistype=1 , seg = 5,
            
    #custom polygon
    pcol=rgb(0.5,0.0,0.0,0.9) , pfcol=rgb(0.5,0.0,0.0,0.5) , plwd=3 , 
    
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(round(x_min, digits = 2),
      round(x_max, digits = 2), (round(x_max, digits = 2)-round(x_min, digits = 2))/5), cglwd=0.8,
    
    #custom labels
    vlcex=0.8,

    #title
    title="Codon usage"
  )

}

aaUsage <- function(DF_CodonAaUsage) {

  # select an aa colomn
  x <- subset(DF_CodonAaUsage[DF_CodonAaUsage$codon_aa == 'aa',], select=c('type','value'))
  rownames(x) <- 1:nrow(x)
  x_min <- min(x$value)
  x_max <- max(x$value)

  # transpose from column to rows *** required by a fmsb package
  datax=as.data.frame(matrix( x$value , ncol=length(x$value)))
  colnames(datax) <- x$type
  datax <- rbind(rep(round(x_max, digits = 2),length(x$value)) ,
    rep(round(x_min, digits = 2),length(x$value)) , datax)

  # plot a radar chart from fmsb package
  radarchart( datax  , axistype=1 , seg = 5,
            
    #custom polygon
    pcol=rgb(0.0,0.0,0.5,0.9) , pfcol=rgb(0.0,0.0,0.5,0.5) , plwd=3 , 
    
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(round(x_min, digits = 2),
      round(x_max, digits = 2), (round(x_max, digits = 2)-round(x_min, digits = 2))/5), cglwd=0.8,
    
    #custom labels
    vlcex=1,

    #title
    title="Amino acid usage"
  )
  
}
