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
