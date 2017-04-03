library(Biostrings)
# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

stats_geneDNA <- function(dna) {
  # Calculate a statistics of sequence.
  #
  # Args:
  #   dna: Nucleic sequence.
  #
  # Returns:
  #   Dataframe of stats

  # DNAString method uppercases letters
  dnaString <- DNAString(dna)
  freq <- alphabetFrequency(dnaString)
  gcCount <- freq["C"] + freq["G"]
  allCount <- freq["A"] + freq["C"] + freq["G"] + freq["T"]

  # Calculate codon usage
  codon <- trinucleotideFrequency(dnaString)
  normalizedCodon <- codon  # TODO
  # Convert to AA usage
  aaUsage <- rep(codon)
  names(aaUsage) <- GENETIC_CODE[names(codon)]
  aaUsage <- sapply(split(aaUsage, names(aaUsage)), sum)


  # normalizeCodon <- codon
  #
  ret <- list(length=length(dnaString),
              gcContent=gcCount/allCount,
              codonUsage=normalizedCodon,
              aaUsage=aaUsage)
  return(ret)
}




