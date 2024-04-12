#-----------------------------------------------------------------
# To create an R program that can generate reverse, complement, and 
# reverse-complement sequences for nucleic acids like DNA and RNA.
# based on Marburg Virus provided by NHI
# (https://www.ncbi.nlm.nih.gov/nuccore/NC_001608.3)
#-----------------------------------------------------------------

### Load necessary libraries
library(readr)

## Decoding RNA's Complementary Base Pairing Rule
# A -> U
# C -> G
# G -> C
# T -> A

tmp_DNA <- character()           # Template DNA Sequence
cmp_RNA <- character()           # Complementary RNA Sequence
cur_base <- character()          # Holds one base from template DNA Sequence

DNA <- read.delim(file.choose(), header = TRUE)

seq <- data.frame(matrix(ncol = 1, nrow = nrow(DNA))) 
names(seq)[1] <- paste(names(DNA)[1], 'RNA sequence')


convert_DNA_to_RNA <- function(DNA, output_file) {
  for (j in 1:nrow(DNA)) {
    cmp_RNA <- ''       # Make sure the comp_RNA is empty
    tmp_DNA <- DNA[j,]
    for(i in 1:nchar(tmp_DNA)){
      cur_base <- substr(tmp_DNA,i,i)
      if(cur_base == "A"){
        cmp_RNA <- paste(cmp_RNA, "U", sep = "")
      } else if(cur_base=="C"){
        cmp_RNA <- paste(cmp_RNA, "G", sep = "")
      } else if(cur_base == "G"){
        cmp_RNA <- paste(cmp_RNA, "C", sep = "")
      } else if(cur_base == "T"){
        cmp_RNA <- paste(cmp_RNA, "A", sep = "")
      } else {
        cmp_RNA <- paste(cmp_RNA, "?", sep = "")
        print(paste("Unknown base at: ", i)) 
        print(i)
      }
    }
    
    # Store the complemented RNA sequence
    seq[j,] <- cmp_RNA
    
    # Write to the output file
    write_delim(data.frame(cmp_RNA), output_file, delim = " ", na = "NA", append = TRUE)
  }
}


output_file <- "marburg_sequence.txt"
convert_DNA_to_RNA(DNA, output_file)