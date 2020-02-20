library(ape)
# read in data
a <- read.dna("data/fasta/mega-aligned/16S.fas", format = "fasta", as.character = T)
b <- read.dna("data/fasta/mega-aligned/18S.fas", format = "fasta", as.character = T)
c <- read.dna("data/fasta/mega-aligned/28S.fas", format = "fasta", as.character = T)
d <- read.dna("data/fasta/mega-aligned/CO1.fas", format = "fasta", as.character = T)
e <- read.dna("data/fasta/mega-aligned/ITS2.fas", format = "fasta", as.character = T)
# merge into one frame
foo1 <- merge(a, b, by = "row.names", all = TRUE)
foo2 <- merge(c, d, by = "row.names", all = TRUE)
foo3 <- merge(foo1, foo2, by = "Row.names", all = TRUE)
# get rid of na and some cleaning
foo3[is.na(foo3)] <- "-"
rownames(foo3) <- foo3[,"Row.names"]
foo3 <- foo3[,-1]
colnames(foo3) <- NULL
# turn to matrix and get rid of na
foo4 <- as.matrix(foo3)
foo4[is.na(foo4)] <- "-"
# read as dna
concat <- as.DNAbin(foo4)
# write this to fasta
write.dna(concat, file = "test.fasta", format = "fasta")




