library(tidyverse)
accession <- readr::read_csv("data/accession-nos.csv")

library(ape)

# Cytochrome Oxidase
CO1 <- dplyr::coalesce(accession$COX1, accession$COI)
co1_dna <- read.GenBank(CO1)
names(co1_dna) <- attr(co1_dna, "species")
co1_dna %>% write.FASTA("data/fasta/co1.fasta")

# ITS2
its2_dna <- read.GenBank(accession$`internal transcribed spacer 2`)
names(its2_dna) <- attr(its2_dna, "species")
its2_dna %>% write.FASTA("data/fasta/its2.fasta")

# 16S
S16 <- read.GenBank(accession$`16S`)
names(S16) <- attr(S16, "species")
S16 %>% write.FASTA("data/fasta/16S.fasta")

# 18S
S18 <- read.GenBank(accession$`18S`)
names(S18) <- attr(S18, "species")
S18 %>% write.FASTA("data/fasta/18S.fasta")

# 28S
S28 <- read.GenBank(accession$`28S`)
names(S28) <- attr(S28, "species")
S28 %>% write.FASTA("data/fasta/28S.fasta")

# Bioconducture - msa -----------------------------------------------------
library(msa)

co1_seq <- readAAStringSet("data/fasta/co1.fasta") %>% msa(method = "Muscle") %>% msaConvert(type = "seqinr::alignment")
its2_seq <- readAAStringSet("data/fasta/its2.fasta") %>% msa(method = "Muscle") %>% msaConvert(type = "seqinr::alignment")
S16_seq <- readAAStringSet("data/fasta/16S.fasta") %>% msa(method = "Muscle") %>% msaConvert(type = "seqinr::alignment")
S18_seq <- readAAStringSet("data/fasta/18S.fasta") %>% msa(method = "Muscle") %>% msaConvert(type = "seqinr::alignment")
S28_seq <- readAAStringSet("data/fasta/28S.fasta") %>% msa(method = "Muscle") %>% msaConvert(type = "seqinr::alignment")


S18_seq %>% as.matrix()


