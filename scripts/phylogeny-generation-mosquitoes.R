### This script will gather all the accession numbers for our species
library(tidyverse)

# Species here under scientific name
ncbi_spp <- read_csv("data/resolved-ncbi-species.csv")

# Tree using just taxonomy - taxize ---------------------------------------

library(taxize)

class_mosquito <- classification(ncbi_spp$scientificname, db = "ncbi")

class_mosquito <- class_mosquito[!duplicated(class_mosquito)]

common_tree <- class2tree(class_mosquito)

plot(common_tree)

common_tree$distmat


# Accession Numbers -------------------------------------------------------

# Load functions
source("scripts/genbank-scraping.R")

# Scrape genbank - now just read in .csv files

# co1_accession <- scrape.genbank(ncbi_spp$scientificname, "COI")
# its2_accession <- scrape.genbank(ncbi_spp$scientificname, "internal transcribed spacer 2")

co1_accession <- read_csv("data/co1.csv")
its2_accession <- read_csv("data/its2.csv")

# Write fasta sequences ---------------------------------------------------

library(ape)

co1_dna <- read.GenBank(co1_accession$COI_copy)

names(co1_dna) <- attr(co1_dna, "species")

its2_dna <- read.GenBank(its2_accession$`internal transcribed spacer 2`)

names(its2_dna) <- attr(its2_dna, "species")

# write.FASTA(co1_dna, "data/co1-raw.fasta")

# write.FASTA(its2_dna, "data/its2-raw.fasta")


# Bioconducture - msa -----------------------------------------------------

library(msa)

co1_sequences <- readAAStringSet("data/co1-raw.fasta")

co1_alignment <- msa(co1_sequences, method = "Muscle")

its2_sequences <- readAAStringSet("data/its2-raw.fasta")

its2_alignment <- msa(its2_sequences, method = "Muscle")

# msaprettyprint would be nice here
# msaPrettyPrint(co1_alignment)

co1_alignment_2 <- msaConvert(co1_alignment, type = "seqinr::alignment")

its2_alignment_2 <- msaConvert(its2_alignment, type = "seqinr::alignment")

# Distance matrix - seqinr ------------------------------------------------

library(seqinr)

# co1_mat <- dist.alignment(co1_alignment_2, "identity")

# its2_mat <- dist.alignment(its2_alignment_2, "identity")

# just load the saved matrices now
co1_mat <- read.csv("data/co1-dist-mat.csv", row.names = 1) %>% as.matrix()

# Phylogeny - ape ---------------------------------------------------------

library(ape)

co1_tree <- nj(co1_mat)

plot(co1_tree, main = "Phylogenetic Tree of COI in Mosquitoes")

# its2 contains little information ---
# its2_tree <- nj(its2_mat)
# plot(its2_tree, main = "Phylogenetic Tree of ITS2 in Mosquitoes")

library(ggtree)

co1_tree %>% 
  ggtree() +
  geom_tiplab()

co1_tree %>% as_tibble()

to_drop <- c("Anopheles_apoci","Anopheles_superpictus","Ochlerotatus_cyprius",
             "Anopheles_turkhudi", "Aedes_echinus", "Anopheles_ainshamsi",
             "Ochlerotatus_eatoni_hewitti_(nomen_nudum)", "Acartomyia_mariae")

co1_tree %>% 
  #drop.tip(to_drop) %>% 
  ggtree(branch.length = "none") + 
  geom_tiplab() +
  coord_cartesian(clip = 'off') + 
  theme_tree2(plot.margin=margin(6, 120, 6, 6)) +
geom_label2(aes(subset=!isTip, label=node), size=2, color="darkred", alpha=0.5)




