### generating the virus phylogeny

# What species?
virus_spp <- 
  c(
    "west nile virus",
    "dengue virus 1",
    "dengue virus 2",
    "dengue virus 3",
    "dengue virus 4",
    "yellow fever virus",
    "Japanese encephalitis virus",
    "Saint Louis encephalitis virus",
    "Murray Valley encephalitis virus "
  )

# Find thes in taxize
x = get_uid(virus_spp)

y = classification(x)

# Sadly cant build a treee fromt hsi so move on to sequences..
# z = class2tree(y, check = FALSE)


# Aligning - downloaded reference genomes from GenBank --------------------

# library(ShortRead)
library(ape)
# path <- "data/virus-genomes/"
# fastas <- ShortRead::readFasta(path)
# virus_raw <- read.FASTA("data/virus-genomes/all-virus.fasta")

library(msa)

virus_raw <- readAAStringSet("data/virus-genomes/all-virus.fasta")

virus_align <- msa(virus_raw, method = "Muscle")

virus_align_2 <-  msaConvert(virus_align, type = "seqinr::alignment")

# Distance matrix - seqinr ------------------------------------------------

library(seqinr)

virus_mat <- dist.alignment(virus_align_2, "identity")

# Phylogeny - ape ---------------------------------------------------------

library(ape)
library(phytools)

# virus_mat <- read_csv("data/virus-mat.csv")

virus_tree <- nj(virus_mat)

plot(as.phylo(virus_tree), main = "Phylogenetic Tree of Zoonotic Flaviridae")
plotTree(virus_tree)


# ggtree ------------------------------------------------------------------

library(ggtree)

virus_tree %>% as_tibble() %>% ggtree()

virus_tree %>% 
  ggtree() +
  geom_treescale() +
  geom_tiplab() +
  xlim(0, .7) +
  labs(
    title = "Phylogenetic relationship of key flavivirus spp"
  )
  