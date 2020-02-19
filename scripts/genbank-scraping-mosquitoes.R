### This script will gather all the accession numbers for our species
library(tidyverse)

# Species here under scientific name
ncbi_spp <- read_csv("data/resolved-ncbi-species.csv")

# source genbank scraping
source("scripts/genbank-scraping.R")

# scrape
accession <- 
  scrape.genbank(ncbi_spp$scientificname, 
                 c("COX1", "COI", "internal transcribed spacer 2", "16S", "28S", "18S")
  )

