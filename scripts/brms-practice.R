# model output for pgls.ives

library(tidyverse)

co1_mat <- read.csv("data/co1-dist-mat.csv", row.names = 1) %>% as.matrix()

library(ape)
library(phytools)

co1_tree <- nj(co1_mat) %>% 
  # resolve multifurcations - apparently this is needed
  multi2di()

# simulate a trait for the webs
# lets say this is going to be winglength wich is usually around 2.5-6mm
trait <- fastBM(co1_tree, bounds = c(2.5, 6))

# need to cosntruct a co-variance matrix of species
A <- ape::vcv.phylo(co1_tree)

# load in trait data as a binary variable - vector or not?
int <- read_csv("interactions-tidy-format.csv")

int_mat <- 
  int %>% 
  # select vertices
  select(label, virus_code) %>%
  # create a interaciton vlaue to pivot around
  mutate(int = if_else(is.na(virus_code), 0, 1)) %>%
  # pivot around them
  pivot_wider(names_from = virus_code, values_from = int) %>% 
  # remove defunct column generated from na values
  select(-`NA`) %>% 
  # replace all NA with 0 if in a numeric column
  mutate_if(is.numeric, ~ replace(., is.na(.), 0))

# construct - pheno/cofactor/phylo object
modeldat <- cbind(int_mat, trait) %>% as_tibble()

library(brms)
# model me
model_simple <- brm(
  # how does vector status of WNV vary by out simulated trait of wing length
  # when taking into account phylogeny?
  WNV ~ trait + (1|label),
  data = modeldat,
  # for 0,1 binary data
  family = bernoulli(),
  # the complete variance of the phylogeny groupings
  cov_ranef = list(label = A)
)

#
summary(model_simple)

# 
plot(model_simple)

# 
plot(conditional_effects(model_simple))

# how well does it fit the data?
pp_check(model_simple, nsamples = 100)
