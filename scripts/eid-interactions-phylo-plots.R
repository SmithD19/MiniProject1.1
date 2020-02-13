library(tidyverse)
library(janitor)
library(readxl)


# Cleaning and Manipulation -----------------------------------------------

path <- "data/mosquitoes_interactions_extracts.xlsx"

# Read in each tab of the xlsx file
EID = 
  path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

# take all the items of the list and put them in the GlobalEnv
list2env(EID, .GlobalEnv)

# clean and tidy names please
publication_data = `publication data`
rm(`publication data`)

# Load in matching species names
# Species here under scientific name - mosquito
ncbi_spp <- 
  read_csv("data/resolved-ncbi-species.csv") %>% 
  mutate(mostaxid = as.numeric(uid))


# Species here under scientific name - virus
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

library(taxize)

# Find these in taxize
# will also write this to csv for future
ncbi_virus = 
  get_uid_(virus_spp) %>% 
  bind_rows() %>% 
  mutate(vtaxid = as.numeric(uid))

# get the species we care about - long convoluted joining of tables
filtered_interactions <- 
  publication_data %>% 
  # filtering
  filter(mostaxid %in% ncbi_spp$uid & vtaxid %in% ncbi_virus$uid) %>% 
  select(mostaxid, vtaxid) %>% 
  # joining
  inner_join(ncbi_virus, by = "vtaxid") %>% 
  mutate(virus_name = scientificname) %>%
  select(mostaxid, vtaxid, virus_name) %>%
  # joining again after resetting table vars
  inner_join(ncbi_spp, by = "mostaxid") %>% 
  mutate(mos_name = scientificname) %>% 
  select(virus_name, mos_name, vtaxid, mostaxid)

# interaction matrix for virus / mosquito
mos_vir_mat <-
  filtered_interactions %>%
  distinct() %>%
  mutate(interaction = 1) %>%
  mutate(scientificname = mos_name) %>%
  select(-mos_name, -mostaxid, vtaxid) %>%
  # join?
  full_join(ncbi_spp) %>%
  # now aggregate the rows into one
  group_by(scientificname, virus_name) %>%
  summarise_if(is.numeric, sum) %>% 
  ungroup()

# mutate_if(is.numeric, ~ replace(., is.na(.), 0))


# summarise(`West Nile Virus` = ifelse(sum(`West Nile virus`) >= 1, 1, 0),
#           `Dengue virus 1` = sum(`Dengue virus 1`),
#           `Dengue virus 2` = sum(`Dengue virus 2`),
#           `Dengue virus 3` = sum(`Dengue virus 3`),
#           `Dengue virus 4` = sum(`Dengue virus 4`),
#           `Yellow fever virus` = sum(`Yellow fever virus`),
#           `Murray Valley encephalitis virus` = sum(`Murray Valley encephalitis virus`),
#           `Saint Louis encephalitis virus` = sum(`Saint Louis encephalitis virus`),
#           `Japanese encephalitis virus` = sum(`Japanese encephalitis virus`)) %>%
# NA to 0
# mutate_if(is.numeric, ~ replace(., is.na(.), 0))

# # turn edglist into interaction matrix
# library(igraph)
# g <- filtered_interactions %>% 
#   select(mos_name, virus_name) %>% 
#   as.matrix() %>% 
#   graph_from_edgelist() %>% simplify
# 
# mos_vir_mat <- 
#   as_adjacency_matrix(g) %>%
#   as.matrix() %>% 
#   as.data.frame()

# fuzzytest ---------------------------------------------------------------

# pre req for coming code - load in tree
library(ape)
library(ggtree)
co1_mat <- read.csv("data/co1-dist-mat.csv", row.names = 1) %>% as.matrix()
co1_tree <- nj(co1_mat)

y <- 
  # get the co1 tree
  co1_tree %>% 
  # in dataframe format
  as_tibble() %>% 
  # get labels
  select(label, node) %>% 
  drop_na() %>% 
  # get rid of any more words than 2
  mutate(scientificname = 
           # little tdy magic here - no idea how this works with %>% ?!?!?!
           label %>% 
           str_replace_all("_", " ") %>% 
           word(start = 1L, end = 2L))

library(fuzzyjoin)
# join them based on string distances
z <- 
  stringdist_inner_join(y, mos_vir_mat, max_dist = 1) %>% 
  mutate(virus_code = 
           case_when(
             virus_name == "West Nile virus" ~ "WNV",
             virus_name == "Dengue virus 1" ~ "DNV1",
             virus_name == "Dengue virus 2" ~ "DNV2",
             virus_name == "Dengue virus 3" ~ "DNV3",
             virus_name == "Dengue virus 4" ~ "DNV4",
             virus_name == "Yellow fever virus" ~ "YFV",
             virus_name == "Japanese encephalitis virus" ~ "JEV",
             virus_name == "Saint Louis encephalitis virus" ~ "SLEV",
             virus_name == "Murray Valley encephalitis virus" ~ "MVEV"
           )) %>% 
  separate(scientificname.x, into = c("genus", "species"), sep = " ")

zz = 
  z %>% 
  pivot_wider(label, names_from = virus_code, values_from = interaction) %>%
  select(-`NA`) %>% as.matrix.data.frame()

rownames(zz) <- zz[,1]

zz <- zz[,2:9]
zz <- ifelse(is.na(zz), "Non-Vector", "Vector")


# ggtree ------------------------------------------------------------------

p =
  co1_tree %>%
  ggtree(branch.length = "none") +
  coord_cartesian(clip = 'off')
 # theme_tree(plot.margin = margin(10, 120, 10, 10))
  

p2 = p %<+% z +
  geom_tiplab(aes(label = scientificname.y), size = 2) +
  # geom_tippoint(aes(col = genus)) +
  scale_fill_viridis_c()

gheatmap(p2, zz, 
         offset = 2, 
         width = .5, 
         font.size = 3,
         colnames_offset_y = 1,
         colnames_position = "top", 
         legend_title = "Vector", 
         color = "black") +
  # scale_fill_viridis_d() +
  labs(title = "Phylogenetic relationship of Palearctic Mosquitoes and their Flaviridae vector status")


# drop the tips where no one is a vector for anything
droptips <- 
  ifelse(zz == "Non-Vector", 0, 1) %>% 
  rowSums()
droptips <- names(droptips[droptips == 0])

#tree with crappy tips dropped
p3 <- p %>% 
  as.phylo %>% 
  drop.tip(droptips) %>% 
  ggtree() +
  geom_tiplab(aes(label = scientificname.y), align=TRUE, linesize = .5)

# need to fileter and drop duplicates
info <- filter(z, !label %in% droptips) %>% distinct(label, genus, scientificname.y)

p4 <-   p3 %<+% info

# filtering this doesnt make a difference
zzz <- zz[!rownames(zz) %in% droptips,]

gheatmap(p4, zz, 
         offset = .1, 
         width = 1, 
         font.size = 4,
         colnames_offset_y = 1,
         colnames_position = "top", 
         legend_title = "Vector", 
         color = "black") +
  scale_fill_viridis_d() +
  labs(title = "Phylogenetic Relationship of European Flaviridae Vectors",
       fill = "Vector Status",
       col = "Genus")



# Making interaction plots - bipartite ------------------------------------

library(bipartite)

# net <- 
#   mos_vir_mat %>%
#   # get rid of ones with no interaction
#   drop_na() %>% 
#   # change the names so easier to read
#   mutate(virus_code = 
#            case_when(
#              virus_name == "West Nile virus" ~ "WNV",
#              virus_name == "Dengue virus 1" ~ "DNV1",
#              virus_name == "Dengue virus 2" ~ "DNV2",
#              virus_name == "Dengue virus 3" ~ "DNV3",
#              virus_name == "Dengue virus 4" ~ "DNV4",
#              virus_name == "Yellow fever virus" ~ "YFV",
#              virus_name == "Japanese encephalitis virus" ~ "JEV",
#              virus_name == "Saint Louis encephalitis virus" ~ "SLEV",
#              virus_name == "Murray Valley encephalitis virus" ~ "MVEV"
#            )) %>% 
#   mutate(webID = "Qualitative-EID2") %>% 
#   select(lower = scientificname, 
#          higher = virus_name,
#          webID,
#          freq = interaction) %>% 
#   frame2webs()

library(igraph)

net <- # THIS IS CONVOLUTED ---- BUT WORKS :SHRUGEMOJI:
  mos_vir_mat %>%
  # get rid of ones with no interaction
  drop_na() %>% 
  # Shorten names
  mutate(short_name = paste0(substr(scientificname, 1, 2), ". ", word(scientificname, 2L))) %>% 
  # change the names so easier to read
  mutate(virus_code = 
           case_when(
             virus_name == "West Nile virus" ~ "WNV",
             virus_name == "Dengue virus 1" ~ "DNV1",
             virus_name == "Dengue virus 2" ~ "DNV2",
             virus_name == "Dengue virus 3" ~ "DNV3",
             virus_name == "Dengue virus 4" ~ "DNV4",
             virus_name == "Yellow fever virus" ~ "YFV",
             virus_name == "Japanese encephalitis virus" ~ "JEV",
             virus_name == "Saint Louis encephalitis virus" ~ "SLEV",
             virus_name == "Murray Valley encephalitis virus" ~ "MVEV"
           )) %>% 
  select(short_name, virus_code) %>% 
  as.matrix() %>% 
  graph_from_edgelist() %>% 
  igraph::as_edgelist() %>% 
  as.data.frame() %>% 
  mutate(webID = "QualEID2",
         freq = 1) %>% 
  select(
    lower = V1,
    higher = V2,
    webID,
    freq
  ) %>% 
  frame2webs()

bipartite::plotweb(net$QualEID2)

palvirus = viridis::viridis(10)
palmos = viridis::viridis(50)

bipartite::plotweb(net$QualEID2, 
                   text.rot = 90, 
                   col.high = palvirus,
                   col.low = palmos,
                   arrow = "up",
                   low.y = .6)
  

# picante -----------------------------------------------------------------

net$QualEID2 %>% class

net2 <- # THIS IS CONVOLUTED ---- BUT WORKS :SHRUGEMOJI:
  mos_vir_mat %>%
  # get rid of ones with no interaction
  drop_na()  %>% 
  select(scientificname, virus_name) %>% 
  as.matrix() %>% 
  graph_from_edgelist() %>% 
  igraph::as_edgelist() %>% 
  as.data.frame() %>% 
  mutate(webID = "QualEID2",
         freq = 1) %>% 
  select(
    lower = V1,
    higher = V2,
    webID,
    freq
  ) %>% 
  frame2webs()


# picante? ----------------------------------------------------------------

virus_mat <- read.csv("data/virus-mat.csv", row.names = 1) %>% as.matrix()
virus_tree <- nj(virus_mat)

virus_tree$tip.label <- c("X03700.1 Yellow fever virus complete genome, 17D vaccine strain",
                          "NC_001477.1 Dengue virus 1, complete genome",
                          "NC_001474.2 Dengue virus 2, complete genome", 
                          "NC_001475.2 Dengue virus 3, complete genome", 
                          "NC_002640.1 Dengue virus 4, complete genome ", 
                          "NC_007580.2 Saint Louis encephalitis virus, complete genome ",
                          "M12294.2 West Nile virus RNA, complete genome ", 
                          "NC_000943.1 Murray Valley encephalitis virus, complete genome", 
                          "NC_001437.1 Japanese encephalitis virus, genome")

colnames(net2$QualEID2) <- c("NC_001477.1 Dengue virus 1, complete genome",
                             "NC_001474.2 Dengue virus 2, complete genome", 
                             "NC_001475.2 Dengue virus 3, complete genome", 
                             "NC_002640.1 Dengue virus 4, complete genome ",
                             "NC_001437.1 Japanese encephalitis virus, genome",
                             "NC_000943.1 Murray Valley encephalitis virus, complete genome",
                             "NC_007580.2 Saint Louis encephalitis virus, complete genome ",
                             "M12294.2 West Nile virus RNA, complete genome ",
                             "X03700.1 Yellow fever virus complete genome, 17D vaccine strain"
                             )


co1_tree$tip.label <- co1_tree$tip.label %>% str_replace("_", " ")

co1_tree$tip.label

rownames(net2$QualEID2) %in% mostree$tip.label

net2$QualEID2 <- net2$QualEID2[-3,]

mostree <- drop.tip(co1_tree, setdiff(co1_tree$tip.label, rownames(net2$QualEID2)))

library(picante)

model1 <- pblm(net2$QualEID2, mostree, virus_tree, bootstrap = TRUE)
model1$MSE
model1$CI.boot
model1$variates
model1$predicted
model1$phylocovs$V1

plot(model1$residuals)
