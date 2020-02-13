# just a literature search

# query_species <- 
#   
#   paste0(
#     "TITLE-ABS-KEY ( ",
#     str_replace(ncbi_spp$scientificname, " ", " PRE/1 "),
#     " )  AND  DOCTYPE ( ar  OR  re )"
#   )

# life history 
query_lifehist <-
  
  paste0(
    "TITLE-ABS-KEY ( (",
    ncbi_spp$scientificname,
    ")  ( life  AND history ) )  AND  DOCTYPE ( ar  OR  re )"
  )

# autogeny
query_autogeny <- 
  
  paste0(
    "TITLE-ABS-KEY ( (",
    ncbi_spp$scientificname,
    ")  ( autogen*  OR  anautogen* ) )  AND  DOCTYPE ( ar  OR  re )"
  )

# wing length
query_winglength <- 
  
  paste0(
    "TITLE-ABS-KEY ( ( ",
    ncbi_spp$scientificname,
    ")  ( wing  AND length )  OR  ( wing  AND size ) )  AND  DOCTYPE ( ar  OR  re )" 
  )

# voltinism
query_voltinism <- 
  
  paste0(
    "TITLE-ABS-KEY ( ( ",
    ncbi_spp$scientificname,
    ")  ( voltinism )  OR  ( multivoltinism ) )  AND  DOCTYPE ( ar  OR  re )" 
  )

# oviposition
query_oviposition <- 
  
  paste0(
    "TITLE-ABS-KEY ( ( ",
    ncbi_spp$scientificname,
    " )  ( oviposition  AND site )  OR  ( breeding  AND habit* ) )  AND  DOCTYPE ( ar  OR  re ) " 
  )

# overwinter
query_overwinter <- 
  
  paste0(
    "TITLE-ABS-KEY ( ( ",
    ncbi_spp$scientificname,
    " )  ( overwinte* )  OR  ( hibernati* )  OR  ( diapause ) )  AND  DOCTYPE ( ar  OR  re )"
  )

# prefhost
query_hostpref <- 
  
  paste0(
    "TITLE-ABS-KEY ( ( ",
    ncbi_spp$scientificname,
    "  )  AND  ( ( prefer*  AND host )  OR  ( blood  AND meal )  OR  ( pref*  AND blood )  OR  ( host  AND select* ) ) )  AND  DOCTYPE ( ar  OR  re ) "
  )

# Fecundity
query_fecundity <- 
  
  paste0(
    "TITLE-ABS-KEY ( ( ",
    ncbi_spp$scientificname,
    "   )  AND  ( fecund* ) )  AND  DOCTYPE ( ar  OR  re ) "
  )

# R0
query_r0 <- 
  
  paste0(
    "TITLE-ABS-KEY ( ( ",
    ncbi_spp$scientificname,
    "   )  AND  ( ( r0 )  OR  ( reproducti*  AND rate ) ) )  AND  DOCTYPE ( ar  OR  re )"
  )

querys = list(
  # "species" = query_species,
  "life_hist" = query_lifehist,
  "autogeny" = query_autogeny,
  "wing_length" = query_winglength,
  "voltinism" = query_voltinism,
  "oviposition" = query_oviposition,
  "overwinter" = query_overwinter,
  "host_pref" = query_hostpref,
  "fecundity" = query_fecundity,
  "R0" = query_r0
)
