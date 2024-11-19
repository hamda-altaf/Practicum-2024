library(tidyverse)
library(nhanesA)
library(dplyr)
dpq_J <- nhanes("DPQ_J", translated = F) # for year 2017-2018  so if you want more, you’d have to merge

dpq_vars <- c("DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
              "DPQ060", "DPQ070", "DPQ080", "DPQ090")
# Filter out rows where any of the DPQ variables are 7 or 9 using package 'dplyr' (refused/don't know) 
df_fin <- dpq_J %>%
  dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 7))) %>%
  dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 9)))

###### variable transformation

df_fin <- df_fin %>% mutate(DEPR_TOT = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090)
