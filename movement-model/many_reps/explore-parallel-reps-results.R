# load, process, and explore the results from the parallel run
# Need to setwd() to this file's location first.

library(tidyverse)

load("big_whale_run.rda")

# check for errors in the reps which are indicated by 
# objects of length 1 in the list of 1000
df_lengths <- lapply(whale_reps, length)

proportion_errors <- sum(df_lengths == 1) / length(df_lengths)
proportion_errors

# index of reps that were successful
idx_success <- df_lengths > 1


whale_successful_reps <- bind_rows(whale_reps[idx_success])

save(whale_successful_reps, file = "whale_successful_reps.rda", 
     compress = "xz")
