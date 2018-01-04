# Fixing up movement model simulations dataset
# Some of the runs have < 3000 days so these need to be cut
# before we start (where the simulation got stuck and "stranded")
# Natalie Cooper Jan 2018
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)

# Read in model simulations output
# Takes a bit of time
resTrack <- read_csv("data/model.sims.csv")

# Remove reps with < 3000 days
remove <- 
  resTrack %>%
  group_by(Rep) %>%
  summarise(ln = length(count2)) %>%
  filter(ln < 3000)

toremove <- match(resTrack$Rep, remove$Rep, nomatch = 0)

resTrack2 <- resTrack[toremove == 0, ]

# Write to data folder
# Use this for later analyses
write.csv(resTrack2, "data/model.sims.3000.csv", quote = FALSE, row.names = FALSE)
