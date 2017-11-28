# Fixing up movement model simulations dataset
# For some reason the data has 1078 replicates not 1000
# Therefore this code removes the excess 78
# I've arbitrarily chosen to remove the 78 with the fewest
# with fewest days in the run < 2957.
# Natalie Cooper Nov 2017
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)

# Read in model simulations output
# Takes a bit of time
resTrack <- read_csv("data/model.sims.csv")

# Remove rep that didn't work
resTrack <- resTrack[-c(3213051:3213211), ]

# Remove excess 78 simulations
# Remove those with fewest days - < 2957

remove <- 
  resTrack %>%
  group_by(Rep) %>%
  summarise(ln = length(count2)) %>%
  filter(ln < 2957)

toremove <- match(resTrack$Rep, remove$Rep, nomatch = 0)

resTrack2 <- resTrack[toremove == 0, ]

# Write to data folder
# Use this for later analyses
write.csv(resTrack2, "data/model.sims1000.csv", quote = FALSE, row.names = FALSE)
