# Fixing up movement model simulations dataset
# Some of the runs end before 3019 days so these need to be cut
# before we start (where the simulation got stuck and "stranded")
# Natalie Cooper Jan 2018
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)

# Read in model simulations output
# Takes a bit of time
resTrack <- read_csv("data/model.sims.csv")

# Remove reps with < 3019 days
remove <- 
  resTrack %>%
  group_by(Rep) %>%
  summarise(last.day = max(count2)) %>%
  filter(last.day < 3019)

toremove <- match(resTrack$Rep, remove$Rep, nomatch = 0)

resTrack2 <- resTrack[toremove == 0, ]

# Remove non needed/confusing column headers
resTrack2 <- 
  resTrack2 %>%
  select(-c(X, X1, km.Lat, km.Lon, count))

# Write to data folder
# Use this for later analyses
write.csv(resTrack2, "data/model.sims.full.csv", quote = FALSE, row.names = FALSE)


