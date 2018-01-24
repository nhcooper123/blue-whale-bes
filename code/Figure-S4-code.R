# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figures S4 for the blue whale paper
# Tidied by Natalie Cooper Jan 2017
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

library(tidyverse)
library(viridis)
library(lubridate)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# set the constant to add as a correction to the d13C data
cnst <- 6

# Read in resident whale model data
resNor <- read.csv("data/Norway.Res.TL.csv", header = TRUE)
resIre <- read.csv("data/Ireland.Res.TL.csv", header = TRUE)
resCan <- read.csv("data/Canaries.Res.TL.csv", header = TRUE)
resMidAtl <- read.csv("data/Atl.Res.TL.csv", header = TRUE)
resCV <- read.csv("data/CV.TL.csv", header = TRUE)
#resMaur <- read.csv("data/Maur.TL.csv", header = TRUE)

# Remove B.state variable as it messes up bind.rows
resNor <- 
  resNor %>%
  select(-B.state)

resNor <- 
  resNor %>%
  select(-B.state)

resIre <- 
  resIre %>%
  select(-B.state)

resCan <- 
  resCan %>%
  select(-B.state)

resMidAtl <- 
  resMidAtl %>%
  select(-B.state)

#resMaur <- 
#  resMaur %>%
#  select(-B.state)

# Here count is correct, but Day.No is not
# so switch them to get the next steps to work
# Also shift up start to 120 days to match others
resCV <- 
  resCV %>%
  select(-B.state, -Day.No) %>%
  mutate(Day.No = count + 119)
  

# Combine datasets
res_all <- bind_rows(list("Norwegian Sea" = resNor, 
                          "West Ireland" = resIre, 
                          Canaries = resCan, 
                          "Mid-Atlantic" = resMidAtl,
                          #"Mauritania" = resMaur,
                          "Cape Verde" = resCV), .id = "Region")

# Remove lines with no d13C values
res_all <- res_all[which(!is.na(res_all$d13C)), ]

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Plot loess fits to the 30 resident simulated whales for 
# each region
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Group data by replicate and region
# And add Day.No variable
tmp2 <- res_all %>% group_by(Region, Rep)  %>% 
  do(., data.frame(Z = predict(loess((.$d13C + cnst) ~ .$Day.No, span = 0.1)),
                   Day.No = .$Day.No))

# re-order the Region Factor
tmp2$Region <- factor(tmp2$Region, levels = c("West Ireland","Norwegian Sea",
                                              "Canaries","Mid-Atlantic",
                                              #"Mauritania",
                                              "Cape Verde"))

# Plot and facet wrap by region
sim_facet <- 
  ggplot(tmp2, aes(Day.No, Z, group = Rep)) + 
    geom_line(col = viridis(3)[2], alpha = 0.25) + 
    xlab("Time (days)") + 
    ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
    theme_classic(base_size = 14) + ylim(-25, -14) + 
    theme(strip.background = element_blank(),
          strip.text = element_text(size = NULL)) +
    facet_wrap(~Region, ncol = 2)

ggsave("manuscript/revision/figures/Figure-S4-regions-d13C.png", sim_facet, 
       device = png(width = 600, height = 400))

