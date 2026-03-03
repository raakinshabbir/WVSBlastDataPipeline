library(tidyverse)

# load data
NST_hitterdata <- read.csv('North Shore PR Twins Summer 2025 Hitting Stats.csv')
LG_hitterdata <- read.csv('League Hitting Stats.csv')

# add strikeout percentages to all players
NST_hitterdata <- NST_hitterdata %>% mutate(K_pct = SO / AB * 100)
LG_hitterdata <- LG_hitterdata %>% mutate(K_pct = SO / AB * 100)

# PBL 2025 total counting stats for wOBA
LG_BB <- sum(LG_hitterdata$BB)
LG_HBP <- sum(LG_hitterdata$HBP)
LG_singles <- sum(LG_hitterdata$X1B)
LG_doubles <- sum(LG_hitterdata$X2B)
LG_triples <- sum(LG_hitterdata$X3B)
LG_HR <- sum(LG_hitterdata$HR)
LG_AB <- sum(LG_hitterdata$AB)
LG_SF <- sum(LG_hitterdata$SF)

# PBL 2025 factor weights (from GPT-5)
BB_factor <- 0.666
HBP_factor <- 0.697
singles_factor <- 0.840
doubles_factor <- 1.187
triples_factor <- 1.500
HR_factor <- 1.919

# add wOBA to all players
LG_hitterdata <- mutate(LG_hitterdata, wOBA = (BB_factor * BB + 
                                                    HBP_factor * HBP + 
                                                    singles_factor * X1B + 
                                                    doubles_factor * X2B + 
                                                    triples_factor * X3B + 
                                                    HR_factor * HR) / (AB + BB + SF + HBP))

# add wOBA to Twins players
NST_hitterdata <- mutate(NST_hitterdata, wOBA = (BB_factor * BB + 
                                                   HBP_factor * HBP + 
                                                   singles_factor * X1B + 
                                                   doubles_factor * X2B + 
                                                   triples_factor * X3B + 
                                                   HR_factor * HR) / (AB + BB + SF + HBP))

# average wOBA in PBL
LG_avg_wOBA <- (BB_factor * LG_BB + 
                  HBP_factor * LG_HBP + 
                  singles_factor * LG_singles + 
                  doubles_factor * LG_doubles + 
                  triples_factor * LG_triples + 
                  HR_factor * LG_HR) / (LG_AB + LG_BB + LG_SF + LG_HBP)
# 0.318

# standard deviation of wOBA in PBL
LG_sd_wOBA <- sd(LG_hitterdata$wOBA) # 0.062

# average strikeout percentage in PBL
LG_avg_K_pct <- mean(LG_hitterdata$K_pct) # 22.52%

# standard deviation of K% in PBL
LG_sd_K_pct <- sd(LG_hitterdata$K_pct) # 8.98%

# average line drive, ground ball, fly ball percentage in PBL
LG_avg_LD_pct <- mean(LG_hitterdata$LD.) # 17.70%
LG_avg_GB_pct <- mean(LG_hitterdata$GB.) # 43.26%
LG_avg_FB_pct <- mean(LG_hitterdata$FB.) # 35.09%

# standard deviation of LD, GB, FB% in PBL
LG_sd_LD_pct <- sd(LG_hitterdata$LD.) # 6.08%
LG_sd_GB_pct <- sd(LG_hitterdata$GB.) # 8.05%
LG_sd_FB_pct <- sd(LG_hitterdata$FB.) # 7.95%

# benchmark for a high wOBA
high_wOBA <- LG_avg_wOBA + 1.5*LG_sd_wOBA # >0.412

# benchmark for a high/low K%
high_K_pct <- LG_avg_K_pct + 0.75*LG_sd_K_pct # >29.25%   MAYBE GO LOWER? 0.5 SD would be a 27% K rate
low_K_pct <- LG_avg_K_pct - 0.75*LG_sd_K_pct # <15.79%
# min = 7%, max = 52%

# benchmark for a high/low line drive, ground ball, and fly ball percentage
# may tweak based on Zach's thoughts
high_LD_pct <- LG_avg_LD_pct + 0.75*LG_sd_LD_pct # >22.26%
low_LD_pct <- LG_avg_LD_pct - 0.75*LG_sd_LD_pct # <13.14%
# min = 5%, max = 34%

high_GB_pct <- LG_avg_GB_pct + 0.75*LG_sd_GB_pct # >49.30%
low_GB_pct <- LG_avg_GB_pct - 0.75*LG_sd_GB_pct # <37.23%
# min = 21%, max = 61%

high_FB_pct <- LG_avg_FB_pct + 0.75*LG_sd_FB_pct # >41.05%
low_FB_pct <- LG_avg_FB_pct - 0.75*LG_sd_FB_pct # <29.12%
# min = 19%, max = 56%

