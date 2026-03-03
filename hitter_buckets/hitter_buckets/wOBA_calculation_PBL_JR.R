library(tidyverse)

# load data
#NST_hitterdata <- read.csv('North Shore PR Twins Summer 2025 Hitting Stats.csv')
LG_hitterdata_JR <- read.csv('League Hitting Stats PBL Jr.csv')

# add strikeout percentages to all players
#NST_hitterdata <- NST_hitterdata %>% mutate(K_pct = SO / AB * 100)
LG_hitterdata_JR <- LG_hitterdata_JR %>% mutate(K_pct = SO / AB * 100)

# PBL 2025 total counting stats for wOBA
LG_BB_JR <- sum(LG_hitterdata_JR$BB)
LG_HBP_JR <- sum(LG_hitterdata_JR$HBP)
LG_singles_JR <- sum(LG_hitterdata_JR$X1B)
LG_doubles_JR <- sum(LG_hitterdata_JR$X2B)
LG_triples_JR <- sum(LG_hitterdata_JR$X3B)
LG_HR_JR <- sum(LG_hitterdata_JR$HR)
LG_AB_JR <- sum(LG_hitterdata_JR$AB)
LG_SF_JR <- sum(LG_hitterdata_JR$SF)

# PBL 2025 factor weights (from GPT-5)
BB_factor <- 0.666
HBP_factor <- 0.697
singles_factor <- 0.840
doubles_factor <- 1.187
triples_factor <- 1.500
HR_factor <- 1.919

# add wOBA to all players
LG_hitterdata_JR <- mutate(LG_hitterdata_JR, wOBA = (BB_factor * BB + 
                                                    HBP_factor * HBP + 
                                                    singles_factor * X1B + 
                                                    doubles_factor * X2B + 
                                                    triples_factor * X3B + 
                                                    HR_factor * HR) / (AB + BB + SF + HBP))

# add wOBA to Twins players
#NST_hitterdata <- mutate(NST_hitterdata, wOBA = (BB_factor * BB + 
#                                                   HBP_factor * HBP + 
#                                                   singles_factor * X1B + 
#                                                   doubles_factor * X2B + 
#                                                   triples_factor * X3B + 
#                                                   HR_factor * HR) / (AB + BB + SF + HBP))

# average wOBA in PBL
LG_avg_wOBA_JR <- (BB_factor * LG_BB_JR + 
                  HBP_factor * LG_HBP_JR + 
                  singles_factor * LG_singles_JR + 
                  doubles_factor * LG_doubles_JR + 
                  triples_factor * LG_triples_JR + 
                  HR_factor * LG_HR_JR) / (LG_AB_JR + LG_BB_JR + LG_SF_JR + LG_HBP_JR)
# 0.299

# standard deviation of wOBA in PBL
LG_sd_wOBA_JR <- sd(LG_hitterdata_JR$wOBA) #

# average strikeout percentage in PBL
LG_avg_K_pct_JR <- mean(LG_hitterdata_JR$K_pct) # %

# standard deviation of K% in PBL
LG_sd_K_pct_JR <- sd(LG_hitterdata_JR$K_pct) # %

# average line drive, ground ball, fly ball percentage in PBL
LG_avg_LD_pct_JR <- mean(LG_hitterdata_JR$LD.) # %
LG_avg_GB_pct_JR <- mean(LG_hitterdata_JR$GB.) # %
LG_avg_FB_pct_JR <- mean(LG_hitterdata_JR$FB.) # %

# standard deviation of LD, GB, FB% in PBL
LG_sd_LD_pct_JR <- sd(LG_hitterdata_JR$LD.) # 
LG_sd_GB_pct_JR <- sd(LG_hitterdata_JR$GB.) # 
LG_sd_FB_pct_JR <- sd(LG_hitterdata_JR$FB.) # 

# benchmark for a high wOBA
high_wOBA <- LG_avg_wOBA + 1.5*LG_sd_wOBA # 

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

