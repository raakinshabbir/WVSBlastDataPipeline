library(tidyverse)
set.seed(3)

blast_data_15U <- read.csv("blast_data_15U.csv")

avg_bat_speed_15U <- mean(blast_data_15U$Bat.Speed..mph.)
avg_bat_speed_15U # 60.88
sd_bat_speed_15U <- sd(blast_data_15U$Bat.Speed..mph.)
sd_bat_speed_15U # 4.17

# create randomly generated bat speeds, assign them to Twins players
random_bat_speeds_15U <- rnorm(17, mean = avg_bat_speed_15U, sd = sd_bat_speed_15U)
#NST_hitterdata <- NST_hitterdata %>% mutate(rng_bat_speed = random_bat_speeds)

blast_data_15U_filtered <- select(blast_data_15U, c('Player.Name', 'Bat.Speed..mph.', 'Early.Connection..deg.'))
blast_data_15U_player <- blast_data_15U_filtered %>% group_by(Player.Name) %>%
  summarise(mean_bat_speed = mean(Bat.Speed..mph.),
            early_con_var = sd(Early.Connection..deg.)*2,
            early_con_mean = mean(Early.Connection..deg.))

mean_ecv_estimate_15U <- mean(blast_data_15U_player$early_con_var)
sd_ecv_estimate_15U <- sd(blast_data_15U_player$early_con_var)

# variances > 9 are to be flagged
# means outside of [86, 94] are to be flagged

high_bat_speed_15U <- avg_bat_speed_15U + 0.75*sd_bat_speed_15U # 58.77
low_bat_speed_15U <- avg_bat_speed_15U - 0.75*sd_bat_speed_15U # 51.85

# create randomly generated bat speeds, assign them to Twins players
random_ecv_15U <- rnorm(17, mean = 8, sd = 0.5*sd_ecv_estimate)
random_ec_angle_15U <- rnorm(17, mean = 90, sd = 0.25*sd(blast_data_filtered$Early.Connection..deg.))
#NST_hitterdata <- NST_hitterdata %>% mutate(rng_ec_angle = random_ec_angle, rng_ecv = random_ecv)


# ------------------------------------------------------------------------------
# ADDING BUCKET COLUMN TO TWINS PLAYERS
# ------------------------------------------------------------------------------


NST_hitterdata <- NST_hitterdata %>% mutate(bucket = case_when(
  # separate elite players first, put them in their own bucket (top 10-15 in league)
  wOBA > high_wOBA ~ "challenge",
  
  # bucket based on red flags (is the player 'bad' at anything)
  rng_ec_angle >= 95 | rng_ec_angle <= 85 | rng_ecv > 9 ~ "launch pos lv1",
  rng_bat_speed < low_bat_speed & K_pct < low_K_pct & GB. > high_GB_pct ~ "bat speed lv1",
  GB. > high_GB_pct | FB. > high_FB_pct ~ "swing decision lv1", # consider damage percentage? want LA of 14-24 degrees, ~90mph exit velo (can change by league/age)
  rng_bat_speed < low_bat_speed & K_pct > high_K_pct ~ "confidence", 
  rng_bat_speed < low_bat_speed ~ "bat speed lv1", # catch anyone with very low bat speed but not in confidence bucket
  
  # if no red flags present, bucket based on yellow flags (is the player below average at anything?)
  rng_ecv > 7 ~ "launch pos lv2",
  rng_bat_speed < avg_bat_speed ~ "bat speed lv2",
  GB. > LG_avg_GB_pct | FB. > LG_avg_FB_pct ~ "swing decision lv2",
  
  # if no yellow flags present, identify which areas the player isn't 'elite' at
  rng_ecv > 5 ~ "launch pos lv3",
  rng_bat_speed < high_bat_speed ~ "bat speed lv3",
  GB. > low_GB_pct | FB. > low_FB_pct ~ "swing decision lv3",
  TRUE ~ "undefined"
))

NST_hitterdata_small <- NST_hitterdata %>% select(Last, First, LD., FB., GB., K_pct, wOBA, rng_bat_speed, rng_ec_angle, rng_ecv, bucket)
view(NST_hitterdata_small)

Caine_data <- NST_hitterdata[1, ]
Caine_blast <- blast_data_player[9, ]

Caine_data <- Caine_data %>% mutate(bat_speed = Caine_blast$mean_bat_speed,
                                    ecv = Caine_blast$early_con_var,
                                    ec_angle = Caine_blast$early_con_mean)

Caine_data_good_ecv <- Caine_data
Caine_data_good_ecv[1, "ecv"] <- 3

Caine_data <- Caine_data %>% mutate(bucket = case_when(
  # separate elite players first, put them in their own bucket (top 10-15 in league)
  wOBA > high_wOBA ~ "challenge",
  
  # bucket based on red flags (is the player 'bad' at anything)
  ec_angle >= 95 | ec_angle <= 85 | ecv > 9 ~ "launch pos lv1",
  bat_speed < low_bat_speed & K_pct < low_K_pct & GB. > high_GB_pct ~ "bat speed lv1",
  GB. > high_GB_pct | FB. > high_FB_pct ~ "swing decision lv1", # damage percentage? want LA of 14-24 degrees, ~90mph exit velo (can change by league/age)
  bat_speed < low_bat_speed & K_pct > high_K_pct ~ "confidence", 
  bat_speed < low_bat_speed ~ "bat speed lv1", # catch anyone with very low bat speed but not in confidence bucket
  
  # if no red flags present, bucket based on yellow flags (is the player below average at anything?)
  ecv > 7 ~ "launch pos lv2",
  bat_speed < avg_bat_speed ~ "bat speed lv2",
  GB. > LG_avg_GB_pct | FB. > LG_avg_FB_pct ~ "swing decision lv2",
  
  # if no yellow flags present, identify which areas the player isn't 'elite' at
  ecv > 5 ~ "launch pos lv3",
  bat_speed < high_bat_speed ~ "bat speed lv3",
  GB. > low_GB_pct | FB. > low_FB_pct ~ "swing decision lv3",
  TRUE ~ "undefined"
))

Caine_data_small <- Caine_data %>% select(Last, First, LD., FB., GB., K_pct, wOBA, bat_speed, ec_angle, ecv, bucket)
View(Caine_data_small)


# what if Caine had elite ecv but the same other stats?
Caine_data_good_ecv <- Caine_data_good_ecv %>% mutate(bucket = case_when(
  # separate elite players first, put them in their own bucket (top 10-15 in league)
  wOBA > high_wOBA ~ "challenge",
  
  # bucket based on red flags (is the player 'bad' at anything)
  ec_angle >= 95 | ec_angle <= 85 | ecv > 9 ~ "launch pos lv1",
  bat_speed < low_bat_speed & K_pct < low_K_pct & GB. > high_GB_pct ~ "bat speed lv1",
  GB. > high_GB_pct | FB. > high_FB_pct ~ "swing decision lv1", # damage percentage? want LA of 14-24 degrees, ~90mph exit velo (can change by league/age)
  bat_speed < low_bat_speed & K_pct > high_K_pct ~ "confidence", 
  bat_speed < low_bat_speed ~ "bat speed lv1", # catch anyone with very low bat speed but not in confidence bucket
  
  # if no red flags present, bucket based on yellow flags (is the player below average at anything?)
  ecv > 7 ~ "launch pos lv2",
  bat_speed < avg_bat_speed ~ "bat speed lv2",
  GB. > LG_avg_GB_pct | FB. > LG_avg_FB_pct ~ "swing decision lv2",
  
  # if no yellow flags present, identify which areas the player isn't 'elite' at
  ecv > 5 ~ "launch pos lv3",
  bat_speed < high_bat_speed ~ "bat speed lv3",
  GB. > low_GB_pct | FB. > low_FB_pct ~ "swing decision lv3",
  TRUE ~ "undefined"
))

Caine_data_good_ecv_small <- Caine_data_good_ecv %>% select(Last, First, LD., FB., GB., K_pct, wOBA, bat_speed, ec_angle, ecv, bucket)
View(Caine_data_good_ecv_small)

