library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
# Shortstops --------------------------------------------------------------
ss_total <- player_pos %>% filter(player_position==6)
ss_total$distance <- 0
ss_total$time <- 0
x <- split(ss_total,ss_total$game_str)
#game index was taken randomly, then written down to make sure I was looking at same games each time I opened the file
games_index_ss <- c(42,33,61,12,64,82,21,89,94,52)
ss_games <- x[games_index_ss]
names(ss_games)
#1
for (n in 2:nrow(ss_games$`1902_14_TeamMD_TeamA2`)){
  if(ss_games$`1902_14_TeamMD_TeamA2`$play_id[n]==ss_games$`1902_14_TeamMD_TeamA2`$play_id[n-1]){
    ss_games$`1902_14_TeamMD_TeamA2`$distance[n] <- ((ss_games$`1902_14_TeamMD_TeamA2`$field_x[n] - ss_games$`1902_14_TeamMD_TeamA2`$field_x[n-1])^2 + (ss_games$`1902_14_TeamMD_TeamA2`$field_y[n] - ss_games$`1902_14_TeamMD_TeamA2`$field_y[n-1])^2)
    ss_games$`1902_14_TeamMD_TeamA2`$time[n] <- ss_games$`1902_14_TeamMD_TeamA2`$timestamp[n] - ss_games$`1902_14_TeamMD_TeamA2`$timestamp[n-1]
  }else{
    ss_games$`1902_14_TeamMD_TeamA2`$distance[n] <- 0
    ss_games$`1902_14_TeamMD_TeamA2`$time[n] <- 0
  } 
}  
#2
for (n in 2:nrow(ss_games$`1902_06_TeamML_TeamB`)){
  if(ss_games$`1902_06_TeamML_TeamB`$play_id[n]==ss_games$`1902_06_TeamML_TeamB`$play_id[n-1]){
    ss_games$`1902_06_TeamML_TeamB`$distance[n] <- ((ss_games$`1902_06_TeamML_TeamB`$field_x[n] - ss_games$`1902_06_TeamML_TeamB`$field_x[n-1])^2 + (ss_games$`1902_06_TeamML_TeamB`$field_y[n] - ss_games$`1902_06_TeamML_TeamB`$field_y[n-1])^2)
    ss_games$`1902_06_TeamML_TeamB`$time[n] <- ss_games$`1902_06_TeamML_TeamB`$timestamp[n] - ss_games$`1902_06_TeamML_TeamB`$timestamp[n-1]
  }else{
    ss_games$`1902_06_TeamML_TeamB`$distance[n] <- 0
    ss_games$`1902_06_TeamML_TeamB`$time[n] <- 0
  } 
}  
#3
for (n in 2:nrow(ss_games$`1902_31_TeamMF_TeamA2`)){
  if(ss_games$`1902_31_TeamMF_TeamA2`$play_id[n]==ss_games$`1902_31_TeamMF_TeamA2`$play_id[n-1]){
    ss_games$`1902_31_TeamMF_TeamA2`$distance[n] <- ((ss_games$`1902_31_TeamMF_TeamA2`$field_x[n] - ss_games$`1902_31_TeamMF_TeamA2`$field_x[n-1])^2 + (ss_games$`1902_31_TeamMF_TeamA2`$field_y[n] - ss_games$`1902_31_TeamMF_TeamA2`$field_y[n-1])^2)
    ss_games$`1902_31_TeamMF_TeamA2`$time[n] <- ss_games$`1902_31_TeamMF_TeamA2`$timestamp[n] - ss_games$`1902_31_TeamMF_TeamA2`$timestamp[n-1]
  }else{
    ss_games$`1902_31_TeamMF_TeamA2`$distance[n] <- 0
    ss_games$`1902_31_TeamMF_TeamA2`$time[n] <- 0
  } 
} 
#4
for (n in 2:nrow(ss_games$`1901_03_TeamLG_TeamA3`)){
  if(ss_games$`1901_03_TeamLG_TeamA3`$play_id[n]==ss_games$`1901_03_TeamLG_TeamA3`$play_id[n-1]){
    ss_games$`1901_03_TeamLG_TeamA3`$distance[n] <- ((ss_games$`1901_03_TeamLG_TeamA3`$field_x[n] - ss_games$`1901_03_TeamLG_TeamA3`$field_x[n-1])^2 + (ss_games$`1901_03_TeamLG_TeamA3`$field_y[n] - ss_games$`1901_03_TeamLG_TeamA3`$field_y[n-1])^2)
    ss_games$`1901_03_TeamLG_TeamA3`$time[n] <- ss_games$`1901_03_TeamLG_TeamA3`$timestamp[n] - ss_games$`1901_03_TeamLG_TeamA3`$timestamp[n-1]
  }else{
    ss_games$`1901_03_TeamLG_TeamA3`$distance[n] <- 0
    ss_games$`1901_03_TeamLG_TeamA3`$time[n] <- 0
  } 
}
#5
for (n in 2:nrow(ss_games$`1903_03_TeamNE_TeamA2`)){
  if(ss_games$`1903_03_TeamNE_TeamA2`$play_id[n]==ss_games$`1903_03_TeamNE_TeamA2`$play_id[n-1]){
    ss_games$`1903_03_TeamNE_TeamA2`$distance[n] <- ((ss_games$`1903_03_TeamNE_TeamA2`$field_x[n] - ss_games$`1903_03_TeamNE_TeamA2`$field_x[n-1])^2 + (ss_games$`1903_03_TeamNE_TeamA2`$field_y[n] - ss_games$`1903_03_TeamNE_TeamA2`$field_y[n-1])^2)
    ss_games$`1903_03_TeamNE_TeamA2`$time[n] <- ss_games$`1903_03_TeamNE_TeamA2`$timestamp[n] - ss_games$`1903_03_TeamNE_TeamA2`$timestamp[n-1]
  }else{
    ss_games$`1903_03_TeamNE_TeamA2`$distance[n] <- 0
    ss_games$`1903_03_TeamNE_TeamA2`$time[n] <- 0
  } 
} 
#6
for (n in 2:nrow(ss_games$`1903_21_TeamNL_TeamB`)){
  if(ss_games$`1903_21_TeamNL_TeamB`$play_id[n]==ss_games$`1903_21_TeamNL_TeamB`$play_id[n-1]){
    ss_games$`1903_21_TeamNL_TeamB`$distance[n] <- ((ss_games$`1903_21_TeamNL_TeamB`$field_x[n] - ss_games$`1903_21_TeamNL_TeamB`$field_x[n-1])^2 + (ss_games$`1903_21_TeamNL_TeamB`$field_y[n] - ss_games$`1903_21_TeamNL_TeamB`$field_y[n-1])^2)
    ss_games$`1903_21_TeamNL_TeamB`$time[n] <- ss_games$`1903_21_TeamNL_TeamB`$timestamp[n] - ss_games$`1903_21_TeamNL_TeamB`$timestamp[n-1]
  }else{
    ss_games$`1903_21_TeamNL_TeamB`$distance[n] <- 0
    ss_games$`1903_21_TeamNL_TeamB`$time[n] <- 0
  } 
}
#7
for (n in 2:nrow(ss_games$`1901_12_TeamLJ_TeamB`)){
  if(ss_games$`1901_12_TeamLJ_TeamB`$play_id[n]==ss_games$`1901_12_TeamLJ_TeamB`$play_id[n-1]){
    ss_games$`1901_12_TeamLJ_TeamB`$distance[n] <- ((ss_games$`1901_12_TeamLJ_TeamB`$field_x[n] - ss_games$`1901_12_TeamLJ_TeamB`$field_x[n-1])^2 + (ss_games$`1901_12_TeamLJ_TeamB`$field_y[n] - ss_games$`1901_12_TeamLJ_TeamB`$field_y[n-1])^2)
    ss_games$`1901_12_TeamLJ_TeamB`$time[n] <- ss_games$`1901_12_TeamLJ_TeamB`$timestamp[n] - ss_games$`1901_12_TeamLJ_TeamB`$timestamp[n-1]
  }else{
    ss_games$`1901_12_TeamLJ_TeamB`$distance[n] <- 0
    ss_games$`1901_12_TeamLJ_TeamB`$time[n] <- 0
  } 
} 
#8
for (n in 2:nrow(ss_games$`1903_26_TeamNK_TeamB`)){
  if(ss_games$`1903_26_TeamNK_TeamB`$play_id[n]==ss_games$`1903_26_TeamNK_TeamB`$play_id[n-1]){
    ss_games$`1903_26_TeamNK_TeamB`$distance[n] <- ((ss_games$`1903_26_TeamNK_TeamB`$field_x[n] - ss_games$`1903_26_TeamNK_TeamB`$field_x[n-1])^2 + (ss_games$`1903_26_TeamNK_TeamB`$field_y[n] - ss_games$`1903_26_TeamNK_TeamB`$field_y[n-1])^2)
    ss_games$`1903_26_TeamNK_TeamB`$time[n] <- ss_games$`1903_26_TeamNK_TeamB`$timestamp[n] - ss_games$`1903_26_TeamNK_TeamB`$timestamp[n-1]
  }else{
    ss_games$`1903_26_TeamNK_TeamB`$distance[n] <- 0
    ss_games$`1903_26_TeamNK_TeamB`$time[n] <- 0
  } 
} 
#9
for (n in 2:nrow(ss_games$`1903_30_TeamNB_TeamA1`)){
  if(ss_games$`1903_30_TeamNB_TeamA1`$play_id[n]==ss_games$`1903_30_TeamNB_TeamA1`$play_id[n-1]){
    ss_games$`1903_30_TeamNB_TeamA1`$distance[n] <- ((ss_games$`1903_30_TeamNB_TeamA1`$field_x[n] - ss_games$`1903_30_TeamNB_TeamA1`$field_x[n-1])^2 + (ss_games$`1903_30_TeamNB_TeamA1`$field_y[n] - ss_games$`1903_30_TeamNB_TeamA1`$field_y[n-1])^2)
    ss_games$`1903_30_TeamNB_TeamA1`$time[n] <- ss_games$`1903_30_TeamNB_TeamA1`$timestamp[n] - ss_games$`1903_30_TeamNB_TeamA1`$timestamp[n-1]
  }else{
    ss_games$`1903_30_TeamNB_TeamA1`$distance[n] <- 0
    ss_games$`1903_30_TeamNB_TeamA1`$time[n] <- 0
  } 
} 
#10
for (n in 2:nrow(ss_games$`1902_23_TeamMA_TeamA1`)){
  if(ss_games$`1902_23_TeamMA_TeamA1`$play_id[n]==ss_games$`1902_23_TeamMA_TeamA1`$play_id[n-1]){
    ss_games$`1902_23_TeamMA_TeamA1`$distance[n] <- ((ss_games$`1902_23_TeamMA_TeamA1`$field_x[n] - ss_games$`1902_23_TeamMA_TeamA1`$field_x[n-1])^2 + (ss_games$`1902_23_TeamMA_TeamA1`$field_y[n] - ss_games$`1902_23_TeamMA_TeamA1`$field_y[n-1])^2)
    ss_games$`1902_23_TeamMA_TeamA1`$time[n] <- ss_games$`1902_23_TeamMA_TeamA1`$timestamp[n] - ss_games$`1902_23_TeamMA_TeamA1`$timestamp[n-1]
  }else{
    ss_games$`1902_23_TeamMA_TeamA1`$distance[n] <- 0
    ss_games$`1902_23_TeamMA_TeamA1`$time[n] <- 0
  } 
}
#Compiling
ssg1 <- ss_games$`1902_14_TeamMD_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg2 <- ss_games$`1902_06_TeamML_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg3 <- ss_games$`1902_31_TeamMF_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg4 <- ss_games$`1901_03_TeamLG_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg5 <- ss_games$`1903_03_TeamNE_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg6 <- ss_games$`1903_21_TeamNL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg7 <- ss_games$`1901_12_TeamLJ_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg8 <- ss_games$`1903_26_TeamNK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg9 <- ss_games$`1903_30_TeamNB_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ssg10 <- ss_games$`1902_23_TeamMA_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
ss_distances <- c(ssg1$distance,
                  ssg2$distance,
                  ssg3$distance,
                  ssg4$distance,
                  ssg5$distance,
                  ssg6$distance,
                  ssg7$distance,
                  ssg8$distance,
                  ssg9$distance,
                  ssg10$distance)
ss_times <- c(ssg1$time,
              ssg2$time,
              ssg3$time,
              ssg4$time,
              ssg5$time,
              ssg6$time,
              ssg7$time,
              ssg8$time,
              ssg9$time,
              ssg10$time)
ss_movement <- as.data.frame(cbind(ss_distances,ss_times))
ss_movement <- rename(ss_movement,distance=ss_distances,time=ss_times)
write.csv(ss_movement,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/ss_movement.csv')
# Third Basemen -----------------------------------------------------------
b3_total <- player_pos %>% filter(player_position==5)
b3_total$distance <- 0
b3_total$time <- 0
x <- split(b3_total,b3_total$game_str)
games_index_3b <- c(94,32,74,4,6,11,26,77,55,40)
b3_games <- x[games_index_3b]
names(b3_games)
#1
for (n in 2:nrow(b3_games$`1903_30_TeamNB_TeamA1`)){
  if(b3_games$`1903_30_TeamNB_TeamA1`$play_id[n]==b3_games$`1903_30_TeamNB_TeamA1`$play_id[n-1]){
    b3_games$`1903_30_TeamNB_TeamA1`$distance[n] <- ((b3_games$`1903_30_TeamNB_TeamA1`$field_x[n] - b3_games$`1903_30_TeamNB_TeamA1`$field_x[n-1])^2 + (b3_games$`1903_30_TeamNB_TeamA1`$field_y[n] - b3_games$`1903_30_TeamNB_TeamA1`$field_y[n-1])^2)
    b3_games$`1903_30_TeamNB_TeamA1`$time[n] <- b3_games$`1903_30_TeamNB_TeamA1`$timestamp[n] - b3_games$`1903_30_TeamNB_TeamA1`$timestamp[n-1]
  }else{
    b3_games$`1903_30_TeamNB_TeamA1`$distance[n] <- 0
    b3_games$`1903_30_TeamNB_TeamA1`$time[n] <- 0
  } 
}  
#2
for (n in 2:nrow(b3_games$`1902_05_TeamML_TeamB`)){
  if(b3_games$`1902_05_TeamML_TeamB`$play_id[n]==b3_games$`1902_05_TeamML_TeamB`$play_id[n-1]){
    b3_games$`1902_05_TeamML_TeamB`$distance[n] <- ((b3_games$`1902_05_TeamML_TeamB`$field_x[n] - b3_games$`1902_05_TeamML_TeamB`$field_x[n-1])^2 + (b3_games$`1902_05_TeamML_TeamB`$field_y[n] - b3_games$`1902_05_TeamML_TeamB`$field_y[n-1])^2)
    b3_games$`1902_05_TeamML_TeamB`$time[n] <- b3_games$`1902_05_TeamML_TeamB`$timestamp[n] - b3_games$`1902_05_TeamML_TeamB`$timestamp[n-1]
  }else{
    b3_games$`1902_05_TeamML_TeamB`$distance[n] <- 0
    b3_games$`1902_05_TeamML_TeamB`$time[n] <- 0
  } 
}  
#3
for (n in 2:nrow(b3_games$`1902_31_TeamMF_TeamA2`)){
  if(b3_games$`1902_31_TeamMF_TeamA2`$play_id[n]==b3_games$`1902_31_TeamMF_TeamA2`$play_id[n-1]){
    b3_games$`1902_31_TeamMF_TeamA2`$distance[n] <- ((b3_games$`1902_31_TeamMF_TeamA2`$field_x[n] - b3_games$`1902_31_TeamMF_TeamA2`$field_x[n-1])^2 + (b3_games$`1902_31_TeamMF_TeamA2`$field_y[n] - b3_games$`1902_31_TeamMF_TeamA2`$field_y[n-1])^2)
    b3_games$`1902_31_TeamMF_TeamA2`$time[n] <- b3_games$`1902_31_TeamMF_TeamA2`$timestamp[n] - b3_games$`1902_31_TeamMF_TeamA2`$timestamp[n-1]
  }else{
    b3_games$`1902_31_TeamMF_TeamA2`$distance[n] <- 0
    b3_games$`1902_31_TeamMF_TeamA2`$time[n] <- 0
  } 
} 
#4
for (n in 2:nrow(b3_games$`1900_04_TeamKK_TeamB`)){
  if(b3_games$`1900_04_TeamKK_TeamB`$play_id[n]==b3_games$`1900_04_TeamKK_TeamB`$play_id[n-1]){
    b3_games$`1900_04_TeamKK_TeamB`$distance[n] <- ((b3_games$`1900_04_TeamKK_TeamB`$field_x[n] - b3_games$`1900_04_TeamKK_TeamB`$field_x[n-1])^2 + (b3_games$`1900_04_TeamKK_TeamB`$field_y[n] - b3_games$`1900_04_TeamKK_TeamB`$field_y[n-1])^2)
    b3_games$`1900_04_TeamKK_TeamB`$time[n] <- b3_games$`1900_04_TeamKK_TeamB`$timestamp[n] - b3_games$`1900_04_TeamKK_TeamB`$timestamp[n-1]
  }else{
    b3_games$`1900_04_TeamKK_TeamB`$distance[n] <- 0
    b3_games$`1900_04_TeamKK_TeamB`$time[n] <- 0
  } 
}
#5
for (n in 2:nrow(b3_games$`1900_06_TeamKL_TeamB`)){
  if(b3_games$`1900_06_TeamKL_TeamB`$play_id[n]==b3_games$`1900_06_TeamKL_TeamB`$play_id[n-1]){
    b3_games$`1900_06_TeamKL_TeamB`$distance[n] <- ((b3_games$`1900_06_TeamKL_TeamB`$field_x[n] - b3_games$`1900_06_TeamKL_TeamB`$field_x[n-1])^2 + (b3_games$`1900_06_TeamKL_TeamB`$field_y[n] - b3_games$`1900_06_TeamKL_TeamB`$field_y[n-1])^2)
    b3_games$`1900_06_TeamKL_TeamB`$time[n] <- b3_games$`1900_06_TeamKL_TeamB`$timestamp[n] - b3_games$`1900_06_TeamKL_TeamB`$timestamp[n-1]
  }else{
    b3_games$`1900_06_TeamKL_TeamB`$distance[n] <- 0
    b3_games$`1900_06_TeamKL_TeamB`$time[n] <- 0
  } 
} 
#6
for (n in 2:nrow(b3_games$`1901_02_TeamLG_TeamA3`)){
  if(b3_games$`1901_02_TeamLG_TeamA3`$play_id[n]==b3_games$`1901_02_TeamLG_TeamA3`$play_id[n-1]){
    b3_games$`1901_02_TeamLG_TeamA3`$distance[n] <- ((b3_games$`1901_02_TeamLG_TeamA3`$field_x[n] - b3_games$`1901_02_TeamLG_TeamA3`$field_x[n-1])^2 + (b3_games$`1901_02_TeamLG_TeamA3`$field_y[n] - b3_games$`1901_02_TeamLG_TeamA3`$field_y[n-1])^2)
    b3_games$`1901_02_TeamLG_TeamA3`$time[n] <- b3_games$`1901_02_TeamLG_TeamA3`$timestamp[n] - b3_games$`1901_02_TeamLG_TeamA3`$timestamp[n-1]
  }else{
    b3_games$`1901_02_TeamLG_TeamA3`$distance[n] <- 0
    b3_games$`1901_02_TeamLG_TeamA3`$time[n] <- 0
  } 
}
#7
for (n in 2:nrow(b3_games$`1901_17_TeamLH_TeamA3`)){
  if(b3_games$`1901_17_TeamLH_TeamA3`$play_id[n]==b3_games$`1901_17_TeamLH_TeamA3`$play_id[n-1]){
    b3_games$`1901_17_TeamLH_TeamA3`$distance[n] <- ((b3_games$`1901_17_TeamLH_TeamA3`$field_x[n] - b3_games$`1901_17_TeamLH_TeamA3`$field_x[n-1])^2 + (b3_games$`1901_17_TeamLH_TeamA3`$field_y[n] - b3_games$`1901_17_TeamLH_TeamA3`$field_y[n-1])^2)
    b3_games$`1901_17_TeamLH_TeamA3`$time[n] <- b3_games$`1901_17_TeamLH_TeamA3`$timestamp[n] - b3_games$`1901_17_TeamLH_TeamA3`$timestamp[n-1]
  }else{
    b3_games$`1901_17_TeamLH_TeamA3`$distance[n] <- 0
    b3_games$`1901_17_TeamLH_TeamA3`$time[n] <- 0
  } 
} 
#8
for (n in 2:nrow(b3_games$`1903_16_TeamNI_TeamA3`)){
  if(b3_games$`1903_16_TeamNI_TeamA3`$play_id[n]==b3_games$`1903_16_TeamNI_TeamA3`$play_id[n-1]){
    b3_games$`1903_16_TeamNI_TeamA3`$distance[n] <- ((b3_games$`1903_16_TeamNI_TeamA3`$field_x[n] - b3_games$`1903_16_TeamNI_TeamA3`$field_x[n-1])^2 + (b3_games$`1903_16_TeamNI_TeamA3`$field_y[n] - b3_games$`1903_16_TeamNI_TeamA3`$field_y[n-1])^2)
    b3_games$`1903_16_TeamNI_TeamA3`$time[n] <- b3_games$`1903_16_TeamNI_TeamA3`$timestamp[n] - b3_games$`1903_16_TeamNI_TeamA3`$timestamp[n-1]
  }else{
    b3_games$`1903_16_TeamNI_TeamA3`$distance[n] <- 0
    b3_games$`1903_16_TeamNI_TeamA3`$time[n] <- 0
  } 
} 
#9
for (n in 2:nrow(b3_games$`1902_26_TeamMC_TeamA1`)){
  if(b3_games$`1902_26_TeamMC_TeamA1`$play_id[n]==b3_games$`1902_26_TeamMC_TeamA1`$play_id[n-1]){
    b3_games$`1902_26_TeamMC_TeamA1`$distance[n] <- ((b3_games$`1902_26_TeamMC_TeamA1`$field_x[n] - b3_games$`1902_26_TeamMC_TeamA1`$field_x[n-1])^2 + (b3_games$`1902_26_TeamMC_TeamA1`$field_y[n] - b3_games$`1902_26_TeamMC_TeamA1`$field_y[n-1])^2)
    b3_games$`1902_26_TeamMC_TeamA1`$time[n] <- b3_games$`1902_26_TeamMC_TeamA1`$timestamp[n] - b3_games$`1902_26_TeamMC_TeamA1`$timestamp[n-1]
  }else{
    b3_games$`1902_26_TeamMC_TeamA1`$distance[n] <- 0
    b3_games$`1902_26_TeamMC_TeamA1`$time[n] <- 0
  } 
} 
#10
for (n in 2:nrow(b3_games$`1902_13_TeamMD_TeamA2`)){
  if(b3_games$`1902_13_TeamMD_TeamA2`$play_id[n]==b3_games$`1902_13_TeamMD_TeamA2`$play_id[n-1]){
    b3_games$`1902_13_TeamMD_TeamA2`$distance[n] <- ((b3_games$`1902_13_TeamMD_TeamA2`$field_x[n] - b3_games$`1902_13_TeamMD_TeamA2`$field_x[n-1])^2 + (b3_games$`1902_13_TeamMD_TeamA2`$field_y[n] - b3_games$`1902_13_TeamMD_TeamA2`$field_y[n-1])^2)
    b3_games$`1902_13_TeamMD_TeamA2`$time[n] <- b3_games$`1902_13_TeamMD_TeamA2`$timestamp[n] - b3_games$`1902_13_TeamMD_TeamA2`$timestamp[n-1]
  }else{
    b3_games$`1902_13_TeamMD_TeamA2`$distance[n] <- 0
    b3_games$`1902_13_TeamMD_TeamA2`$time[n] <- 0
  } 
}
#Compiling
b3g1 <- b3_games$`1903_30_TeamNB_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g2 <- b3_games$`1902_05_TeamML_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g3 <- b3_games$`1903_13_TeamNG_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g4 <- b3_games$`1900_04_TeamKK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g5 <- b3_games$`1900_06_TeamKL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g6 <- b3_games$`1901_02_TeamLG_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g7 <- b3_games$`1901_17_TeamLH_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g8 <- b3_games$`1903_16_TeamNI_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g9 <- b3_games$`1902_26_TeamMC_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3g10 <- b3_games$`1902_13_TeamMD_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b3_distances <- c(b3g1$distance,
                  b3g2$distance,
                  b3g3$distance,
                  b3g4$distance,
                  b3g5$distance,
                  b3g6$distance,
                  b3g7$distance,
                  b3g8$distance,
                  b3g9$distance,
                  b3g10$distance)
b3_times <- c(b3g1$time,
              b3g2$time,
              b3g3$time,
              b3g4$time,
              b3g5$time,
              b3g6$time,
              b3g7$time,
              b3g8$time,
              b3g9$time,
              b3g10$time)
b3_movement <- as.data.frame(cbind(b3_distances,b3_times))
b3_movement <- rename(b3_movement,distance=b3_distances,time=b3_times)
write.csv(b3_movement,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/b3_movement.csv')
# First Basemen -----------------------------------------------------------
b1_total <- player_pos %>% filter(player_position==3)
b1_total$distance <- 0
b1_total$time <- 0
x <- split(b1_total,b1_total$game_str)
games_index_b1 <- c(18,1,57,80,85,34,38,96,50,32)
b1_games <- x[games_index_b1]
names(b1_games)
#1
for (n in 2:nrow(b1_games$`1901_09_TeamLK_TeamB`)){
  if(b1_games$`1901_09_TeamLK_TeamB`$play_id[n]==b1_games$`1901_09_TeamLK_TeamB`$play_id[n-1]){
    b1_games$`1901_09_TeamLK_TeamB`$distance[n] <- ((b1_games$`1901_09_TeamLK_TeamB`$field_x[n] - b1_games$`1901_09_TeamLK_TeamB`$field_x[n-1])^2 + (b1_games$`1901_09_TeamLK_TeamB`$field_y[n] - b1_games$`1901_09_TeamLK_TeamB`$field_y[n-1])^2)
    b1_games$`1901_09_TeamLK_TeamB`$time[n] <- b1_games$`1901_09_TeamLK_TeamB`$timestamp[n] - b1_games$`1901_09_TeamLK_TeamB`$timestamp[n-1]
  }else{
    b1_games$`1901_09_TeamLK_TeamB`$distance[n] <- 0
    b1_games$`1901_09_TeamLK_TeamB`$time[n] <- 0
  } 
}  
b1g1 <- b1_games$`1901_09_TeamLK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#2
for (n in 2:nrow(b1_games$`1900_01_TeamKJ_TeamB`)){
  if(b1_games$`1900_01_TeamKJ_TeamB`$play_id[n]==b1_games$`1900_01_TeamKJ_TeamB`$play_id[n-1]){
    b1_games$`1900_01_TeamKJ_TeamB`$distance[n] <- ((b1_games$`1900_01_TeamKJ_TeamB`$field_x[n] - b1_games$`1900_01_TeamKJ_TeamB`$field_x[n-1])^2 + (b1_games$`1900_01_TeamKJ_TeamB`$field_y[n] - b1_games$`1900_01_TeamKJ_TeamB`$field_y[n-1])^2)
    b1_games$`1900_01_TeamKJ_TeamB`$time[n] <- b1_games$`1900_01_TeamKJ_TeamB`$timestamp[n] - b1_games$`1900_01_TeamKJ_TeamB`$timestamp[n-1]
  }else{
    b1_games$`1900_01_TeamKJ_TeamB`$distance[n] <- 0
    b1_games$`1900_01_TeamKJ_TeamB`$time[n] <- 0
  } 
}  
b1g2 <- b1_games$`1900_01_TeamKJ_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#3
for (n in 2:nrow(b1_games$`1902_27_TeamMH_TeamA3`)){
  if(b1_games$`1902_27_TeamMH_TeamA3`$play_id[n]==b1_games$`1902_27_TeamMH_TeamA3`$play_id[n-1]){
    b1_games$`1902_27_TeamMH_TeamA3`$distance[n] <- ((b1_games$`1902_27_TeamMH_TeamA3`$field_x[n] - b1_games$`1902_27_TeamMH_TeamA3`$field_x[n-1])^2 + (b1_games$`1902_27_TeamMH_TeamA3`$field_y[n] - b1_games$`1902_27_TeamMH_TeamA3`$field_y[n-1])^2)
    b1_games$`1902_27_TeamMH_TeamA3`$time[n] <- b1_games$`1902_27_TeamMH_TeamA3`$timestamp[n] - b1_games$`1902_27_TeamMH_TeamA3`$timestamp[n-1]
  }else{
    b1_games$`1902_27_TeamMH_TeamA3`$distance[n] <- 0
    b1_games$`1902_27_TeamMH_TeamA3`$time[n] <- 0
  } 
} 
b1g3 <- b1_games$`1902_27_TeamMH_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#4
for (n in 2:nrow(b1_games$`1903_19_TeamNL_TeamB`)){
  if(b1_games$`1903_19_TeamNL_TeamB`$play_id[n]==b1_games$`1903_19_TeamNL_TeamB`$play_id[n-1]){
    b1_games$`1903_19_TeamNL_TeamB`$distance[n] <- ((b1_games$`1903_19_TeamNL_TeamB`$field_x[n] - b1_games$`1903_19_TeamNL_TeamB`$field_x[n-1])^2 + (b1_games$`1903_19_TeamNL_TeamB`$field_y[n] - b1_games$`1903_19_TeamNL_TeamB`$field_y[n-1])^2)
    b1_games$`1903_19_TeamNL_TeamB`$time[n] <- b1_games$`1903_19_TeamNL_TeamB`$timestamp[n] - b1_games$`1903_19_TeamNL_TeamB`$timestamp[n-1]
  }else{
    b1_games$`1903_19_TeamNL_TeamB`$distance[n] <- 0
    b1_games$`1903_19_TeamNL_TeamB`$time[n] <- 0
  } 
}
b1g4 <- b1_games$`1903_19_TeamNL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#5
for (n in 2:nrow(b1_games$`1903_24_TeamNA_TeamA1`)){
  if(b1_games$`1903_24_TeamNA_TeamA1`$play_id[n]==b1_games$`1903_24_TeamNA_TeamA1`$play_id[n-1]){
    b1_games$`1903_24_TeamNA_TeamA1`$distance[n] <- ((b1_games$`1903_24_TeamNA_TeamA1`$field_x[n] - b1_games$`1903_24_TeamNA_TeamA1`$field_x[n-1])^2 + (b1_games$`1903_24_TeamNA_TeamA1`$field_y[n] - b1_games$`1903_24_TeamNA_TeamA1`$field_y[n-1])^2)
    b1_games$`1903_24_TeamNA_TeamA1`$time[n] <- b1_games$`1903_24_TeamNA_TeamA1`$timestamp[n] - b1_games$`1903_24_TeamNA_TeamA1`$timestamp[n-1]
  }else{
    b1_games$`1903_24_TeamNA_TeamA1`$distance[n] <- 0
    b1_games$`1903_24_TeamNA_TeamA1`$time[n] <- 0
  } 
}
b1g5 <- b1_games$`1903_24_TeamNA_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#6
for (n in 2:nrow(b1_games$`1902_07_TeamMJ_TeamB`)){
  if(b1_games$`1902_07_TeamMJ_TeamB`$play_id[n]==b1_games$`1902_07_TeamMJ_TeamB`$play_id[n-1]){
    b1_games$`1902_07_TeamMJ_TeamB`$distance[n] <- ((b1_games$`1902_07_TeamMJ_TeamB`$field_x[n] - b1_games$`1902_07_TeamMJ_TeamB`$field_x[n-1])^2 + (b1_games$`1902_07_TeamMJ_TeamB`$field_y[n] - b1_games$`1902_07_TeamMJ_TeamB`$field_y[n-1])^2)
    b1_games$`1902_07_TeamMJ_TeamB`$time[n] <- b1_games$`1902_07_TeamMJ_TeamB`$timestamp[n] - b1_games$`1902_07_TeamMJ_TeamB`$timestamp[n-1]
  }else{
    b1_games$`1902_07_TeamMJ_TeamB`$distance[n] <- 0
    b1_games$`1902_07_TeamMJ_TeamB`$time[n] <- 0
  } 
}
b1g6 <- b1_games$`1902_07_TeamMJ_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#7
for (n in 2:nrow(b1_games$`1902_11_TeamMI_TeamA3`)){
  if(b1_games$`1902_11_TeamMI_TeamA3`$play_id[n]==b1_games$`1902_11_TeamMI_TeamA3`$play_id[n-1]){
    b1_games$`1902_11_TeamMI_TeamA3`$distance[n] <- ((b1_games$`1902_11_TeamMI_TeamA3`$field_x[n] - b1_games$`1902_11_TeamMI_TeamA3`$field_x[n-1])^2 + (b1_games$`1902_11_TeamMI_TeamA3`$field_y[n] - b1_games$`1902_11_TeamMI_TeamA3`$field_y[n-1])^2)
    b1_games$`1902_11_TeamMI_TeamA3`$time[n] <- b1_games$`1902_11_TeamMI_TeamA3`$timestamp[n] - b1_games$`1902_11_TeamMI_TeamA3`$timestamp[n-1]
  }else{
    b1_games$`1902_11_TeamMI_TeamA3`$distance[n] <- 0
    b1_games$`1902_11_TeamMI_TeamA3`$time[n] <- 0
  } 
} 
b1g7 <- b1_games$`1902_11_TeamMI_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#8
for (n in 2:nrow(b1_games$`1903_31_TeamNB_TeamA1`)){
  if(b1_games$`1903_31_TeamNB_TeamA1`$play_id[n]==b1_games$`1903_31_TeamNB_TeamA1`$play_id[n-1]){
    b1_games$`1903_31_TeamNB_TeamA1`$distance[n] <- ((b1_games$`1903_31_TeamNB_TeamA1`$field_x[n] - b1_games$`1903_31_TeamNB_TeamA1`$field_x[n-1])^2 + (b1_games$`1903_31_TeamNB_TeamA1`$field_y[n] - b1_games$`1903_31_TeamNB_TeamA1`$field_y[n-1])^2)
    b1_games$`1903_31_TeamNB_TeamA1`$time[n] <- b1_games$`1903_31_TeamNB_TeamA1`$timestamp[n] - b1_games$`1903_31_TeamNB_TeamA1`$timestamp[n-1]
  }else{
    b1_games$`1903_31_TeamNB_TeamA1`$distance[n] <- 0
    b1_games$`1903_31_TeamNB_TeamA1`$time[n] <- 0
  } 
}
b1g8 <- b1_games$`1903_31_TeamNB_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#9
for (n in 2:nrow(b1_games$`1902_21_TeamME_TeamA2`)){
  if(b1_games$`1902_21_TeamME_TeamA2`$play_id[n]==b1_games$`1902_21_TeamME_TeamA2`$play_id[n-1]){
    b1_games$`1902_21_TeamME_TeamA2`$distance[n] <- ((b1_games$`1902_21_TeamME_TeamA2`$field_x[n] - b1_games$`1902_21_TeamME_TeamA2`$field_x[n-1])^2 + (b1_games$`1902_21_TeamME_TeamA2`$field_y[n] - b1_games$`1902_21_TeamME_TeamA2`$field_y[n-1])^2)
    b1_games$`1902_21_TeamME_TeamA2`$time[n] <- b1_games$`1902_21_TeamME_TeamA2`$timestamp[n] - b1_games$`1902_21_TeamME_TeamA2`$timestamp[n-1]
  }else{
    b1_games$`1902_21_TeamME_TeamA2`$distance[n] <- 0
    b1_games$`1902_21_TeamME_TeamA2`$time[n] <- 0
  } 
} 
b1g9 <- b1_games$`1902_21_TeamME_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#10
for (n in 2:nrow(b1_games$`1902_05_TeamML_TeamB`)){
  if(b1_games$`1902_05_TeamML_TeamB`$play_id[n]==b1_games$`1902_05_TeamML_TeamB`$play_id[n-1]){
    b1_games$`1902_05_TeamML_TeamB`$distance[n] <- ((b1_games$`1902_05_TeamML_TeamB`$field_x[n] - b1_games$`1902_05_TeamML_TeamB`$field_x[n-1])^2 + (b1_games$`1902_05_TeamML_TeamB`$field_y[n] - b1_games$`1902_05_TeamML_TeamB`$field_y[n-1])^2)
    b1_games$`1902_05_TeamML_TeamB`$time[n] <- b1_games$`1902_05_TeamML_TeamB`$timestamp[n] - b1_games$`1902_05_TeamML_TeamB`$timestamp[n-1]
  }else{
    b1_games$`1902_05_TeamML_TeamB`$distance[n] <- 0
    b1_games$`1902_05_TeamML_TeamB`$time[n] <- 0
  } 
}
b1g10 <- b1_games$`1902_05_TeamML_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
#Compiling
b1_distances <- c(b1g1$distance,
                  b1g2$distance,
                  b1g3$distance,
                  b1g4$distance,
                  b1g5$distance,
                  b1g6$distance,
                  b1g7$distance,
                  b1g8$distance,
                  b1g9$distance,
                  b1g10$distance)
b1_times <- c(b1g1$time,
              b1g2$time,
              b1g3$time,
              b1g4$time,
              b1g5$time,
              b1g6$time,
              b1g7$time,
              b1g8$time,
              b1g9$time,
              b1g10$time)
b1_movement <- as.data.frame(cbind(b1_distances,b1_times))
b1_movement <- rename(b1_movement,distance=b1_distances,time=b1_times)
write.csv(b1_movement,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/b1_movement.csv')
# Second Basemen ----------------------------------------------------------
b2_total <- player_pos %>% filter(player_position==4)
b2_total$distance <- 0
b2_total$time <- 0
x <- split(b2_total,b2_total$game_str)
games_index_b2 <- c(7,48,61,40,44,9,83,32,12,6)
b2_games <- x[games_index_b2]
names(b2_games)
#1
for (n in 2:nrow(b2_games$`1900_07_TeamKL_TeamB`)){
  if(b2_games$`1900_07_TeamKL_TeamB`$play_id[n]==b2_games$`1900_07_TeamKL_TeamB`$play_id[n-1]){
    b2_games$`1900_07_TeamKL_TeamB`$distance[n] <- ((b2_games$`1900_07_TeamKL_TeamB`$field_x[n] - b2_games$`1900_07_TeamKL_TeamB`$field_x[n-1])^2 + (b2_games$`1900_07_TeamKL_TeamB`$field_y[n] - b2_games$`1900_07_TeamKL_TeamB`$field_y[n-1])^2)
    b2_games$`1900_07_TeamKL_TeamB`$time[n] <- b2_games$`1900_07_TeamKL_TeamB`$timestamp[n] - b2_games$`1900_07_TeamKL_TeamB`$timestamp[n-1]
  }else{
    b2_games$`1900_07_TeamKL_TeamB`$distance[n] <- 0
    b2_games$`1900_07_TeamKL_TeamB`$time[n] <- 0
  } 
}  
#2
for (n in 2:nrow(b2_games$`1902_19_TeamME_TeamA2`)){
  if(b2_games$`1902_19_TeamME_TeamA2`$play_id[n]==b2_games$`1902_19_TeamME_TeamA2`$play_id[n-1]){
    b2_games$`1902_19_TeamME_TeamA2`$distance[n] <- ((b2_games$`1902_19_TeamME_TeamA2`$field_x[n] - b2_games$`1902_19_TeamME_TeamA2`$field_x[n-1])^2 + (b2_games$`1902_19_TeamME_TeamA2`$field_y[n] - b2_games$`1902_19_TeamME_TeamA2`$field_y[n-1])^2)
    b2_games$`1902_19_TeamME_TeamA2`$time[n] <- b2_games$`1902_19_TeamME_TeamA2`$timestamp[n] - b2_games$`1902_19_TeamME_TeamA2`$timestamp[n-1]
  }else{
    b2_games$`1902_19_TeamME_TeamA2`$distance[n] <- 0
    b2_games$`1902_19_TeamME_TeamA2`$time[n] <- 0
  } 
}  
#3
for (n in 2:nrow(b2_games$`1902_31_TeamMF_TeamA2`)){
  if(b2_games$`1902_31_TeamMF_TeamA2`$play_id[n]==b2_games$`1902_31_TeamMF_TeamA2`$play_id[n-1]){
    b2_games$`1902_31_TeamMF_TeamA2`$distance[n] <- ((b2_games$`1902_31_TeamMF_TeamA2`$field_x[n] - b2_games$`1902_31_TeamMF_TeamA2`$field_x[n-1])^2 + (b2_games$`1902_31_TeamMF_TeamA2`$field_y[n] - b2_games$`1902_31_TeamMF_TeamA2`$field_y[n-1])^2)
    b2_games$`1902_31_TeamMF_TeamA2`$time[n] <- b2_games$`1902_31_TeamMF_TeamA2`$timestamp[n] - b2_games$`1902_31_TeamMF_TeamA2`$timestamp[n-1]
  }else{
    b2_games$`1902_31_TeamMF_TeamA2`$distance[n] <- 0
    b2_games$`1902_31_TeamMF_TeamA2`$time[n] <- 0
  } 
} 
#4
for (n in 2:nrow(b2_games$`1902_13_TeamMD_TeamA2`)){
  if(b2_games$`1902_13_TeamMD_TeamA2`$play_id[n]==b2_games$`1902_13_TeamMD_TeamA2`$play_id[n-1]){
    b2_games$`1902_13_TeamMD_TeamA2`$distance[n] <- ((b2_games$`1902_13_TeamMD_TeamA2`$field_x[n] - b2_games$`1902_13_TeamMD_TeamA2`$field_x[n-1])^2 + (b2_games$`1902_13_TeamMD_TeamA2`$field_y[n] - b2_games$`1902_13_TeamMD_TeamA2`$field_y[n-1])^2)
    b2_games$`1902_13_TeamMD_TeamA2`$time[n] <- b2_games$`1902_13_TeamMD_TeamA2`$timestamp[n] - b2_games$`1902_13_TeamMD_TeamA2`$timestamp[n-1]
  }else{
    b2_games$`1902_13_TeamMD_TeamA2`$distance[n] <- 0
    b2_games$`1902_13_TeamMD_TeamA2`$time[n] <- 0
  } 
}
#5
for (n in 2:nrow(b2_games$`1902_15_TeamMK_TeamB`)){
  if(b2_games$`1902_15_TeamMK_TeamB`$play_id[n]==b2_games$`1902_15_TeamMK_TeamB`$play_id[n-1]){
    b2_games$`1902_15_TeamMK_TeamB`$distance[n] <- ((b2_games$`1902_15_TeamMK_TeamB`$field_x[n] - b2_games$`1902_15_TeamMK_TeamB`$field_x[n-1])^2 + (b2_games$`1902_15_TeamMK_TeamB`$field_y[n] - b2_games$`1902_15_TeamMK_TeamB`$field_y[n-1])^2)
    b2_games$`1902_15_TeamMK_TeamB`$time[n] <- b2_games$`1902_15_TeamMK_TeamB`$timestamp[n] - b2_games$`1902_15_TeamMK_TeamB`$timestamp[n-1]
  }else{
    b2_games$`1902_15_TeamMK_TeamB`$distance[n] <- 0
    b2_games$`1902_15_TeamMK_TeamB`$time[n] <- 0
  } 
} 
#6
for (n in 2:nrow(b2_games$`1900_09_TeamKK_TeamB`)){
  if(b2_games$`1900_09_TeamKK_TeamB`$play_id[n]==b2_games$`1900_09_TeamKK_TeamB`$play_id[n-1]){
    b2_games$`1900_09_TeamKK_TeamB`$distance[n] <- ((b2_games$`1900_09_TeamKK_TeamB`$field_x[n] - b2_games$`1900_09_TeamKK_TeamB`$field_x[n-1])^2 + (b2_games$`1900_09_TeamKK_TeamB`$field_y[n] - b2_games$`1900_09_TeamKK_TeamB`$field_y[n-1])^2)
    b2_games$`1900_09_TeamKK_TeamB`$time[n] <- b2_games$`1900_09_TeamKK_TeamB`$timestamp[n] - b2_games$`1900_09_TeamKK_TeamB`$timestamp[n-1]
  }else{
    b2_games$`1900_09_TeamKK_TeamB`$distance[n] <- 0
    b2_games$`1900_09_TeamKK_TeamB`$time[n] <- 0
  } 
}
#7
for (n in 2:nrow(b2_games$`1903_22_TeamNA_TeamA1`)){
  if(b2_games$`1903_22_TeamNA_TeamA1`$play_id[n]==b2_games$`1903_22_TeamNA_TeamA1`$play_id[n-1]){
    b2_games$`1903_22_TeamNA_TeamA1`$distance[n] <- ((b2_games$`1903_22_TeamNA_TeamA1`$field_x[n] - b2_games$`1903_22_TeamNA_TeamA1`$field_x[n-1])^2 + (b2_games$`1903_22_TeamNA_TeamA1`$field_y[n] - b2_games$`1903_22_TeamNA_TeamA1`$field_y[n-1])^2)
    b2_games$`1903_22_TeamNA_TeamA1`$time[n] <- b2_games$`1903_22_TeamNA_TeamA1`$timestamp[n] - b2_games$`1903_22_TeamNA_TeamA1`$timestamp[n-1]
  }else{
    b2_games$`1903_22_TeamNA_TeamA1`$distance[n] <- 0
    b2_games$`1903_22_TeamNA_TeamA1`$time[n] <- 0
  } 
} 
#8
for (n in 2:nrow(b2_games$`1902_05_TeamML_TeamB`)){
  if(b2_games$`1902_05_TeamML_TeamB`$play_id[n]==b2_games$`1902_05_TeamML_TeamB`$play_id[n-1]){
    b2_games$`1902_05_TeamML_TeamB`$distance[n] <- ((b2_games$`1902_05_TeamML_TeamB`$field_x[n] - b2_games$`1902_05_TeamML_TeamB`$field_x[n-1])^2 + (b2_games$`1902_05_TeamML_TeamB`$field_y[n] - b2_games$`1902_05_TeamML_TeamB`$field_y[n-1])^2)
    b2_games$`1902_05_TeamML_TeamB`$time[n] <- b2_games$`1902_05_TeamML_TeamB`$timestamp[n] - b2_games$`1902_05_TeamML_TeamB`$timestamp[n-1]
  }else{
    b2_games$`1902_05_TeamML_TeamB`$distance[n] <- 0
    b2_games$`1902_05_TeamML_TeamB`$time[n] <- 0
  } 
} 
#9
for (n in 2:nrow(b2_games$`1901_03_TeamLG_TeamA3`)){
  if(b2_games$`1901_03_TeamLG_TeamA3`$play_id[n]==b2_games$`1901_03_TeamLG_TeamA3`$play_id[n-1]){
    b2_games$`1901_03_TeamLG_TeamA3`$distance[n] <- ((b2_games$`1901_03_TeamLG_TeamA3`$field_x[n] - b2_games$`1901_03_TeamLG_TeamA3`$field_x[n-1])^2 + (b2_games$`1901_03_TeamLG_TeamA3`$field_y[n] - b2_games$`1901_03_TeamLG_TeamA3`$field_y[n-1])^2)
    b2_games$`1901_03_TeamLG_TeamA3`$time[n] <- b2_games$`1901_03_TeamLG_TeamA3`$timestamp[n] - b2_games$`1901_03_TeamLG_TeamA3`$timestamp[n-1]
  }else{
    b2_games$`1901_03_TeamLG_TeamA3`$distance[n] <- 0
    b2_games$`1901_03_TeamLG_TeamA3`$time[n] <- 0
  } 
} 
#10
for (n in 2:nrow(b2_games$`1900_06_TeamKL_TeamB`)){
  if(b2_games$`1900_06_TeamKL_TeamB`$play_id[n]==b2_games$`1900_06_TeamKL_TeamB`$play_id[n-1]){
    b2_games$`1900_06_TeamKL_TeamB`$distance[n] <- ((b2_games$`1900_06_TeamKL_TeamB`$field_x[n] - b2_games$`1900_06_TeamKL_TeamB`$field_x[n-1])^2 + (b2_games$`1900_06_TeamKL_TeamB`$field_y[n] - b2_games$`1900_06_TeamKL_TeamB`$field_y[n-1])^2)
    b2_games$`1900_06_TeamKL_TeamB`$time[n] <- b2_games$`1900_06_TeamKL_TeamB`$timestamp[n] - b2_games$`1900_06_TeamKL_TeamB`$timestamp[n-1]
  }else{
    b2_games$`1900_06_TeamKL_TeamB`$distance[n] <- 0
    b2_games$`1900_06_TeamKL_TeamB`$time[n] <- 0
  } 
} 
#Compiling
b2g1 <- b2_games$`1900_07_TeamKL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g2 <- b2_games$`1902_19_TeamME_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g3 <- b2_games$`1902_31_TeamMF_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g4 <- b2_games$`1902_13_TeamMD_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g5 <- b2_games$`1902_15_TeamMK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g6 <- b2_games$`1900_09_TeamKK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g7 <- b2_games$`1903_22_TeamNA_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g8 <- b2_games$`1902_05_TeamML_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g9 <- b2_games$`1901_03_TeamLG_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2g10 <- b2_games$`1900_06_TeamKL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
b2_distances <- c(b2g1$distance,
                  b2g2$distance,
                  b2g3$distance,
                  b2g4$distance,
                  b2g5$distance,
                  b2g6$distance,
                  b2g7$distance,
                  b2g8$distance,
                  b2g9$distance,
                  b2g10$distance)
b2_times <- c(b2g1$time,
              b2g2$time,
              b2g3$time,
              b2g4$time,
              b2g5$time,
              b2g6$time,
              b2g7$time,
              b2g8$time,
              b2g9$time,
              b2g10$time)
b2_movement <- as.data.frame(cbind(b2_distances,b2_times))
b2_movement <- rename(b2_movement,distance=b2_distances,time=b2_times)
write.csv(b2_movement,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/b2_movement.csv')

# Left Field --------------------------------------------------------------
lf_total <- player_pos %>% filter(player_position==7)
lf_total$distance <- 0
lf_total$time <- 0
x <- split(lf_total,lf_total$game_str)
games_index_lf <- c(85,22,43,44,13,81,37,80,65,73)
lf_games <- x[games_index_lf]
names(lf_games)
#1
for (n in 2:nrow(lf_games$`1903_24_TeamNA_TeamA1`)){
  if(lf_games$`1903_24_TeamNA_TeamA1`$play_id[n]==lf_games$`1903_24_TeamNA_TeamA1`$play_id[n-1]){
    lf_games$`1903_24_TeamNA_TeamA1`$distance[n] <- ((lf_games$`1903_24_TeamNA_TeamA1`$field_x[n] - lf_games$`1903_24_TeamNA_TeamA1`$field_x[n-1])^2 + (lf_games$`1903_24_TeamNA_TeamA1`$field_y[n] - lf_games$`1903_24_TeamNA_TeamA1`$field_y[n-1])^2)
    lf_games$`1903_24_TeamNA_TeamA1`$time[n] <- lf_games$`1903_24_TeamNA_TeamA1`$timestamp[n] - lf_games$`1903_24_TeamNA_TeamA1`$timestamp[n-1]
  }else{
    lf_games$`1903_24_TeamNA_TeamA1`$distance[n] <- 0
    lf_games$`1903_24_TeamNA_TeamA1`$time[n] <- 0
  } 
}  
#2
for (n in 2:nrow(lf_games$`1901_13_TeamLL_TeamB`)){
  if(lf_games$`1901_13_TeamLL_TeamB`$play_id[n]==lf_games$`1901_13_TeamLL_TeamB`$play_id[n-1]){
    lf_games$`1901_13_TeamLL_TeamB`$distance[n] <- ((lf_games$`1901_13_TeamLL_TeamB`$field_x[n] - lf_games$`1901_13_TeamLL_TeamB`$field_x[n-1])^2 + (lf_games$`1901_13_TeamLL_TeamB`$field_y[n] - lf_games$`1901_13_TeamLL_TeamB`$field_y[n-1])^2)
    lf_games$`1901_13_TeamLL_TeamB`$time[n] <- lf_games$`1901_13_TeamLL_TeamB`$timestamp[n] - lf_games$`1901_13_TeamLL_TeamB`$timestamp[n-1]
  }else{
    lf_games$`1901_13_TeamLL_TeamB`$distance[n] <- 0
    lf_games$`1901_13_TeamLL_TeamB`$time[n] <- 0
  } 
}  
#3
for (n in 2:nrow(lf_games$`1902_14_TeamMK_TeamB`)){
  if(lf_games$`1902_14_TeamMK_TeamB`$play_id[n]==lf_games$`1902_14_TeamMK_TeamB`$play_id[n-1]){
    lf_games$`1902_14_TeamMK_TeamB`$distance[n] <- ((lf_games$`1902_14_TeamMK_TeamB`$field_x[n] - lf_games$`1902_14_TeamMK_TeamB`$field_x[n-1])^2 + (lf_games$`1902_14_TeamMK_TeamB`$field_y[n] - lf_games$`1902_14_TeamMK_TeamB`$field_y[n-1])^2)
    lf_games$`1902_14_TeamMK_TeamB`$time[n] <- lf_games$`1902_14_TeamMK_TeamB`$timestamp[n] - lf_games$`1902_14_TeamMK_TeamB`$timestamp[n-1]
  }else{
    lf_games$`1902_14_TeamMK_TeamB`$distance[n] <- 0
    lf_games$`1902_14_TeamMK_TeamB`$time[n] <- 0
  } 
} 
#4
for (n in 2:nrow(lf_games$`1902_15_TeamMK_TeamB`)){
  if(lf_games$`1902_15_TeamMK_TeamB`$play_id[n]==lf_games$`1902_15_TeamMK_TeamB`$play_id[n-1]){
    lf_games$`1902_15_TeamMK_TeamB`$distance[n] <- ((lf_games$`1902_15_TeamMK_TeamB`$field_x[n] - lf_games$`1902_15_TeamMK_TeamB`$field_x[n-1])^2 + (lf_games$`1902_15_TeamMK_TeamB`$field_y[n] - lf_games$`1902_15_TeamMK_TeamB`$field_y[n-1])^2)
    lf_games$`1902_15_TeamMK_TeamB`$time[n] <- lf_games$`1902_15_TeamMK_TeamB`$timestamp[n] - lf_games$`1902_15_TeamMK_TeamB`$timestamp[n-1]
  }else{
    lf_games$`1902_15_TeamMK_TeamB`$distance[n] <- 0
    lf_games$`1902_15_TeamMK_TeamB`$time[n] <- 0
  } 
}
#5
for (n in 2:nrow(lf_games$`1901_04_TeamLI_TeamA3`)){
  if(lf_games$`1901_04_TeamLI_TeamA3`$play_id[n]==lf_games$`1901_04_TeamLI_TeamA3`$play_id[n-1]){
    lf_games$`1901_04_TeamLI_TeamA3`$distance[n] <- ((lf_games$`1901_04_TeamLI_TeamA3`$field_x[n] - lf_games$`1901_04_TeamLI_TeamA3`$field_x[n-1])^2 + (lf_games$`1901_04_TeamLI_TeamA3`$field_y[n] - lf_games$`1901_04_TeamLI_TeamA3`$field_y[n-1])^2)
    lf_games$`1901_04_TeamLI_TeamA3`$time[n] <- lf_games$`1901_04_TeamLI_TeamA3`$timestamp[n] - lf_games$`1901_04_TeamLI_TeamA3`$timestamp[n-1]
  }else{
    lf_games$`1901_04_TeamLI_TeamA3`$distance[n] <- 0
    lf_games$`1901_04_TeamLI_TeamA3`$time[n] <- 0
  } 
} 
#6
for (n in 2:nrow(lf_games$`1903_20_TeamNL_TeamB`)){
  if(lf_games$`1903_20_TeamNL_TeamB`$play_id[n]==lf_games$`1903_20_TeamNL_TeamB`$play_id[n-1]){
    lf_games$`1903_20_TeamNL_TeamB`$distance[n] <- ((lf_games$`1903_20_TeamNL_TeamB`$field_x[n] - lf_games$`1903_20_TeamNL_TeamB`$field_x[n-1])^2 + (lf_games$`1903_20_TeamNL_TeamB`$field_y[n] - lf_games$`1903_20_TeamNL_TeamB`$field_y[n-1])^2)
    lf_games$`1903_20_TeamNL_TeamB`$time[n] <- lf_games$`1903_20_TeamNL_TeamB`$timestamp[n] - lf_games$`1903_20_TeamNL_TeamB`$timestamp[n-1]
  }else{
    lf_games$`1903_20_TeamNL_TeamB`$distance[n] <- 0
    lf_games$`1903_20_TeamNL_TeamB`$time[n] <- 0
  } 
}
#7
for (n in 2:nrow(lf_games$`1902_10_TeamMI_TeamA3`)){
  if(lf_games$`1902_10_TeamMI_TeamA3`$play_id[n]==lf_games$`1902_10_TeamMI_TeamA3`$play_id[n-1]){
    lf_games$`1902_10_TeamMI_TeamA3`$distance[n] <- ((lf_games$`1902_10_TeamMI_TeamA3`$field_x[n] - lf_games$`1902_10_TeamMI_TeamA3`$field_x[n-1])^2 + (lf_games$`1902_10_TeamMI_TeamA3`$field_y[n] - lf_games$`1902_10_TeamMI_TeamA3`$field_y[n-1])^2)
    lf_games$`1902_10_TeamMI_TeamA3`$time[n] <- lf_games$`1902_10_TeamMI_TeamA3`$timestamp[n] - lf_games$`1902_10_TeamMI_TeamA3`$timestamp[n-1]
  }else{
    lf_games$`1902_10_TeamMI_TeamA3`$distance[n] <- 0
    lf_games$`1902_10_TeamMI_TeamA3`$time[n] <- 0
  } 
} 
#8
for (n in 2:nrow(lf_games$`1903_19_TeamNL_TeamB`)){
  if(lf_games$`1903_19_TeamNL_TeamB`$play_id[n]==lf_games$`1903_19_TeamNL_TeamB`$play_id[n-1]){
    lf_games$`1903_19_TeamNL_TeamB`$distance[n] <- ((lf_games$`1903_19_TeamNL_TeamB`$field_x[n] - lf_games$`1903_19_TeamNL_TeamB`$field_x[n-1])^2 + (lf_games$`1903_19_TeamNL_TeamB`$field_y[n] - lf_games$`1903_19_TeamNL_TeamB`$field_y[n-1])^2)
    lf_games$`1903_19_TeamNL_TeamB`$time[n] <- lf_games$`1903_19_TeamNL_TeamB`$timestamp[n] - lf_games$`1903_19_TeamNL_TeamB`$timestamp[n-1]
  }else{
    lf_games$`1903_19_TeamNL_TeamB`$distance[n] <- 0
    lf_games$`1903_19_TeamNL_TeamB`$time[n] <- 0
  } 
} 
#9
for (n in 2:nrow(lf_games$`1903_04_TeamNC_TeamA1`)){
  if(lf_games$`1903_04_TeamNC_TeamA1`$play_id[n]==lf_games$`1903_04_TeamNC_TeamA1`$play_id[n-1]){
    lf_games$`1903_04_TeamNC_TeamA1`$distance[n] <- ((lf_games$`1903_04_TeamNC_TeamA1`$field_x[n] - lf_games$`1903_04_TeamNC_TeamA1`$field_x[n-1])^2 + (lf_games$`1903_04_TeamNC_TeamA1`$field_y[n] - lf_games$`1903_04_TeamNC_TeamA1`$field_y[n-1])^2)
    lf_games$`1903_04_TeamNC_TeamA1`$time[n] <- lf_games$`1903_04_TeamNC_TeamA1`$timestamp[n] - lf_games$`1903_04_TeamNC_TeamA1`$timestamp[n-1]
  }else{
    lf_games$`1903_04_TeamNC_TeamA1`$distance[n] <- 0
    lf_games$`1903_04_TeamNC_TeamA1`$time[n] <- 0
  } 
} 
#10
for (n in 2:nrow(lf_games$`1903_12_TeamNC_TeamA1`)){
  if(lf_games$`1903_12_TeamNC_TeamA1`$play_id[n]==lf_games$`1903_12_TeamNC_TeamA1`$play_id[n-1]){
    lf_games$`1903_12_TeamNC_TeamA1`$distance[n] <- ((lf_games$`1903_12_TeamNC_TeamA1`$field_x[n] - lf_games$`1903_12_TeamNC_TeamA1`$field_x[n-1])^2 + (lf_games$`1903_12_TeamNC_TeamA1`$field_y[n] - lf_games$`1903_12_TeamNC_TeamA1`$field_y[n-1])^2)
    lf_games$`1903_12_TeamNC_TeamA1`$time[n] <- lf_games$`1903_12_TeamNC_TeamA1`$timestamp[n] - lf_games$`1903_12_TeamNC_TeamA1`$timestamp[n-1]
  }else{
    lf_games$`1903_12_TeamNC_TeamA1`$distance[n] <- 0
    lf_games$`1903_12_TeamNC_TeamA1`$time[n] <- 0
  } 
}
#Compiling
lfg1 <- lf_games$`1903_24_TeamNA_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg2 <- lf_games$`1901_13_TeamLL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg3 <- lf_games$`1902_14_TeamMK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg4 <- lf_games$`1902_15_TeamMK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg5 <- lf_games$`1901_04_TeamLI_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg6 <- lf_games$`1903_20_TeamNL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg7 <- lf_games$`1902_10_TeamMI_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg8 <- lf_games$`1903_19_TeamNL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg9 <- lf_games$`1903_04_TeamNC_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lfg10 <- lf_games$`1903_12_TeamNC_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
lf_distances <- c(lfg1$distance,
                  lfg2$distance,
                  lfg3$distance,
                  lfg4$distance,
                  lfg5$distance,
                  lfg6$distance,
                  lfg7$distance,
                  lfg8$distance,
                  lfg9$distance,
                  lfg10$distance)
lf_times <- c(lfg1$time,
              lfg2$time,
              lfg3$time,
              lfg4$time,
              lfg5$time,
              lfg6$time,
              lfg7$time,
              lfg8$time,
              lfg9$time,
              lfg10$time)
lf_movement <- as.data.frame(cbind(lf_distances,lf_times))
lf_movement <- rename(lf_movement,distance=lf_distances,time=lf_times)
write.csv(lf_movement,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/lf_movement.csv')
# Center Field ------------------------------------------------------------
cf_total <- player_pos %>% filter(player_position==8)
cf_total$distance <- 0
cf_total$time <- 0
x <- split(cf_total,cf_total$game_str)
games_index_cf <- c(38,17,47,7,76,72,67,41,13,53)
cf_games <- x[games_index_cf]
names(cf_games)
#1
for (n in 2:nrow(cf_games$`1902_11_TeamMI_TeamA3`)){
  if(cf_games$`1902_11_TeamMI_TeamA3`$play_id[n]==cf_games$`1902_11_TeamMI_TeamA3`$play_id[n-1]){
    cf_games$`1902_11_TeamMI_TeamA3`$distance[n] <- ((cf_games$`1902_11_TeamMI_TeamA3`$field_x[n] - cf_games$`1902_11_TeamMI_TeamA3`$field_x[n-1])^2 + (cf_games$`1902_11_TeamMI_TeamA3`$field_y[n] - cf_games$`1902_11_TeamMI_TeamA3`$field_y[n-1])^2)
    cf_games$`1902_11_TeamMI_TeamA3`$time[n] <- cf_games$`1902_11_TeamMI_TeamA3`$timestamp[n] - cf_games$`1902_11_TeamMI_TeamA3`$timestamp[n-1]
  }else{
    cf_games$`1902_11_TeamMI_TeamA3`$distance[n] <- 0
    cf_games$`1902_11_TeamMI_TeamA3`$time[n] <- 0
  } 
}  
#2
for (n in 2:nrow(cf_games$`1901_08_TeamLK_TeamB`)){
  if(cf_games$`1901_08_TeamLK_TeamB`$play_id[n]==cf_games$`1901_08_TeamLK_TeamB`$play_id[n-1]){
    cf_games$`1901_08_TeamLK_TeamB`$distance[n] <- ((cf_games$`1901_08_TeamLK_TeamB`$field_x[n] - cf_games$`1901_08_TeamLK_TeamB`$field_x[n-1])^2 + (cf_games$`1901_08_TeamLK_TeamB`$field_y[n] - cf_games$`1901_08_TeamLK_TeamB`$field_y[n-1])^2)
    cf_games$`1901_08_TeamLK_TeamB`$time[n] <- cf_games$`1901_08_TeamLK_TeamB`$timestamp[n] - cf_games$`1901_08_TeamLK_TeamB`$timestamp[n-1]
  }else{
    cf_games$`1901_08_TeamLK_TeamB`$distance[n] <- 0
    cf_games$`1901_08_TeamLK_TeamB`$time[n] <- 0
  } 
}  
#3
for (n in 2:nrow(cf_games$`1902_18_TeamMB_TeamA1`)){
  if(cf_games$`1902_18_TeamMB_TeamA1`$play_id[n]==cf_games$`1902_18_TeamMB_TeamA1`$play_id[n-1]){
    cf_games$`1902_18_TeamMB_TeamA1`$distance[n] <- ((cf_games$`1902_18_TeamMB_TeamA1`$field_x[n] - cf_games$`1902_18_TeamMB_TeamA1`$field_x[n-1])^2 + (cf_games$`1902_18_TeamMB_TeamA1`$field_y[n] - cf_games$`1902_18_TeamMB_TeamA1`$field_y[n-1])^2)
    cf_games$`1902_18_TeamMB_TeamA1`$time[n] <- cf_games$`1902_18_TeamMB_TeamA1`$timestamp[n] - cf_games$`1902_18_TeamMB_TeamA1`$timestamp[n-1]
  }else{
    cf_games$`1902_18_TeamMB_TeamA1`$distance[n] <- 0
    cf_games$`1902_18_TeamMB_TeamA1`$time[n] <- 0
  } 
} 
#4
for (n in 2:nrow(cf_games$`1900_07_TeamKL_TeamB`)){
  if(cf_games$`1900_07_TeamKL_TeamB`$play_id[n]==cf_games$`1900_07_TeamKL_TeamB`$play_id[n-1]){
    cf_games$`1900_07_TeamKL_TeamB`$distance[n] <- ((cf_games$`1900_07_TeamKL_TeamB`$field_x[n] - cf_games$`1900_07_TeamKL_TeamB`$field_x[n-1])^2 + (cf_games$`1900_07_TeamKL_TeamB`$field_y[n] - cf_games$`1900_07_TeamKL_TeamB`$field_y[n-1])^2)
    cf_games$`1900_07_TeamKL_TeamB`$time[n] <- cf_games$`1900_07_TeamKL_TeamB`$timestamp[n] - cf_games$`1900_07_TeamKL_TeamB`$timestamp[n-1]
  }else{
    cf_games$`1900_07_TeamKL_TeamB`$distance[n] <- 0
    cf_games$`1900_07_TeamKL_TeamB`$time[n] <- 0
  } 
}
#5
for (n in 2:nrow(cf_games$`1903_15_TeamNG_TeamA3`)){
  if(cf_games$`1903_15_TeamNG_TeamA3`$play_id[n]==cf_games$`1903_15_TeamNG_TeamA3`$play_id[n-1]){
    cf_games$`1903_15_TeamNG_TeamA3`$distance[n] <- ((cf_games$`1903_15_TeamNG_TeamA3`$field_x[n] - cf_games$`1903_15_TeamNG_TeamA3`$field_x[n-1])^2 + (cf_games$`1903_15_TeamNG_TeamA3`$field_y[n] - cf_games$`1903_15_TeamNG_TeamA3`$field_y[n-1])^2)
    cf_games$`1903_15_TeamNG_TeamA3`$time[n] <- cf_games$`1903_15_TeamNG_TeamA3`$timestamp[n] - cf_games$`1903_15_TeamNG_TeamA3`$timestamp[n-1]
  }else{
    cf_games$`1903_15_TeamNG_TeamA3`$distance[n] <- 0
    cf_games$`1903_15_TeamNG_TeamA3`$time[n] <- 0
  } 
} 
#6
for (n in 2:nrow(cf_games$`1903_11_TeamNC_TeamA1`)){
  if(cf_games$`1903_11_TeamNC_TeamA1`$play_id[n]==cf_games$`1903_11_TeamNC_TeamA1`$play_id[n-1]){
    cf_games$`1903_11_TeamNC_TeamA1`$distance[n] <- ((cf_games$`1903_11_TeamNC_TeamA1`$field_x[n] - cf_games$`1903_11_TeamNC_TeamA1`$field_x[n-1])^2 + (cf_games$`1903_11_TeamNC_TeamA1`$field_y[n] - cf_games$`1903_11_TeamNC_TeamA1`$field_y[n-1])^2)
    cf_games$`1903_11_TeamNC_TeamA1`$time[n] <- cf_games$`1903_11_TeamNC_TeamA1`$timestamp[n] - cf_games$`1903_11_TeamNC_TeamA1`$timestamp[n-1]
  }else{
    cf_games$`1903_11_TeamNC_TeamA1`$distance[n] <- 0
    cf_games$`1903_11_TeamNC_TeamA1`$time[n] <- 0
  } 
}
#7
for (n in 2:nrow(cf_games$`1903_06_TeamND_TeamA2`)){
  if(cf_games$`1903_06_TeamND_TeamA2`$play_id[n]==cf_games$`1903_06_TeamND_TeamA2`$play_id[n-1]){
    cf_games$`1903_06_TeamND_TeamA2`$distance[n] <- ((cf_games$`1903_06_TeamND_TeamA2`$field_x[n] - cf_games$`1903_06_TeamND_TeamA2`$field_x[n-1])^2 + (cf_games$`1903_06_TeamND_TeamA2`$field_y[n] - cf_games$`1903_06_TeamND_TeamA2`$field_y[n-1])^2)
    cf_games$`1903_06_TeamND_TeamA2`$time[n] <- cf_games$`1903_06_TeamND_TeamA2`$timestamp[n] - cf_games$`1903_06_TeamND_TeamA2`$timestamp[n-1]
  }else{
    cf_games$`1903_06_TeamND_TeamA2`$distance[n] <- 0
    cf_games$`1903_06_TeamND_TeamA2`$time[n] <- 0
  } 
} 
#8
for (n in 2:nrow(cf_games$`1902_13_TeamMK_TeamB`)){
  if(cf_games$`1902_13_TeamMK_TeamB`$play_id[n]==cf_games$`1902_13_TeamMK_TeamB`$play_id[n-1]){
    cf_games$`1902_13_TeamMK_TeamB`$distance[n] <- ((cf_games$`1902_13_TeamMK_TeamB`$field_x[n] - cf_games$`1902_13_TeamMK_TeamB`$field_x[n-1])^2 + (cf_games$`1902_13_TeamMK_TeamB`$field_y[n] - cf_games$`1902_13_TeamMK_TeamB`$field_y[n-1])^2)
    cf_games$`1902_13_TeamMK_TeamB`$time[n] <- cf_games$`1902_13_TeamMK_TeamB`$timestamp[n] - cf_games$`1902_13_TeamMK_TeamB`$timestamp[n-1]
  }else{
    cf_games$`1902_13_TeamMK_TeamB`$distance[n] <- 0
    cf_games$`1902_13_TeamMK_TeamB`$time[n] <- 0
  } 
} 
#9
for (n in 2:nrow(cf_games$`1901_04_TeamLI_TeamA3`)){
  if(cf_games$`1901_04_TeamLI_TeamA3`$play_id[n]==cf_games$`1901_04_TeamLI_TeamA3`$play_id[n-1]){
    cf_games$`1901_04_TeamLI_TeamA3`$distance[n] <- ((cf_games$`1901_04_TeamLI_TeamA3`$field_x[n] - cf_games$`1901_04_TeamLI_TeamA3`$field_x[n-1])^2 + (cf_games$`1901_04_TeamLI_TeamA3`$field_y[n] - cf_games$`1901_04_TeamLI_TeamA3`$field_y[n-1])^2)
    cf_games$`1901_04_TeamLI_TeamA3`$time[n] <- cf_games$`1901_04_TeamLI_TeamA3`$timestamp[n] - cf_games$`1901_04_TeamLI_TeamA3`$timestamp[n-1]
  }else{
    cf_games$`1901_04_TeamLI_TeamA3`$distance[n] <- 0
    cf_games$`1901_04_TeamLI_TeamA3`$time[n] <- 0
  } 
} 
#10
for (n in 2:nrow(cf_games$`1902_24_TeamMA_TeamA1`)){
  if(cf_games$`1902_24_TeamMA_TeamA1`$play_id[n]==cf_games$`1902_24_TeamMA_TeamA1`$play_id[n-1]){
    cf_games$`1902_24_TeamMA_TeamA1`$distance[n] <- ((cf_games$`1902_24_TeamMA_TeamA1`$field_x[n] - cf_games$`1902_24_TeamMA_TeamA1`$field_x[n-1])^2 + (cf_games$`1902_24_TeamMA_TeamA1`$field_y[n] - cf_games$`1902_24_TeamMA_TeamA1`$field_y[n-1])^2)
    cf_games$`1902_24_TeamMA_TeamA1`$time[n] <- cf_games$`1902_24_TeamMA_TeamA1`$timestamp[n] - cf_games$`1902_24_TeamMA_TeamA1`$timestamp[n-1]
  }else{
    cf_games$`1902_24_TeamMA_TeamA1`$distance[n] <- 0
    cf_games$`1902_24_TeamMA_TeamA1`$time[n] <- 0
  } 
}
#Compiling
cfg1 <- cf_games$`1902_11_TeamMI_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg2 <- cf_games$`1901_08_TeamLK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg3 <- cf_games$`1902_18_TeamMB_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg4 <- cf_games$`1900_07_TeamKL_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg5 <- cf_games$`1903_15_TeamNG_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg6 <- cf_games$`1903_11_TeamNC_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg7 <- cf_games$`1903_06_TeamND_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg8 <- cf_games$`1902_13_TeamMK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg9 <- cf_games$`1901_04_TeamLI_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cfg10 <- cf_games$`1902_24_TeamMA_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
cf_distances <- c(cfg1$distance,
                  cfg2$distance,
                  cfg3$distance,
                  cfg4$distance,
                  cfg5$distance,
                  cfg6$distance,
                  cfg7$distance,
                  cfg8$distance,
                  cfg9$distance,
                  cfg10$distance)
cf_times <- c(cfg1$time,
              cfg2$time,
              cfg3$time,
              cfg4$time,
              cfg5$time,
              cfg6$time,
              cfg7$time,
              cfg8$time,
              cfg9$time,
              cfg10$time)
cf_movement <- as.data.frame(cbind(cf_distances,cf_times))
cf_movement <- rename(cf_movement,distance=cf_distances,time=cf_times)
write.csv(cf_movement,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/cf_movement.csv')
# Right Field -------------------------------------------------------------
rf_total <- player_pos %>% filter(player_position==9)
rf_total$distance <- 0
rf_total$time <- 0
x <- split(rf_total,rf_total$game_str)
games_index_rf <- c(66,75,48,39,5,45,28,55,16,83)
rf_games <- x[games_index_rf]
names(rf_games)
#1
for (n in 2:nrow(rf_games$`1903_05_TeamND_TeamA2`)){
  if(rf_games$`1903_05_TeamND_TeamA2`$play_id[n]==rf_games$`1903_05_TeamND_TeamA2`$play_id[n-1]){
    rf_games$`1903_05_TeamND_TeamA2`$distance[n] <- ((rf_games$`1903_05_TeamND_TeamA2`$field_x[n] - rf_games$`1903_05_TeamND_TeamA2`$field_x[n-1])^2 + (rf_games$`1903_05_TeamND_TeamA2`$field_y[n] - rf_games$`1903_05_TeamND_TeamA2`$field_y[n-1])^2)
    rf_games$`1903_05_TeamND_TeamA2`$time[n] <- rf_games$`1903_05_TeamND_TeamA2`$timestamp[n] - rf_games$`1903_05_TeamND_TeamA2`$timestamp[n-1]
  }else{
    rf_games$`1903_05_TeamND_TeamA2`$distance[n] <- 0
    rf_games$`1903_05_TeamND_TeamA2`$time[n] <- 0
  } 
}  
#2
for (n in 2:nrow(rf_games$`1903_14_TeamNG_TeamA3`)){
  if(rf_games$`1903_14_TeamNG_TeamA3`$play_id[n]==rf_games$`1903_14_TeamNG_TeamA3`$play_id[n-1]){
    rf_games$`1903_14_TeamNG_TeamA3`$distance[n] <- ((rf_games$`1903_14_TeamNG_TeamA3`$field_x[n] - rf_games$`1903_14_TeamNG_TeamA3`$field_x[n-1])^2 + (rf_games$`1903_14_TeamNG_TeamA3`$field_y[n] - rf_games$`1903_14_TeamNG_TeamA3`$field_y[n-1])^2)
    rf_games$`1903_14_TeamNG_TeamA3`$time[n] <- rf_games$`1903_14_TeamNG_TeamA3`$timestamp[n] - rf_games$`1903_14_TeamNG_TeamA3`$timestamp[n-1]
  }else{
    rf_games$`1903_14_TeamNG_TeamA3`$distance[n] <- 0
    rf_games$`1903_14_TeamNG_TeamA3`$time[n] <- 0
  } 
}  
#3
for (n in 2:nrow(rf_games$`1902_19_TeamME_TeamA2`)){
  if(rf_games$`1902_19_TeamME_TeamA2`$play_id[n]==rf_games$`1902_19_TeamME_TeamA2`$play_id[n-1]){
    rf_games$`1902_19_TeamME_TeamA2`$distance[n] <- ((rf_games$`1902_19_TeamME_TeamA2`$field_x[n] - rf_games$`1902_19_TeamME_TeamA2`$field_x[n-1])^2 + (rf_games$`1902_19_TeamME_TeamA2`$field_y[n] - rf_games$`1902_19_TeamME_TeamA2`$field_y[n-1])^2)
    rf_games$`1902_19_TeamME_TeamA2`$time[n] <- rf_games$`1902_19_TeamME_TeamA2`$timestamp[n] - rf_games$`1902_19_TeamME_TeamA2`$timestamp[n-1]
  }else{
    rf_games$`1902_19_TeamME_TeamA2`$distance[n] <- 0
    rf_games$`1902_19_TeamME_TeamA2`$time[n] <- 0
  } 
} 
#4
for (n in 2:nrow(rf_games$`1902_12_TeamMI_TeamA3`)){
  if(rf_games$`1902_12_TeamMI_TeamA3`$play_id[n]==rf_games$`1902_12_TeamMI_TeamA3`$play_id[n-1]){
    rf_games$`1902_12_TeamMI_TeamA3`$distance[n] <- ((rf_games$`1902_12_TeamMI_TeamA3`$field_x[n] - rf_games$`1902_12_TeamMI_TeamA3`$field_x[n-1])^2 + (rf_games$`1902_12_TeamMI_TeamA3`$field_y[n] - rf_games$`1902_12_TeamMI_TeamA3`$field_y[n-1])^2)
    rf_games$`1902_12_TeamMI_TeamA3`$time[n] <- rf_games$`1902_12_TeamMI_TeamA3`$timestamp[n] - rf_games$`1902_12_TeamMI_TeamA3`$timestamp[n-1]
  }else{
    rf_games$`1902_12_TeamMI_TeamA3`$distance[n] <- 0
    rf_games$`1902_12_TeamMI_TeamA3`$time[n] <- 0
  } 
}
#5
for (n in 2:nrow(rf_games$`1900_05_TeamKK_TeamB`)){
  if(rf_games$`1900_05_TeamKK_TeamB`$play_id[n]==rf_games$`1900_05_TeamKK_TeamB`$play_id[n-1]){
    rf_games$`1900_05_TeamKK_TeamB`$distance[n] <- ((rf_games$`1900_05_TeamKK_TeamB`$field_x[n] - rf_games$`1900_05_TeamKK_TeamB`$field_x[n-1])^2 + (rf_games$`1900_05_TeamKK_TeamB`$field_y[n] - rf_games$`1900_05_TeamKK_TeamB`$field_y[n-1])^2)
    rf_games$`1900_05_TeamKK_TeamB`$time[n] <- rf_games$`1900_05_TeamKK_TeamB`$timestamp[n] - rf_games$`1900_05_TeamKK_TeamB`$timestamp[n-1]
  }else{
    rf_games$`1900_05_TeamKK_TeamB`$distance[n] <- 0
    rf_games$`1900_05_TeamKK_TeamB`$time[n] <- 0
  } 
} 
#6
for (n in 2:nrow(rf_games$`1902_16_TeamMD_TeamA2`)){
  if(rf_games$`1902_16_TeamMD_TeamA2`$play_id[n]==rf_games$`1902_16_TeamMD_TeamA2`$play_id[n-1]){
    rf_games$`1902_16_TeamMD_TeamA2`$distance[n] <- ((rf_games$`1902_16_TeamMD_TeamA2`$field_x[n] - rf_games$`1902_16_TeamMD_TeamA2`$field_x[n-1])^2 + (rf_games$`1902_16_TeamMD_TeamA2`$field_y[n] - rf_games$`1902_16_TeamMD_TeamA2`$field_y[n-1])^2)
    rf_games$`1902_16_TeamMD_TeamA2`$time[n] <- rf_games$`1902_16_TeamMD_TeamA2`$timestamp[n] - rf_games$`1902_16_TeamMD_TeamA2`$timestamp[n-1]
  }else{
    rf_games$`1902_16_TeamMD_TeamA2`$distance[n] <- 0
    rf_games$`1902_16_TeamMD_TeamA2`$time[n] <- 0
  } 
}
#7
for (n in 2:nrow(rf_games$`1902_01_TeamMG_TeamA3`)){
  if(rf_games$`1902_01_TeamMG_TeamA3`$play_id[n]==rf_games$`1902_01_TeamMG_TeamA3`$play_id[n-1]){
    rf_games$`1902_01_TeamMG_TeamA3`$distance[n] <- ((rf_games$`1902_01_TeamMG_TeamA3`$field_x[n] - rf_games$`1902_01_TeamMG_TeamA3`$field_x[n-1])^2 + (rf_games$`1902_01_TeamMG_TeamA3`$field_y[n] - rf_games$`1902_01_TeamMG_TeamA3`$field_y[n-1])^2)
    rf_games$`1902_01_TeamMG_TeamA3`$time[n] <- rf_games$`1902_01_TeamMG_TeamA3`$timestamp[n] - rf_games$`1902_01_TeamMG_TeamA3`$timestamp[n-1]
  }else{
    rf_games$`1902_01_TeamMG_TeamA3`$distance[n] <- 0
    rf_games$`1902_01_TeamMG_TeamA3`$time[n] <- 0
  } 
} 
#8
for (n in 2:nrow(rf_games$`1902_26_TeamMC_TeamA1`)){
  if(rf_games$`1902_26_TeamMC_TeamA1`$play_id[n]==rf_games$`1902_26_TeamMC_TeamA1`$play_id[n-1]){
    rf_games$`1902_26_TeamMC_TeamA1`$distance[n] <- ((rf_games$`1902_26_TeamMC_TeamA1`$field_x[n] - rf_games$`1902_26_TeamMC_TeamA1`$field_x[n-1])^2 + (rf_games$`1902_26_TeamMC_TeamA1`$field_y[n] - rf_games$`1902_26_TeamMC_TeamA1`$field_y[n-1])^2)
    rf_games$`1902_26_TeamMC_TeamA1`$time[n] <- rf_games$`1902_26_TeamMC_TeamA1`$timestamp[n] - rf_games$`1902_26_TeamMC_TeamA1`$timestamp[n-1]
  }else{
    rf_games$`1902_26_TeamMC_TeamA1`$distance[n] <- 0
    rf_games$`1902_26_TeamMC_TeamA1`$time[n] <- 0
  } 
} 
#9
for (n in 2:nrow(rf_games$`1901_07_TeamLK_TeamB`)){
  if(rf_games$`1901_07_TeamLK_TeamB`$play_id[n]==rf_games$`1901_07_TeamLK_TeamB`$play_id[n-1]){
    rf_games$`1901_07_TeamLK_TeamB`$distance[n] <- ((rf_games$`1901_07_TeamLK_TeamB`$field_x[n] - rf_games$`1901_07_TeamLK_TeamB`$field_x[n-1])^2 + (rf_games$`1901_07_TeamLK_TeamB`$field_y[n] - rf_games$`1901_07_TeamLK_TeamB`$field_y[n-1])^2)
    rf_games$`1901_07_TeamLK_TeamB`$time[n] <- rf_games$`1901_07_TeamLK_TeamB`$timestamp[n] - rf_games$`1901_07_TeamLK_TeamB`$timestamp[n-1]
  }else{
    rf_games$`1901_07_TeamLK_TeamB`$distance[n] <- 0
    rf_games$`1901_07_TeamLK_TeamB`$time[n] <- 0
  } 
} 
#10
for (n in 2:nrow(rf_games$`1903_22_TeamNA_TeamA1`)){
  if(rf_games$`1903_22_TeamNA_TeamA1`$play_id[n]==rf_games$`1903_22_TeamNA_TeamA1`$play_id[n-1]){
    rf_games$`1903_22_TeamNA_TeamA1`$distance[n] <- ((rf_games$`1903_22_TeamNA_TeamA1`$field_x[n] - rf_games$`1903_22_TeamNA_TeamA1`$field_x[n-1])^2 + (rf_games$`1903_22_TeamNA_TeamA1`$field_y[n] - rf_games$`1903_22_TeamNA_TeamA1`$field_y[n-1])^2)
    rf_games$`1903_22_TeamNA_TeamA1`$time[n] <- rf_games$`1903_22_TeamNA_TeamA1`$timestamp[n] - rf_games$`1903_22_TeamNA_TeamA1`$timestamp[n-1]
  }else{
    rf_games$`1903_22_TeamNA_TeamA1`$distance[n] <- 0
    rf_games$`1903_22_TeamNA_TeamA1`$time[n] <- 0
  } 
}
#Compiling
rfg1 <- rf_games$`1903_05_TeamND_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg2 <- rf_games$`1903_14_TeamNG_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg3 <- rf_games$`1902_19_TeamME_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg4 <- rf_games$`1902_12_TeamMI_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg5 <- rf_games$`1900_05_TeamKK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg6 <- rf_games$`1902_16_TeamMD_TeamA2` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg7 <- rf_games$`1902_01_TeamMG_TeamA3` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg8 <- rf_games$`1902_26_TeamMC_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg9 <- rf_games$`1901_07_TeamLK_TeamB` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rfg10 <- rf_games$`1903_22_TeamNA_TeamA1` %>% group_by(play_id) %>% summarise(distance=sum(distance),time=sum(time))
rf_distances <- c(rfg1$distance,
                  rfg2$distance,
                  rfg3$distance,
                  rfg4$distance,
                  rfg5$distance,
                  rfg6$distance,
                  rfg7$distance,
                  rfg8$distance,
                  rfg9$distance,
                  rfg10$distance)
rf_times <- c(rfg1$time,
              rfg2$time,
              rfg3$time,
              rfg4$time,
              rfg5$time,
              rfg6$time,
              rfg7$time,
              rfg8$time,
              rfg9$time,
              rfg10$time)
rf_movement <- as.data.frame(cbind(rf_distances,rf_times))
rf_movement <- rename(rf_movement,distance=rf_distances,time=rf_times)
write.csv(rf_movement,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/rf_movement.csv')



# Analysis ----------------------------------------------------------------
setwd('/Users/mileskee7/Desktop/SMT-Data-Challenge 3/')
b1 <- read.csv('b1_movement.csv')
b2 <- read.csv('b2_movement.csv')
b3 <- read.csv('b3_movement.csv')
ss <- read.csv('ss_movement.csv')
lf <- read.csv('lf_movement.csv')
cf <- read.csv('cf_movement.csv')
rf <- read.csv('rf_movement.csv')
b1$id <- 'b1'
b2$id <- 'b2'
b3$id <- 'b3'
ss$id <- 'ss'
lf$id <- 'lf'
cf$id <- 'cf'
rf$id <- 'rf'
b1mean <- mean(b1$distance)
b2mean <- mean(b2$distance)
b3mean <- mean(b3$distance)
ssmean <- mean(ss$distance)
lfmean <- mean(lf$distance)
cfmean <- mean(cf$distance)
rfmean <- mean(rf$distance)
b1sd <- sd(b1$distance)
b2sd <- sd(b2$distance)
b3sd <- sd(b3$distance)
sssd <- sd(ss$distance)
lfsd <- sd(lf$distance)
cfsd <- sd(cf$distance)
rfsd <- sd(rf$distance)
means <- rbind(b1mean,b2mean,b3mean,ssmean,lfmean,cfmean,rfmean)
sds <- rbind(b1sd,b2sd,b3sd,sssd,lfsd,cfsd,rfsd)
stddevs <- as.data.frame(cbind(c('b1','b2','b3','ss','lf','cf','rf'),sds))
stddevs <- rename(stddevs,'Position'=V1,'StdDev'=V2)
stddevs$StdDev <- as.numeric(stddevs$StdDev)
means <- as.data.frame(cbind(c('b1','b2','b3','ss','lf','cf','rf'),means))
means <- rename(means,'Position'=V1,'Mean'=V2)
means$Mean <- as.numeric(means$Mean)
all_dist <- rbind(b1,b2,b3,ss,lf,cf,rf)
#All positions distance moved
ggplot(all_dist,aes(x=distance)) + geom_histogram(binwidth=5) + xlim(0,200) + ylim(0,1250) + ggtitle('All Positions Distance Moved') + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 
#Each position boxplot distance moved
ggplot(all_dist,aes(x=distance,y=id)) + geom_boxplot() + xlim(0,300) + ylab('Position') + theme_bw() + ggtitle("Boxplot of each position's distance") + theme(plot.title = element_text(hjust = 0.5))
#Mean and Std dev of each position
order <- c('b1','b2','b3','ss','lf','cf','rf')
ggplot(means,aes(x=factor(Position,levels=order),y=Mean)) + geom_point() + theme_bw() + xlab('Position') + ggtitle("Mean of each position's distance") + theme(plot.title = element_text(hjust=.5))
ggplot(stddevs,aes(x=factor(Position,levels=order),y=StdDev)) + geom_point() + theme_bw() + xlab('Position') + ggtitle("Std Dev of each position's distance") + theme(plot.title = element_text(hjust=.5))
sum(b3$distance == 0)
sum(b1$distance == 0)
#MLB Application
defense <- read.csv('/Users/mileskee7/Desktop/SMT-Data-Challenge 3/Defensive Metrics.csv')
defense <- defense %>% select('Name','Pos','DRS','OAA') %>% filter(Pos != 'C')
inf <- defense %>% filter(str_detect(Pos, '1B|2B|3B|SS'))
of <- defense %>% filter(str_detect(Pos, 'LF|CF|RF'))
inf$rank <- rank(desc(inf$DRS))
of$rank <- rank(desc(of$OAA))      
ifmean <- mean(rbind(b1$distance,b2$distance,b3$distance,ss$distance))
b1adj <- b1mean / ifmean
b2adj <- b2mean / ifmean
b3adj <- b3mean / ifmean
ssadj <- ssmean / ifmean
inf$newdrs <- 0
for (n in 1:nrow(inf)){
  if (inf$Pos[n] == '1B'){
    if(inf$DRS[n] > 0){
      inf$newdrs[n] <- inf$DRS[n] * b1adj
    }else if(inf$DRS[n] < 0){
      inf$newdrs[n] <- inf$DRS[n] / b1adj
    }
  }
  else if(inf$Pos[n] == '2B'){
    if(inf$DRS[n] > 0){
      inf$newdrs[n] <- inf$DRS[n] * b2adj
    }else if(inf$DRS[n] < 0){
      inf$newdrs[n] <- inf$DRS[n] / b2adj
    }
  }
  else if(inf$Pos[n] == '3B'){
    if(inf$DRS[n] > 0){
      inf$newdrs[n] <- inf$DRS[n] * b3adj
    }else if(inf$DRS[n] < 0){
      inf$newdrs[n] <- inf$DRS[n] / b3adj
    }
  }
  else if(inf$Pos[n] == 'SS'){
    if(inf$DRS[n] > 0){
      inf$newdrs[n] <- inf$DRS[n] * ssadj
    }else if(inf$DRS[n] < 0){
      inf$newdrs[n] <- inf$DRS[n] / ssadj
    }
  }
}  
inf$newrank <-rank(desc(inf$newdrs))
inf$diff <- inf$rank - inf$newrank

ofmean <- mean(rbind(cf$distance,rf$distance,lf$distance))

cfadj <- cfmean / ofmean
rfadj <- rfmean / ofmean
lfadj <- lfmean / ofmean

of$newoaa <- 0
for (n in 1:nrow(of)){
  if (of$Pos[n] == 'CF'){
    if(of$OAA[n] > 0){
      of$newoaa[n] <- of$OAA[n] * cfadj
    }else if(of$OAA[n] < 0){
      of$newoaa[n] <- of$OAA[n] / cfadj
    }
  }
  else if (of$Pos[n] == 'LF'){
    if(of$OAA[n] > 0){
      of$newoaa[n] <- of$OAA[n] * lfadj
    }else if(of$OAA[n] < 0){
      of$newoaa[n] <- of$OAA[n] / lfadj
    }
  }
  else if (of$Pos[n] == 'RF'){
    if(of$OAA[n] > 0){
      of$newoaa[n] <- of$OAA[n] * rfadj
    }else if(of$OAA[n] < 0){
      of$newoaa[n] <- of$OAA[n] / rfadj
    }
  }
}  
of$newrank <- rank(desc(of$newoaa))
of$diff <- of$rank - of$newrank  

ggplot(inf %>% group_by(Pos) %>% summarise(avg_move = mean(diff)),aes(x=Pos,y=avg_move)) + geom_point() + ggtitle('Average Rank Difference by Infield Position') + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 
ggplot(of %>% group_by(Pos) %>% summarise(avg_move = mean(diff)),aes(x=Pos,y=avg_move)) + geom_point() + ggtitle('Average Rank Difference by Outfield Position') + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

b1total <- cbind(b1mean,b1sd,b1adj)
b2total <- cbind(b2mean,b2sd,b2adj)
b3total <- cbind(b3mean,b3sd,b3adj)
sstotal <- cbind(ssmean,sssd,ssadj)
lftotal <- cbind(lfmean,lfsd,lfadj)
cftotal <- cbind(cfmean,cfsd,cfadj)
rftotal <- cbind(rfmean,rfsd,rfadj)
export <- as.data.frame(rbind(b1total,b2total,b3total,sstotal,lftotal,cftotal,rftotal))
export <- export %>% rename('Mean'='b1mean','Standard Deviation'='b1sd','Adjustment'='b1adj')
rownames(export) <- c('1B','2B','3B','SS','LF','CF','RF')
write.table(export,'/Users/mileskee7/Desktop/SMT-Data-Challenge 3/export.txt')
