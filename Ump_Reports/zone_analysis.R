# Umpire Research

if(!requireNamespace("here")) install.packages("here")
library(tidyverse)
library(ggforce)

if(interactive()) {
  here::i_am("Ump_Reports/zone_analysis.R")
}

source(here("Ump_Reports", "ump_report.R"))


kcl_files <- list.files(here("kclData/"), pattern = "\\.csv$", full.names = T)
kcl_data <- bind_rows(lapply(kcl_files, read_csv))


df <- classify(kcl_data)

game_list <- split(df, df$GameID)


accuracies <- function(df){
  split_list <- splits(df)
  
  # overall accuracy
  correct <- sum(df$Accuracy == "Correct")
  total <- nrow(df)
  accuracy <- correct / total
  
  
  # in zone accuracy
  iz_correct <- sum(split_list$iz$Accuracy == "Correct")
  iz_total <- nrow(split_list$iz)
  iz_acc <- iz_correct / iz_total
  
  
  # out of zone accuracy
  oz_correct <- sum(split_list$oz$Accuracy == "Correct")
  oz_total <- nrow(split_list$oz)
  oz_acc <- oz_correct / oz_total 
  
  return(list(
    accuracy = accuracy,
    iz_acc = iz_acc,
    oz_acc = oz_acc
  ))
}


accs <- lapply(game_list, accuracies)
overall_acc <- map_dbl(accs, "accuracy")
iz_acc <- map_dbl(accs, "iz_acc")
oz_acc <- map_dbl(accs, "iz_acc")



