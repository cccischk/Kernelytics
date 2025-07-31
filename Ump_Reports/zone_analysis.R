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


# Full season strike zone exploration

ggplot(df, aes(x=PlateLocSide, y = PlateLocHeight)) +
  geom_circle(aes(x0 = PlateLocSide, y0 = PlateLocHeight, r = 0.1208,
                  fill = PitchCall),
              color = NA, alpha = 0.6) + 
  coord_fixed() + 
  scale_x_continuous(limits = c(-2.5,2.5), expand = c(0,0)) +
  scale_y_continuous(limits = c(1,4), expand = c(0,0)) + 
  scale_fill_manual(
    values = c("BallCalled" = "red", "StrikeCalled" = "green"),
    #labels = c("Called Strike", "Called Ball")) +
  # scale_color_manual(
  #   values = c("BallCalled" = "red", "StrikeCalled" = "green"),
  #   labels = c("Called Strike", "Called Ball")) +
  ) + 
  labs(
    x = "Horizontal Pitch Location (ft)",
    y = "Vertical Plate Location (ft)",
    color = "Pitch Call"
  ) +
  theme_void() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linetype = "dashed") +
  theme(plot.margin = margin(0,0,0,0))


# Cut the zone into bins and calculate strike probability
bin_size = 0.2
strike_probs <- df %>%
  mutate(
    x_bin = floor(PlateLocSide / bin_size) * bin_size,
    y_bin = floor(PlateLocHeight / bin_size) * bin_size
  ) %>%
  group_by(x_bin, y_bin) %>%
  summarise(
    strikes = sum(PitchCall == "StrikeCalled"),
    total = n(),
    strike_prob = str ikes / total
  ) %>%
  ungroup()

# Strike probability plot
ggplot(strike_probs, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
    ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void()

# 