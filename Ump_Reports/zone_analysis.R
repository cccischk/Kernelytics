library(tidyverse)
library(here)
library(akima)
library(ggplot2)
library(sp)
library(sf)
library(isoband)

### Umpire Research

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

# test_df <-read_csv("kclData/7_2 GroundSloths @ BlueCaps.csv")
# 
# 
# test_df_exp <- expected_zone(test_df, interp)


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
    date = df$Date[1],
    accuracy = accuracy,
    iz_acc = iz_acc,
    oz_acc = oz_acc
  ))
}


# Obtain full-season accuracy values
accs <- lapply(game_list, accuracies)
overall_acc <- map_dbl(accs, "accuracy")
iz_acc <- map_dbl(accs, "iz_acc")
oz_acc <- map_dbl(accs, "oz_acc")
dates <- map_chr(accs, "date")

# Plot full-season accuracy distributions
boxplot(overall_acc, iz_acc, oz_acc,
        horizontal = T, names = c("Overall", "In-Zone", "Out-of-Zone"),
        main = "KCL Umpire Accuracy Distributions")

#TODO Find best and worst games
acc_df <- data.frame(dates, overall_acc)
best_dates <- acc_df[order(-acc_df$overall_acc), ]
worst_dates <- acc_df[order(acc_df$overall_acc), ]


#---------------------------KCL Expected Zone----------------------------------#
bin_size = 0.2
# Cut the zone into bins and calculate strike probability
strike_probabilities <- function(df, bin_size, min = 0){
  strike_probs <- df %>%
    mutate(
      x_bin = round(PlateLocSide / bin_size) * bin_size,
      y_bin = round(PlateLocHeight / bin_size) * bin_size
    ) %>%
    group_by(x_bin, y_bin) %>%
    summarise(
      strikes = sum(PitchCall == "StrikeCalled"),
      total = n(),
      strike_prob = strikes / total
    ) %>%
    ungroup() %>%
    filter(total > min)
  return(strike_probs)
}


# Grid data for expected strike zone contour (for plotting)

interpol <- function(strike_probs){
  ret <- with(strike_probs, akima::interp(
    x = x_bin,
    y = y_bin,
    z = strike_prob,
    xo = seq(min(x_bin), max(x_bin), length = 100),
    yo = seq(min(y_bin), max(y_bin), length = 100),
    extrap = FALSE
  ))
  
  grid_df <- expand.grid(
    x = ret$x,
    y = ret$y
  )
  
  grid_df$strike_prob <- as.vector(ret$z)
  return(list(
    interp = ret,
    grid_df = grid_df
  ))
}
  


### FUNCTIONS ###


### EXPECTED CLASSIFY FUNCTION ###
# Create the zone and classify the pitches by expected call
expected_zone <- function(df, interp, level = 0.5, ball_radius_ft = 0.1208){
  zone_poly <- build_expected_zone_polygon(interp, level = level)
  
  touches <- ball_touches_zone_(
    df = df |> dply::select(PlateLocSide, PlateLocHeight),
    zone_poly = zone_poly,
    ball_radius_ft = ball_radius_ft
  )
  
  df$ExpectedCall <- ifelse(touches, "StrikeCalled", "BallCalled")
  df
}

# # Helper - translate to an object that can be used to detect pitches as inside or outside
# # of the shape.
# build_expected_zone_polygon <- function(interp, level = 0.5){
#   # Extract level curves
#   cls <- contourLines(x = interp$x, y = interp$y, z = interp$z, levels = level)
#   if (length(cls) == 0) stop("No contour found at the specified level.")
#   
#   # Make contours into an sf polygon
#   polys <- lapply(seq_along(cls), function(i){
#     xi <- cls[[i]]$x
#     yi <- cls[[i]]$y
#     # close ring
#     if (xi[1] != xi[length(xi)] || yi[1] != yi[length(yi)]){
#       xi <- c(xi, xi[1]); yi <- c(yi, yi[1])
#     }
#     st_polygon(list(cbind(xi, yi)))
#   })
#   
#   # Combine to MULTIPOLYGON
#   zone <- st_sfc(polys, crs = NA) |> st_union() |> st_make_valid()
#   return(zone)
# }
# 
# 
# 

# # Helper function - detects if ball touches zone
ball_touches_zone <- function(df, zone_poly, ball_radius_ft = 0.1208){
  # points -> sf
  pts <- st_as_sf(df, coords = c("PlateLocSide", "PlateLocHeight"), crs = NA)
  # circle for each pitch (buffer by radius)
  balls <- st_buffer(pts, dist = ball_radius_ft)
  # intersects?
  as.logical(st_intersects(balls, zone_poly, sparse = T) |> lengths())

}
# 
# # Helper function - returns total expected accuracy of df
# expected_accuracy <- function(df){
#   expected_correct <- sum(df$PitchCall == df$ExpectedCall)
#   return(expected_correct / nrow(df))
# }
# 

# Code to run

# Overall zone data
strike_probs <- strike_probabilities(df, 0.2, 5)
overall_grid <- interpol(strike_probs)
interp <- overall_grid$interp
grid_df <- overall_grid$grid_df

# Left handed hitter data
lhh <- filter(df, BatterSide == "Left")
lhh_probs <- strike_probabilities(lhh, 0.2, 1)
lhh_grid <- interpol(lhh_probs)
lhh_interp <- lhh_grid$interp
lhh_grid_df <- lhh_grid$grid_df

# Right handed hitter data
rhh <- filter(df, BatterSide == "Right")
rhh_probs <- strike_probabilities(rhh, 0.2, 1)
rhh_grid <- interpol(rhh_probs)
rhh_interp <- rhh_grid$interp
rhh_grid_df <- rhh_grid$grid_df

# Left handed pitcher data
lhp <- filter(df, PitcherThrows == "Left")
lhp_probs <- strike_probabilities(lhp, 0.2)
lhp_grid <- interpol(lhp_probs)
lhp_interp <- lhp_grid$interp
lhp_grid_df <- lhp_grid$grid_df

# Right handed pitcher data
rhp <- filter(df, PitcherThrows == "Right")
rhp_probs <- strike_probabilities(rhp, 0.2)
rhp_grid <- interpol(rhp_probs)
rhp_interp <- rhp_grid$interp
rhp_grid_df <- rhp_grid$grid_df

# Fastball data
fb <- filter(df, TaggedPitchType == "Fastball")
fb_probs <- strike_probabilities(fb, 0.2)
fb_grid <- interpol(fb_probs)
fb_interp <- fb_grid$interp
fb_grid_df <- fb_grid$grid_df

# Breaking ball data
br <- filter(df, TaggedPitchType %in% c("Curveball", "Slider"))
br_probs <- strike_probabilities(br, 0.2)
br_grid <- interpol(br_probs)
br_interp <- br_grid$interp
br_grid_df <- br_grid$grid_df


# Data with n_min filter
strike_probs_10 <- filter(strike_probs, total >= 10)
strike_probs_15 <- filter(strike_probs, total >= 15)
strike_probs_20 <- filter(strike_probs, total >= 20)

# Full season strike zone exploration

ggplot(filter(rhh, PitchCall == "StrikeCalled"), aes(x=PlateLocSide, y = PlateLocHeight)) +
  geom_circle(aes(x0 = PlateLocSide, y0 = PlateLocHeight, r = 0.1208,
                  fill = PitchCall),
              color = NA, alpha = 0.6) +
  coord_fixed() +
  scale_x_continuous(limits = c(-2.5,2.5), expand = c(0,0)) +
  scale_y_continuous(limits = c(1,4), expand = c(0,0)) +
  scale_fill_manual(
    values = c("BallCalled" = "green", "StrikeCalled" = "red"),
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









# Strike percentage plot
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
  labs(title = "Full Season Strike Percentage Plot") +
  geom_contour(
    data = grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5
    )
  ) 


# Smooth strike percentage plot
ggplot(grid_df, aes(x=x, y=y, fill = strike_prob)) +
  geom_tile(color = NA) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) +
  geom_contour(
    data = grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void()

# Strike percentage plots by batter handedness

# LHH Strike percentage plot

ggplot(lhh_probs, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) +
  geom_contour(
    data = lhh_grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5
    )
  ) +
  labs(title = "Left-Handed Hitter Strike Percentage Plot") +

# RHH Srike percentage plot
ggplot(rhh_probs, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) +
  geom_contour(
    data = rhh_grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void() +  
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5
    )
  ) +
  labs(title = "Right-Handed Hitter Strike Percentage Plot")


# LHP Strike percentage plot
ggplot(lhp_probs, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) +
  geom_contour(
    data = lhp_grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void()

# RHP Strike percentage plot
ggplot(rhp_probs, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) +
  geom_contour(
    data = rhp_grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void()


# Strike percentage plot - experimental strike zone
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
  geom_contour(
    data = grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void()


# Fastball strike percentage plot
ggplot(fb_probs, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) +
  geom_contour(
    data = fb_grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void()


# Breaking ball strike percentage plot
ggplot(br_probs, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) +
  geom_contour(
    data = br_grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void()

# Full season with pitch minimum 10
ggplot(strike_probs_10, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) + 
  labs(title = "Full Season Strike Percentage Plot (n_min = 10)") +
  geom_contour(
    data = grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5
    )
  ) 

# Full season with pitch minimum 10
ggplot(strike_probs_15, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) + 
  labs(title = "Full Season Strike Percentage Plot (n_min = 15)") +
  geom_contour(
    data = grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5
    )
  ) 

# Full season with pitch minimum 20
ggplot(strike_probs_20, aes(x=x_bin, y=y_bin, fill = strike_prob)) +
  geom_tile(color = NA, width = bin_size, height = bin_size) +
  scale_fill_gradient2(
    name = "Strike %",
    low = "blue",
    mid = "lightgrey",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)
  ) + 
  labs(title = "Full Season Strike Percentage Plot (n_min = 20)") +
  geom_contour(
    data = grid_df,
    aes(x = x, y = y, z = strike_prob),
    breaks = c(0.5),
    color = "black",
    linewidth = 0.7
  ) +
  coord_fixed(xlim = c(-2,2), ylim = c(0,5)) +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linewidth = 1.5) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5
    )
  ) 

