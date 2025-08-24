### Report Rendering

library(readr)

#-------------------------------SINGLE REPORT----------------------------------#


# CHANGE THESE VALUES

file = "07_28_2025 6_28_53 PM-Lafayette Aviators 2025@Normal Cornbelters.csv"
home = "NOR"
away = "LAF"
month = "07"
day = "28"
league = "KCL"


# RUN THIS TO RENDER
# OUTPUT: Ump_Reports/output
rmarkdown::render(
  "updated_umpire_report.Rmd",
  params = list(
    game_data = file,
    home_team = home,
    away_team = away,
    game_date = paste0(month, "/", day),
    league = league
  ),
  output_file = paste0(
    "output/",  month, "_", day, "_", home, "_vs_", away, "_Report.pdf"
  )
)



#----------------------------FULL SEASON REPORTS-------------------------------#


kcl_files <- list.files(here("kclData/"), pattern = "\\.csv$", full.names = T)
kcl_data <- bind_rows(lapply(kcl_files, read_csv))

for (i in kcl_files){
  df <- read_csv(i, n_max = 1)
  month <- substr(df$Date, 1,2)
  day <- substr(df$Date, 4, 5)
  print(gsub("/", "_", df$Date))
  home <- tolower(df$HomeTeam)
  away <- tolower(df$AwayTeam)
  home <- switch(home,
                 "kcl merchants 2025" = "Merchants",
                 "kcl bobcats 2025" = "Bobcats",
                 "kcl groundsloths 2025" = "Groundsloths",
                 "kcl bluecaps 2025" = "Bluecaps",
                 "No team found")
  away <- switch(away,
                 "kcl merchants 2025" = "Merchants",
                 "kcl bobcats 2025" = "Bobcats",
                 "kcl groundsloths 2025" = "Groundsloths",
                 "kcl bluecaps 2025" = "Bluecaps",
                 "No team found")
  rmarkdown::render(
    "updated_umpire_report.Rmd",
    params = list(
      game_data = basename(i),
      home_team = home,
      away_team = away,
      game_date = df$Date,
      league = "KCL"
    ),
    output_file = paste0(
      "Season Reports/", gsub("/", "_", df$Date), "_", away, "_vs_",
      home, "_ump_report.pdf"
    )
  )
}

for (i in kcl_files){
  list <- append(list, i)
}
