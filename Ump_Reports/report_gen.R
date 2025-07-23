### Report Rendering

# CHANGE THESE VALUES

file = "07_19_Championship_Merchants_vs_Bobcats.csv"
home = "Bobcats"
away = "Merchants"
month = "07"
day = "19"
league = "KCL"


# RUN THIS TO RENDER
# OUTPUT: Ump_Reports/output
rmarkdown::render(
  "umpire_report.Rmd",
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



