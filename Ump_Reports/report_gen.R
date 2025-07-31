### Report Rendering

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



