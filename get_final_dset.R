### The purpose of this script is to filter the data to get a final data document
# The original data document is too big (700 MB), so I'm going to try to only keep rows and columns of interest

library (readr)
library(tidyverse)

nfl_data <- read_csv("NFL Play by Play 2009-2018 (v5).csv")


# I want only the data involving the Vikings to keep this small
vikings_data <- nfl_data |>
  filter(home_team == "MIN" | away_team=="MIN")

write_csv(vikings_data,"final_vikings_data.csv")
