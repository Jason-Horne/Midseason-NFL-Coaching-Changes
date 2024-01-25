library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


key <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 1)

LAC_2023 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 2)

CAR_2023 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 3)

LV_2023 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 4)

DEN_2022 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 5)

IND_2022 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 6)

CAR_2022 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 7)

LV_2021 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 8)

JAX_2021 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 9)

DET_2020 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 10)

ATL_2020 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 11)

HOU_2020 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 12)

CAR_2019 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 13)

WAS_2019 <-read_excel("NFL Coaches Fired Data.xlsx", 
                       sheet = 14)

GB_2018 <-read_excel("NFL Coaches Fired Data.xlsx", 
                      sheet = 15)

CLE_2018 <-read_excel("NFL Coaches Fired Data.xlsx", 
                      sheet = 16)

clean <- function(team) {
  
  team <- team[-c(1), ]
  team <- team[, -c(2:5, 7)]

  colnames(team) <- c("Week", "Outcome", "Record", "Location", "Opponent", 
                      "Points_scored", "Points_allowed", "Off_1st_down", 
                      "Off_total_yards", "Off_pass_yards", "Off_rush_yards", 
                      "team_turnovers", "1st_downs_allowed", "Total_yards_allowed", 
                      "Pass_yards_allowed", "Rush_yards_allowed", 
                      "Turnovers_forced", "Expected_points_off", 
                      "Expected_points_def", "Expected_points_st")
  
  team <- team[team$Opponent != "Bye Week", ]

  team$Location[is.na(team$Location)] <- "Home"
  team$Location[team$Location == "@"] <- "Away"
  
  team$team_turnovers[is.na(team$team_turnovers)] <- 0
  team$Turnovers_forced[is.na(team$Turnovers_forced)] <- 0
  
  data_type_change <- c("Expected_points_off", "Expected_points_def", "Expected_points_st")
  team[, data_type_change] <- lapply(team[, data_type_change], function(x) as.numeric(as.character(x)))
  
  return(team)}

LAC_2023 <- clean(LAC_2023)
CAR_2023 <- clean(CAR_2023)
LV_2023 <- clean(LV_2023)
DEN_2022 <- clean(DEN_2022)
IND_2022 <- clean(IND_2022)
CAR_2022 <- clean(CAR_2022)
LV_2021 <- clean(LV_2021)
JAX_2021 <- clean(JAX_2021)
DET_2020 <- clean(DET_2020)
ATL_2020 <- clean(ATL_2020)
HOU_2020 <- clean(HOU_2020)
CAR_2019 <- clean(CAR_2019)
WAS_2019 <- clean(WAS_2019)
GB_2018 <- clean(GB_2018)
CLE_2018 <- clean(CLE_2018)

record <- function(team){
  
    team$Wins <- as.numeric(sapply(strsplit(team$Record, "-"), `[`, 1))
    team$Losses <- as.numeric(sapply(strsplit(team$Record, "-"), `[`, 2))
    team$Ties <- as.numeric(sapply(strsplit(team$Record, "-"), `[`, 3, simplify = TRUE))
    
    team$Ties[is.na(team$Ties)] <- 0
    
    total_games <- team$Wins + team$Losses + team$Ties
    team$Win_per <- round(team$Wins / total_games, 4)
    
    team <- cbind(team[, 1:3], team[, c("Wins", "Losses", "Ties", "Win_per")], team[, -(1:6)])
    team <- team[, -((ncol(team) - 3):ncol(team))]
    
    return(team)}

LAC_2023 <- record(LAC_2023)
CAR_2023 <- record(CAR_2023)
LV_2023 <- record(LV_2023)
DEN_2022 <- record(DEN_2022)
IND_2022 <- record(IND_2022)
CAR_2022 <- record(CAR_2022)
LV_2021 <- record(LV_2021)
JAX_2021 <- record(JAX_2021)
DET_2020 <- record(DET_2020)
ATL_2020 <- record(ATL_2020)
HOU_2020 <- record(HOU_2020)
CAR_2019 <- record(CAR_2019)
WAS_2019 <- record(WAS_2019)
GB_2018 <- record(GB_2018)
CLE_2018 <- record(CLE_2018)