custom_order <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "WC")

LAC_2023$Week <- factor(LAC_2023$Week, levels = custom_order)
CAR_2023$Week <- factor(CAR_2023$Week, levels = custom_order)
LV_2023$Week <- factor(LV_2023$Week, levels = custom_order)
DEN_2022$Week <- factor(DEN_2022$Week, levels = custom_order)
IND_2022$Week <- factor(IND_2022$Week, levels = custom_order)
CAR_2022$Week <- factor(CAR_2022$Week, levels = custom_order)
LV_2021$Week <- factor(LV_2021$Week, levels = custom_order)
JAX_2021$Week <- factor(JAX_2021$Week, levels = custom_order)
DET_2020$Week <- factor(DET_2020$Week, levels = custom_order)
ATL_2020$Week <- factor(ATL_2020$Week, levels = custom_order)
HOU_2020$Week <- factor(HOU_2020$Week, levels = custom_order)
CAR_2019$Week <- factor(CAR_2019$Week, levels = custom_order)
WAS_2019$Week <- factor(WAS_2019$Week, levels = custom_order)
GB_2018$Week <- factor(GB_2018$Week, levels = custom_order)
CLE_2018$Week <- factor(CLE_2018$Week, levels = custom_order)

LAC_2023$Team <- "LAC_2023"
CAR_2023$Team <- "CAR_2023"
LV_2023$Team <- "LV_2023"
DEN_2022$Team <- "DEN_2022"
IND_2022$Team <- "IND_2022"
CAR_2022$Team <- "CAR_2022"
LV_2021$Team <- "LV_2021"
JAX_2021$Team <- "JAX_2021"
DET_2020$Team <- "DET_2020"
ATL_2020$Team <- "ATL_2020"
HOU_2020$Team <- "HOU_2020"
GB_2018$Team <- "GB_2018"
CLE_2018$Team <- "CLE_2018"
CAR_2019$Team <- "CAR_2019"
WAS_2019$Team <- "WAS_2019"

df_2023 <- rbind(LAC_2023, CAR_2023, LV_2023)
df_2023$Week <- factor(df_2023$Week, levels = custom_order)
df_2022 <- rbind(DEN_2022, IND_2022, CAR_2022)
df_2022$Week <- factor(df_2022$Week, levels = custom_order)
df_2021 <- rbind(LV_2021, JAX_2021)
df_2021$Week <- factor(df_2021$Week, levels = custom_order)
df_2020 <- rbind(DET_2020, ATL_2020, HOU_2020)
df_2020$Week <- factor(df_2020$Week, levels = custom_order)
df_2019 <- rbind(CAR_2019, WAS_2019)
df_2019$Week <- factor(df_2019$Week, levels = custom_order)
df_2018 <- rbind(GB_2018, CLE_2018)
df_2018$Week <- factor(df_2018$Week, levels = custom_order)
  
  df_2023$Year <- 2023
  df_2022$Year <- 2022
  df_2021$Year <- 2021
  df_2020$Year <- 2020
  df_2019$Year <- 2019
  df_2018$Year <- 2018
 
  combined_df <- bind_rows(df_2023, df_2022, df_2021, df_2020, df_2019, df_2018)

  legend_order <- c("GB_2018", "CLE_2018", "CAR_2019", "WAS_2019",
                    "DET_2020", "ATL_2020", "HOU_2020", "LV_2021",
                    "JAX_2021", "DEN_2022", "IND_2022", "CAR_2022",
                    "LV_2023", "LAC_2023", "CAR_2023")
#ByYear
  ggplot(combined_df, aes(x = Week, y = Win_per, color = Team)) +
    geom_point(size = 1.3) +
    geom_line(aes(group = Team)) +  
    labs(x = "Week", y = "Win Percentage", title = "Team Win Percentage By Week", font = "bold") +
    scale_color_manual(values = c("LAC_2023" = "yellow", "CAR_2023" = "blue", "LV_2023" = "black",
                                  "DEN_2022" = "orange", "IND_2022" = "lightblue", "CAR_2022" = "blue",
                                  "LV_2021" = "black", "JAX_2021" = "limegreen",
                                  "DET_2020" = "turquoise", "ATL_2020" = "red", "HOU_2020" = "darkblue",
                                  "CAR_2019" = "blue", "WAS_2019" = "red",
                                  "GB_2018" = "darkgreen", "CLE_2018" = "orange"),
                       limits = legend_order) + 
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
    geom_point(data = subset(combined_df, 
                             (Team == "CLE_2018" & Week == 8) | (Team == "GB_2018" & Week == 12) |
                               (Team == "WAS_2019" & Week == 5) | (Team == "CAR_2019" & Week == 12) |
                               (Team == "HOU_2020" & Week == 4) | (Team == "ATL_2020" & Week == 5) |
                               (Team == "DET_2020" & Week == 11) | (Team == "JAX_2021" & Week == 13) |
                               (Team == "LV_2021" & Week == 5) | (Team == "CAR_2022" & Week == 5) |
                               (Team == "IND_2022" & Week == 9) | (Team == "DEN_2022" & Week == 15) |
                               (Team == "LV_2023" & Week == 8) | (Team == "CAR_2023" & Week == 11) |
                               (Team == "LAC_2023" & Week == 14)), aes(x = Week, y = Win_per), color = "yellow", size = 2.5) +
    ylim(0, 1) +
    facet_wrap(~ Year, nrow = 2, ncol = 3) +
    geom_text(data = subset(combined_df, Year == 2018), x = 6, y = 1, 
              label = "Coach Fired At Yellow Point", color = "Black", size = 3, angle = 0, hjust = 0, vjust = 0) 

combined_df <- rbind(df_2023, df_2022, df_2021, df_2020, df_2019, df_2018)
combined_df$Year <- factor(rep(c(2023, 2022, 2021, 2020, 2019, 2018), 
                               times = c(nrow(df_2023), nrow(df_2022), nrow(df_2021), 
                                         nrow(df_2020), nrow(df_2019), nrow(df_2018))))
#Rough
  ggplot(combined_df, aes(x = Week, y = Win_per, color = Team, group = interaction(Team, Year))) +
    geom_line(size = 1) +
    labs(x = "Week", y = "Win Percentage", title = "Team's Win Percentage") +
    scale_color_manual(values = c("LAC_2023" = "yellow", "CAR_2023" = "purple", "LV_2023" = "grey",
                                  "DEN_2022" = "orange", "IND_2022" = "darkblue", "CAR_2022" = "lightblue",
                                  "LV_2021" = "black", "JAX_2021" = "turquoise","DET_2020" = "blue",
                                  "ATL_2020" = "red", "HOU_2020" = "darkred","CAR_2019" = "magenta",
                                  "WAS_2019" = "pink","GB_2018" = "darkgreen", "CLE_2018" = "brown")) +
    geom_hline(yintercept = c(0, 0.25, 0.50, 0.75, 1), 
               linetype = c("solid", "dashed", "dashed", "dashed", "solid"), 
               color = "black") +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_point(data = subset(combined_df, 
                             (Team == "CLE_2018" & Week == 8) | (Team == "GB_2018" & Week == 12) |
                               (Team == "WAS_2019" & Week == 5) | (Team == "CAR_2019" & Week == 12) |
                               (Team == "HOU_2020" & Week == 4) | (Team == "ATL_2020" & Week == 5) |
                               (Team == "DET_2020" & Week == 11) | (Team == "JAX_2021" & Week == 13) |
                               (Team == "LV_2021" & Week == 5) | (Team == "CAR_2022" & Week == 5) |
                               (Team == "IND_2022" & Week == 9) | (Team == "DEN_2022" & Week == 15) |
                               (Team == "LV_2023" & Week == 8) | (Team == "CAR_2023" & Week == 11) |
                               (Team == "LAC_2023" & Week == 14)), aes(x = Week, y = Win_per), color = "black", size = 2.6) +
    ylim(0, 1)
  
#Smooth
  ggplot(combined_df, aes(x = Week, y = Win_per, color = Team, group = interaction(Team, Year))) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(x = "Week", y = "Win Percentage", title = "Team's Win Percentage") +
    scale_color_manual(values = c("LAC_2023" = "yellow", "CAR_2023" = "purple", "LV_2023" = "grey",
                                  "DEN_2022" = "orange", "IND_2022" = "darkblue", "CAR_2022" = "lightblue",
                                  "LV_2021" = "black", "JAX_2021" = "turquoise","DET_2020" = "blue",
                                  "ATL_2020" = "red", "HOU_2020" = "darkred","CAR_2019" = "magenta",
                                  "WAS_2019" = "pink","GB_2018" = "darkgreen", "CLE_2018" = "brown")) +
    geom_hline(yintercept = c(0, 0.25, 0.50, 0.75, 1), 
               linetype = c("solid", "dashed", "dashed", "dashed", "solid"), 
               color = "black") +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_point(data = subset(combined_df, 
                            (Team == "CLE_2018" & Week == 8) | (Team == "GB_2018" & Week == 12) |
                            (Team == "WAS_2019" & Week == 5) | (Team == "CAR_2019" & Week == 12) |
                            (Team == "HOU_2020" & Week == 4) | (Team == "ATL_2020" & Week == 5) |
                            (Team == "DET_2020" & Week == 11) | (Team == "JAX_2021" & Week == 13) |
                            (Team == "LV_2021" & Week == 5) | (Team == "CAR_2022" & Week == 5) |
                            (Team == "IND_2022" & Week == 9) | (Team == "DEN_2022" & Week == 15) |
                            (Team == "LV_2023" & Week == 8) | (Team == "CAR_2023" & Week == 11) |
                            (Team == "LAC_2023" & Week == 14)), aes(x = Week, y = Win_per), color = "black", size = 2.6) +
    ylim(0, 1)
  