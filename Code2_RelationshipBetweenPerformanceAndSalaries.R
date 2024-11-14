library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(randomForest)
library(caret)
library(factoextra)

# Read the data
salaries <- read.csv("Code_2_15AllStarPlayersSalaries.csv")
stats_2021 <- read.csv("2021-2022 NBA Player Stats - Regular.csv")
stats_2022 <- read.csv("2022-2023 NBA Player Stats - Regular.csv")
stats_2023 <- read.csv("2023-2024 NBA Player Stats - Regular.csv")

# Function to process the statistics data
process_stats <- function(stats, year) {
  stats %>%
    select(-c(Rk, Pos, Age, Tm)) %>%  
    group_by(Player) %>%               
    summarise(across(G:PTS, mean, na.rm = TRUE), .groups = 'drop') %>%  
    rename_with(~ paste0(.x, year), G:PTS)  
}

# Data processing for each season
stats_2021_processed <- process_stats(stats_2021, "2021")
stats_2022_processed <- process_stats(stats_2022, "2022")
stats_2023_processed <- process_stats(stats_2023, "2023")

# Merge all the tables
merged_data <- salaries %>%
  left_join(stats_2021_processed, by = "Player") %>%
  left_join(stats_2022_processed, by = "Player") %>%
  left_join(stats_2023_processed, by = "Player")

# delete rows with any missing values
merged_data <- merged_data %>%
  filter(!if_all(everything(), ~ is.na(.) | . == ""))

print(colnames(merged_data))

cor_data <- as.data.frame(scale(merged_data %>%
                                  select(G2021, G2022, G2023, MP2021, MP2022, MP2023, FT2021, FT2022, FT2023, 
                                         FTA2021, FTA2022, FTA2023, FG2021, FG2022, FG2023, FGA2021, FGA2022, FGA2023, 
                                         X3P2021, X3P2022, X3P2023, X3PA2021, X3PA2022, X3PA2023, X2P2021, X2P2022, X2P2023, 
                                         X2PA2021, X2PA2022, X2PA2023, PF2021, PF2022, PF2023, PTS2021, PTS2022, PTS2023, 
                                         AST2021, AST2022, AST2023, STL2021, STL2022, STL2023, BLK2021, BLK2022, BLK2023, 
                                         TRB2021, TRB2022, TRB2023, TOV2021, TOV2022, TOV2023,
                                         Salary.2021.2022., Salary.2022.2023., Salary.2023.2024.)))

cor_data_merged <- cor_data %>%
  mutate(
    G = rowMeans(select(., starts_with("G")), na.rm = TRUE),  # merge all G series variables
    MP = rowMeans(select(., starts_with("MP")), na.rm = TRUE),  # merge all MP series variables
    FT = rowMeans(select(., starts_with("FT")), na.rm = TRUE),  # merge all FT series variables
    FTA = rowMeans(select(., starts_with("FTA")), na.rm = TRUE),  # merge all FTA series variables
    FG = rowMeans(select(., starts_with("FG")), na.rm = TRUE),  # merge all FG series variables
    FGA = rowMeans(select(., starts_with("FGA")), na.rm = TRUE),  # merge all FGA series variables
    X3P = rowMeans(select(., starts_with("X3P")), na.rm = TRUE),  # merge all X3P series variables
    X3PA = rowMeans(select(., starts_with("X3PA")), na.rm = TRUE),  # merge all X3PA series variables
    X2P = rowMeans(select(., starts_with("X2P")), na.rm = TRUE),  # merge all X2P series variables
    X2PA = rowMeans(select(., starts_with("X2PA")), na.rm = TRUE),  # merge all X2PA series variables
    PF = rowMeans(select(., starts_with("PF")), na.rm = TRUE),  # merge all PF series variables
    PTS = rowMeans(select(., starts_with("PTS")), na.rm = TRUE),  # merge all PTS series variables
    AST = rowMeans(select(., starts_with("AST")), na.rm = TRUE),  # merge all AST series variables
    STL = rowMeans(select(., starts_with("STL")), na.rm = TRUE),  # merge all STL series variables
    BLK = rowMeans(select(., starts_with("BLK")), na.rm = TRUE),  # merge all BLK series variables
    TRB = rowMeans(select(., starts_with("TRB")), na.rm = TRUE),  # merge all TRB series variables
    TOV = rowMeans(select(., starts_with("TOV")), na.rm = TRUE),  # merge all TOV series variables
    AverageSalary = rowMeans(select(., starts_with("Salary")), na.rm = TRUE)  # merge all Salary series variables
  ) %>%
  select(-ends_with("2021"), -ends_with("2022"), -ends_with("2023"),-starts_with("Salary") )  # remove the original columns

# correlation matrix
cor_matrix <- cor(cor_data_merged)

# corrplot
corrplot(cor_matrix, 
         method = "circle", 
         type = "lower", 
         order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 40, 
         number.cex = 0.6)#Change the size of number inside



# Keep the columns for prediction
process_stats_with_age_new <- function(stats, year) {
  stats %>%
    select(Player, Age, G, MP, FG, FGA, X3P, X3PA, X2P, X2PA, eFG., 
           FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS) %>%  
    group_by(Player, Age) %>%  
    summarise(across(G:PTS, mean, na.rm = TRUE), .groups = 'drop') %>%  
    rename_with(~ paste0(.x, year), G:PTS)  
}

stats_2021_processed_new <- process_stats_with_age_new(stats_2021, "2021")
stats_2022_processed_new <- process_stats_with_age_new(stats_2022, "2022")
stats_2023_processed_new <- process_stats_with_age_new(stats_2023, "2023")

merged_salaries_stats <- salaries %>%
  left_join(stats_2021_processed_new, by = "Player") %>%
  left_join(stats_2022_processed_new, by = "Player") %>%
  left_join(stats_2023_processed_new, by = "Player")

merged_salaries_stats <- merged_salaries_stats %>%
  filter(!if_all(everything(), ~ is.na(.) | . == ""))

print(colnames(merged_salaries_stats))

correlation_data_new <- merged_salaries_stats %>%
  mutate(Salary_mean = rowMeans(select(., starts_with("Salary.")), na.rm = TRUE)) %>%
  select(Salary_mean, Age)


correlation_matrix_new <- cor(correlation_data_new, use = "pairwise.complete.obs")

print(correlation_matrix_new)









# Prepare the data for training the model
predictor_columns <- c("G2021", "G2022", "G2023", 
                       "MP2021", "MP2022", "MP2023",
                       "FT2021", "FT2022", "FT2023",
                       "FTA2021", "FTA2022", "FTA2023",
                       "FG2021", "FG2022", "FG2023",
                       "FGA2021", "FGA2022", "FGA2023",
                       "X3P2021", "X3P2022", "X3P2023",
                       "X3PA2021", "X3PA2022", "X3PA2023",
                       "X2P2021", "X2P2022", "X2P2023",
                       "X2PA2021", "X2PA2022", "X2PA2023",
                       "PF2021", "PF2022", "PF2023",
                       "PTS2021", "PTS2022", "PTS2023",
                       "AST2021", "AST2022", "AST2023",
                       "STL2021", "STL2022", "STL2023",
                       "BLK2021", "BLK2022", "BLK2023",
                       "TRB2021", "TRB2022", "TRB2023",
                       "TOV2021", "TOV2022", "TOV2023"
                       )  
target_column <- "Salary.2023.2024."  

training_data <- merged_data %>%
  select(all_of(predictor_columns), all_of(target_column)) %>%
  na.omit() 

print(colnames(training_data))

#Random Forest Model
set.seed(123) 
rf_model <- randomForest(as.formula(paste(target_column, paste(predictor_columns, collapse = " + "), sep = " ~ ")),
                         data = training_data, importance = TRUE, ntree = 50000)
#200000 -> Some data will be NaN.

print(rf_model)

# Importance of features
importance_data <- as.data.frame(importance(rf_model))
importance_data$Feature <- rownames(importance_data)

colnames(importance_data) <- c("MeanDecreaseGini", "MeanDecreaseAccuracy", "Feature")  # "MeanDecreaseAccuracy"

importance_data <- importance_data %>%
  arrange(desc(MeanDecreaseGini))

ggplot(importance_data, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Feature Importance from Random Forest Model",
       x = "Features",
       y = "Mean Decrease in Gini") +
  theme_minimal()
#TRB:Total rebounds per game.
#BLK:Blocks per game.





# Predictions
predictions <- predict(rf_model, newdata = training_data)

result <- data.frame(Player = merged_data$Player, 
                     Actual_Salary = merged_data[["Salary.2023.2024."]], 
                     Predicted_Salary = predictions,
                     Ratio = predictions / merged_data[["Salary.2023.2024."]])

print(result)
#Anthony Edwards   13534817/22182929 - 1.6389530

AnthonyEdwards <- merged_data %>%
  filter(Player == "Anthony Edwards") %>%
  mutate(PredcitedSalary = predict(rf_model, newdata = .))


print(AnthonyEdwards)
#colnames(AnthonyEdwards)
stats_columns <- c("G2021", "G2022", "G2023", 
                   "MP2021", "MP2022", "MP2023",
                   "FT2021", "FT2022", "FT2023",
                   "FTA2021", "FTA2022", "FTA2023",
                   "FG2021", "FG2022", "FG2023",
                   "FGA2021", "FGA2022", "FGA2023",
                   "X3P2021", "X3P2022", "X3P2023",
                   "X3PA2021", "X3PA2022", "X3PA2023",
                   "X2P2021", "X2P2022", "X2P2023",
                   "X2PA2021", "X2PA2022", "X2PA2023",
                   "PF2021", "PF2022", "PF2023",
                   "PTS2021", "PTS2022", "PTS2023",
                   "AST2021", "AST2022", "AST2023",
                   "STL2021", "STL2022", "STL2023",
                   "BLK2021", "BLK2022", "BLK2023",
                   "TRB2021", "TRB2022", "TRB2023",
                   "TOV2021", "TOV2022", "TOV2023"
)
AnthonyEdwards_stats <- AnthonyEdwards %>%
  select(all_of(stats_columns)) %>%
  pivot_longer(everything(), names_to = c(".value", "Year"), names_pattern = "(.+)(\\d{4})") 

custom_colors <- c(
  "Games" = "#FF9999",
  "Minutes Played" = "#66B3FF",
  "Free Throws Made" = "#99FF99",
  "Free Throws Attempted" = "#FFCC99",
  "Field Goals Made" = "#FFB3E6",
  "Field Goals Attempted" = "#FFFF99",
  "Three Point Field Goals Made" = "#FF6666",
  "Three Point Field Goals Attempted" = "#99CCFF",
  "Two Point Field Goals Made" = "#CCFF99",
  "Two Point Field Goals Attempted" = "#FF9966",
  "Personal Fouls" = "#FFCCFF",
  "Points" = "#FFD700",
  "Assists" = "#FF4500",
  "Steals" = "#00FF7F",
  "Blocks" = "#1E90FF",
  "Rebounds" = "#8A2BE2",
  "Turnovers" = "#A0522D"
)

ggplot(AnthonyEdwards_stats, aes(x = Year)) +
  # Bar charts for different statistics
  geom_bar(aes(y = G, fill = "Games"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = MP, fill = "Minutes Played"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = PTS, fill = "Points"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = TRB, fill = "Rebounds"), stat = "identity", position = "dodge") +
  # Line charts with black color for trends
  geom_line(aes(y = G, group = 1), color = "Black", size = 1, linetype = "solid") +
  geom_line(aes(y = MP, group = 1), color = "Black", size = 1, linetype = "solid") +
  geom_line(aes(y = PTS, group = 1), color = "Black", size = 1, linetype = "solid") +
  geom_line(aes(y = TRB, group = 1), color = "Black", size = 1, linetype = "solid") +
  # Add text labels on the lines at each data point
  geom_text(aes(y = G, label = G), color = "black", size = 3, vjust = -0.5) +
  geom_text(aes(y = MP, label = MP), color = "black", size = 3, vjust = -0.5) +
  geom_text(aes(y = PTS, label = PTS), color = "black", size = 3, vjust = -0.5) +
  geom_text(aes(y = TRB, label = TRB), color = "black", size = 3, vjust = -0.5) +
  # Labels and customization
  labs(title = "Anthony Edwards Stats Over Years",
       x = "Year",
       y = "Statistics") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) + 
  theme(legend.title = element_blank())

#Well played, should increase salary.


# Evaluate the model
ss_total <- sum((result$Actual_Salary - mean(result$Actual_Salary))^2) 
ss_residual <- sum((result$Actual_Salary - result$Predicted_Salary)^2)  
r_squared <- 1 - (ss_residual / ss_total)

cat("R² :", r_squared, "\n")

# Test the model on new data
james_harden_2021 <- stats_2021 %>% filter(Player == "James Harden") %>% select(G, MP, FT, FTA, FG, FGA, X3P, X3PA, X2P, X2PA, PF, PTS, AST, STL, BLK, TRB, TOV) %>% slice(1)
james_harden_2022 <- stats_2022 %>% filter(Player == "James Harden") %>% select(G, MP, FT, FTA, FG, FGA, X3P, X3PA, X2P, X2PA, PF, PTS, AST, STL, BLK, TRB, TOV) %>% slice(1)
james_harden_2023 <- stats_2023 %>% filter(Player == "James Harden") %>% select(G, MP, FT, FTA, FG, FGA, X3P, X3PA, X2P, X2PA, PF, PTS, AST, STL, BLK, TRB, TOV) %>% slice(1)

JamesHarden <- data.frame(
  Player = "James Harden",  
  
  G2021 = james_harden_2021$G,
  G2022 = james_harden_2022$G,
  G2023 = james_harden_2023$G,
  
  MP2021 = james_harden_2021$MP,
  MP2022 = james_harden_2022$MP,
  MP2023 = james_harden_2023$MP,
  
  FT2021 = james_harden_2021$FT,
  FT2022 = james_harden_2022$FT,
  FT2023 = james_harden_2023$FT,
  
  FTA2021 = james_harden_2021$FTA,
  FTA2022 = james_harden_2022$FTA,
  FTA2023 = james_harden_2023$FTA,
  
  FG2021 = james_harden_2021$FG,
  FG2022 = james_harden_2022$FG,
  FG2023 = james_harden_2023$FG,
  
  FGA2021 = james_harden_2021$FGA,
  FGA2022 = james_harden_2022$FGA,
  FGA2023 = james_harden_2023$FGA,
  
  X3P2021 = james_harden_2021$X3P,
  X3P2022 = james_harden_2022$X3P,
  X3P2023 = james_harden_2023$X3P,
  
  X3PA2021 = james_harden_2021$X3PA,
  X3PA2022 = james_harden_2022$X3PA,
  X3PA2023 = james_harden_2023$X3PA,
  
  X2P2021 = james_harden_2021$X2P,
  X2P2022 = james_harden_2022$X2P,
  X2P2023 = james_harden_2023$X2P,
  
  X2PA2021 = james_harden_2021$X2PA,
  X2PA2022 = james_harden_2022$X2PA,
  X2PA2023 = james_harden_2023$X2PA,
  
  PF2021 = james_harden_2021$PF,
  PF2022 = james_harden_2022$PF,
  PF2023 = james_harden_2023$PF,
  
  PTS2021 = james_harden_2021$PTS,
  PTS2022 = james_harden_2022$PTS,
  PTS2023 = james_harden_2023$PTS,
  
  AST2021 = james_harden_2021$AST,
  AST2022 = james_harden_2022$AST,
  AST2023 = james_harden_2023$AST,
  
  STL2021 = james_harden_2021$STL,
  STL2022 = james_harden_2022$STL,
  STL2023 = james_harden_2023$STL,
  
  BLK2021 = james_harden_2021$BLK,
  BLK2022 = james_harden_2022$BLK,
  BLK2023 = james_harden_2023$BLK,
  
  TRB2021 = james_harden_2021$TRB,
  TRB2022 = james_harden_2022$TRB,
  TRB2023 = james_harden_2023$TRB,
  
  TOV2021 = james_harden_2021$TOV,
  TOV2022 = james_harden_2022$TOV,
  TOV2023 = james_harden_2023$TOV,
  
  Salary.2021.2022. = 44310840,
  Salary.2022.2023. = 33000000,
  Salary.2023.2024. = 35680595
)

# Results for James Harden
print(JamesHarden)


# Make predictions for James Harden
predicted_salary <- predict(rf_model, newdata = JamesHarden)
print(paste("Predicted Salary for James Harden in 2024-2025: ", round(predicted_salary, 2)))
#29466834.97 -> Decrease the salary, because of the injury and the age of the player.

JamesHarden <- JamesHarden %>%
  mutate(Predicted_Salary = predicted_salary)


ja_morant_2021 <- stats_2021 %>% filter(Player == "Ja Morant") %>% select(G, MP, FT, FTA, FG, FGA, X3P, X3PA, X2P, X2PA, PF, PTS, AST, STL, BLK, TRB, TOV) %>% slice(1)
ja_morant_2022 <- stats_2022 %>% filter(Player == "Ja Morant") %>% select(G, MP, FT, FTA, FG, FGA, X3P, X3PA, X2P, X2PA, PF, PTS, AST, STL, BLK, TRB, TOV) %>% slice(1)
ja_morant_2023 <- stats_2023 %>% filter(Player == "Ja Morant") %>% select(G, MP, FT, FTA, FG, FGA, X3P, X3PA, X2P, X2PA, PF, PTS, AST, STL, BLK, TRB, TOV) %>% slice(1)

JaMorant <- data.frame(
  Player = "Ja Morant",
  
  G2021 = ja_morant_2021$G,
  G2022 = ja_morant_2022$G,
  G2023 = ja_morant_2023$G,
  
  MP2021 = ja_morant_2021$MP,
  MP2022 = ja_morant_2022$MP,
  MP2023 = ja_morant_2023$MP,
  
  FT2021 = ja_morant_2021$FT,
  FT2022 = ja_morant_2022$FT,
  FT2023 = ja_morant_2023$FT,
  
  FTA2021 = ja_morant_2021$FTA,
  FTA2022 = ja_morant_2022$FTA,
  FTA2023 = ja_morant_2023$FTA,
  
  FG2021 = ja_morant_2021$FG,
  FG2022 = ja_morant_2022$FG,
  FG2023 = ja_morant_2023$FG,
  
  FGA2021 = ja_morant_2021$FGA,
  FGA2022 = ja_morant_2022$FGA,
  FGA2023 = ja_morant_2023$FGA,
  
  X3P2021 = ja_morant_2021$X3P,
  X3P2022 = ja_morant_2022$X3P,
  X3P2023 = ja_morant_2023$X3P,
  
  X3PA2021 = ja_morant_2021$X3PA,
  X3PA2022 = ja_morant_2022$X3PA,
  X3PA2023 = ja_morant_2023$X3PA,
  
  X2P2021 = ja_morant_2021$X2P,
  X2P2022 = ja_morant_2022$X2P,
  X2P2023 = ja_morant_2023$X2P,
  
  X2PA2021 = ja_morant_2021$X2PA,
  X2PA2022 = ja_morant_2022$X2PA,
  X2PA2023 = ja_morant_2023$X2PA,
  
  PF2021 = ja_morant_2021$PF,
  PF2022 = ja_morant_2022$PF,
  PF2023 = ja_morant_2023$PF,
  
  PTS2021 = ja_morant_2021$PTS,
  PTS2022 = ja_morant_2022$PTS,
  PTS2023 = ja_morant_2023$PTS,
  
  AST2021 = ja_morant_2021$AST,
  AST2022 = ja_morant_2022$AST,
  AST2023 = ja_morant_2023$AST,
  
  STL2021 = ja_morant_2021$STL,
  STL2022 = ja_morant_2022$STL,
  STL2023 = ja_morant_2023$STL,
  
  BLK2021 = ja_morant_2021$BLK,
  BLK2022 = ja_morant_2022$BLK,
  BLK2023 = ja_morant_2023$BLK,
  
  TRB2021 = ja_morant_2021$TRB,
  TRB2022 = ja_morant_2022$TRB,
  TRB2023 = ja_morant_2023$TRB,
  
  TOV2021 = ja_morant_2021$TOV,
  TOV2022 = ja_morant_2022$TOV,
  TOV2023 = ja_morant_2023$TOV,
  
  Salary.2021.2022. = 9603360,  
  Salary.2022.2023. = 12119440,  
  Salary.2023.2024. = 34005250   
)

print(JaMorant)

predicted_salary_Ja <- predict(rf_model, newdata = JaMorant)
print(paste("Predicted Salary for Ja Morant in 2024-2025: ", round(predicted_salary_Ja, 2)))
#35869000.85 -> Increase the salary, because of the performance of the player, show potential.


New_merged_data <- merged_data %>%
  mutate(
    Avg_PTS = (PTS2021 + PTS2022 + PTS2023) / 3,
    Avg_TRB = (TRB2021 + TRB2022 + TRB2023) / 3,
    Avg_AST = (AST2021 + AST2022 + AST2023) / 3,
    
    SD_PTS = apply(select(., PTS2021, PTS2022, PTS2023), 1, sd, na.rm = TRUE),
    SD_TRB = apply(select(., TRB2021, TRB2022, TRB2023), 1, sd, na.rm = TRUE),
    SD_AST = apply(select(., AST2021, AST2022, AST2023), 1, sd, na.rm = TRUE),
    
    PTS_Trend = (PTS2023 - PTS2021) / PTS2021 * 100,
    TRB_Trend = (TRB2023 - TRB2021) / TRB2021 * 100,
    AST_Trend = (AST2023 - AST2021) / AST2021 * 100
  )


model <- lm(Salary.2023.2024. ~ Avg_PTS + SD_PTS + PTS_Trend + 
              Avg_TRB + SD_TRB + TRB_Trend + 
              Avg_AST + SD_AST + AST_Trend, 
            data = New_merged_data)

model_simplified <- lm(Salary.2023.2024. ~ Avg_PTS + PTS_Trend + Avg_TRB + TRB_Trend + AST_Trend, 
                       data = New_merged_data)

summary(model_simplified)

