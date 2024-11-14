# Load necessary libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(cluster)
library(caret)

# Read the data
salaries <- read.csv("2021-2024PlayersSalaries.csv")
stats_2021 <- read.csv("2021-2022 NBA Player Stats - Regular.csv")
stats_2022 <- read.csv("2022-2023 NBA Player Stats - Regular.csv")
stats_2023 <- read.csv("2023-2024 NBA Player Stats - Regular.csv")

# Columns to select
select_columns <- c("Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X2P", "X2PA", 
                    "eFG.", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")

# Function to process data for each year
process_year_data <- function(data, year) {
  data %>%
    select(all_of(select_columns)) %>%
    rename_with(~ paste0(.x, year), -Player)  # Add year suffix to each column, except Player
}

# Process data for each year
stats_2021_processed <- process_year_data(stats_2021, 2021)
stats_2022_processed <- process_year_data(stats_2022, 2022)
stats_2023_processed <- process_year_data(stats_2023, 2023)

# Merge the data
merged_data <- salaries %>%
  left_join(stats_2021_processed, by = "Player") %>%
  left_join(stats_2022_processed, by = "Player") %>%
  left_join(stats_2023_processed, by = "Player")

# Select and rename columns for final analysis
final_data <- merged_data %>%
  select(Player, Age2021 = Age2021, Age2022 = Age2022, Age2023 = Age2023, 
         Salary.2021.2022., Salary.2022.2023., Salary.2023.2024., 
         ends_with("2021"), ends_with("2022"), ends_with("2023")) %>%
  filter(!is.na(Age2022)) %>%
  distinct()

# Filter young players under the age of 25 for analysis
young_players <- final_data %>%
  filter(Age2022 < 25)

scaled_data <- young_players %>%
  select_if(is.numeric) %>%
  na.omit()

filtered_young_players <- young_players[complete.cases(young_players %>% select_if(is.numeric)), ]

pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

filtered_young_players$Potential_Score <- pca_result$x[, 1]

young_players <- left_join(young_players, filtered_young_players %>% 
                 select(Player, Potential_Score), by = "Player") %>%
                 distinct(Player, .keep_all = TRUE)

print(young_players)

young_potential_players<- young_players %>%
  select(Player, Age2022, Potential_Score) %>%
  distinct(Player, .keep_all = TRUE) %>%
  arrange(desc(Potential_Score))

print(young_potential_players)



# Prepare the training data for Random Forest
train_data <- young_players %>%
  select(Player, 
         Salary.2021.2022., Salary.2022.2023., Salary.2023.2024., 
         ends_with("2021"), ends_with("2022"), ends_with("2023")) %>%
  filter(!is.na(Salary.2023.2024.)) %>%
  mutate(Salary_Next_Year = Salary.2023.2024.) %>%
  select(-Salary.2023.2024.)

train_data <- na.omit(train_data)

# Train the Random Forest model
set.seed(123)
rf_model <- randomForest(Salary_Next_Year ~ ., data = train_data, ntree = 50000)

# Print the model summary to check performance
print(rf_model)


# Make predictions for the next season (2024-2025) salary
train_data$Predicted_Salary2024.2025. <- predict(rf_model, newdata = train_data)


# Add the predictions back to the young_players dataset for further analysis
young_players <- young_players %>%
  left_join(train_data %>% select(Player, Predicted_Salary2024.2025.), by = "Player")

print(young_players)


train_data$Predicted_Salary <- predict(rf_model, newdata = train_data)

# R-squared
rsq <- 1 - sum((train_data$Salary_Next_Year - train_data$Predicted_Salary)^2) / sum((train_data$Salary_Next_Year - mean(train_data$Salary_Next_Year))^2)
cat("R-squared: ", rsq, "\n")

# confusion matrix





# K-means Clustering
# Handle missing data: Filter out rows with missing values in the cluster data
cluster_data <- young_players %>%
  select(Potential_Score = Predicted_Salary2024.2025., Salary.2023.2024.) %>%
  na.omit()

# Elbow Method to determine optimal k
wss <- numeric(10) 
for (k in 1:10) {
  kmeans_model <- kmeans(cluster_data, centers = k)
  wss[k] <- kmeans_model$tot.withinss  
}

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")


# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 3)

# Ensure the cluster labels are assigned back to the correct rows in young_players
# Rejoin the cluster results with young_players while handling the missing data
young_players$Cluster <- NA  # Initialize the cluster column with NA

# Assign clusters back to the young_players data frame
young_players[!is.na(young_players$Predicted_Salary2024.2025.), "Cluster"] <- kmeans_result$cluster

# Remove rows with NA values in the Cluster column
young_players_filtered <- young_players %>%
  filter(!is.na(Cluster)) %>%
  distinct(Player, .keep_all = TRUE)

# Visualize the clustering result with fewer clusters, excluding NA values
ggplot(young_players_filtered, aes(x = Predicted_Salary2024.2025., y = Salary.2023.2024., color = as.factor(Cluster))) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("1" = "red", "2" = "blue", "3" = "green"),
    labels = c("1" = "High Potential", "2" = "Medium Potential", "3" = "Low Potential")
  ) +
  labs(title = "Clustering of Young Players Based on Predicted Salary and Current Salary",
       x = "Predicted Salary for 2024-2025", y = "Current Salary (2023-2024)", color = "Cluster") +
  theme_minimal()

young_players <- young_players %>%
  mutate(Ratio = Predicted_Salary2024.2025. / Salary.2023.2024.) %>%
  select(Player, Age2022, Predicted_Salary2024.2025., Salary.2023.2024., Ratio, Cluster) %>%
  distinct(Player, .keep_all = TRUE)

# Print young players with clusters
print(young_players %>% select(Player, Age2022, Ratio, Predicted_Salary2024.2025., Salary.2023.2024., Cluster) %>% distinct(Player, .keep_all = TRUE))

result <- young_players %>% filter(Ratio > 1) %>% arrange(desc(Ratio))
print(result)

