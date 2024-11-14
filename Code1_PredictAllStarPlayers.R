library(ggplot2) # Data visualization
library(reshape2) # Data manipulation
library(dplyr) # Data manipulation
library(tidyr) # Data tidying
library(randomForest) # Random forest model
library(caret) # Model evaluation
library(pROC) # ROC curve

# Read the data
data <- read.csv("2023-2024 NBA Player Stats - Regular.csv")

# Data Cleaning
position_distribution <- data %>%
  mutate(Pos = gsub("-", "", Pos)) %>% 
  uncount(sapply(strsplit(as.character(Pos), "-"), length)) %>% 
  mutate(Pos = trimws(unlist(strsplit(as.character(Pos), "-")))) %>% 
  filter(Pos %in% c("PG", "SG", "SF", "PF", "C")) %>% 
  group_by(Pos) %>% 
  summarise(Count = n()) %>% 
  ungroup()

df_unique <- data[!duplicated(data$Player), ]

# Distibution of players by positions
position_distribution <- position_distribution %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Pie plot
ggplot(position_distribution, aes(x = "", y = Percentage, fill = Pos)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Player Distribution by Positions", x = "", y = "", fill = "Position") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))



# Age vs. Points Scored
age_pts_data <- data %>%
  select(Age, PTS)

# Correlation between Age and Points Scored
correlation_Age_PTS <- cor(age_pts_data$Age, age_pts_data$PTS, method = "pearson")
print(paste("Pearson Correlation Coefficient between Age and Points:", correlation_Age_PTS))

# Scatter plot
ggplot(age_pts_data, aes(x = Age, y = PTS)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # regression line
  labs(title = "Age vs. Points Scored", x = "Age", y = "Points Scored") +
  theme_minimal()
# correlation_Age_PTS = 0.079, which is close to 0, indicating a weak linear relationship between Age and Points Scored.



# MP vs. Points Scored
mp_pts_data <- data %>%
  select(MP, PTS)

# Correlation between Minutes Played and Points Scored
correlation_MP_PTS <- cor(mp_pts_data$MP, mp_pts_data$PTS, method = "pearson")
print(paste("Pearson Correlation Coefficient between Minutes Played and Points:", correlation_MP_PTS))

# Scatter plot
ggplot(mp_pts_data, aes(x = MP, y = PTS)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Regression line
  labs(title = "Minutes Played vs. Points Scored", x = "Minutes Played", y = "Points Scored") +
  theme_minimal()
# correlation_MP_PTS = 0.887, which indicates a strong positive linear relationship between Minutes Played and Points Scored.


# PCA
numeric_data <- data %>%
  select(PTS, AST, STL, BLK, TRB, G) %>% 
  na.omit() 

pca_result <- prcomp(numeric_data, scale. = TRUE)

summary(pca_result)

pca_result$rotation

loadings_df <- as.data.frame(pca_result$rotation)
loadings_df$Feature <- rownames(loadings_df)

ggplot(loadings_df, aes(x = PC1, y = PC2, label = Feature)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5) +
  labs(title = "PCA: Loadings Plot", x = "PC1", y = "PC2") +
  theme_minimal()

explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)

plot(cumulative_variance, type = "b", xlab = "Number of Principal Components", ylab = "Cumulative Explained Variance",
     main = "Cumulative Explained Variance", col = "blue", pch = 16)

pc1_loadings <- abs(pca_result$rotation[, "PC1"])
weights <- pc1_loadings / sum(pc1_loadings)
print(weights)

# Predict
# Data Preprocessing
# Set weights for each feature
PTSweight <- 0.19
ASTweight <- 0.17
TRBweight <- 0.17
STLweight <- 0.18
BLKweight <- 0.14
GWeight <- 0.15
# We can set different weights based on the position and feature we want to focus on.
# For example, we can give more weight to BLK for Centers and more weight to AST for Point Guards.
data <- data %>%
  mutate(Composite_Score = (PTS * PTSweight +
                              AST * ASTweight +
                              STL * STLweight +
                              TRB * TRBweight +
                              BLK * BLKweight +
                              G * GWeight)) # The number of games played.

threshold <- quantile(data$Composite_Score, 0.75)
data$All_Star <- ifelse(data$Composite_Score >= threshold, 1, 0)


# Select features and remove rows with missing values
features <- data %>%
  select(Player, Pos, PTS, AST, STL, TRB, BLK, G, All_Star) %>%
  distinct(Player, .keep_all = TRUE) %>%
  na.omit()

features$Pos <- as.factor(features$Pos)
features$All_Star <- as.factor(features$All_Star)

features <- features %>% distinct(Player, .keep_all = TRUE)
write.csv(features, "features.csv", row.names = FALSE)

# Partition the data into training and testing sets
set.seed(123)  
total_rows <- nrow(features)
train_size <- round(0.6 * total_rows) 
train_index <- sample(1:total_rows, train_size)
train_data <- features[train_index, ]
valid_data <- features[-train_index, ]

train_data$All_Star <- as.factor(train_data$All_Star)
valid_data$All_Star <- as.factor(valid_data$All_Star)
str(train_data)
str(valid_data)

# Train a random forest model for classification
rf_model <- randomForest(All_Star ~ Pos + PTS + AST + STL + BLK, data = train_data, importance = TRUE)

# Check the model summary
print(rf_model)

# Make predictions on the validation data
predictions <- predict(rf_model, valid_data)

# Add the predicted All-Star status to the validation data
valid_data$Predicted_All_Star <- predictions


# Top 10 players by position
top_players <- features %>%
  group_by(Pos) %>%
  filter(All_Star == 1) %>%  # Filter All-Star players
  arrange(desc(PTS)) %>%  # Sort by points scored
  slice_head(n = 10) %>%  # Select top 10 players
  ungroup()

print(top_players, n = Inf)

return_list <- data.frame(Player = c(
  "LeBron James", "Kevin Durant", "Nikola Jokic", "Anthony Davis", "Kawhi Leonard",
  "Paul George", "Alperen Sengun", "Victor Wembanyama", "Chet Holmgren", "Karl-Anthony Towns",
  "Luka Doncic", "Stephen Curry", "Shai Gilgeous-Alexander", "James Harden", "Kyrie Irving",
  "Anthony Edwards", "Ja Morant", "De'Aaron Fox", "Klay Thompson", "Austin Reaves",
  "Giannis Antetokounmpo", "Joel Embiid", "Jayson Tatum", "Jimmy Butler", "Jaylen Brown",
  "Bam Adebayo", "Mikal Bridges", "Kristaps Porzingis", "Kyle Kuzma", "Paolo Banchero",
  "Tyrese Haliburton", "Damian Lillard", "Trae Young", "Donovan Mitchell", "Tyrese Maxey",
  "Jalen Brunson", "LaMelo Ball", "Derrick White", "DeMar DeRozan", "Jrue Holiday"
))

return_list <- return_list %>%
  mutate(Match = ifelse(Player %in% top_players$Player, 1, 0))

print(return_list)
write.csv(return_list, "return_list.csv", row.names = FALSE)

matching_players <- return_list %>%
  filter(Player %in% top_players$Player)

matching_count <- nrow(matching_players)
print(matching_count/40)

# Confusion Matrix
confusion_matrix <- confusionMatrix(predictions, valid_data$All_Star)
print(confusion_matrix)

matrix_data <- as.table(confusion_matrix$table)

df <- as.data.frame(matrix_data)
colnames(df) <- c("Prediction", "Reference", "Frequency")

accuracy <- confusion_matrix$overall["Accuracy"]

ggplot(df, aes(x = Prediction, y = Reference, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") +
  theme_minimal() +
  annotate("text", x = 1.5, y = 1.5, label = paste("Accuracy:", round(accuracy, 3)),
           size = 5, color = "black", fontface = "bold")

# Feature Importance
feature_importance <- importance(rf_model)

importance_df <- as.data.frame(feature_importance)
importance_df$Feature <- rownames(importance_df)

importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) 

print(importance_df)

# Viualize Feature Importance
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance in Random Forest Model",
       x = "Features",
       y = "Mean Decrease in Gini") +
  theme_minimal()

# ROC Curve
predicted_probabilities <- predict(rf_model, valid_data, type = "prob")[, 2]

roc_curve <- roc(valid_data$All_Star, predicted_probabilities)

plot(roc_curve, 
     col = "blue", 
     lwd = 2, 
     main = "ROC Curve",
     print.auc = TRUE)  
# AUC = 0.965, which indicates that the model has good predictive power.


# Predicting All-Star status for a new player
good_player <- data.frame(Pos = "PG", PTS = 20, AST = 10, STL = 10, BLK = 10)
bad_player <- data.frame(Pos = "PG", PTS = 5, AST = 2, STL = 2, BLK = 1)

# Ensure that the Pos column is a factor with the same levels as in the training data
good_player$Pos <- factor(good_player$Pos, levels = levels(features$Pos))
bad_player$Pos <- factor(bad_player$Pos, levels = levels(features$Pos))

# Predict the All-Star status for the new player
predicted_status_good <- predict(rf_model, good_player)
predicted_status_bad <- predict(rf_model, bad_player)

# Output the prediction
print(paste("Predicted All-Star status for", good_player$Player, ":", ifelse(predicted_status_good == 1, "Yes", "No")))
print(paste("Predicted All-Star status for", bad_player$Player, ":", ifelse(predicted_status_bad == 1, "Yes", "No")))

# Get predicted probabilities for further insights
predicted_probabilities_good <- predict(rf_model, good_player, type = "prob")
predicted_probabilities_bad <- predict(rf_model, bad_player, type = "prob")

print(predicted_probabilities_good)
print(predicted_probabilities_bad)



