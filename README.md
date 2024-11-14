# BusinessDataMiningProject
Based on the NBA dataset with performance and salaries from 2021-2024.  

## Business Questions and Roadmap   
1. Idea: Try to predict if a player can be an all-star player based on his performance, which would affect the revenue of each teams.
   
   Procedure: Data cleaning -> Principal Components Analysis to get weights for variables -> Composite_Score to evaluate players ->
   RandomForest -> Select top 10 players by position -> Calculate the actual accuracy against the return list -> Evaluate model and test
   
   Result: We had predicted all-star players.
   
3. Idea: Try to get the relationship between Performance and Salaries from 2021-2024, which can help the department of management make decisions, for example, adjust the salary hat.
   
   Procedure: Data cleaning -> Train randomforest model based on all 17 types of variables -> Predict the salaries next year ->
   Find abnormal data(Anthony Edwards) -> Do research on Anthony Edwards specifically -> Analyze results, evaluate model and test
   
   Result: Anthony Edwards is very young with high performance in court. So we considered finding more young and potential players like him.

5. Idea: Try to find more young and potential players, which can help managers do players trade in the NBA.
   
   Procedure: Data cleaning -> Select young (age < 25) players -> PCA to get weights for Potential_Score -> Select young and potential players ->
   Train randomforest model -> To predict salaries of young and potential players -> Use K-mean to classify players into High / Medium / Low potential categories
   
   Result: We had the list of young and potential players and we knew which players had high potential so that the manager could do trade.  


## Files description  
csv:  
2021-2022 NBA Player Stats - Regular.csv : 2021-2022 NBA Player Regular Game Performance  

2022-2023 NBA Player Stats - Regular.csv : 2022-2023 NBA Player Regular Game Performance  

2023-2024 NBA Player Stats - Regular.csv : 2023-2024 NBA Player Regular Game Performance  

2021-2024PlayersSalaries.csv : 2021-2024 NBA Player Salaries  

**Code_2_15AllStarPlayersSalaries.csv** : Specifically for Code2_RelationshipBetweenPerformanceAndSalaries.R, including 15 AllStarPlayers selected from AllStarPlayers we predicted from Code1.  

Code1_PredictAllStarPlayers.R : With 2023-2024 NBA Player Regular Game Performance, to predict if a player can be all star player in the season, corresponding to the return list.  

Return list: https://x.com/NBAPR/status/1742969199549358405  

Code2_RelationshipBetweenPerformanceAndSalaries.R : With 2021-2024 NBA Player Regular Game Performance and Salaries, to get the relationship between Performance and Salaries, and predict the salaries for each player in 2024-2025 season.   

Code3_FindPotentialPlayers.R : With the performance and salaries we have got from Code2, try to find players whose salaries do not match with performance. To find young and potential players, and classify them to High, Medium, Low potential players.  

Data Mining_NBA.pptx : Slides we made for presentation and recording.

## Collaboration  
https://docs.google.com/document/d/1SVofc6MZUsdM31_2t_GF3ggmwYIYf3Z5m-3g21EQ_mg/edit?tab=t.0  

## TEAMMATES  
CHENYU WANG, GEUNJU PARK, PANAGIOTIS GEORGIADIS, SHANSHAN TAN, XIAOXUE JI  

# Last update: 2024/11/14
