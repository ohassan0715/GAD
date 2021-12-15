# ******************************************************************************************************************************************
# Hypothesis: Unemployed gamers are more likely to receive worse scores on the psychological assessments 
#             than their counterparts in this study.
# ******************************************************************************************************************************************


# Load libraries
library(tidyverse) # Data Manipulation, Analysis, Visualization
library(gridExtra) # Multi-panel Layout for plots
library(corrplot)  # Correlation plotting
library(caret)     # Data Splitting and Classification/Regression
library(sqldf)     # SQL

# Load dataset
g1 <- read.csv('GamingStudy_data_v2.csv')

# Get a full picture of the dataset 
dim(g1)
summary(g1)
str(g1)

# Weekly_Hours histogram
ggplot(g1, aes(y=Weekly_Hours)) +
  geom_boxplot(na.rm = FALSE)

# Create a new dataset that filters out the Weekly_Hours outliers
# The records with those outliers indicate the repondents immaturely
# answered questions, which is now a known risk since the survey was on Reddit.
g2 <- g1[g1$Weekly_Hours < 105, ]

# Add three new variables to clean up the whyplay, Work, and Playstyle columns
g2$motivation <- ifelse(g2$whyplay %in% c('having fun','improving', 'relaxing','winning'), g2$whyplay, 'Other')
g2$occupation <- ifelse(g2$Work %in% c('Student at college / university','Student at school'), 'Student',
                        ifelse(g2$Work %in% 'Unemployed / between jobs', 'Unemployed', g2$Work))
g2$styleofplay <- ifelse(g2$Playstyle %in% c('Singleplayer', 'Multiplayer - online - with strangers',
                                             'Multiplayer - online - with online acquaintances or teammates',
                                             'Multiplayer - online - with real life friends',
                                             'Multiplayer - offline (people in the same room)'), g2$Playstyle, 'Other')

# Check to see if the transformations worked
table(g2$motivation) 
table(g2$occupation) 
table(g2$styleofplay)

# Before exporting to CSV, change the first column to something more readable
g2 <- g2 %>%
  rename(playernum = S..No.)

colnames(g2)

# Export to CSV and load back into Tableau
write.csv(g2, file = "GamingStudy_data_v4.csv", row.names = FALSE)
# g2 <- read.csv('GamingStudy_data_v4.csv')

str(g2)

# Weekly_Hours histogram after removing outliers
# There are still outliers, but these add value to our analysis.
ggplot(g2, aes(y=Weekly_Hours)) +
  geom_boxplot(na.rm = FALSE)

# Histograms for each psychological assessment (GAD, SWL, SPIN)
# Check their distributions
p1 <- ggplot(g2, aes(x=GAD_T)) +
  geom_histogram(binwidth = 5, color = 'black', fill = 'white') +
  labs(title = 'GAD Results', x='', y='') +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = .5))

p2 <- ggplot(g2, aes(x=SWL_T)) +
  geom_histogram(binwidth = 5, color = 'black', fill = 'white') +
  labs(title = 'SWL Results', x='', y='') +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = .5))

p3 <- ggplot(g2, aes(x=SPIN_T)) +
  geom_histogram(binwidth = 5, color = 'black', fill = 'white') +
  labs(title = 'SPIN Results', x='', y='') +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = .5))

grid1 <- grid.arrange(p1,p2,p3,ncol = 2)

# Add a new column, just to check correlations
g2$danger <- ifelse(g2$GAD_T > 5 & g2$SWL_T < 20 & g2$SPIN_T > 20, 1, 0)

# Create a subset of data with no missing values
c1 <- g2[!rowSums(is.na(g2[c('Weekly_Hours','GAD_T','SWL_T','SPIN_T','danger')])), c('Weekly_Hours','GAD_T','SWL_T','SPIN_T','danger')]

# Correlation plot to view correlations between Weekly_Hours, GAD_T, SWL_T, SPIN_T, and the new variable danger
corrplot(cor(c1), method = 'number', type = 'upper', is.corr = FALSE)


### Data Preparation Using the most recent Gaming Anxiety Data version

# Import dataset and read summary
d1 <- read.csv('GamingStudy_data_v4.csv')
summary(d1)
str(d1)

# Transform occupation once more to fill NA's with unknown
d1$occupation <- ifelse(d1$occupation %in% 'Employed', 'Employed',
                        ifelse(d1$occupation %in% 'Student', 'Student',
                               ifelse(d1$occupation %in% 'Unemployed', 'Unemployed', 'Unknown')))


# Change some of the character variables to factor
names(d1)
nams <- c("Degree", "motivation", "occupation")
d1[,nams] <- lapply(d1[,nams], factor)

# Create a subset
d2 <- d1[,c(4,6,8,11,12,13,15,16)]
summary(d2)

# Remove NA's from test totals, but use mean imputation for SPIN_T
d2 <- d2 %>% drop_na(GAD_T)
d2$SPIN_T[is.na(d2$SPIN_T)] <- mean(d2$SPIN_T, na.rm = TRUE)

# Plot the test totals to check outliers
p1 <- ggplot(d2, aes(y=GAD_T)) +
  geom_boxplot(position = 'dodge2')

p2 <- ggplot(d2, aes(y=SWL_T)) +
  geom_boxplot(position = 'dodge2')

p3 <- ggplot(d2, aes(y=SPIN_T)) +
  geom_boxplot(position = 'dodge2')

grid.arrange(p1,p2,p3,ncol=2)

# Check summaries to get an even closer look
summary(d2$GAD_T)
summary(d2$SWL_T)
summary(d2$SPIN_T)

# Create function to normalize test totals
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the test totals
nams <- c('GAD_T','SWL_T','SPIN_T')
d2[,nams] <- lapply(d2[,nams], normalize)
rm(nams)

# Create two more subsets of the data.
# These are what we will model on.
d3 <- d2[,c('occupation','GAD_T','SWL_T','SPIN_T')]
d4 <- sqldf("SELECT *
            FROM d3 d
            WHERE d.occupation NOT IN ('Student','Unknown')")

d4$occupation <-  droplevels(d4$occupation) # Drop levels to keep it at two (Employed/Unemployed)
str(d4)
summary(d4)

# Create a dummy dataframe to test the current theory
# This may be contributing to confirmation bias; however,
# the results will still provide value.
a <- data.frame(GAD_T=c(0.5555,0.6666,0.7777,0.5555),SWL_T=c(0.3333,0.3333,0.3333,0.4444),SPIN_T=c(0.5555,0.6666,0.7777,0.5555))

# Build K-Nearest Neighbors (KNN) Model
# Occupation will be used as the response variable due to the hypothesis (See top of the page)
set.seed(1234)
index <- createDataPartition(d3$occupation, p=0.7, list = FALSE)
train <- d3[index,]
test <- d3[-index,]

# Train a model
model_knn <- train(train[, 2:4], train[, 1], method='knn')

# Predict the labels of the test set
predictions<-predict(object=model_knn,test[,2:4])
pred2 <- predict(object=model_knn,a)

# Evaluate the predictions
table(predictions)
table(pred2) # Students make up a majority of the dataset

# Confusion matrix 
confusionMatrix(predictions,test[,1])


# Build SVM Model on subset of data that only has Employed and Unemployed players
set.seed(1234)
TrainingIndex <- createDataPartition(d4$occupation, p=0.7, list = FALSE)
TrainingSet <- d4[TrainingIndex,] # Training Set
TestingSet <- d4[-TrainingIndex,] # Test Set

# Build Training model
Model <- train(occupation ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(occupation ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$occupation)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$occupation)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$occupation)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Predict outcomes for dummy dataset
predA <- predict(Model,a)
print(predA) # Unemployed players typically show higher anxiety and social phobia, 
             # yet lower satisfaction with life than employed players.