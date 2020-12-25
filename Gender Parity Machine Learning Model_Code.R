# title: "Gender Parity Machine Learning Model_Code.R"
# author: "Erika Di Donato"
# date: "24/12/2020"

# The goal of this project is to create a machine learning model combining the WEF Gender Gap Data with other global indicators to better reveal why gender dispartities still exist.

# Download packages
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table",
                                          repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("lattice",
                                          repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("mice", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("ggcorrplot", 
                                     repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("Rcpp", 
                                     repos = "http://cran.us.r-project.org")

library(Rcpp)
library(tidyverse)
library(caret)
library(data.table)
library(randomForest)
library(lubridate)
library(corrplot)
library(ggplot2)
library(readxl)
library(dslabs)
library(knitr)
library(tinytex)
library(magrittr)
library(reshape2)
library(lattice)
library(mice)
library(ggcorrplot)
library(rpart.plot)




# Download 2013 Gender Gap Report
urlHum  <- "https://data.humdata.org/dataset/"
pathH1 <- "29f2f52f-a9c2-4ff9-a99e-42b894dc18e9/resource/"
pathH2 <- "a8fe8c72-1359-4ce3-b8cc-03d9e5e85875/download/"
fileGGG <- "table-3b-detailed-rankings-2013.csv"

Global_Gender_Gap_2013 <- paste0(urlHum, pathH1, pathH2, fileGGG) %>% read.csv()

rm(urlHum, pathH1, pathH2, fileGGG)

# Download UN Reports
urlRemoteUN <- "https://data.un.org/_Docs/SYB/CSV/"

filePop <-
  "SYB62_246_201907_Population%20Growth,%20Fertility%20and%20Mortality%20Indicators.csv"
Population_Indicators <- paste0(urlRemoteUN, filePop) %>%
  read.csv(skip = 1)

fileRat <- "SYB63_319_202009_Ratio%20of%20Girls%20to%20Boys%20in%20Education.csv"
Ratio_of_girls_to_boys_in_school <- paste0(urlRemoteUN, fileRat) %>%
  read.csv(skip = 1)

filePar <- "SYB63_317_202009_Seats%20held%20by%20Women%20in%20Parliament.csv"
Seats_held_by_women_in_parliament <- paste0(urlRemoteUN, filePar) %>%
  read.csv(skip = 1)

rm(urlRemoteUN, filePop, fileRat, filePar)


# Histogram of Population_Indicators
Population_Indicators %>%
  ggplot(aes(Year)) +
  geom_histogram(binwidth = 0.5, fill = I("dodgerblue"), col = I("black")) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Population Indicators") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of Ratio_of_girls_to_boys_in_school
Ratio_of_girls_to_boys_in_school %>%
  ggplot(aes(Year)) +
  geom_histogram(binwidth = 0.5, fill = I("orange"), col = I("black")) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Ratio of Girls to Boys in School") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of Women_in_parliament
Seats_held_by_women_in_parliament %>%
  ggplot(aes(Year)) +
  geom_histogram(binwidth = 0.5, fill = I("red"), col = I("black")) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Women in Parliament") +
  theme(plot.title = element_text(hjust = 0.5))

# Population Indicators reformatting
PopInd <- Population_Indicators[, -c(1, 6, 7)] %>% filter(Year >= 2010 & Year <= 2015)
PopIndMean <- aggregate(x = PopInd$Value,
                        by = list(PopInd$X,PopInd$Series),
                        FUN = mean)
PopT <- dcast(PopIndMean,Group.1 ~ Group.2,value.var = "x") %>% rename(Country = Group.1)

# Ratio_of_girls_to_boys_in_school reformatting
RatioGtB <- Ratio_of_girls_to_boys_in_school[, -c(1, 6, 7)] %>% 
  filter(Year >= 2010 & Year <= 2015)
RatioGtBMean <- aggregate(x = RatioGtB$Value,
                          by = list(RatioGtB$X,RatioGtB$Series),
                          FUN = mean)
RatioGtBT <- dcast(RatioGtBMean,Group.1 ~ Group.2,value.var = "x") %>% 
  rename(Country = Group.1)

# Seats_held_by_women_in_parliament reformatting
WomnInParl <- Seats_held_by_women_in_parliament[, c(2, 3, 4, 7)] %>% 
  filter(Year >= 2010 & Year <= 2015)
WomnInParlMean <- aggregate(x = WomnInParl$Value,
                            by = list(WomnInParl$X,WomnInParl$Series),
                            FUN = mean)
WomnInParlT <- dcast(WomnInParlMean,Group.1 ~ Group.2,value.var = "x") %>% 
  rename(Country = Group.1)

# Combine the 3 UN tables
UNstatsA <- full_join(PopT, RatioGtBT, by = "Country")
UNstats <- full_join(UNstatsA, WomnInParlT, by = "Country")

# Synchronize Country names between the UN table and the Global_Gender_Gap_2013 report
CountryString <- as.character(UNstats$Country)
CountryString <- replace(CountryString, 7,"Angola*")
CountryString <- replace(CountryString, 27,"Bhutan*")
CountryString <- replace(CountryString, 28,"Bolivia")
CountryString <- replace(CountryString, 38,"Cape Verde")
CountryString <- replace(CountryString, 58,"C»te d'Ivoire")
CountryString <- replace(CountryString, 63,"Czech Republic")
CountryString <- replace(CountryString, 111,"Iran, Islamic Rep.")
CountryString <- replace(CountryString, 124,"Kyrgyz Republic")
CountryString <- replace(CountryString, 189,"Korea, Rep.")
CountryString <- replace(CountryString, 190,"Moldova")
CountryString <- replace(CountryString, 212,"Slovak Republic")
CountryString <- replace(CountryString, 232,"Syria")
CountryString <- replace(CountryString, 250,"Tanzania")
CountryString <- replace(CountryString, 251,"United States")
CountryString <- replace(CountryString, 256,"Venezuela")
CountryString <- replace(CountryString, 257,"Vietnam")

Country_df <- as_tibble(CountryString)
UNstats$Country <- as_factor(Country_df$value)

# Left-join new UN table to the Global_Gender_Gap_2013 report
GGG_2013 <- left_join(Global_Gender_Gap_2013, UNstats, by = "Country")

rm(Country_df, CountryString, Global_Gender_Gap_2013, PopInd, PopIndMean, PopT,
   Population_Indicators, Ratio_of_girls_to_boys_in_school, RatioGtB, RatioGtBMean,
   RatioGtBT, Seats_held_by_women_in_parliament, UNstats, UNstatsA, WomnInParl,
   WomnInParlMean, WomnInParlT)


##############################
## Process
##############################

# Validation set will be 20.6% of GGG_2013 data (p = .2 achieves this due to rounding)
set.seed(3, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(3)`
val_index <- createDataPartition(
  y = GGG_2013$Overall.Score, times = 1, p = 0.2, list = FALSE)
training_master <- GGG_2013[-val_index,]
temp <- GGG_2013[val_index,]

# Make sure Overall.Score in validation set are also in GGG_2013 set
validation <- temp %>% 
  semi_join(GGG_2013, by = "Overall.Score")

# Add rows removed from validation set back into GGG_2013 set
removed <- anti_join(temp, validation)
GGG_2013 <- rbind(GGG_2013, removed)

rm(val_index, temp, removed)

# Test set will be 20.6% of GGG_2013 data data (p = 0.25 of training_master achieves this)
set.seed(5, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(5)`
test_index <- createDataPartition(
  y = training_master$Overall.Score, times = 1, p = 0.25, list = FALSE)
train <- training_master[-test_index,]
temp <- training_master[test_index,]

# Make sure Overall.Score in validation set are also in training_master set
test <- temp %>% 
  semi_join(training_master, by = "Overall.Score")

# Add rows removed from vtest set back into training_master set
removed <- anti_join(temp, test)
training_master <- rbind(training_master, removed)

rm(test_index, temp, removed, GGG_2013, training_master)

##############################
### Data Cleaning
##############################

# Summary statistics
dim(train)
names(train)
anyNA(train)

# Remove columns that do not add insights and summarize remaining columns
train <- train[, -c(2, 3, 5, 7, 9, 11)]
summary(train)

# Rename columns
names(train) [2] <- "Score"
names(train)  [3] <- "EcoPart"
names(train)  [4] <- "Edu"
names(train)  [5] <- "Health"
names(train)  [6] <- "Poli"
names(train)  [7] <- "InfMortRt"
names(train)  [8] <- "LifExpBoth"
names(train)  [9] <- "LifExpFem"
names(train) [10] <- "LifExpMal"
names(train) [11] <- "MatMortRt"
names(train) [12] <- "PopIncRt"
names(train) [13] <- "TotFertRt"
names(train) [14] <- "RatGtBPrim"
names(train) [15] <- "RatGtBSec"
names(train) [16] <- "RatGtBTer"
names(train) [17] <- "WmnInParl"

# Divide MatMortRt by 10 (now reflects Deaths per 10000 of the population)
train$MatMortRt <-train$MatMortRt/10

# Create scatterplots and regression lines for Indicators with missing values
attach(train)
plot(RatGtBPrim, Score, main = "Score ~ RatGtBPrim",
   	xlab = "RatGtBPrim", ylab = "Score", pch = 19)
	abline(lm(Score ~ RatGtBPrim), col ="red") # regression line (y~x)
plot(RatGtBSec, Score, main = "Score ~ RatGtBSec",
   	xlab = "RatGtBSec", ylab = "Score", pch = 19)
	abline(lm(Score ~ RatGtBSec), col ="red") # regression line (y~x)
plot(RatGtBTer, Score, main = "Score ~ RatGtBTer",
   	xlab = "RatGtBTer", ylab = "Score", pch = 19)
	abline(lm(Score ~ RatGtBTer), col ="red") # regression line (y~x)
plot(WmnInParl, Score, main = "Score ~ WmnInParl",
   	xlab = "WmnInParl", ylab = "Score", pch = 19)
	abline(lm(Score ~ WmnInParl), col ="red") # regression line (y~x)

	
# Stochastic regression imputation for NA's
imp <- mice(train, method = "norm", m = 1) # Impute data
train <- complete(imp) # Store data

# Check for any missing values
anyNA(train)

##############################
### Data Exploration and Visualizations
##############################

# Review new table statistics
summary(train)

# Review boxplots
boxplot(train[,2:6], col = c(
  "darkolivegreen1","darkolivegreen4","darkolivegreen1","darkolivegreen4",
  "darkolivegreen1"))
mtext(1, text = "UN Data", line = 4.5)
boxplot(train[,c(7, 11, 17)],  col = c(
  "darkolivegreen1","darkolivegreen4","darkolivegreen1"))
mtext(1, text = "Mortality Rates and Political Power", line = 4.5)
boxplot(train[,8:10], col = c(
  "darkolivegreen1","darkolivegreen4","darkolivegreen1"))
mtext(1, text = "Life Expectancy", line = 4.5)
boxplot(train[,c(12, 13)], col = c(
  "darkolivegreen1","darkolivegreen4","darkolivegreen1"))
mtext(1, text = "Population Growth and Fertility Rates", line = 4.5)
boxplot(train[,14:16], col = c(
  "darkolivegreen1","darkolivegreen4","darkolivegreen1"))
mtext(1, text = "Ratio of Girls to Boys in School", line = 4.5)


# Create correlation matrix: cordata
cordata = train[,c(3:17)]
corr <- round(cor(cordata), 3)
corr


# Create Correlation Matrix
M <- cor(corr)
corrplot(M, method = "pie", order = "AOE")

# Plot correlated data
plot(Edu, RatGtBSec, data = train$full)
plot(InfMortRt, MatMortRt, data = train$full)
plot(InfMortRt, TotFertRt, data = train$full)
plot(LifExpBoth, LifExpFem, data = train$full)
plot(LifExpBoth, LifExpMal, data = train$full)
plot(LifExpFem, LifExpMal, data = train$full)
plot(MatMortRt, TotFertRt, data = train$full)
plot(InfMortRt, LifExpBoth, data = train$full)
plot(InfMortRt, LifExpFem, data = train$full)
plot(InfMortRt, LifExpMal, data = train$full)
plot(LifExpBoth, MatMortRt, data = train$full)
plot(LifExpBoth, TotFertRt, data = train$full)
plot(LifExpFem, MatMortRt, data = train$full)
plot(LifExpFem, TotFertRt, data = train$full)


# Remove the Country column
train_noCountry <- train[, -1]

# Run the test for heteroskedasticity and plot the residuals
train_noCountry_ols <- lm(Score ~., data = train_noCountry)
train_resi <- train_noCountry_ols$residuals
ggplot(data = train_noCountry, aes(y = Score, x = train_resi)) + 
  geom_point(col = 'blue') + geom_abline(slope = 0) +
  ggtitle("Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

rm(train_noCountry_ols, train_resi)


##############################
## Modelling
##############################

# Duplicate transfortmations on test set
# Remove columns 
test <- test[, -c(2, 3, 5, 7, 9, 11)]
# Rename columns
names(test) [2] <- "Score"
names(test)  [3] <- "EcoPart"
names(test)  [4] <- "Edu"
names(test)  [5] <- "Health"
names(test)  [6] <- "Poli"
names(test)  [7] <- "InfMortRt"
names(test)  [8] <- "LifExpBoth"
names(test)  [9] <- "LifExpFem"
names(test) [10] <- "LifExpMal"
names(test) [11] <- "MatMortRt"
names(test) [12] <- "PopIncRt"
names(test) [13] <- "TotFertRt"
names(test) [14] <- "RatGtBPrim"
names(test) [15] <- "RatGtBSec"
names(test) [16] <- "RatGtBTer"
names(test) [17] <- "WmnInParl"
# Divide MatMortRt by 10
test$MatMortRt <- test$MatMortRt/10
# Stochastic regression imputation for NA's
imp_test <- mice(test, method = "norm", m = 1) # Impute data
test <- complete(imp_test) # Store data

# Remove country 
test_noCountry <- test[, -1]


# Define RMSE as our evaluation criteria
RMSE <- function(true_Score, predicted_Score){
  sqrt(mean((true_Score - predicted_Score)^2))
}

# Define mu as the Score average
mu <- mean(train_noCountry$Score)
mu

rm(imp_test)


# Naive Forecast Based on Mean Rating
RMSE_naive <- RMSE(test_noCountry$Score, mu)

# Save results in a tibble
RMSE_results = tibble(Method = "Naive Forecast", RMSE = RMSE_naive)
RMSE_results <- RMSE_results %>% mutate_if(is.numeric, ~ round(.,digits = 8))
# Return model RMSE results
RMSE_results


# Define the Linear Model
train_lin <- train_noCountry

linModel_formula = Score ~ .
linModel <- lm(linModel_formula, data = train_lin)

# Observe the coeffficients
LinCoefficient <- round(summary(linModel)$coefficients, 5)
LinCoefficient


# Define the Linear Model with Dimension Reduction
train_lin <- train_noCountry
linModel_formula = Score ~ EcoPart + Edu + Health + Poli
linModel <- lm(linModel_formula, data = train_lin)

# Observe the coeffficients
LinCoefficient <- round(summary(linModel)$coefficients, 5)
LinCoefficient


## Note: this is a Trivial Solution


# Linear Model Forecast
predicted_Score <- predict(linModel, test_noCountry)

RMSE_model_LinearT <- RMSE(predicted_Score, test_noCountry$Score)

# Save results in a tibble
RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "Full Linear Model (Trivial Solution)",
                                 RMSE = RMSE_model_LinearT))

# Return model RMSE results
RMSE_results <- RMSE_results %>% mutate_if(is.numeric, ~ round(.,digits = 8))
RMSE_results


# Define the new Linear Model
train_lin <- train_noCountry[, c(1, 6:16)]
linModel_formula = Score ~.
linModel <- lm(linModel_formula, data = train_lin)

# Observe the coeffficients
LinCoefficient <- round(summary(linModel)$coefficients, 5)
LinCoefficient


# Define the new Linear Model with Dimension Reduction
train_lin <- train_noCountry[, c(1, 6:16)]
linModel_formula = Score ~ LifExpBoth + PopIncRt + RatGtBPrim + RatGtBTer
linModel <- lm(linModel_formula, data = test_noCountry)

# Observe the coeffficients
LinCoefficient <- round(summary(linModel)$coefficients, 5)
LinCoefficient


# New Linear Model Forecast
predicted_Score <- predict(linModel, test_noCountry)

RMSE_model_LinearUN <- RMSE(predicted_Score, test_noCountry$Score)

# Save results in a tibble
RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "Reduced Linear Model (UN Data)",
                                 RMSE = RMSE_model_LinearUN))

# Return model RMSE results
RMSE_results <- RMSE_results %>% mutate_if(is.numeric, ~ round(.,digits = 8))
RMSE_results


# rpart Training
train_rpart <- train_noCountry
test_rpart <- test_noCountry
set.seed(100)
rpartMod <- train(Score ~ ., data = train_rpart, method = "rpart")
rpartImp <- varImp(rpartMod)
print(rpartImp)
rpart.plot(rpartMod$finalModel)


# rpart Model Forecast
predicted_Score <- predict(rpartMod$finalModel, test_rpart)

RMSE_model_rpart <- RMSE(predicted_Score, test_rpart$Score)

# Save results in a tibble
RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "rpart Model",
                                 RMSE = RMSE_model_rpart))

# Return model RMSE results
RMSE_results <- RMSE_results %>% mutate_if(is.numeric, ~ round(.,digits = 8))
RMSE_results


# Define Random Forest Model
set.seed(7, sample.kind = "Rounding")

fit_rtree <- train(Score ~., data = train_noCountry, method = "rf",
                   tuneGrid = data.frame(mtry = seq(1:15)), importance = TRUE)
print(fit_rtree)

imp <- varImp(fit_rtree)
imp

plot(fit_rtree$results$mtry, fit_rtree$results$RMSE, main = "RMSE-minimizing mtry")


# Tune and Test Random Forest Model
fit_rtree2 <- train(Score ~., data = train_noCountry, method = "rf",
                    tuneGrid = data.frame(mtry = 10), importance = TRUE)

predicted_Score <- predict(fit_rtree2, test_noCountry)


RMSE_model_rtree2 <- RMSE(predicted_Score, test$Score)

# Save results in a tibble
RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "Random Forest Model",
                                 RMSE = RMSE_model_rtree2))

# Return model RMSE results
RMSE_results <- RMSE_results %>% mutate_if(is.numeric, ~ round(.,digits = 8))
RMSE_results


##############################
## Model Results
##############################

# Duplicate transfortmations on validation set
# Remove columns 
validation <- validation[, -c(2, 3, 5, 7, 9, 11)]
# Rename columns
names(validation) [2] <- "Score"
names(validation)  [3] <- "EcoPart"
names(validation)  [4] <- "Edu"
names(validation)  [5] <- "Health"
names(validation)  [6] <- "Poli"
names(validation)  [7] <- "InfMortRt"
names(validation)  [8] <- "LifExpBoth"
names(validation)  [9] <- "LifExpFem"
names(validation) [10] <- "LifExpMal"
names(validation) [11] <- "MatMortRt"
names(validation) [12] <- "PopIncRt"
names(validation) [13] <- "TotFertRt"
names(validation) [14] <- "RatGtBPrim"
names(validation) [15] <- "RatGtBSec"
names(validation) [16] <- "RatGtBTer"
names(validation) [17] <- "WmnInParl"
# Divide MatMortRt by 10
test$MatMortRt <- test$MatMortRt/10
# Stochastic regression imputation for NA's
imp_validation <- mice(validation, method = "norm", m = 1) # Impute data
validation <- complete(imp_validation) # Store data


# Random Forest Validation
predicted_Score <- predict(fit_rtree2, validation)


Final_RMSE_model_rtree2 <- RMSE(predicted_Score, validation$Score)

# Save results in a tibble
RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "Final Random Forest Model Validation",
                          RMSE = Final_RMSE_model_rtree2))

##############################
## Model Performance
##############################

# Return Final RMSE results
RMSE_results <- RMSE_results %>% mutate_if(is.numeric, ~ round(.,digits = 8))
RMSE_results
Final_RMSE_model_rtree2

