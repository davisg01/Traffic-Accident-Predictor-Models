cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
require(data.table)

#library imports 
library(dplyr)
library(stargazer)
library(ggplot2)
library(bit64)
library(nnet)
library(rpart)
library(rpart.plot)
library(tree)
library(PerformanceAnalytics)
library(MASS)
library(randomForest)
library(rsq)
library(Hmisc)
library(corrplot)
library(pscl)
library(groupdata2)


## change this to your own import and names
veh <- fread("C:/Users/Davis/Downloads/veh_prestine.csv")
per <- fread("C:/Users/Davis/Downloads/pers_prestine.csv")

# assigning new unique primary code for merging 
veh <- veh %>% 
  mutate(pk = row_number())

per <- per %>% 
  mutate(pk = row_number())

merged_data <- distinct(merge(veh, per, by = "pk"))

# cleaning speeding variable 
merged_data$DRUGS[merged_data$DRUGS == 8] <- 0
merged_data$SPEEDREL[merged_data$SPEEDREL == 2] <- 0
merged_data$SPEEDREL[merged_data$SPEEDREL == 3] <- 1
merged_data$SPEEDREL[merged_data$SPEEDREL == 4] <- 1
merged_data$SPEEDREL[merged_data$SPEEDREL == 5] <- 1

## this removes any NA's in the dataset 
merged_data <- na.omit(merged_data)

## this removes vehicle body types that are uncommon on the road 

cleaned_data <- merged_data %>%
  filter(!TOWED %in% c(8, 9) & !DRUGS %in% c(9) & !SPEEDREL %in% c(9) & !AIR_BAG %in% c(98, 99) & !DEFORMED %in% c(8, 9)
         & !EJECT_IM %in% c(8) & !MXVSEV_IM %in% c(6, 9) & !HOUR %in% c(99)
         & !VPICBODYCLASS %in% c(4,6,10,12,16,62,63,64,65,66,67,69,70,71,72,73,74,75,77,78,80,81,
                                 82,83,84,85,86,87,90,94,97,98,103,104,105,107,108,110,111,112,113,114,117,119,
                                 124,125,126,127,996,997,998,999))



# Merge similar vehicle body types together 
cleaned_data$VPICBODYCLASS[cleaned_data$VPICBODYCLASS %in% c(9, 95)] <- 2
cleaned_data$VPICBODYCLASS[cleaned_data$VPICBODYCLASS == 7] <- 4
cleaned_data$VPICBODYCLASS[cleaned_data$VPICBODYCLASS == 8] <- 4
cleaned_data$VPICBODYCLASS[cleaned_data$VPICBODYCLASS == 11] <- 6
cleaned_data$VPICBODYCLASS[cleaned_data$VPICBODYCLASS == 13] <- 7
cleaned_data$VPICBODYCLASS[cleaned_data$VPICBODYCLASS == 15] <- 8
cleaned_data$VPICBODYCLASS[cleaned_data$VPICBODYCLASS == 60] <- 9

#Categorizes 24 hours into 6 categories 

cleaned_data$HOUR[cleaned_data$HOUR %in% c(0,1,2,3)] <- 1
cleaned_data$HOUR[cleaned_data$HOUR %in% c(4,5,6,7)] <- 2
cleaned_data$HOUR[cleaned_data$HOUR %in% c(8,9,10,11)] <- 3
cleaned_data$HOUR[cleaned_data$HOUR %in% c(12,13,14,15)] <- 4
cleaned_data$HOUR[cleaned_data$HOUR %in% c(16,17,18,19)] <- 5
cleaned_data$HOUR[cleaned_data$HOUR %in% c(20,21,22,23)] <- 6

############# BELOW IS USED TO FIX THE CORRELATION, THIS ISNT RUN BECAUSE WE ALREADY ASSIGNED THE NUMBERS TO MAKE OUR NEW SEVERITY VARIABLE

#cleaned_data$TOWED[cleaned_data$TOWED == 5] <- 0
#cleaned_data$TOWED[cleaned_data$TOWED == 3] <- 1

#cleaned_data$AIR_BAG[cleaned_data$AIR_BAG == 20] <- 0
#cleaned_data$AIR_BAG[cleaned_data$AIR_BAG %in% c(1,2,3,7,8,9)] <- 1

#cleaned_data$V_ALCH_IM[cleaned_data$V_ALCH_IM == 2] <- 0

# Create a subset of the data with only the numeric variables of interest
#relevant_data <- cleaned_data[, c("VPICBODYCLASS", "DRUGS", "SPEEDREL", "V_ALCH_IM", "MXVSEV_IM", "ROLLOVER", "TOWED", "DEFORMED", "AIR_BAG", "EJECT_IM", "HOUR" )]

# Calculate the correlation matrix
#correlation_matrix <- cor(relevant_data)

# Print the correlation matrix
#print(correlation_matrix)


# Create a graphical representation of the correlation matrix
#corrplot(correlation_matrix, method = "color", addCoef.col = "black", type = "upper", diag = FALSE, tl.col = "black", tl.srt = 45)

################## H1 #####################################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

H1 <- cleaned_data[,c("VPICBODYCLASS","DRUGS","SPEEDREL","V_ALCH_IM","AGE_IM","URBANICITY"
                      ,"VPICMAKE","SEX_IM","VPICMAKENAME")]

# downsampling for balanced dataset 
H1 <- downsample(H1, cat_col = "VPICBODYCLASS")

H1$VPICBODYCLASS <- as.factor(H1$VPICBODYCLASS)
H1$VPICMAKE <- as.factor(H1$VPICMAKE)
H1$VPICMAKENAME <- as.factor(H1$VPICMAKENAME)

# shuffling the data 
shuffle_index <-sample(1:nrow(H1))
H1 <- H1[shuffle_index,]
n_cut <- round(nrow(H1)*.7,0)
h1data_train <- H1[1:n_cut,]
h1data_test <- H1[(n_cut+1):nrow(H1),]

## this has AIC of 250,326
#h1mglm <- glm(VPICBODYCLASS~DRUGS+SPEEDREL+V_ALCH_IM, data = h1data_train)
#summary(h1mglm)

## ordinal logistic regression model 
h1model <- polr(VPICBODYCLASS~DRUGS+SPEEDREL+V_ALCH_IM, data = h1data_train, Hess = TRUE)
summary(h1model)

## multinomial logistic regression model 
h1multi <- multinom(VPICBODYCLASS~DRUGS+SPEEDREL+V_ALCH_IM, data = h1data_train)
summary(h1multi)
h1data2 <- data.frame(DRUGS = 1, SPEEDREL = 1, V_ALCH_IM = 1)
h1prob2 <- predict(h1multi, newdata = h1data2, type = "prob")
h1prob2


pR2(h1model)
pR2(h1multi)

############################
## more variables added to lower AIC 
h1multi2 <- multinom(VPICBODYCLASS~DRUGS+SPEEDREL+V_ALCH_IM+AGE_IM+SPEEDREL*URBANICITY+SEX_IM, data = h1data_train)
summary(h1multi2)


h1data <- data.frame(DRUGS = 1, SPEEDREL = 1, V_ALCH_IM = 1, AGE_IM = 30, URBANICITY = 1,SEX_IM = 1)
h1prob <- predict(h1multi2, newdata = h1data, type = "prob")
h1prob

#confusion matrix
h1predict <- predict(h1multi2, h1data_test, type = 'class')
h1confmatrix <- table(h1data_test$VPICBODYCLASS, h1predict)
h1confmatrix

h1accuracy_test <- sum(diag(h1confmatrix)) / sum(h1confmatrix)
print(paste('Accuracy for test:', h1accuracy_test))

######## for car brand 
h1multi3 <- multinom(VPICMAKENAME~DRUGS+SPEEDREL+V_ALCH_IM+AGE_IM+SPEEDREL*URBANICITY+SEX_IM, data = h1data_train)
summary(h1multi3)

h1data3 <- data.frame(DRUGS = 0, SPEEDREL = 0, V_ALCH_IM = 2, AGE_IM = 70, URBANICITY = 1,SEX_IM =2)
probnew2 <- predict(h1multi3, newdata = h1data3, type = "prob")
probnew2

################## H2 #####################################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################


H2 <- cleaned_data[, c("VPICBODYCLASS", "MXVSEV_IM", "ROLLOVER", "TOWED", "DEFORMED", "EJECT_IM", "AIR_BAG")]
#convert VPICBODAYCLASS to factor variable
H2$VPICBODYCLASS <- as.factor(H2$VPICBODYCLASS)

#calculate severity score, then create a column “severity_score” in H2 dataset
H2 <- H2 %>%
  mutate(
    Severity_Score = case_when(
      (MXVSEV_IM == 0 & ROLLOVER == 0 & TOWED != 2 & DEFORMED == 0 & EJECT_IM == 0 & AIR_BAG == 20) ~ 0,
      (MXVSEV_IM <= 1 & ROLLOVER <= 1 & TOWED != 2 & DEFORMED <= 2 & EJECT_IM <= 1 & AIR_BAG == 20) ~ 1,
      (MXVSEV_IM <= 2 & ROLLOVER <= 2 & TOWED <= 3 & DEFORMED <= 4 & EJECT_IM <= 2) ~ 2,
      (MXVSEV_IM <= 3 & ROLLOVER <= 9 & TOWED <= 7 & DEFORMED <= 6 & EJECT_IM <= 3) ~ 3,
      TRUE ~ 4
    )
  )
#convert Severity_Score to a factor variable because this is also categorical 
H2$Severity_Score <- as.factor(H2$Severity_Score)
table(H2$VPICBODYCLASS, H2$Severity_Score)


H2$Convertible <- ifelse(H2$VPICBODYCLASS == 1, 1, 0)
H2$Van <- ifelse(H2$VPICBODYCLASS == 2, 1, 0)
H2$Coupe <- ifelse(H2$VPICBODYCLASS == 3, 1, 0)
H2$Hatchback <- ifelse(H2$VPICBODYCLASS == 5, 1, 0)
H2$SUV <- ifelse(H2$VPICBODYCLASS == 4, 1, 0)
H2$Truck <- ifelse(H2$VPICBODYCLASS == 6, 1, 0)
H2$Sedan <- ifelse(H2$VPICBODYCLASS == 7, 1, 0)
H2$Wagon <- ifelse(H2$VPICBODYCLASS == 8, 1, 0)
H2$Pickup <- ifelse(H2$VPICBODYCLASS == 9, 1, 0)



##########
H2$Severity_Level_0 <- ifelse(H2$Severity_Score == 0, 1, 0)
H2$Severity_Level_1 <- ifelse(H2$Severity_Score == 1, 1, 0)
H2$Severity_Level_2 <- ifelse(H2$Severity_Score == 2, 1, 0)
H2$Severity_Level_3 <- ifelse(H2$Severity_Score == 3, 1, 0)
H2$Severity_Level_4 <- ifelse(H2$Severity_Score == 4, 1, 0)


###
shuffle_index <- sample(1:nrow(H2))
H2 <- H2[shuffle_index,]


n_cut <- round(nrow(H2)*.7,0)
h2data_train <- H2[1:n_cut,]
h2data_test <- H2[(n_cut+1):nrow(H2),]


#############CART0###############
# Convert Severity_Level_0 to a factor
h2data_train$Severity_Level_0 <- as.factor(h2data_train$Severity_Level_0)
h2data_test$Severity_Level_0 <- as.factor(h2data_test$Severity_Level_0)

# Feature engineering
h2data_train$NewFeature <- h2data_train$Convertible + h2data_train$SUV
h2data_test$NewFeature <- h2data_test$Convertible + h2data_test$SUV

# Define the training control
train_control <- trainControl(method = "cv", number = 5)


library(rpart.plot)
library(rpart)
class_weights <- ifelse(h2data_train$Severity_Level_0 == 0, 1, sum(h2data_train$Severity_Level_0 == 0) / sum(h2data_train$Severity_Level_0 == 1))

# Fit the weighted CART model
CART_model1_weighted <- rpart(Severity_Level_0 ~ Convertible + Van + Coupe + Hatchback + SUV + Truck + Sedan + Wagon + Pickup, data = h2data_train, cp = -1, weights = class_weights)
rpart.plot(CART_model1_weighted,type = 3, digits = 3)
summary(CART_model1_weighted)

any(is.na(h2data_test))
all(names(h2data_test) == names(h2data_train))

# Print model object
print(CART_model1_weighted)


# Make predictions on the test data
predicted_unseen_weighted <- predict(CART_model1_weighted, h2data_test, type = "class")

# Calculate confusion matrix
ConfMatrix_weighted <- table(h2data_test$Severity_Level_0, predicted_unseen_weighted)
ConfMatrix_weighted


accuracy_Test <- sum(diag(ConfMatrix_weighted)) / sum(ConfMatrix_weighted)
print(paste('Accuracy for test:', accuracy_Test))
#################################################

###############CART4################################
# Convert Severity_Level_4 to a factor
h2data_train$Severity_Level_4 <- as.factor(h2data_train$Severity_Level_4)
h2data_test$Severity_Level_4 <- as.factor(h2data_test$Severity_Level_4)

# Feature engineering
h2data_train$NewFeature <- h2data_train$Convertible + h2data_train$SUV
h2data_test$NewFeature <- h2data_test$Convertible + h2data_test$SUV

# Define the training control
train_control <- trainControl(method = "cv", number = 5)

####################################CART4
library(rpart)
library(rpart.plot)

class_weights4 <- ifelse(h2data_train$Severity_Level_4 == 0, 1, sum(h2data_train$Severity_Level_4 == 0) / sum(h2data_train$Severity_Level_4 == 1))
# Fit the weighted CART model
CART_model4_weighted <- rpart(Severity_Level_4 ~ Convertible + Van + Coupe + Hatchback + SUV + Truck + Sedan + Wagon + Pickup, data = h2data_train, cp = -1, weights = class_weights4)
rpart.plot(CART_model4_weighted,type = 3, digits = 3)

summary(CART_model4_weighted)

any(is.na(h2data_test))
all(names(h2data_test) == names(h2data_train))

# Print model object
print(CART_model4_weighted)


# Make predictions on the test data
predicted_unseen_weighted4 <- predict(CART_model4_weighted, h2data_test, type = "class")

# Calculate confusion matrix
ConfMatrix_weighted4 <- table(h2data_test$Severity_Level_4, predicted_unseen_weighted4)
ConfMatrix_weighted4

accuracy_Test <- sum(diag(ConfMatrix_weighted4)) / sum(ConfMatrix_weighted4)
print(paste('Accuracy for test:', accuracy_Test))

################## H3 #####################################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
H3 <- cleaned_data[, c("HOUR", "MXVSEV_IM", "ROLLOVER", "TOWED", "DEFORMED", "EJECT_IM", "AIR_BAG")]

H3 <- downsample(H3, cat_col = "HOUR")

H3$HOUR <- as.factor(H3$HOUR)


#calculate severity score, then create a column “severity_score” in H2 dataset
H3 <- H3 %>%
  mutate(
    Severity_Score = case_when(
      (MXVSEV_IM == 0 & ROLLOVER == 0 & TOWED != 2 & DEFORMED == 0 & EJECT_IM == 0 & AIR_BAG == 20) ~ 0,
      (MXVSEV_IM <= 1 & ROLLOVER <= 1 & TOWED != 2 & DEFORMED <= 2 & EJECT_IM <= 1 & AIR_BAG == 20) ~ 1,
      (MXVSEV_IM <= 2 & ROLLOVER <= 2 & TOWED <= 3 & DEFORMED <= 4 & EJECT_IM <= 2) ~ 2,
      (MXVSEV_IM <= 3 & ROLLOVER <= 9 & TOWED <= 7 & DEFORMED <= 6 & EJECT_IM <= 3) ~ 3,
      TRUE ~ 4
    )
  )
H3$Severity_Score <- as.factor(H3$Severity_Score)
#polr
h3model <- polr(HOUR~ Severity_Score, data = H3, Hess = TRUE)
summary(h3model)

#multi
h3multi <- multinom(HOUR~Severity_Score, data = H3)
summary(h3multi)



#pseudoR2 of two models
pR2(h3model)
pR2(h3multi)

# p-value matrix for multinominal model
# Get the coefficients and standard errors from the summary
coefficients <- summary(h3multi)$coefficients
std_errors <- summary(h3multi)$standard.errors
# Calculate z-scores
z_scores <- coefficients / std_errors
# Obtain p-values from z-scores
p_values <- 2 * (1 - pnorm(abs(z_scores), 0, 1))
# Display p-values
p_values


# Calculate predicted probabilities for each HOUR category
predicted_probs <- predict(h3multi, newdata = H3, type = "probs")
# Calculate the average predicted probability for each HOUR category
average_probs <- colMeans(predicted_probs)
# Display the average probabilities for each HOUR category
average_probs


# Create a data frame with all possible combinations of Severity_Score
new_data <- expand.grid(Severity_Score = unique(H3$Severity_Score))
# Calculate predicted probabilities for each HOUR category and Severity_Score level
predicted_probs_by_severity <- predict(h3multi, newdata = new_data, type = "probs")
# Combine the new_data and predicted probabilities
results <- cbind(new_data, predicted_probs_by_severity)
# Display the results
results

#visualize the probability of severity for HOUR
# Melt the data frame to a long format
library(reshape2)
long_results <- melt(results, id.vars = "Severity_Score", variable.name = "HOUR", value.name = "probability")
# Plot the heatmap
library(ggplot2)
ggplot(long_results, aes(x = HOUR, y = Severity_Score, fill = probability)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "blue", high = "red", midpoint = median(long_results$probability)) +
  theme_minimal() +
  labs(title = "Probability of Severity Score by Hour", x = "Hour", y = "Severity Score")




