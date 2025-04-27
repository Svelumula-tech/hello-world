library(mlbench)
library(purrr)

data("PimaIndiansDiabetes2")
ds <- as.data.frame(na.omit(PimaIndiansDiabetes2))
## fit a logistic regression model to obtain a parametric equation
logmodel <- glm(diabetes ~ .,
                data = ds,
                family = "binomial")
summary(logmodel)

cfs <- coefficients(logmodel) ## extract the coefficients
prednames <- variable.names(ds)[-9] ## fetch the names of predictors in a vector
prednames

sz <- 100000000 ## to be used in sampling
##sample(ds$pregnant, size = sz, replace = T)

dfdata <- map_dfc(prednames,
                  function(nm){ ## function to create a sample-with-replacement for each pred.
                    eval(parse(text = paste0("sample(ds$",nm,
                                             ", size = sz, replace = T)")))
                  }) ## map the sample-generator on to the vector of predictors
## and combine them into a dataframe

names(dfdata) <- prednames
dfdata

class(cfs[2:length(cfs)])

length(cfs)
length(prednames)
## Next, compute the logit values
pvec <- map((1:8),
            function(pnum){
              cfs[pnum+1] * eval(parse(text = paste0("dfdata$",
                                                     prednames[pnum])))
            }) %>% ## create beta[i] * x[i]
  reduce(`+`) + ## sum(beta[i] * x[i])
  cfs[1] ## add the intercept

## exponentiate the logit to obtain probability values of thee outcome variable
dfdata$outcome <- ifelse(1/(1 + exp(-(pvec))) > 0.5,
                         1, 0)


##XGBoost Direct:
library(xgboost)
library(dplyr)

set.seed(123)

# Define dataset sizes
sizes <- c(100, 1000, 10000, 100000, 1000000)

# Initialize a data frame to store results
results <- data.frame(
  Dataset_Size = integer(),
  Accuracy = numeric(),
  Time_Taken_Sec = numeric()
)

# Loop over each size
for (sz in sizes) {
  
  # Sample data
  idx <- sample(1:nrow(dfdata), sz)
  tempdata <- dfdata[idx, ]
  
  # Split into train and test (80/20)
  train_idx <- sample(1:nrow(tempdata), 0.8 * nrow(tempdata))
  train <- tempdata[train_idx, ]
  test <- tempdata[-train_idx, ]
  
  # Prepare matrices for xgboost
  dtrain <- xgb.DMatrix(data = as.matrix(select(train, -outcome)), label = train$outcome)
  dtest <- xgb.DMatrix(data = as.matrix(select(test, -outcome)), label = test$outcome)
  
  # Train the model with simple 5-fold cross-validation
  start_time <- Sys.time()
  
  model <- xgboost(
    data = dtrain,
    objective = "binary:logistic",
    nrounds = 50,
    max_depth = 3,
    eta = 0.1,
    verbose = 0
  )
  
  end_time <- Sys.time()
  
  # Make predictions
  preds <- predict(model, dtest)
  preds_class <- ifelse(preds > 0.5, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(preds_class == test$outcome)
  
  # Store the results
  results <- rbind(results, data.frame(
    Dataset_Size = sz,
    Accuracy = round(accuracy, 4),
    Time_Taken_Sec = round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
  ))
}

results


## XGBoost with caret
library(caret)
library(xgboost)
library(dplyr)

set.seed(123)

# Define dataset sizes
sizes <- c(100, 1000, 10000, 100000)

# Initialize results data frame
results_caret <- data.frame(
  Dataset_Size = integer(),
  Accuracy = numeric(),
  Time_Taken_Sec = numeric()
)

# Loop over each size
for (sz in sizes) {
  
  # Sample the data
  idx <- sample(1:nrow(dfdata), sz)
  tempdata <- dfdata[idx, ]
  
  # 80/20 train-test split
  train_idx <- sample(1:nrow(tempdata), 0.8 * nrow(tempdata))
  train <- tempdata[train_idx, ]
  test <- tempdata[-train_idx, ]
  
  # Set up 5-fold cross-validation
  train_control <- trainControl(method = "cv", number = 5)
  
  # Start timer
  start_time <- Sys.time()
  
  # Train model
  model <- train(
    x = train[, -ncol(train)],
    y = as.factor(train$outcome),
    method = "xgbTree",
    trControl = train_control,
    verbose = FALSE
  )
  
  # End timer
  end_time <- Sys.time()
  
  # Predict on test set
  preds <- predict(model, newdata = test[, -ncol(test)])
  preds_class <- as.numeric(as.character(preds))
  
  # Calculate accuracy
  accuracy <- mean(preds_class == test$outcome)
  
  # Calculate time taken
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Save results
  results_caret <- rbind(results_caret, data.frame(
    Dataset_Size = sz,
    Accuracy = accuracy,
    Time_Taken_Sec = time_taken
  ))
}

# Print results
results_caret

