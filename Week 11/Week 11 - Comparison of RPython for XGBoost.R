#Loading Libraries:
library(mlbench)
library(purrr)
library(caret)
library(xgboost)
library(dplyr)

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



###########XGBOOST - direct use#################
library(xgboost)
library(dplyr)

set.seed(123)

sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)
results <- data.frame(Size = integer(), Accuracy = numeric(), Time = numeric())

for (sz in sizes) {
  
  cat("\nProcessing size:", sz, "\n")
  
  ## sample sz rows from dfdata
  sample_idx <- sample(1:nrow(dfdata), sz)
  dftemp <- dfdata[sample_idx, ]
  
  ## split into predictors (X) and labels (y)
  X <- as.matrix(select(dftemp, -outcome))
  y <- dftemp$outcome
  
  ## simple train/test split
  train_idx <- sample(1:sz, sz * 0.8)
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[-train_idx, ]
  y_test <- y[-train_idx]
  
  ## start timing
  t0 <- Sys.time()
  
  ## fit XGBoost model
  model <- xgboost(data = X_train, label = y_train, objective = "binary:logistic", nrounds = 50, verbose = 0)
  
  ## end timing
  t1 <- Sys.time()
  
  ## predict
  preds <- predict(model, X_test)
  preds_class <- ifelse(preds > 0.5, 1, 0)
  
  ## calculate accuracy
  acc <- mean(preds_class == y_test)
  
  ## save results
  results <- rbind(results, data.frame(Size = sz, Accuracy = acc, Time = as.numeric(t1 - t0, units = "secs")))
}

results


#########################XGBoost - using caret###########
set.seed(123)

sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)
results_caret <- data.frame(Size = integer(), Accuracy = numeric(), Time = numeric())

for (sz in sizes) {
  
  cat("\nProcessing size:", sz, "\n")
  
  ## sample sz rows from dfdata
  sample_idx <- sample(1:nrow(dfdata), sz)
  dftemp <- dfdata[sample_idx, ]
  
  ## split into predictors (X) and labels (y)
  X <- select(dftemp, -outcome)
  y <- as.factor(dftemp$outcome) ## caret needs factors for classification
  
  ## set up 5-fold cross-validation
  train_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
  
  ## start timing
  t0 <- Sys.time()
  
  ## train the model using caret
  model_caret <- train(x = X, y = y, method = "xgbTree", trControl = train_control, tuneLength = 1
                       )
  
  ## end timing
  t1 <- Sys.time()
  
  ## extract cross-validated accuracy
  acc <- max(model_caret$results$Accuracy)
  
  ## save results
  results_caret <- rbind(results_caret, data.frame(Size = sz, Accuracy = acc, Time = as.numeric(t1 - t0, units = "secs")))
}

results_caret

