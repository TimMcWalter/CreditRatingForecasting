library(randomForest)
library(caret)
library(randomForestSRC)
#library(ggRandomForests)

# Start
print("Start analysis")

# Load dataset
df <- read.csv("ML_EXCEL.csv", sep = ",", header = TRUE)

# Clean column names
colnames(df) <- gsub(" ", "", colnames(df))

# Inspect data
print(colnames(df))
summary(df)
str(df)

# Define target variable as factor for classification
df$Fitch.Long.Term.Issuer.Default.Rating <- as.factor(
  df$Fitch.Long.Term.Issuer.Default.Rating
)

# Create financial ratios
# Liquidity
df$current_ratio <- df$Curr.Assets.LF / df$Curr.Liab.LF

# Profitability
df$fcf_margin_ratio <- df$FCF.T12M / df$Revenue.T12M
df$net_margin_ratio <- df$NI...Profit.T12M / df$Revenue.T12M
df$ebit_margin_ratio <- df$EBIT.T12M / df$Revenue.T12M
df$ebitda_margin_ratio <- df$EBITDA.T12M / df$Revenue.T12M

# Returns
df$roa_ratio <- df$NI...Profit.T12M / df$Tot.Assets.LF
df$roe_ratio <- df$NI...Profit.T12M / df$SE.End.Pd.LF

# Leverage
df$debt_to_equity_ratio <- df$Tot.Debt.LF / df$SE.End.Pd.LF
df$debt_to_assets_ratio <- df$Tot.Debt.LF / df$Tot.Assets.LF
df$liabilities_to_assets_ratio <- df$Total.Liab.LF / df$Tot.Assets.LF

# Debt capacity
df$debt_to_ebitda_ratio <- df$Tot.Debt.LF / df$EBITDA.T12M
df$debt_to_ebit_ratio <- df$Tot.Debt.LF / df$EBIT.T12M

# Market valuation
df$marketcap_to_revenue_ratio <- df$Market.Cap / df$Revenue.T12M
df$marketcap_to_assets_ratio <- df$Market.Cap / df$Tot.Assets.LF

# Cash flow strength
df$fcf_to_debt_ratio <- df$FCF.T12M / df$Tot.Debt.LF
df$fcf_to_assets_ratio <- df$FCF.T12M / df$Tot.Assets.LF

# Inspect updated data
summary(df)
str(df)

# Summary statistics and exploratory analysis
print(table(df$Fitch.Long.Term.Issuer.Default.Rating))

numeric_vars <- df[sapply(df, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

hist(df$current_ratio,
     main = "Current Ratio Distribution",
     xlab = "Current Ratio")

hist(df$debt_to_equity_ratio,
     main = "Debt to Equity Ratio",
     xlab = "Debt to Equity Ratio")

hist(df$roa_ratio,
     main = "Return on Assets",
     xlab = "ROA Ratio")

# Prepare data for modelling
df_model <- df

# Remove identifier columns if they exist
if ("Ticker" %in% names(df_model)) {
  df_model$Ticker <- NULL
}

if ("Short.Name" %in% names(df_model)) {
  df_model$Short.Name <- NULL
}

# Replace Inf and -Inf with NA
df_model[] <- lapply(df_model, function(x) {
  if (is.numeric(x)) {
    x[is.infinite(x)] <- NA
  }
  return(x)
})

# Remove missing values
df_model <- na.omit(df_model)

# Check remaining missing values
print(colSums(is.na(df_model)))

# Define input and output variables
target <- df_model$Fitch.Long.Term.Issuer.Default.Rating
predictors <- df_model[,!(names(df_model) %in% "Fitch.Long.Term.Issuer.Default.Rating")]

rating_counts <- table(df_model$Fitch.Long.Term.Issuer.Default.Rating)

barplot(
  rating_counts,
  col="steelblue",
  main="Distribution of Credit Ratings",
  xlab="Rating",
  ylab="Number of Observations",
  las=2
)
#drop classes with no observations
df_model$Fitch.Long.Term.Issuer.Default.Rating <- droplevels(
  df_model$Fitch.Long.Term.Issuer.Default.Rating
)
# Train Random Forest with default hyperparameters
set.seed(600)

rf_default <- randomForest(
  Fitch.Long.Term.Issuer.Default.Rating ~ .,
  data = df_model,
  importance = TRUE,
  keep.forest = TRUE,
  keep.inbag = TRUE,
  ntree = 500
)


print(rf_default)

# Plot out-of-bag classification error
plot(rf_default$err.rate[, 1],
     type = "l",
     xlab = "Number of Trees",
     ylab = "OOB Classification Error")

# Train Random Forest with more trees
set.seed(600)

rf_large <- randomForest(
  Fitch.Long.Term.Issuer.Default.Rating ~ .,
  data = df_model,
  importance = TRUE,
  keep.forest = TRUE,
  keep.inbag = TRUE,
  ntree = 2000
)

print(rf_large)

# Plot out-of-bag classification error for larger forest
plot(rf_large$err.rate[, 1],
     type = "l",
     xlab = "Number of Trees",
     ylab = "OOB Classification Error")

# Hyperparameter tuning using 10-fold cross validation
control <- trainControl(
  method = "cv",
  number = 10
)

set.seed(600)

tunegrid <- expand.grid(.mtry = 1:(ncol(df_model) - 1))

rf_gridsearch <- train(
  Fitch.Long.Term.Issuer.Default.Rating ~ .,
  data = df_model,
  method = "rf",
  metric = "Accuracy",
  tuneGrid = tunegrid,
  trControl = control,
  ntree = 500
)

print(rf_gridsearch)
plot(rf_gridsearch)

# Hyperparameter tuning using leave-one-out cross validation
control_loocv <- trainControl(method = "LOOCV")

set.seed(600)

#rf_loocv <- train(
  #Fitch.Long.Term.Issuer.Default.Rating ~ .,
  #data = df_model,
  #method = "rf",
  #metric = "Accuracy",
  #tuneGrid = tunegrid,
  #trControl = control_loocv,
  #ntree = 500
  #)

#print(rf_loocv)

# In-sample predictions and accuracy
set.seed(600)

pred_rf <- predict(rf_default, newdata = df_model)

print(confusionMatrix(
  pred_rf,
  df_model$Fitch.Long.Term.Issuer.Default.Rating
))

# Permutation importance
IMP <- importance(
  rf_default,
  type = 1,
  scale = TRUE
)

print(IMP)

varImpPlot(
  rf_default,
  sort = TRUE,
  type = 1,
  main = "Variable Importance (%IncMSE)"
)

# Minimal depth importance
set.seed(600)

varselect <- var.select(
  Fitch.Long.Term.Issuer.Default.Rating ~ .,
  data = df_model,
  method = "md",
  ntree = 500
)

mdimp <- gg_minimal_depth(varselect)
plot(mdimp)
