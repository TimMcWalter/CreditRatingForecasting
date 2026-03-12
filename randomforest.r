library(randomForest)
library(caret)
library(randomForestSRC)

## start
print("new line")
df <- read.csv("data_cleansed_csv.csv",sep = ";", header = TRUE)
colnames(df) <- gsub(" ", "", colnames(df))
print(colnames(df))
print(summary(df))
print(str(df))

df$Fitch.Long.Term.Issuer.Default.Rating <- as.factor(df$Fitch.Long.Term.Issuer.Default.Rating)

## create new ratios
df$current_ratio <- df$Curr.Assets.LF /df$Curr.Liab.LF
df$fcf_margin_ratio <- df$FCF.T12M / df$Revenue.T12M
df$net_margin_ratio <- df$NI...Profit.T12M / df$Revenue.T12M
df$ebit_margin_ratio <- df$EBIT.T12M / df$Revenue.T12M
df$ebitda_margin_ratio <- df$EBITDA.T12M / df$Revenue.T12M
# Return ratios
df$roa_ratio <- df$NI...Profit.T12M / df$Tot.Assets.LF
df$roe_ratio <- df$NI...Profit.T12M / df$SE.End.Pd.LF
# Leverage ratios
df$debt_to_equity_ratio <- df$Tot.Debt.LF / df$SE.End.Pd.LF
df$debt_to_assets_ratio <- df$Tot.Debt.LF / df$Tot.Assets.LF
df$liabilities_to_assets_ratio <- df$Total.Liab.LF / df$Tot.Assets.LF
# Coverage / debt capacity ratios
df$debt_to_ebitda_ratio <- df$Tot.Debt.LF / df$EBITDA.T12M
df$debt_to_ebit_ratio <- df$Tot.Debt.LF / df$EBIT.T12M
# Market related ratios
df$marketcap_to_revenue_ratio <- df$Market.Cap / df$Revenue.T12M
df$marketcap_to_assets_ratio <- df$Market.Cap / df$Tot.Assets.LF
# Cash flow ratios
df$fcf_to_debt_ratio <- df$FCF.T12M / df$Tot.Debt.LF
df$fcf_to_assets_ratio <- df$FCF.T12M / df$Tot.Assets.LF
print(str(df))
#end of creating new ratios

numeric_vars <- df[sapply(df, is.numeric)]
cor_matrix <- cor(numeric_vars, use= "complete.obs")
table(df$Fitch.Long.Term.Issuer.Default.Rating)

hist(df$current_ratio, main="current ratio distrib", xlab= "Current Ratio")
hist(df$debt_to_equity_ratio, main="Debt to Equity", xlab="Debt to Equity")
hist(df$roa_ratio, main="Return on Assets", xlab="ROA")

df_model <- df
df_model$Ticker <-NULL
df_model$Short.Name <- NULL

target <- df_model$Fitch.Long.Term.Issuer.Default.Rating
X <- df_model[,!(names(df_model) %in% "Fitch.Long.Term.Issuer.Default.Rating")]

df_model <- na.omit(df_model)
set.seed(42)
colSums(is.na(df_model))

rt_default <- randomForest(
  Fitch.Long.Term.Issuer.Default.Rating ~.,
  data = df_model,
  importance = TRUE,
  keep.forest = TRUE,
  keep.inbag = TRUE,
  ntree = 500
)

print(rt_default)

plot(rt_default$err.rate[,1], type="l",
     xlab="Number of Trees",
     ylab="OOB Classification Error")