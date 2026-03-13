library(randomForest)
library(caret)
library(dplyr)
library(WDI)

# =========================
# 1) Load and merge data
# =========================
df <- read.csv("dataWithBlanks.csv", sep = ";", header = TRUE)
df_land_sector <- read.csv("landsectordata.csv", sep = ";", header = TRUE)

df <- merge(df, df_land_sector, by = "Ticker", all.x = TRUE)

# Drop duplicate / unwanted columns if they exist
df <- df %>%
  select(-any_of(c("Name", "Fitch.Long.Term.Issuer.Default.Rating.y")))

# Rename target
names(df)[names(df) == "Fitch.Long.Term.Issuer.Default.Rating.x"] <- 
  "Fitch.Long.Term.Issuer.Default.Rating"

# Missing accounting entries count before macro merge / imputation
df$Entries_missing_Accounting <- rowSums(is.na(df))

# =========================
# 2) Add macro data
# =========================
snapshot_year <- 2023

df <- df %>%
  mutate(
    country_iso2 = toupper(trimws(Cntry.Terrtry)),
    country_iso2 = recode(country_iso2, "UK" = "GB", "EL" = "GR")
  )

# Load valid World Bank codes
data("WDI_data", package = "WDI")

valid_codes <- unique(WDI_data$country$iso2c)
valid_codes <- valid_codes[!is.na(valid_codes) & valid_codes != ""]

countries <- unique(df$country_iso2)
countries <- countries[countries %in% valid_codes]

invalid_codes <- sort(setdiff(unique(df$country_iso2), valid_codes))
print(invalid_codes)

macro_wide <- WDI(
  country = countries,
  indicator = c(
    gdp_growth     = "NY.GDP.MKTP.KD.ZG",
    gdp_per_capita = "NY.GDP.PCAP.CD",
    inflation_cpi  = "FP.CPI.TOTL.ZG",
    unemployment   = "SL.UEM.TOTL.ZS"
  ),
  start = snapshot_year,
  end = snapshot_year,
  extra = FALSE
) %>%
  transmute(
    country_iso2 = toupper(iso2c),
    gdp_growth,
    gdp_per_capita,
    inflation_cpi,
    unemployment
  )

df <- df %>%
  left_join(macro_wide, by = "country_iso2")

# =========================
# 3) Feature engineering
# =========================
df$current_ratio <- df$Curr.Assets.LF / df$Curr.Liab.LF
df$fcf_margin_ratio <- df$FCF.T12M / df$Revenue.T12M
df$net_margin_ratio <- df$NI...Profit.T12M / df$Revenue.T12M
df$ebit_margin_ratio <- df$EBIT.T12M / df$Revenue.T12M
df$ebitda_margin_ratio <- df$EBITDA.T12M / df$Revenue.T12M

df$roa_ratio <- df$NI...Profit.T12M / df$Tot.Assets.LF
df$roe_ratio <- df$NI...Profit.T12M / df$SE.End.Pd.LF

df$debt_to_equity_ratio <- df$Tot.Debt.LF / df$SE.End.Pd.LF
df$debt_to_assets_ratio <- df$Tot.Debt.LF / df$Tot.Assets.LF
df$liabilities_to_assets_ratio <- df$Total.Liab.LF / df$Tot.Assets.LF

df$debt_to_ebitda_ratio <- df$Tot.Debt.LF / df$EBITDA.T12M
df$debt_to_ebit_ratio <- df$Tot.Debt.LF / df$EBIT.T12M

df$marketcap_to_revenue_ratio <- df$Market.Cap / df$Revenue.T12M
df$marketcap_to_assets_ratio <- df$Market.Cap / df$Tot.Assets.LF

df$fcf_to_debt_ratio <- df$FCF.T12M / df$Tot.Debt.LF
df$fcf_to_assets_ratio <- df$FCF.T12M / df$Tot.Assets.LF

# =========================
# 4) Target + cleanup
# =========================
df$Fitch.Long.Term.Issuer.Default.Rating <- as.factor(
  df$Fitch.Long.Term.Issuer.Default.Rating
)

# Drop identifier columns if they exist
df <- df %>%
  select(-any_of(c("Ticker", "Short.Name")))

# Replace Inf / -Inf with NA
df[] <- lapply(df, function(x) {
  if (is.numeric(x)) x[is.infinite(x)] <- NA
  x
})

# Convert character columns to factor
df <- df %>%
  mutate(across(where(is.character), as.factor))

# Drop unused target levels
df$Fitch.Long.Term.Issuer.Default.Rating <- droplevels(
  df$Fitch.Long.Term.Issuer.Default.Rating
)

# Drop country predictors before RF
df <- df %>%
  select(-any_of(c("Cntry.Terrtry", "country_iso2")))

# =========================
# 5) Missingness handling
# =========================
target_name <- "Fitch.Long.Term.Issuer.Default.Rating"

for (col in names(df)) {
  if (col == target_name) next
  
  if (anyNA(df[[col]])) {
    df[[paste0(col, "_missing")]] <- as.factor(ifelse(is.na(df[[col]]), "Yes", "No"))
    
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- 0
    }
    
    if (is.factor(df[[col]])) {
      df[[col]] <- addNA(df[[col]])
      levels(df[[col]])[is.na(levels(df[[col]]))] <- "Missing"
    }
  }
}

# Remove rows with missing target only
df_model <- df %>%
  filter(!is.na(Fitch.Long.Term.Issuer.Default.Rating))

# =========================
# 6) Sanity checks
# =========================
print(colSums(is.na(df_model)))

high_card_factors <- names(df_model)[sapply(df_model, function(x) is.factor(x) && nlevels(x) > 53)]
print(high_card_factors)

# If any factor still has >53 levels, drop it for classic randomForest
if (length(high_card_factors) > 0) {
  df_model <- df_model %>%
    select(-all_of(high_card_factors))
}

str(df_model)

# =========================
# 7) Random Forest models
# =========================
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

plot(
  rf_default$err.rate[, 1],
  type = "l",
  xlab = "Number of Trees",
  ylab = "OOB Classification Error"
)

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

plot(
  rf_large$err.rate[, 1],
  type = "l",
  xlab = "Number of Trees",
  ylab = "OOB Classification Error"
)

# =========================
# 8) Hyperparameter tuning
# =========================
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