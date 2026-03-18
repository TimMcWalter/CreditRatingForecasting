# Load packages
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(nnet))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(WDI))

# =========================
# 1) Load and merge data
# =========================
# Working directory should be set to the CreditRatingForecasting folder,
# or use full paths below.
data_dir <- "C:/HSG/ML in finance 2.0/CreditRatingForecasting"

df            <- read.csv(file.path(data_dir, "dataWithBlanks.csv"),   sep = ";", header = TRUE)
df_land_sector <- read.csv(file.path(data_dir, "landsectordata.csv"), sep = ";", header = TRUE)

# Merge on Ticker (left join keeps all firm rows)
df <- merge(df, df_land_sector, by = "Ticker", all.x = TRUE)

# Drop duplicate / unwanted columns introduced by the merge
df <- df %>%
  select(-any_of(c("Name", "Fitch.Long.Term.Issuer.Default.Rating.y")))

# Rename target back to clean name (suffix .x added by merge)
names(df)[names(df) == "Fitch.Long.Term.Issuer.Default.Rating.x"] <-
  "Fitch.Long.Term.Issuer.Default.Rating"

# Count missing accounting entries before macro merge / imputation
df$Entries_missing_Accounting <- rowSums(is.na(df))

# =========================
# 2) Add macro data (World Bank)
# =========================
snapshot_year <- 2023

df <- df %>%
  mutate(
    country_iso2 = toupper(trimws(Cntry.Terrtry)),
    # Bloomberg country codes → ISO 3166-1 alpha-2 (verified from company names)
    country_iso2 = recode(country_iso2,
      "UK" = "GB",   # United Kingdom
      "EL" = "GR",   # Greece
      "BP" = "BG",   # Bulgaria  (EUROHOLD, CB FIRST)
      "ED" = "EC",   # Ecuador   (BANCO DEL AUSTRO, BANCO BOLIVARIAN)
      "GG" = "GE",   # Georgia   (LIBERTY BANK JSC, BANK OF GEORGIA)
      "IV" = "CI",   # Ivory Coast (BICICI)
      "JN" = "JP",   # Japan     (KIOXIA, MODEC)
      "KU" = "KW",   # Kuwait    (AL AHLI BANK KUWAIT, BOUBYAN BANK)
      "LL" = "LV",   # Latvia    (ELEVING GROUP SA)
      "LX" = "LU",   # Luxembourg (ARDAGH, EUROFINS)
      "MB" = "MT",   # Malta     (BANK VALLETTA, FIMBANK)
      "MJ" = "MN",   # Mongolia  (MONGOL MINING, GOLOMT BANK)
      "PD" = "PL",   # Poland    (ALIOR BANK, HANDLOWY, BNP PARIBAS BANK)
      "PN" = "PA",   # Panama    (GRUPO ASSA, BANISTMO)
      "PO" = "PT",   # Portugal  (EDP SA, NOS SGPS)
      "SP" = "ES",   # Spain     (AENA, BBVA)
      "SW" = "SE",   # Sweden    (AUTOLIV, ATLAS COPCO)
      "TA" = "TW",   # Taiwan    (TCC, UMC, NANYA TECH)
      "TU" = "TR",   # Turkey    (ANADOLU EFES, AKBANK)
      "U2" = "MO",   # Macau     (WYNN MACAU, SANDS CHINA, MGM CHINA)
      "UR" = "UY"    # Uruguay   (ARCOS DORADOS, MERCADOLIBRE)
      # GS = Guernsey: no World Bank macro data, left as-is
    )
  )

# Load valid World Bank country codes
data("WDI_data", package = "WDI")
valid_codes <- unique(WDI_data$country$iso2c)
valid_codes <- valid_codes[!is.na(valid_codes) & valid_codes != ""]

countries <- unique(df$country_iso2)
countries <- countries[countries %in% valid_codes]

# Known World Bank exclusions (not fixable — WB political/coverage limitation):
#   GS = Guernsey   (British Crown dependency; WB has no macro data)
#   TW = Taiwan     (not a UN member; intentionally excluded from WB database)
known_exclusions <- c("GS", "TW")
invalid_codes    <- sort(setdiff(unique(df$country_iso2), valid_codes))
unexpected_codes <- setdiff(invalid_codes, known_exclusions)
if (length(unexpected_codes) > 0) {
  message("WARNING - unexpected country codes not in World Bank data: ",
          paste(unexpected_codes, collapse = ", "))
} else {
  message("All country codes resolved. Known WB non-members skipped: ",
          paste(intersect(invalid_codes, known_exclusions), collapse = ", "),
          " (Guernsey/Taiwan — no WB data by design).")
}

macro_wide <- WDI(
  country = countries,
  indicator = c(
    gdp_growth     = "NY.GDP.MKTP.KD.ZG",
    gdp_per_capita = "NY.GDP.PCAP.CD",
    inflation_cpi  = "FP.CPI.TOTL.ZG",
    unemployment   = "SL.UEM.TOTL.ZS"
  ),
  start = snapshot_year,
  end   = snapshot_year,
  extra = FALSE
) %>%
  transmute(
    country_iso2   = toupper(iso2c),
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
df$current_ratio              <- df$Curr.Assets.LF     / df$Curr.Liab.LF
df$fcf_margin_ratio           <- df$FCF.T12M            / df$Revenue.T12M
df$net_margin_ratio           <- df$NI...Profit.T12M    / df$Revenue.T12M
df$ebit_margin_ratio          <- df$EBIT.T12M           / df$Revenue.T12M
df$ebitda_margin_ratio        <- df$EBITDA.T12M         / df$Revenue.T12M
df$roa_ratio                  <- df$NI...Profit.T12M    / df$Tot.Assets.LF
df$roe_ratio                  <- df$NI...Profit.T12M    / df$SE.End.Pd.LF
df$debt_to_equity_ratio       <- df$Tot.Debt.LF         / df$SE.End.Pd.LF
df$debt_to_assets_ratio       <- df$Tot.Debt.LF         / df$Tot.Assets.LF
df$liabilities_to_assets_ratio <- df$Total.Liab.LF     / df$Tot.Assets.LF
df$debt_to_ebitda_ratio       <- df$Tot.Debt.LF         / df$EBITDA.T12M
df$debt_to_ebit_ratio         <- df$Tot.Debt.LF         / df$EBIT.T12M
df$marketcap_to_revenue_ratio <- df$Market.Cap          / df$Revenue.T12M
df$marketcap_to_assets_ratio  <- df$Market.Cap          / df$Tot.Assets.LF
df$fcf_to_debt_ratio          <- df$FCF.T12M            / df$Tot.Debt.LF
df$fcf_to_assets_ratio        <- df$FCF.T12M            / df$Tot.Assets.LF

# =========================
# 4) Target encoding + cleanup
# =========================
rating_levels <- c(
  "D", "CC", "CCC-", "CCC", "CCC+",
  "B-", "B", "B+",
  "BB-", "BB", "BB+",
  "BBB-", "BBB", "BBB+",
  "A-", "A", "A+",
  "AA-", "AA", "AA+", "AAA"
)

df$rating_grade <- factor(
  df$Fitch.Long.Term.Issuer.Default.Rating,
  levels = rating_levels
)

# Drop identifier columns (not predictive)
df <- df %>%
  select(-any_of(c("Ticker", "Short.Name", "Cntry.Terrtry", "country_iso2",
                   "Fitch.Long.Term.Issuer.Default.Rating",
                   "Entries_missing_Accounting")))

# Replace Inf / -Inf with NA
df[] <- lapply(df, function(x) {
  if (is.numeric(x)) x[is.infinite(x)] <- NA
  x
})

# Convert remaining character columns to factor
df <- df %>%
  mutate(across(where(is.character), as.factor))

# =========================
# 5) Missingness handling (mirror of 1600code.r)
# =========================
# For each predictor with NAs:
#   - add a binary missingness-indicator factor column
#   - impute numeric NAs with 0
#   - impute factor NAs with level "Missing"
target_name <- "rating_grade"

for (col in setdiff(names(df), target_name)) {
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

# Remove rows where the target itself is missing
df_model <- df %>%
  filter(!is.na(rating_grade))

df_model$rating_grade <- droplevels(df_model$rating_grade)

# =========================
# 6) Select features for the neural network
# =========================
# Core financial ratios and raw financials (same set as original Neural_network.R)
# plus the four macro variables added by the 1600code.r pipeline.
# Missingness-indicator columns are included automatically because they were
# added to df_model in the loop above.

financial_cols <- c(
  "NI...Profit.T12M", "EBITDA.T12M", "EBIT.T12M", "Total.Liab.LF",
  "FCF.T12M", "Tot.Assets.LF", "Market.Cap", "P.E", "Total.Return.YTD",
  "Revenue.T12M", "EPS.T12M", "Tot.Debt.LF",
  "current_ratio", "fcf_margin_ratio", "net_margin_ratio",
  "ebit_margin_ratio", "ebitda_margin_ratio",
  "roa_ratio", "roe_ratio",
  "debt_to_equity_ratio", "debt_to_assets_ratio", "liabilities_to_assets_ratio",
  "debt_to_ebitda_ratio", "debt_to_ebit_ratio",
  "marketcap_to_revenue_ratio", "marketcap_to_assets_ratio",
  "fcf_to_debt_ratio", "fcf_to_assets_ratio"
)

macro_cols <- c("gdp_growth", "gdp_per_capita", "inflation_cpi", "unemployment")

# Keep only columns that actually exist in df_model
keep_cols <- c(
  intersect(c(financial_cols, macro_cols), names(df_model)),
  # include any _missing indicator columns that were created
  grep("_missing$", names(df_model), value = TRUE),
  target_name
)
keep_cols <- unique(keep_cols)

dataframe <- df_model[, keep_cols]

# =========================
# 7) Preprocessing / scaling
# =========================
predictor_names <- setdiff(names(dataframe), target_name)

# Separate numeric and factor predictors
numeric_preds <- predictor_names[sapply(dataframe[, predictor_names, drop = FALSE], is.numeric)]
factor_preds  <- predictor_names[sapply(dataframe[, predictor_names, drop = FALSE], is.factor)]

# Min-max scale numeric predictors
maxs   <- apply(dataframe[, numeric_preds, drop = FALSE], 2, max)
mins   <- apply(dataframe[, numeric_preds, drop = FALSE], 2, min)
scales <- maxs - mins
scales[scales == 0] <- 1   # avoid division by zero for constant columns

scaled_numeric <- as.data.frame(
  scale(dataframe[, numeric_preds, drop = FALSE], center = mins, scale = scales)
)

# One-hot encode factor predictors for nnet
if (length(factor_preds) > 0) {
  # model.matrix drops the intercept and expands factors into dummies
  dummy_matrix <- model.matrix(~ . - 1,
                               data = dataframe[, factor_preds, drop = FALSE])
  dummy_df <- as.data.frame(dummy_matrix)
} else {
  dummy_df <- data.frame(row.names = seq_len(nrow(dataframe)))
}

scaled <- cbind(scaled_numeric, dummy_df)
scaled[[target_name]] <- dataframe[[target_name]]

# Update predictor_names to include dummy columns
predictor_names_final <- setdiff(names(scaled), target_name)

# =========================
# 8) Train / test split
# =========================
set.seed(100)
split <- sample.split(scaled[[target_name]], SplitRatio = 0.7)
train <- subset(scaled, split == TRUE)
test  <- subset(scaled, split == FALSE)

train[[target_name]] <- droplevels(train[[target_name]])
test[[target_name]]  <- factor(test[[target_name]],
                               levels = levels(train[[target_name]]))

# =========================
# 9) Train neural network
# =========================
f <- as.formula(paste(target_name, "~",
                      paste(predictor_names_final, collapse = " + ")))

nn <- nnet(
  f,
  data  = train,
  size  = 5,
  maxit = 300,
  trace = FALSE
)

# =========================
# 10) Feature importance (permutation)
# =========================
perm_importance <- function(model, data, predictors, target) {
  baseline_pred <- predict(model, newdata = data[, predictors, drop = FALSE],
                           type = "class")
  baseline_acc  <- mean(baseline_pred == data[[target]])

  importance_scores        <- numeric(length(predictors))
  names(importance_scores) <- predictors

  for (i in seq_along(predictors)) {
    perm_data                  <- data
    perm_data[[predictors[i]]] <- sample(perm_data[[predictors[i]]])
    perm_pred <- predict(model, newdata = perm_data[, predictors, drop = FALSE],
                         type = "class")
    perm_acc             <- mean(perm_pred == data[[target]])
    importance_scores[i] <- baseline_acc - perm_acc
  }

  data.frame(Feature = predictors, Importance = importance_scores,
             row.names = NULL)
}

importance <- perm_importance(nn, test, predictor_names_final, target_name)
importance <- importance[order(-importance$Importance), ]
print(importance)

# =========================
# 11) Prediction & evaluation
# =========================
predicted_rating <- predict(nn, newdata = test[, predictor_names_final, drop = FALSE],
                            type = "class")

accuracy <- mean(predicted_rating == test[[target_name]])
conf_mat <- table(actual = test[[target_name]], predicted = predicted_rating)

results_df <- data.frame(
  actual_rating    = as.character(test[[target_name]]),
  predicted_rating = as.character(predicted_rating)
)

cat("Accuracy:", round(accuracy, 6), "\n")
print(conf_mat)
print(head(results_df, 20))

# =========================
# 12) Explanatory plots
# =========================
plots_dir <- "C:/HSG/ML in finance 2.0/plots"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

present_levels   <- levels(train[[target_name]])
actual_numeric   <- as.numeric(factor(test[[target_name]],
                                      levels = present_levels, ordered = TRUE))
predicted_numeric <- as.numeric(factor(predicted_rating,
                                       levels = present_levels, ordered = TRUE))
used_levels <- present_levels[sort(unique(c(actual_numeric, predicted_numeric)))]
used_ticks  <- match(used_levels, present_levels)

# 1) Distribution of ratings in the test split
png(filename = file.path(plots_dir, "nn_1_test_rating_distribution.png"),
    width = 1100, height = 700)
barplot(
  table(test[[target_name]]),
  col    = "steelblue",
  border = "white",
  main   = "Neural Network Test-Set Rating Distribution",
  xlab   = "Fitch rating",
  ylab   = "Count",
  las    = 2
)
dev.off()

# 2) Confusion matrix heatmap
conf_mat_matrix <- as.matrix(conf_mat)
png(filename = file.path(plots_dir, "nn_2_confusion_matrix.png"),
    width = 1100, height = 900)
par(mar = c(8, 8, 4, 2))
image(
  seq_len(ncol(conf_mat_matrix)),
  seq_len(nrow(conf_mat_matrix)),
  t(conf_mat_matrix[rev(seq_len(nrow(conf_mat_matrix))), , drop = FALSE]),
  col  = colorRampPalette(c("white", "gold", "firebrick"))(100),
  xaxt = "n",
  yaxt = "n",
  xlab = "Predicted rating",
  ylab = "Actual rating",
  main = paste("Confusion Matrix Heatmap | Accuracy =", round(accuracy, 4))
)
axis(1, at = seq_len(ncol(conf_mat_matrix)),
     labels = colnames(conf_mat_matrix), las = 2, cex.axis = 0.8)
axis(2, at = seq_len(nrow(conf_mat_matrix)),
     labels = rev(rownames(conf_mat_matrix)), las = 2, cex.axis = 0.8)
for (i in seq_len(nrow(conf_mat_matrix))) {
  for (j in seq_len(ncol(conf_mat_matrix))) {
    text(j, nrow(conf_mat_matrix) - i + 1,
         labels = conf_mat_matrix[i, j], cex = 0.75)
  }
}
dev.off()

# 3) Actual vs predicted ratings on the ordered rating scale
png(filename = file.path(plots_dir, "nn_3_actual_vs_predicted.png"),
    width = 1100, height = 700)
plot(
  actual_numeric,
  predicted_numeric,
  xaxt = "n",
  yaxt = "n",
  pch  = 19,
  col  = rgb(0.1, 0.3, 0.7, 0.45),
  xlab = "Actual Fitch rating",
  ylab = "Predicted Fitch rating",
  main = "Neural Network: Actual vs Predicted Ratings"
)
axis(1, at = used_ticks, labels = used_levels, las = 2, cex.axis = 0.8)
axis(2, at = used_ticks, labels = used_levels, las = 2, cex.axis = 0.8)
abline(a = 0, b = 1, col = "firebrick", lwd = 2)
grid(col = "gray85")
dev.off()

# 4) Distribution of absolute rating errors in notches
abs_error <- abs(actual_numeric - predicted_numeric)
max_error <- if (length(abs_error) > 0) max(abs_error) else 0

png(filename = file.path(plots_dir, "nn_4_absolute_error_distribution.png"),
    width = 1100, height = 700)
hist(
  abs_error,
  breaks = seq(-0.5, max_error + 0.5, by = 1),
  col    = "lightgray",
  border = "gray40",
  main   = "Neural Network: Distribution of Absolute Rating Errors",
  xlab   = "Absolute error in notches",
  ylab   = "Count"
)
axis(1, at = 0:max_error)
dev.off()

# 5) Cumulative accuracy by allowed notch error
tolerances          <- 0:max_error
cumulative_accuracy <- sapply(tolerances, function(tol) mean(abs_error <= tol))

png(filename = file.path(plots_dir, "nn_5_cumulative_accuracy_by_tolerance.png"),
    width = 1100, height = 700)
plot(
  tolerances,
  cumulative_accuracy,
  type = "b",
  pch  = 19,
  col  = "black",
  ylim = c(0, 1.02),
  xlab = "Allowed prediction error (notches)",
  ylab = "Cumulative share of predictions",
  main = "Neural Network: Cumulative Accuracy by Rating Tolerance"
)
grid(col = "gray85")
text(
  tolerances,
  pmin(cumulative_accuracy + 0.03, 1.01),
  labels = paste0(round(cumulative_accuracy * 100, 1), "%"),
  cex = 0.8
)
if (length(cumulative_accuracy) > 1) {
  text(1, cumulative_accuracy[2] - 0.04,
       labels = paste0("Within 1 notch: ", round(cumulative_accuracy[2] * 100, 1), "%"),
       pos = 4, cex = 0.85)
}
text(0, max(cumulative_accuracy[1] - 0.04, 0.04),
     labels = paste0("Exact: ", round(cumulative_accuracy[1] * 100, 1), "%"),
     pos = 4, cex = 0.85)
dev.off()

cat("Saved plots to:", plots_dir, "\n")
