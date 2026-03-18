# Load libraries
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(WDI))

# =========================
# 1) Load and merge data
# =========================
data_dir <- "C:/HSG/ML in finance 2.0/CreditRatingForecasting"

df             <- read.csv(file.path(data_dir, "dataWithBlanks.csv"),  sep = ";", header = TRUE)
df_land_sector <- read.csv(file.path(data_dir, "landsectordata.csv"), sep = ";", header = TRUE)

# Merge firm data with land/sector metadata on Ticker (left join)
df <- merge(df, df_land_sector, by = "Ticker", all.x = TRUE)

# Drop duplicate column introduced by merge
df <- df %>%
  select(-any_of(c("Name", "Fitch.Long.Term.Issuer.Default.Rating.y")))

# Rename target (gets .x suffix after merge)
names(df)[names(df) == "Fitch.Long.Term.Issuer.Default.Rating.x"] <-
  "Fitch.Long.Term.Issuer.Default.Rating"

# Count missing accounting entries before imputation
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

data("WDI_data", package = "WDI")
valid_codes <- unique(WDI_data$country$iso2c)
valid_codes <- valid_codes[!is.na(valid_codes) & valid_codes != ""]

countries     <- unique(df$country_iso2)
countries     <- countries[countries %in% valid_codes]

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
# Liquidity
df$current_ratio <- df$Curr.Assets.LF / df$Curr.Liab.LF

# Profitability
df$fcf_margin_ratio    <- df$FCF.T12M         / df$Revenue.T12M
df$net_margin_ratio    <- df$NI...Profit.T12M  / df$Revenue.T12M
df$ebit_margin_ratio   <- df$EBIT.T12M         / df$Revenue.T12M
df$ebitda_margin_ratio <- df$EBITDA.T12M       / df$Revenue.T12M

# Returns
df$roa_ratio <- df$NI...Profit.T12M / df$Tot.Assets.LF
df$roe_ratio <- df$NI...Profit.T12M / df$SE.End.Pd.LF

# Leverage
df$debt_to_equity_ratio        <- df$Tot.Debt.LF   / df$SE.End.Pd.LF
df$debt_to_assets_ratio        <- df$Tot.Debt.LF   / df$Tot.Assets.LF
df$liabilities_to_assets_ratio <- df$Total.Liab.LF / df$Tot.Assets.LF

# Debt capacity
df$debt_to_ebitda_ratio <- df$Tot.Debt.LF / df$EBITDA.T12M
df$debt_to_ebit_ratio   <- df$Tot.Debt.LF / df$EBIT.T12M

# Market valuation
df$marketcap_to_revenue_ratio <- df$Market.Cap / df$Revenue.T12M
df$marketcap_to_assets_ratio  <- df$Market.Cap / df$Tot.Assets.LF

# Cash flow strength
df$fcf_to_debt_ratio   <- df$FCF.T12M / df$Tot.Debt.LF
df$fcf_to_assets_ratio <- df$FCF.T12M / df$Tot.Assets.LF

# =========================
# 4) Target: ordered numeric rating
# =========================
rating_levels <- c(
  "D", "CC", "CCC-", "CCC", "CCC+",
  "B-", "B", "B+",
  "BB-", "BB", "BB+",
  "BBB-", "BBB", "BBB+",
  "A-", "A", "A+",
  "AA-", "AA", "AA+", "AAA"
)

df$rating_num <- as.numeric(
  factor(
    df$Fitch.Long.Term.Issuer.Default.Rating,
    levels  = rating_levels,
    ordered = TRUE
  )
)

# =========================
# 5) Drop identifier columns
# =========================
df <- df %>%
  select(-any_of(c(
    "Ticker", "Short.Name", "Cntry.Terrtry", "country_iso2",
    "Fitch.Long.Term.Issuer.Default.Rating",
    "Entries_missing_Accounting"
  )))

# Replace Inf / -Inf with NA (from ratio divisions)
df[] <- lapply(df, function(x) {
  if (is.numeric(x)) x[is.infinite(x)] <- NA
  x
})

# Convert remaining character columns to factor
df <- df %>%
  mutate(across(where(is.character), as.factor))

# =========================
# 6) Missingness handling (mirrors 1600code.r)
# =========================
# For every predictor with NAs:
#   - add a binary <col>_missing factor column as an extra signal
#   - impute numeric NAs with 0
#   - impute factor NAs with level "Missing"
# The target (rating_num) is excluded from imputation.
target_name <- "rating_num"

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

# Keep only rows where the target is available
df_model <- df %>%
  filter(!is.na(rating_num))

# =========================
# 7) Select modelling columns
# =========================
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

# Include all columns that are present (financial + macro + missingness indicators)
keep_cols <- c(
  intersect(c(financial_cols, macro_cols), names(df_model)),
  grep("_missing", names(df_model), value = TRUE),
  target_name
)
keep_cols <- unique(keep_cols)

dataframe_selected <- df_model[, keep_cols]

cat("Rows in modelling dataset:", nrow(dataframe_selected), "\n")
cat("Columns in modelling dataset:", ncol(dataframe_selected), "\n")

# =========================
# 8) Full-data model summary
# =========================
lm.fit <- lm(rating_num ~ ., data = dataframe_selected)
summary(lm.fit)

# =========================
# 9) Train / test split (90 / 10)
# =========================
set.seed(100)
index <- sample(seq_len(nrow(dataframe_selected)),
                round(0.9 * nrow(dataframe_selected)),
                replace = FALSE)
learn <- dataframe_selected[ index, ]
test  <- dataframe_selected[-index, ]

n_l <- nrow(learn)
n_t <- nrow(test)
cat("Train rows:", n_l, "| Test rows:", n_t, "\n")

# =========================
# 10) Fit model on training data and predict
# =========================
lm.fit3 <- lm(rating_num ~ ., data = learn)

prediction         <- predict(lm.fit3, newdata = test)
prediction_clipped <- pmin(pmax(prediction,
                                min(dataframe_selected$rating_num)),
                           max(dataframe_selected$rating_num))

# =========================
# 11) Evaluate
# =========================
rss <- sum((prediction - test$rating_num)^2)
tss <- sum((test$rating_num - mean(test$rating_num))^2)
rsq <- 1 - rss / tss

print(paste("R-squared:", round(rsq, 4)))

# =========================
# 12) Explanatory plots
# =========================
plots_dir <- "C:/HSG/ML in finance 2.0/plots"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

unique_ticks <- sort(unique(dataframe_selected$rating_num))

# 1) Distribution of numeric ratings in the modelling dataset
png(filename = file.path(plots_dir, "1_rating_distribution.png"),
    width = 1100, height = 700)
hist(
  dataframe_selected$rating_num,
  breaks = seq(min(dataframe_selected$rating_num) - 0.5,
               max(dataframe_selected$rating_num) + 0.5, by = 1),
  col    = "steelblue",
  border = "white",
  main   = "Distribution of Fitch Ratings (Numeric)",
  xlab   = "Fitch rating",
  ylab   = "Count",
  xaxt   = "n"
)
axis(1, at = unique_ticks, labels = rating_levels[unique_ticks], las = 2, cex.axis = 0.8)
dev.off()

# 2) Actual vs predicted on test split
png(filename = file.path(plots_dir, "2_actual_vs_predicted.png"),
    width = 1100, height = 700)
plot(
  test$rating_num,
  prediction_clipped,
  xaxt = "n",
  yaxt = "n",
  pch  = 19,
  col  = rgb(0.1, 0.3, 0.7, 0.45),
  xlab = "Actual Fitch rating",
  ylab = "Predicted Fitch rating",
  main = paste("Actual vs Predicted Ratings (Test Set) | R-squared =",
               round(rsq, 4))
)
axis(1, at = unique_ticks, labels = rating_levels[unique_ticks], las = 2, cex.axis = 0.8)
axis(2, at = unique_ticks, labels = rating_levels[unique_ticks], las = 2, cex.axis = 0.8)
abline(a = 0, b = 1, col = "firebrick", lwd = 2)
grid(col = "gray85")
dev.off()

# 3) Top 12 coefficient magnitudes (excluding intercept)
coefs     <- coef(lm.fit3)
coefs     <- coefs[names(coefs) != "(Intercept)"]
coef_abs  <- sort(abs(coefs), decreasing = TRUE)
top_n     <- min(12, length(coef_abs))
top_names <- names(coef_abs)[1:top_n]
top_vals  <- coefs[top_names]

png(filename = file.path(plots_dir, "3_top_coefficients.png"),
    width = 1300, height = 800)
par(mar = c(10, 5, 4, 2))
barplot(
  top_vals,
  las  = 2,
  col  = ifelse(top_vals >= 0, "darkgreen", "tomato"),
  main = "Top Linear Model Coefficients by Absolute Magnitude",
  ylab = "Coefficient value"
)
abline(h = 0, col = "gray20", lwd = 1.5)
dev.off()

# 4) Distribution of absolute rating errors in notches
rounded_prediction <- round(prediction_clipped)
abs_error          <- abs(rounded_prediction - test$rating_num)
max_error          <- if (length(abs_error) > 0) max(abs_error) else 0

png(filename = file.path(plots_dir, "4_absolute_error_distribution.png"),
    width = 1100, height = 700)
hist(
  abs_error,
  breaks = seq(-0.5, max_error + 0.5, by = 1),
  col    = "lightgray",
  border = "gray40",
  main   = "Distribution of Absolute Rating Errors",
  xlab   = "Absolute error in notches",
  ylab   = "Count"
)
axis(1, at = 0:max_error)
dev.off()

# 5) Cumulative accuracy by allowed notch error
tolerances          <- 0:max_error
cumulative_accuracy <- sapply(tolerances, function(tol) mean(abs_error <= tol))

png(filename = file.path(plots_dir, "5_cumulative_accuracy_by_tolerance.png"),
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
  main = "Cumulative Accuracy by Rating Tolerance"
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

print(paste("Saved plots to:", plots_dir))
