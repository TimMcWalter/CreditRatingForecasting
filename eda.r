#EDA for the df and df_model
#thus make sure those dataframes are created before running this

library(dplyr)
library(ggplot2)

# -------------------------
# 1) Basic setup
# -------------------------
df_model$Fitch.Long.Term.Issuer.Default.Rating <- as.factor(df_model$Fitch.Long.Term.Issuer.Default.Rating)

rating_order <- c(
  "AAA", "AA+", "AA", "AA-",
  "A+", "A", "A-",
  "BBB+", "BBB", "BBB-",
  "BB+", "BB", "BB-",
  "B+", "B", "B-",
  "CCC+", "CCC", "CCC-",
  "CC", "C", "RD", "D"
)

existing_ratings <- intersect(rating_order, levels(df_model$Fitch.Long.Term.Issuer.Default.Rating))
if (length(existing_ratings) > 0) {
  df_model$Fitch.Long.Term.Issuer.Default.Rating <- factor(
    df_model$Fitch.Long.Term.Issuer.Default.Rating,
    levels = existing_ratings,
    ordered = TRUE
  )
}

theme_set(theme_minimal())

# -------------------------
# 2) Rating distribution
# -------------------------
p1 <- ggplot(df_model, aes(x = Fitch.Long.Term.Issuer.Default.Rating)) +
  geom_bar() +
  labs(
    title = "Distribution of Fitch Ratings",
    x = "Fitch Rating",
    y = "Number of Firms"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# -------------------------
# 3) Missingness plot
# -------------------------
missing_table <- data.frame(
  Variable = names(df_model),
  Missing_Count = sapply(df_model, function(x) sum(is.na(x))),
  Missing_Percent = sapply(df_model, function(x) mean(is.na(x)) * 100)
)

missing_plot_data <- missing_table %>%
  filter(Missing_Count > 0) %>%
  arrange(desc(Missing_Percent))

if (nrow(missing_plot_data) > 0) {
  p2 <- ggplot(missing_plot_data,
               aes(x = reorder(Variable, Missing_Percent), y = Missing_Percent)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Missing Values by Variable",
      x = "Variable",
      y = "Missing (%)"
    )
  
  print(p2)
}

# Optional: distribution of total missing accounting entries
if ("Entries_missing_Accounting" %in% names(df_model)) {
  p3 <- ggplot(df_model, aes(x = Entries_missing_Accounting)) +
    geom_histogram(binwidth = 1, boundary = -0.5) +
    labs(
      title = "Distribution of Missing Accounting Entries",
      x = "Number of Missing Accounting Entries",
      y = "Count"
    )
  
  print(p3)
}

# -------------------------
# 4) Boxplots of important firm-level predictors by rating
# -------------------------

# 4.1 Market Cap by rating
if ("Market.Cap" %in% names(df_model)) {
  p4 <- ggplot(df_model, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = Market.Cap)) +
    geom_boxplot(outlier.alpha = 0.2) +
    scale_y_log10() +
    labs(
      title = "Market Capitalization by Credit Rating",
      x = "Fitch Rating",
      y = "Market Capitalization (log scale)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p4)
}

# 4.2 Revenue by rating
if ("Revenue.T12M" %in% names(df_model)) {
  p5 <- ggplot(df_model, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = Revenue.T12M)) +
    geom_boxplot(outlier.alpha = 0.2) +
    scale_y_log10() +
    labs(
      title = "Revenue by Credit Rating",
      x = "Fitch Rating",
      y = "Revenue T12M (log scale)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p5)
}

# 4.3 Total Assets by rating
if ("Tot.Assets.LF" %in% names(df_model)) {
  p6 <- ggplot(df_model, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = Tot.Assets.LF)) +
    geom_boxplot(outlier.alpha = 0.2) +
    scale_y_log10() +
    labs(
      title = "Total Assets by Credit Rating",
      x = "Fitch Rating",
      y = "Total Assets (log scale)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p6)
}

# 4.4 Debt-to-assets ratio by rating
if ("debt_to_assets_ratio" %in% names(df_model)) {
  p7 <- ggplot(df_model, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = debt_to_assets_ratio)) +
    geom_boxplot(outlier.alpha = 0.2) +
    labs(
      title = "Debt-to-Assets Ratio by Credit Rating",
      x = "Fitch Rating",
      y = "Debt-to-Assets Ratio"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p7)
}

# 4.5 Optional: profitability by rating
if ("roa_ratio" %in% names(df_model)) {
  p8 <- ggplot(df_model, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = roa_ratio)) +
    geom_boxplot(outlier.alpha = 0.2) +
    labs(
      title = "Return on Assets by Credit Rating",
      x = "Fitch Rating",
      y = "ROA"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p8)
}

# -------------------------
# 5) Sector plots
# -------------------------
if ("GICS.Sector" %in% names(df_model)) {
  # 5.1 Sector distribution
  p9 <- ggplot(df_model, aes(x = reorder(GICS.Sector, GICS.Sector, function(x) -length(x)))) +
    geom_bar() +
    coord_flip() +
    labs(
      title = "Distribution of Firms by Sector",
      x = "GICS Sector",
      y = "Number of Firms"
    )
  
  print(p9)
  
  # 5.2 Rating composition within sectors
  p10 <- ggplot(df_model, aes(x = GICS.Sector, fill = Fitch.Long.Term.Issuer.Default.Rating)) +
    geom_bar(position = "fill") +
    coord_flip() +
    labs(
      title = "Rating Composition within Sectors",
      x = "GICS Sector",
      y = "Proportion",
      fill = "Fitch Rating"
    )
  
  print(p10)
}

# -------------------------
# 6) Macro variable distributions
# -------------------------
if ("gdp_growth" %in% names(df_model)) {
  p11 <- ggplot(df_model, aes(x = gdp_growth)) +
    geom_histogram(bins = 30) +
    labs(
      title = "Distribution of GDP Growth",
      x = "GDP Growth",
      y = "Count"
    )
  
  print(p11)
}

if ("gdp_per_capita" %in% names(df_model)) {
  p12 <- ggplot(df_model, aes(x = gdp_per_capita)) +
    geom_histogram(bins = 30) +
    labs(
      title = "Distribution of GDP per Capita",
      x = "GDP per Capita",
      y = "Count"
    )
  
  print(p12)
}

if ("inflation_cpi" %in% names(df_model)) {
  p13 <- ggplot(df_model, aes(x = inflation_cpi)) +
    geom_histogram(bins = 30) +
    labs(
      title = "Distribution of Inflation",
      x = "Inflation CPI",
      y = "Count"
    )
  
  print(p13)
}

if ("unemployment" %in% names(df_model)) {
  p14 <- ggplot(df_model, aes(x = unemployment)) +
    geom_histogram(bins = 30) +
    labs(
      title = "Distribution of Unemployment",
      x = "Unemployment",
      y = "Count"
    )
  
  print(p14)
}

# -------------------------
# 7) Mean feature values by rating (cleaner than large tables)
# -------------------------

# 7.1 Mean Market Cap by rating
if ("Market.Cap" %in% names(df_model)) {
  plot_df <- df_model %>%
    group_by(Fitch.Long.Term.Issuer.Default.Rating) %>%
    summarise(mean_value = mean(Market.Cap, na.rm = TRUE), .groups = "drop")
  
  p15 <- ggplot(plot_df, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = mean_value, group = 1)) +
    geom_line() +
    geom_point() +
    scale_y_log10() +
    labs(
      title = "Mean Market Capitalization by Credit Rating",
      x = "Fitch Rating",
      y = "Mean Market Capitalization (log scale)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p15)
}

# 7.2 Mean debt-to-assets by rating
if ("debt_to_assets_ratio" %in% names(df_model)) {
  plot_df <- df_model %>%
    group_by(Fitch.Long.Term.Issuer.Default.Rating) %>%
    summarise(mean_value = mean(debt_to_assets_ratio, na.rm = TRUE), .groups = "drop")
  
  p16 <- ggplot(plot_df, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = mean_value, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Mean Debt-to-Assets Ratio by Credit Rating",
      x = "Fitch Rating",
      y = "Mean Debt-to-Assets Ratio"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p16)
}

# 7.3 Mean GDP growth by rating
if ("gdp_growth" %in% names(df_model)) {
  plot_df <- df_model %>%
    group_by(Fitch.Long.Term.Issuer.Default.Rating) %>%
    summarise(mean_value = mean(gdp_growth, na.rm = TRUE), .groups = "drop")
  
  p17 <- ggplot(plot_df, aes(x = Fitch.Long.Term.Issuer.Default.Rating, y = mean_value, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Mean GDP Growth by Credit Rating",
      x = "Fitch Rating",
      y = "Mean GDP Growth"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p17)
}

# -------------------------
# 8) Optional: save selected plots
# Uncomment if needed
# -------------------------
ggsave("plot_rating_distribution.png", p1, width = 8, height = 5)
ggsave("plot_marketcap_by_rating.png", p4, width = 8, height = 5)
ggsave("plot_revenue_by_rating.png", p5, width = 8, height = 5)
ggsave("plot_debt_to_assets_by_rating.png", p7, width = 8, height = 5)
ggsave("plot_sector_rating_composition.png", p10, width = 9, height = 6)

