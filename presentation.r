rating_counts <- table(df_model$Fitch.Long.Term.Issuer.Default.Rating)

barplot(
  rating_counts,
  col="steelblue",
  main="Distribution of Credit Ratings",
  xlab="Rating",
  ylab="Number of Observations",
  las=2
)

plot(
  rf_default$err.rate[,1],
  type="l",
  col="blue",
  lwd=2,
  xlab="Number of Trees",
  ylab="OOB Classification Error",
  main="Out-of-Bag Error vs Number of Trees"
)
plot(
  rf_gridsearch$results$mtry,
  rf_gridsearch$results$Accuracy,
  type="b",
  pch=19,
  col="darkgreen",
  xlab="mtry",
  ylab="Cross-Validation Accuracy",
  main="Random Forest Hyperparameter Tuning"
)

varImpPlot(
  rf_default,
  sort=TRUE,
  type=1,
  main="Variable Importance (Mean Decrease Accuracy)"
)
conf_mat <- table(
  Actual = df_model$Fitch.Long.Term.Issuer.Default.Rating,
  Predicted = rf_default$predicted
)


hist(
  df_model$debt_to_equity_ratio,
  col="lightblue",
  main="Distribution of Debt-to-Equity Ratio",
  xlab="Debt-to-Equity Ratio"
)

hist(
  df_model$roa_ratio,
  col="lightblue",
  main="Distribution of ROA",
  xlab="Return on Assets"

)

boxplot(
  debt_to_ebitda_ratio ~ Fitch.Long.Term.Issuer.Default.Rating,
  data=df_model,
  col="lightgreen",
  las=2,
  main="Debt-to-EBITDA Ratio by Rating",
  xlab="Rating",
  ylab="Debt-to-EBITDA"
)
plot(
  df_model$debt_to_ebitda_ratio,
  df_model$fcf_to_debt_ratio,
  pch=19,
  col="darkblue",
  xlab="Debt-to-EBITDA",
  ylab="FCF-to-Debt",
  main="Debt Capacity vs Cash Flow Strength"
)
#
cm <- matrix(c(
  46,18,8,0,1,0,0,0,0,0,0,1,0,12,2,22,0,0,2,0,0,0,0,
   9,40,4,0,1,0,0,0,0,1,1,0,1,28,3,47,0,0,0,0,1,0,0,
  12,6,12,0,6,0,0,0,0,0,0,0,0,5,1,7,0,0,1,0,0,0,0,
   1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,
   1,4,8,0,12,0,0,0,0,0,1,1,0,1,1,2,0,0,0,0,0,0,0,
   1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   2,0,0,0,0,0,0,9,4,6,4,10,1,8,7,1,0,0,0,0,0,0,0,
   0,0,1,0,0,0,0,7,4,4,3,4,1,5,2,1,0,0,0,3,0,0,0,
   0,0,0,0,0,0,0,3,3,19,4,19,2,6,6,1,0,0,1,0,2,0,0,
   2,3,0,0,0,0,0,1,2,6,19,12,17,17,32,7,0,0,0,0,0,0,0,
   1,1,1,0,0,0,0,4,0,11,11,32,9,20,16,4,0,0,0,0,0,0,0,
   0,1,0,0,0,0,0,0,0,3,17,7,28,36,37,11,0,0,0,0,1,0,0,
   4,9,1,0,0,0,0,0,0,1,4,6,14,121,51,61,0,0,0,0,0,0,0,
   1,5,0,0,0,0,0,4,0,6,10,2,14,79,89,28,0,0,0,0,1,0,0,
  12,26,0,0,0,0,0,0,0,1,0,4,3,78,27,68,0,0,0,0,1,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
   0,0,0,0,0,0,0,1,0,0,0,0,0,2,0,0,0,0,1,0,0,0,0,
   0,0,0,0,0,0,0,0,1,0,0,3,0,1,3,0,0,1,4,0,2,0,0,
   0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,1,5,3,1,1,0,2,1,0,0,0,3,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,2,0,1,1,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0
), nrow = 23, byrow = TRUE)

labels_in_matrix <- c(
  "A", "A-", "A+", "AA", "AA-", "AA+", "AAA",
  "B", "B-", "B+", "BB", "BB-", "BB+",
  "BBB", "BBB-", "BBB+",
  "C", "CC", "CCC", "CCC-", "CCC+", "D", "RD"
)

rownames(cm) <- labels_in_matrix
colnames(cm) <- labels_in_matrix

# ==========================================
# 2) Define the true ordinal order of ratings
# ==========================================
true_order <- c(
  "AAA",
  "AA+", "AA", "AA-",
  "A+", "A", "A-",
  "BBB+", "BBB", "BBB-",
  "BB+", "BB", "BB-",
  "B+", "B", "B-",
  "CCC+", "CCC", "CCC-",
  "CC", "C",
  "D", "RD"
)

rating_to_num <- setNames(seq_along(true_order), true_order)

# ==========================================
# 3) Expand confusion matrix into row-level data
# ==========================================
actual <- c()
predicted <- c()

for (i in seq_len(nrow(cm))) {
  for (j in seq_len(ncol(cm))) {
    n_ij <- cm[i, j]
    if (n_ij > 0) {
      actual <- c(actual, rep(rownames(cm)[i], n_ij))
      predicted <- c(predicted, rep(colnames(cm)[j], n_ij))
    }
  }
}

actual_num <- unname(rating_to_num[actual])
pred_num   <- unname(rating_to_num[predicted])

abs_errors <- abs(actual_num - pred_num)

# ==========================================
# 4) Build cumulative tolerance table
# ==========================================
max_tol <- max(abs_errors)

cum_df <- data.frame(
  tolerance = 0:max_tol
)

cum_df$count <- sapply(cum_df$tolerance, function(t) sum(abs_errors <= t))
cum_df$share <- cum_df$count / length(abs_errors)

print(cum_df)

# ==========================================
# 5) Plot cumulative accuracy chart
# ==========================================
plot(
  cum_df$tolerance,
  cum_df$share,
  type = "b",
  pch = 19,
  xaxt = "n",
  xlab = "Allowed prediction error (notches)",
  ylab = "Cumulative share of predictions",
  main = "Cumulative Accuracy by Rating Tolerance",
  ylim = c(0, 1)
)

axis(1, at = cum_df$tolerance)
grid()

text(
  cum_df$tolerance,
  cum_df$share,
  labels = paste0(round(100 * cum_df$share, 1), "%"),
  pos = 3,
  cex = 0.8
)

points(cum_df$tolerance[1], cum_df$share[1], pch = 19, cex = 1.3)
text(
  x = cum_df$tolerance[1],
  y = cum_df$share[1],
  labels = paste0(" Exact: ", round(100 * cum_df$share[1], 1), "%"),
  pos = 4,
  cex = 0.9
)

if (nrow(cum_df) >= 2) {
  points(cum_df$tolerance[2], cum_df$share[2], pch = 19, cex = 1.3)
  text(
    x = cum_df$tolerance[2],
    y = cum_df$share[2],
    labels = paste0(" Within 1 notch: ", round(100 * cum_df$share[2], 1), "%"),
    pos = 4,
    cex = 0.9
  )
}

numeric_vars <- df_model[sapply(df_model, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)
library(corrplot)

png("correlation_matrix.png", width = 1600, height = 1400, res = 200)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.cex = 0.8,
  tl.col = "black",
  diag = FALSE
)

dev.off()