
## start
print("new line")
df <- read.csv("data_cleansed_csv.csv",sep = ";", header = TRUE)
colnames(df) <- gsub(" ", "", colnames(df))
print(colnames(df))
print(summary(df))
print(str(df))
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

