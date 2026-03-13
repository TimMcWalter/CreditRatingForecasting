library(WDI)
library(dplyr)

snapshot_year <- 2023

# 1) Clean your country column
df <- df %>%
  mutate(country_iso2 = toupper(trimws(Cntry.Terrtry)))

# 2) Get list of valid ISO-2 codes from WDI / World Bank
valid_codes <- unique(WDI_data$country$iso2c)
valid_codes <- valid_codes[!is.na(valid_codes) & valid_codes != ""]

# 3) Keep only valid country codes
countries <- unique(df$country_iso2)
countries <- countries[countries %in% valid_codes]

# Optional: inspect dropped codes
invalid_codes <- setdiff(unique(df$country_iso2), valid_codes)
print(sort(invalid_codes))

# 4) Pull macro data
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
  
print(colSums(is.na(macro_wide)))
macro_wide <- na.omit(macro_wide)

# 5) Merge back
df_model <- df %>%
  left_join(macro_wide, by = "country_iso2")