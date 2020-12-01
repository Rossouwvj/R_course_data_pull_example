
# Setup
#====================================================================================

library(pacman)
p_load(rdbnomics, tidyverse, lubridate, plotly, kableExtra, readxl)


# Using DBNomics
#====================================================================================

# Single case example
  ca_gs <- rdb(ids = "SARB/CA/Q_BAAPOG_BOGAS") %>% 
    filter(period >= as.Date("2000-01-01"))
  
  ggplot(ca_gs, aes(x=period, y=value)) + 
    geom_line() + theme_bw() + ggtitle("Balance on G&S as % of GDP") + ylab("") + xlab("")

# Multiple series example
  ca_multiple <- rdb(ids = c("SARB/CA/Q_BAAPOG_BOGAS","SARB/CA/Q_BAAPOG_BOPI"))
  
  ggplot(ca_multiple, aes(x=period, y=value)) + 
    geom_line() + theme_bw() + facet_wrap(~series_name, scales = "free_y", labeller = label_wrap_gen(multi_line = TRUE))

# Pulling in an entire database
policy_rates <-  rdb(provider_code = "BIS", dataset_code = "cbpol" , mask = "D..") %>% 
  filter(!is.na(value))

# Filter out only BRICS from 2019-09-30
policy_rates_brics <- policy_rates %>% 
  filter(`Reference area` %in% c("China", "Brazil", "South Africa", "India", "Russia")) %>% 
  filter(period >= as.Date("2019-09-30"))
  
ggplot(policy_rates_brics, aes(period, value, group = 1)) + 
  geom_line() + theme_bw() + facet_wrap(~`Reference area`, scales = "free_y")

# Filter out G7 from 2019-09-30
policy_rates_g7 <- policy_rates %>% 
  filter(`Reference area` %in% c("United States", "Euro area", "Japan", "United Kingdom", "Canada")) %>% 
  filter(period >= as.Date("2019-09-30"))

ggplot(policy_rates_g7, aes(period, value, group = 1)) + 
  geom_line() + theme_bw() + facet_wrap(~`Reference area`, scales = "free_y")



# A table of that -------------------

change_index <- which(policy_rates_brics$value != dplyr::lag(policy_rates_brics$value) & 
        policy_rates_brics$`Reference area` == dplyr::lag(policy_rates_brics$`Reference area`))


prev_pol <- policy_rates_brics[(change_index-1),] %>% 
  group_by(series_name) %>% 
  summarise(period=max(period))

pol_tab_prev <- policy_rates_brics %>% 
  inner_join(prev_pol)

last_pol <- policy_rates_brics[change_index,] %>% 
  group_by(series_name) %>% 
  summarise(period=max(period))

pol_tab_last <- policy_rates_brics %>% 
  inner_join(last_pol)

pol_tab_all <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 4))
colnames(pol_tab_all) <- c("Country", "Current rate", "Prev rate", "Date of change")

pol_tab_all$Country <- pol_tab_last$`Reference area`
pol_tab_all$`Current rate` <- pol_tab_last$value
pol_tab_all$`Prev rate` <- pol_tab_prev$value
pol_tab_all$`Date of change` <- pol_tab_last$original_period

pol_tab_all %>%
  kbl() %>%
  kable_styling()

#====================================================================================
# Loading a zip file directly 
#====================================================================================

download.file("http://www.statssa.gov.za/timeseriesdata/Excel/P0141%20-%20CPI%20Average%20Prices%20All%20urban%20(202010).zip", "cpi.zip")
unzip("cpi.zip")
cpi <- read_excel("CPI_Average Prices_All urban(202010).xlsx")

