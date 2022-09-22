####
# 1_Compustat_data_creation
# Author: Mike Konczal
# Date: June 21th, 2022
#
# This is R code for taking Compustat data and cleaning it for
# the paper "Prices, Profits, and Power: An Analysis of 2021 Firm-Level Markups"
# by Mike Konczal and Niko Lusiani, from the Roosevelt Institute:
# https://rooseveltinstitute.org/publications/prices-profits-and-power/
#
# This methodology and specific code is replicated, in large part, from the paper:
# "The Rise of Market Power and the Macroeconomic Implications"
# by De Loecker, Jan; Eeckhout, Jan; Unger, Gabriel, 2020,
# whose replication code can be found at:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5GH8XO
#
# Their Stata code has been replicated over to R, any errors are introduced here.
#
# This code takes in Compustat data, cleans it, and then creates the necessary variables.
# You need to access Compustat data, taking the full dataset and the
# variables referenced below, for this code to work.

# Set your own working directory
library(janitor)
library(tidyverse)
library(ggtext)
library(haven)

# Insert your data source here
cs_m <- read_stata("/Users/mkonczal/Documents/data_folder/compustat/compustat_full_060622.dta")
###########

# Check how many values there are
tail(cs_m %>% group_by(fyear) %>% summarize(n = n()))
# As of March 2nd 2022, there are 5,809 observations for 2021.
# As of March 22nd 2022, there are 8,152 observations for 2021.
# As of May 11th 2022, there are 11,017 observations for 2021.
# As of June 6th 2022, there are 11,490 observations for 2021.

cs_m <- cs_m %>%
  rename(year = fyear) %>%
  group_by(gvkey, year) %>%
  mutate(nrobs = n()) %>%
  ungroup() %>%
  arrange(gvkey, year) %>%
  filter(!(( nrobs == 2 | nrobs ==3) & indfmt == "FS"))

cs_m <- cs_m %>%
  filter(!(gvkey == lag(gvkey,1) & year == lag(year,1)))

cs_m <- cs_m %>%
  filter(!is.na(naics), !(naics == "")) %>%
  mutate(ind2d = substr(naics,1,2)) %>%
  group_by(ind2d) %>%
  mutate(nrind2 = cur_group_id()) %>%
  ungroup() %>%
  
  mutate(ind3d = substr(naics,1,3)) %>%
  group_by(ind3d) %>%
  mutate(nrind3 = cur_group_id()) %>%
  ungroup() %>%
  
  mutate(ind4d = substr(naics,1,4)) %>%
  group_by(ind4d) %>%
  mutate(nrind4 = cur_group_id()) %>%
  ungroup() %>%
  
  mutate(newmk2 = prcc_f * csho) %>%
  mutate(mkvalt = ifelse(is.na(mkvalt), newmk2, mkvalt))

# Adjust for units.
cs_m <- cs_m %>%
    mutate(sale = sale*1000) %>%
    mutate(xlr = xlr*1000) %>%
    mutate(oibdp = oibdp*1000) %>%
    mutate(cogs	= cogs*1000) %>%
    mutate(xsga = xsga*1000) %>%
    mutate(mkvalt = mkvalt*1000) %>%
    mutate(dvt = dvt*1000) %>%
    mutate(ppegt = ppegt*1000) %>%
    mutate(intan = intan*1000)

# Data source with inflation metrics.
macro_vars <- read_stata(file="data/macro_vars.dta")

cs_m <- cs_m %>%
    inner_join(macro_vars, by="year") %>%
    #Deflated values
    mutate(sale_D	= (sale/USGDP)*100) %>%
    mutate(cogs_D = (cogs/USGDP)*100) %>%
    mutate(xsga_D = (xsga/USGDP)*100) %>%
    mutate(mkvalt_D = (mkvalt/USGDP)*100) %>%
    mutate(dividend_D	= (dvt/USGDP)*100) %>%
    mutate(capital_D = (ppegt/USGDP)*100) %>%
    mutate(intan_D = (intan/USGDP)*100) %>%
    mutate(xlr_D = (xlr/USGDP)*100) %>%
    mutate(kexp = (usercost*capital_D)) %>%
    mutate(mat1 = ((sale-xlr-oibdp)/USGDP)*100)

# Start filtering out missing data.
cs_m <- cs_m %>%
    filter(!(sale_D<0) | is.na(sale_D)) %>% 
    filter(!(cogs_D<0) | is.na(cogs_D)) %>%
    filter(!(xsga<0) | is.na(xsga)) %>%
    mutate(s_g = sale/cogs) %>%
    filter(s_g != Inf) %>%
    filter(s_g>0) %>%
    mutate(trim=0) %>%
    filter(year>1949)

# Filter out the top and bottom 1 percent of sales to cogs ratio.
cs_m <-cs_m %>%
  group_by(year) %>%
  mutate(s_g_p_1 = quantile(s_g, .01, type=2, na.rm=TRUE)) %>%
  mutate(s_g_p_99 = quantile(s_g, .99, type=2, na.rm=TRUE)) %>%
  ungroup()

cs_m <- cs_m %>%
  filter(s_g > s_g_p_1, s_g < s_g_p_99)

cs_m <- cs_m %>% group_by(gvkey) %>% mutate(id = cur_group_id()) %>% filter(!is.na(id)) %>% ungroup()

cs_m <- cs_m %>%
  mutate(costshare0 = 0.85) %>%
  mutate(costshare1 = cogs_D/(cogs_D+kexp)) %>%
  mutate(costshare2 = cogs_D/(cogs_D+xsga_D+kexp)) %>%
  
  mutate(mu_0 = costshare0*(sale_D/cogs_D)) %>%
  mutate(mu_1 = costshare1*(sale_D/cogs_D)) %>%
  mutate(mu_2 = costshare2*(sale_D/cogs_D))

# Filter out the top and bottom 1 percent of costshares.
cs_m <- cs_m %>% group_by(year) %>%
  mutate(cs1_p1 = quantile(costshare1, type=2, 0.01, na.rm=TRUE)) %>%
  mutate(cs1_p99 = quantile(costshare1, type=2, 0.99, na.rm=TRUE)) %>%
  filter(costshare1 != 0, !is.na(costshare1)) %>%
  filter(costshare1 >= cs1_p1, costshare1 <= cs1_p99) %>%
  ungroup()

cs_m <- cs_m %>% group_by(year) %>%
  mutate(cs2_p1 = quantile(costshare2, type=2, 0.01, na.rm=TRUE)) %>%
  mutate(cs2_p99 = quantile(costshare2, type=2, 0.99, na.rm=TRUE)) %>%
  filter(costshare2 != 0, !is.na(costshare2)) %>%
  filter(costshare2 >= cs2_p1, costshare2 <= cs2_p99) %>%
  ungroup()

# Extra formatting for variables we have brought in
cs_m <- cs_m %>%
  mutate(ni = ni*1000, ni_D = 100*ni/USGDP) %>%
  mutate(prstkc = prstkc*1000, prstkc_D = 100*prstkc/USGDP) %>%
  mutate(capx = capx*1000, capx_D = 100*capx/USGDP)

# Read in their theta values extended to 2021, to use as a check against a flat theta
theta_10 <- read.csv("data/theta_values.csv")

# Create markups by firm
cs_m <- cs_m %>%
  group_by(ind2d, year) %>%
  mutate(ind2d = as.numeric(ind2d)) %>%
  inner_join(theta_10, by=c("ind2d", "year")) %>%
  mutate(markup1 = theta_WI1_ct*(sale_D/cogs_D)) %>%
  mutate(markup2 = 0.85*sale_D/cogs_D) %>%
  mutate(markup3 = 0.85*(sale_D/(cogs_D+xsga_D))) %>%
  mutate(profit_margin = (sale_D - cogs_D - xsga_D)/sale_D) %>%
  ungroup() %>%
  mutate(ni_margin = (ni_D)/sale_D)

# Create size-adjusted markups for firms
cs_m <- cs_m %>%
  group_by(year) %>%
  mutate(totsales = sum(sale_D)) %>%
  ungroup() %>%
  mutate(share_firm_agg = sale_D/totsales) %>%
  mutate(markup1_S = markup1*share_firm_agg) %>%
  mutate(markup2_S = markup2*share_firm_agg) %>%
  mutate(markup3_S = markup3*share_firm_agg) %>%
  mutate(profits2_S = profit_margin*share_firm_agg) %>%
  mutate(ni_S = ni_margin*share_firm_agg) %>%
  group_by(year) %>%
  mutate(markup1_ACG = sum(share_firm_agg*markup1)) %>%
  mutate(markup2_ACG = sum(share_firm_agg*markup2)) %>%
  mutate(markup3_ACG = sum(share_firm_agg*markup3)) %>%
  mutate(profits2_ACG = sum(share_firm_agg*profit_margin)) %>%
  ungroup()  %>%
  arrange(year)

# Save the file in the relevant directory
save(cs_m, file = "/Users/mkonczal/Documents/data_folder/compustat/compustat_markups.RData")