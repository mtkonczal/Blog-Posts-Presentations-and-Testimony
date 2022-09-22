####
# 2_Analysis_for_paper
# Author: Mike Konczal
# Date: June 21th, 2022
#
# This is R code for creating the analysis and graphics that form
# the paper "Prices, Profits, and Power: An Analysis of 2021 Firm-Level Markups"
# by Mike Konczal and Niko Lusiani, from the Roosevelt Institute:
# https://rooseveltinstitute.org/publications/prices-profits-and-power/
#
# This methodology, specific code, and especially several of the graphics are
# replicated, in large part, from the paper:
# "The Rise of Market Power and the Macroeconomic Implications"
# by De Loecker, Jan; Eeckhout, Jan; Unger, Gabriel, 2020,
# whose replication code can be found at:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5GH8XO
#
# Their Stata code has been replicated over to R, any errors are introduced here.
#
# This code takes in Compustat data that has been cleaned through 1_Compustat_data_creation,
# and it assumes it is already loaded in the environment when it is run.

# Set your own working directory
library(janitor)
library(tidyverse)
library(ggtext)
library(haven)
library(scales)

# Uncomment this if you need to run the previous step.
#source("1_Compustat_data_creation.R")

#### Figure 1: Total markups by year ####
g3_1 <- cs_m %>%
  select(year, markup1_ACG, markup2_ACG, markup3_ACG) %>%
  group_by(year) %>%
  summarize(Markup1 = mean(markup1_ACG), Markup2 = mean(markup2_ACG), Markup3 = mean(markup3_ACG), diff = Markup1 - Markup2) %>%
  ungroup()

g3_1p <- ggplot(g3_1, aes(year, Markup2)) + geom_line(color="red") + theme_classic() +
  xlab("") + ylab("") + scale_x_continuous(breaks = seq(1955,2021,6)) +
  scale_y_continuous(breaks = seq(1.0,1.8,0.1)) +
  theme(panel.grid.major.y = element_line(size=0.5))

#### Figure 2: Sales versus cogs of average firm by year ####
moving_levels <- cs_m %>%
  group_by(year) %>% summarize("Total Sales" = sum(sale_D)/n(),
                               "Cost of Goods Sold" = sum(cogs_D)/n()) %>%
  pivot_longer(2:3, names_to="type", values_to="type_values")

ml_g1 <- ggplot(moving_levels, aes(x=year, y=type_values, color=type)) + geom_line() + theme_classic() +
  theme(legend.position="bottom", legend.title=element_blank()) +
  xlab("") + ylab("") + scale_x_continuous(breaks = seq(1955,2021,6)) +
  theme(panel.grid.major.y = element_line(size=0.5))  + scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = comma)

#### Figure 3: Profits ####
g31_ni <- cs_m %>%
  group_by(year) %>%
  filter(!is.na(ni_D)) %>%
  mutate(ni_ACG = sum(share_firm_agg*ni_D)) %>%
  summarize(ni_profits = sum(ni)/sum(sale))

g31_1 <- cs_m %>%
  group_by(year) %>%
  summarize(profits2 = mean(profits2_ACG)) %>%
  ungroup() %>%
  left_join(g31_ni, by="year") %>% rename("Operating Profit Margin" = profits2, "Net Profit Margin" = ni_profits) %>%
  pivot_longer(2:3, names_to="type", values_to="type_values")

g31_1p <- ggplot(g31_1, aes(year, type_values, color=type)) + geom_line() + theme_classic() +
  theme(legend.position = "bottom", legend.title=element_blank()) + xlab("") + ylab("") + scale_x_continuous(breaks = seq(1955,2021,6)) +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.major.y = element_line(size=0.5)) + scale_color_brewer(palette = "Set1")

#### Figure 4: Distribution of markups ####
pcs_m <- cs_m %>%
  group_by(year) %>%
  mutate(p10 = quantile(markup2,.1), p25 = quantile(markup2,.25), p50 = quantile(markup2,.50), p75 = quantile(markup2,.75), p90 = quantile(markup2,.9)) %>%
  mutate(ps10 = quantile(markup2_S,.1), ps25 = quantile(markup2_S,.25), ps50 = quantile(markup2_S,.50), ps75 = quantile(markup2_S,.75), ps90 = quantile(markup2_S,.9)) %>%  
  ungroup() %>% arrange(year) %>% filter(year != lag(year,1)) %>%
  pivot_longer(p10:p90, names_to="p_name", values_to="p_value") %>%
  pivot_longer(ps10:ps90, names_to="ps_name", values_to="ps_value")

dist1 <- ggplot(pcs_m, aes(x=year, y=p_value, color=p_name)) + geom_line() + theme_classic() + theme(legend.position="bottom") +
  xlab("") + ylab("") + scale_x_continuous(breaks = seq(1955,2021,6)) +
  theme(panel.grid.major.y = element_line(size=0.5)) + theme(legend.title=element_blank()) + scale_color_brewer(palette = "Set1")

#### Figure 5: Industry breakdown of markups ####
i_mu <- cs_m
test1 <- cs_m %>% group_by(ind2d) %>% summarize(org = n())
i_mu$ind2d <- ifelse(i_mu$ind2d == 49, 48, i_mu$ind2d)
i_mu$ind2d <- ifelse(i_mu$ind2d == 32, 31, i_mu$ind2d)
i_mu$ind2d <- ifelse(i_mu$ind2d == 33, 31, i_mu$ind2d)
i_mu$ind2d <- ifelse(i_mu$ind2d == 45, 44, i_mu$ind2d)
test2 <- i_mu %>% group_by(ind2d) %>% summarize(adjusted = n())
test3 <- left_join(test1, test2, by="ind2d") %>% mutate(diff = org - adjusted)

i_mu <- i_mu %>%
  group_by(year, ind2d) %>%
  mutate(totsales = sum(sale_D)) %>%
  mutate(share_firm_agg = sale_D/totsales) %>%
  mutate(MARKUP1_ACGi = sum(share_firm_agg*markup2)) %>%
  ungroup() %>%
  arrange(year) %>%
  select(year, MARKUP1_ACGi, ind2d) %>%
  group_by(year, ind2d) %>%
  #We aren't actually taking a mean, it's all the same value for all ind2d in each year
  summarize(mean(MARKUP1_ACGi)) %>%
  rename(meanMU = `mean(MARKUP1_ACGi)`) %>%
  ungroup() %>% group_by(ind2d) %>% arrange(year) %>%
  mutate(laggedMU = lag(meanMU,2) + lag(meanMU,1) + lag(meanMU,3), diff_laggedMU = meanMU - laggedMU/3) %>%
  filter(year > 2010) %>%
  ungroup()

naics_two_digit_codes <- read_csv("data/2_digit_codes.csv")
i_mu <- i_mu %>% left_join(naics_two_digit_codes, by=c("ind2d"="Sector")) %>% filter(!is.na(Description))
i_mu_f <- ggplot(i_mu, aes(year, meanMU)) + geom_line() + facet_wrap("Description", ncol=3) +
  theme_classic() +   theme(panel.grid.major.y = element_line(size=0.5)) +   xlab("") + ylab("")
i_mu2 <- i_mu %>% filter(year == 2021)
i_mu_g <- ggplot(i_mu2, aes(x=Description, y=diff_laggedMU)) + geom_bar(stat="identity", fill="#377EB8") + coord_flip() + theme_classic() +
  xlab("") + ylab("")

#### Figure 6: Within versus between industry markups ####
table2_0 <- cs_m %>%
  group_by(year) %>%
  mutate(TOTSALES = sum(sale_D)) %>% ungroup() %>%
  group_by (year, ind3d) %>%
  mutate(TOTSALES_IND_3 = sum(sale_D)) %>% ungroup() %>%
  mutate(share_IND3 = TOTSALES_IND_3/TOTSALES) %>%
  mutate(share_ind_3 = sale_D/TOTSALES_IND_3) %>%
  group_by(year, ind3d) %>%
  mutate(MARKUP_sp1_IND_3 = sum(share_ind_3*markup2)) %>%
  mutate(ThetaW1_c_IND3 = sum(share_ind_3*0.85)) %>%
  ungroup()

table2 <- table2_0 %>%
  select(ind3d, year, MARKUP_sp1_IND_3, share_IND3, markup2_ACG, ThetaW1_c_IND3) %>%
  arrange(ind3d, year) %>%
  filter(!(ind3d == lag(ind3d,1) & year == lag(year,1))) %>%
  group_by(ind3d) %>%
  mutate(delta_mu_1_IND3_st = MARKUP_sp1_IND_3 - lag(MARKUP_sp1_IND_3, 1)) %>%
  mutate(within_1_IND3_st		=	lag(share_IND3,1)*delta_mu_1_IND3_st) %>%
  mutate(delta_sh_1_IND3_st		=	share_IND3 -lag(share_IND3,1)) %>%
  mutate(between_1_IND3_st		=	lag(MARKUP_sp1_IND_3,1)*delta_sh_1_IND3_st) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(WITHIN1_IND3_st	= sum(within_1_IND3_st, na.rm = TRUE)) %>%
  mutate(BETWEEN1_IND3_st	= sum(between_1_IND3_st, na.rm = TRUE)) %>%
  ungroup() %>% arrange(year) %>%
  filter(year != lag(year,1)) %>%
  mutate(DMARKUP_spec1 = markup2_ACG - lag(markup2_ACG,1)) %>%
  select(year, markup2_ACG, DMARKUP_spec1, WITHIN1_IND3_st, BETWEEN1_IND3_st) %>%
  arrange(year) %>% filter(year > 1980) %>%
  mutate(within = cumsum(WITHIN1_IND3_st), between = cumsum(BETWEEN1_IND3_st)) %>%
  select(year, within, between) %>% rename("Within Industry" = within, "Between Industry" = between) %>%
  rbind(c(1980, 0, 0)) %>%
  gather(key = "key", value = "value", "Within Industry", "Between Industry")

within_graphic2 <- ggplot(table2, aes(year, value, color=key)) + geom_line() + theme_classic() +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  xlab("") + ylab("Cumulative Change in Markups") + scale_x_continuous(breaks = seq(1981,2021,5)) +
  theme(panel.grid.major.y = element_line(size=0.5)) + scale_color_brewer(palette = "Set1")

#### Figure 7-8, Table 1: Regressions ####
mp <- cs_m %>% arrange(year) %>% group_by(conm) %>%
  mutate(diff_MU1S = markup1_S - lag(markup1_S, 2), diff_MU2S = markup2_S - lag(markup2_S, 2), lagMU1S = lag(markup1_S, 2), lagMU2S = lag(markup2_S, 2)) %>%
  mutate(avg_MU2 = (lag(markup2_S,1)+lag(markup2_S,2)+lag(markup2_S,3))/3, avg_diff_MU2S = markup2_S - avg_MU2) %>%
  mutate(avg_MU2NS = (lag(markup2,1)+lag(markup2,2)+lag(markup2,3))/3, lagMU2NS = lag(markup2,2), avg_diff_MU2NS = markup2 - avg_MU2NS) %>%
  mutate(lag_share_firm_agg = lag(share_firm_agg, 1)) %>%
  mutate(diff_profit = profits2_S - lag(profits2_S,1)) %>%
  filter(year == 2021) %>% ungroup()

mp_g1 <- ggplot(mp, aes(avg_MU2, avg_diff_MU2S)) + geom_point(alpha=0.4) + theme_classic() + geom_smooth(method="lm") +
  xlab("Average Size-Adjusted Markup 2018-2020") + ylab("Change in Size-Adjusted Markup to 2021") + theme(panel.grid.major.y = element_line(size=0.5))

mp_outliers <- mp %>% filter(!is.na(avg_MU2), !is.na(avg_diff_MU2S)) %>%
  filter(avg_MU2 < quantile(avg_MU2,0.99) & avg_diff_MU2S < quantile(avg_diff_MU2S,0.99) &
           avg_MU2 > quantile(avg_MU2,0.01) & avg_diff_MU2S > quantile(avg_diff_MU2S,0.01)) %>%
  mutate(Nout_avg_diff_MU2S = avg_diff_MU2S)

mp_g2 <- ggplot(mp_outliers, aes(avg_MU2, avg_diff_MU2S)) + geom_point(alpha=0.4) + theme_classic() + geom_smooth(method="lm") +
  xlab("Average Size-Adjusted Markup 2018-2020") + ylab("Change in Size-Adjusted Markup to 2021") + theme(panel.grid.major.y = element_line(size=0.5))

S_lm <- lm(avg_diff_MU2S ~ avg_MU2 + factor(ind3d), data=mp)
summary(S_lm)

S_lm_NI <- lm(avg_diff_MU2S ~ avg_MU2, data=mp)
summary(S_lm_NI)

NOut_lm <- lm(Nout_avg_diff_MU2S ~ avg_MU2 + factor(ind3d), data=mp_outliers)
summary(NOut_lm)

Nout_lm_NI <- lm(Nout_avg_diff_MU2S ~ avg_MU2, data=mp_outliers)
summary(Nout_lm_NI)

NS_lm <- lm(avg_diff_MU2NS ~ avg_MU2NS + factor(ind3d), data=mp)
summary(NS_lm)

NS_lm_NI <- lm(avg_diff_MU2NS ~ avg_MU2NS, data=mp)
summary(NS_lm_NI)

mp_lm_list <- list(S_lm, S_lm_NI, NOut_lm, Nout_lm_NI, NS_lm, NS_lm_NI)