#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)
library(gt)
library(scales)
source('functions.R')

trades <- vroom('../temporary/trades_paxful_cleaned.csv')
matched_trades <- vroom('../temporary/matched_paxful_trades.csv')
total_volume <- vroom('../temporary/total_volume_by_country.csv')
flows <- vroom('../temporary/bilateral_flows_balanced.csv')
outflows <- vroom('../temporary/outflows_balanced.csv')
country_data <- read.csv('../temporary/country_data.csv')

matched_cross_border_trades <- matched_trades %>%
  filter(user_cc != user_cc2)


####US outflows graph

stimulus_1 <- ymd('2020-04-12')

stimulus_2 <- ymd('2021-01-03')

stimulus_3 <- ymd('2021-03-07')

stimulus_graph <- outflows %>%
  filter(user_cc == 'US',
         time >= '2019-01-01',
         time <= '2021-01-01') %>%
  ggplot(., aes(x = time, y = outflow)) +
  geom_line(color = 'blue', size = 0.8) +
  geom_vline(xintercept = stimulus_1, color = 'red', linewidth = 0.8) +
  annotate(x = stimulus_1, y = +Inf, label = "CARES ACT", vjust = 2, geom = "label") +
  scale_x_date(breaks = "month", date_labels = '%b %Y') +
  scale_y_continuous(breaks = c(400000, 600000, 800000, 1000000), labels = c('400,000', '600,000', '800,000', '1,000,000'))+
  xlab('Date') +
  ylab("USD") +
  ggtitle("Outflows from US") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = unit(c(1,1,1,1), "cm"))

show(stimulus_graph)
ggsave('../output/figures_paxful/stimulus_timing.png', plot = stimulus_graph, width = 9, height = 6, dpi = 300)

stimulus_graph_extended <- outflows %>%
  filter(user_cc == 'US',
         time >= '2019-01-01',
         time <= '2021-12-31') %>%
  ggplot(., aes(x = time, y = outflow)) +
  geom_line(color = 'blue', size = 0.8) +
  geom_vline(xintercept = stimulus_1, color = 'red', linewidth = 0.8) +
  geom_vline(xintercept = stimulus_2, color = 'red', linewidth = 0.8) +
  geom_vline(xintercept = stimulus_3, color = 'red', linewidth = 0.8) +
  annotate(x = stimulus_1, y = +Inf, label = "Round 1", vjust = 2, geom = "label") +
  annotate(x = stimulus_2, y = +Inf, label = "Round 2", vjust = 4, geom = "label") +
  annotate(x = stimulus_3, y = +Inf, label = "Round 3", vjust = 2, geom = "label") +
  scale_x_date(breaks = "month", date_labels = '%b %Y') +
  scale_y_continuous(breaks = c(400000, 600000, 800000, 1000000), labels = c('400,000', '600,000', '800,000', '1,000,000'))+
  xlab('Date') +
  ylab("USD") +
  ggtitle("Outflows from US") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = unit(c(1,1,1,1), "cm"))

show(stimulus_graph_extended)
ggsave('../output/figures_paxful/stimulus_timing_extended.png', plot = stimulus_graph_extended, width = 9, height = 6, dpi = 300)


####Paxful volume w/ price
volume_price <- getVolumePrice(trades, amount_usd, 'week')

paxful_volume_price <- volume_price %>%
  ggplot(., aes(x = time)) +
  geom_line(aes(y = volume, color = "Volume", linetype = 'Volume'), size=0.6) +
  geom_line(aes(y = price*1200, color = 'Bitcoin Price', linetype='Bitcoin Price'), size=0.6) +
  xlab("") +
  ylab("Volume (USD)") +
  theme_bw() +
  ggtitle("Paxful Volume (Weekly)")+
  scale_x_date(limit = c(as.Date('2017-03-01'), as.Date('2022-09-01')),
               breaks = date_breaks("3 months"),
               labels = date_format(format= "%b %Y")) +
  scale_y_continuous(breaks = c(40000000, 80000000), labels = c("$40M", "$80M"),
                     sec.axis = sec_axis(trans = ~./1200, name = "USD/Bitcoin")) +
  scale_color_manual(name = "Legend", values = c(
    'Volume' = 'blue',
    'Bitcoin Price' = 'red'
  )) +
  scale_linetype_manual(name = 'Legend', values = c(
    'Volume' = 1,
    'Bitcoin Price' = 2
  )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.15,0.85),
        legend.background = element_rect(fill = "white",
                                         size = 0.3,
                                         linetype = "solid",
                                         color = 'black'))

show(paxful_volume_price)
ggsave('../output/figures_paxful/paxful_volume_price.png', plot = paxful_volume_price, width = 9, height = 6, dpi = 300)

#Descriptive_stats
unmatched_stats <- list()
matched_stats <- list()
matched_cross_border_stats <- list()
unmatched_stats_2020 <- list()

unmatched_stats[['N']] <- nrow(trades)
matched_stats[['N']] <- nrow(matched_trades)
matched_cross_border_stats[['N']] <- nrow(matched_cross_border_trades)

unmatched_stats[['Average Trade Size']] <- mean(trades$amount_usd)
matched_stats[['Average Trade Size']] <- mean(matched_trades$amount_usd)
matched_cross_border_stats[['Average Trade Size']] <- mean(matched_cross_border_trades$amount_usd)

unmatched_stats[['Maximum Trade Size']] <- max(trades$amount_usd)
matched_stats[['Maximum Trade Size']] <- max(matched_trades$amount_usd)
matched_cross_border_stats[['Maximum Trade Size']] <- max(matched_cross_border_trades$amount_usd)

unmatched_stats[['Total Volume']] <- sum(trades$amount_usd)
matched_stats[['Total Volume']] <- sum(matched_trades$amount_usd)
matched_cross_border_stats[['Total Volume']] <- sum(matched_cross_border_trades$amount_usd)


unmatched_stats_2020[['Total Volume']] <- trades %>%
  filter(date >= '2020-01-01' & date < '2021-01-01') %>%
  pull(amount_usd) %>%
  sum

stats <- do.call(rbind, Map(data.frame, All = unmatched_stats, Matched = matched_stats))

stats <- as_tibble(cbind(unmatched_stats, matched_stats), rownames = NA) %>%
  rownames_to_column()

stats$unmatched_stats <- as.numeric(stats$unmatched_stats)
stats$matched_stats <- as.numeric(stats$matched_stats)



row_target <- c(2,3)

tbl <- as_tibble(stats) |>
  gt() |>
  tab_header(
    title = "Summary Statistics"
  ) |>
  fmt_currency(
    currency = "USD",
    decimals = 2,
    rows = row_target
  ) |>
  fmt_currency(
    rows = 4,
    currency = "USD",
    decimals = 1,
    suffixing = TRUE
  ) |>
  fmt_number(
    rows = 1,
    use_seps = TRUE,
    decimals = 0
  ) |>
  cols_label(
    unmatched_stats = "All Trades",
    matched_stats = "Matched"
  )

tbl |> gtsave('../output/summary_stats.tex')

as.character(tbl)



####Chart top importers/exporters

total_inflows <- flows %>%
  filter(user_cc != user_cc2) %>%
  group_by(user_cc2) %>%
  summarize(total = sum(volume)) %>%
  ungroup() %>%
  mutate(share = total/sum(total)) %>%
  left_join(country_data[, c("alpha.2", "label")], by = c("user_cc2" = "alpha.2")) %>%
  rename(code = user_cc2)

total_outflows <- flows %>%
  filter(user_cc != user_cc2) %>%
  group_by(user_cc) %>%
  summarize(total = sum(volume)) %>%
  ungroup() %>%
  mutate(share = total/sum(total)) %>%
  left_join(country_data[, c("alpha.2", "label", "income_group")], by = c("user_cc" = "alpha.2")) %>%
  rename(code = user_cc)

total_domestic <- flows %>%
  filter(user_cc == user_cc2) %>%
  group_by(user_cc) %>%
  summarize(total = sum(volume)) %>%
  ungroup() %>%
  mutate(share = total/sum(total)) %>%
  left_join(country_data[, c("alpha.2", "label")], by = c("user_cc" = "alpha.2")) %>%
  rename(code = user_cc)

total_combined_flows <- inner_join(total_inflows[, c("code", "total")], total_outflows[, c("code", "total")], by = "code", suffix = c("_inflows", "_outflows")) %>%
  inner_join(total_domestic[, c("code", "total")], by = "code") %>%
  rename(domestic = total) %>%
  inner_join(total_volume, by = c("code" = "user_cc")) %>%
  mutate(non_vehicle = total - domestic - total_inflows - total_outflows) %>%
  left_join(country_data[, c("alpha.2", "label")], by = c("code" = "alpha.2"))

country_pair_flows <- flows %>%
  filter(user_cc != user_cc2) %>%
  group_by(user_cc, user_cc2) %>%
  summarize(total = sum(volume)) %>%
  ungroup() %>%
  inner_join(., ., by = c("user_cc" = "user_cc2", "user_cc2" = "user_cc")) %>%
  mutate(combined = total.x + total.y) %>%
  distinct(combined, .keep_all=TRUE) %>%
  mutate(share = combined/sum(combined)) %>%
  left_join(country_data[, c("alpha.2", "label")], by = c("user_cc" = "alpha.2")) %>%
  left_join(country_data[, c('alpha.2', 'label')], by = c("user_cc2" = "alpha.2")) %>%
  mutate(combined_name = paste(label.x, label.y, sep = " - "))

#####graphs

##inflows
inflows_graph <- total_inflows %>%
  slice_max(share, n = 10) %>%
  ggplot(aes(x= reorder(label, share), y = share)) +
  geom_bar(stat = "identity", fill = "blue3") +
  coord_flip() +
  theme_bw() +
  ggtitle("Top Receivers") +
  xlab("") +
  ylab("Share of Global Inflows")

show(inflows_graph)

#outflows
outflows_graph <- total_outflows %>%
  slice_max(share, n = 10) %>%
  ggplot(aes(x= reorder(label, share), y = share)) +
  geom_bar(stat = "identity", fill = "red3") +
  coord_flip() +
  theme_bw() +
  ggtitle("Top Senders") +
  xlab("") +
  ylab("Share of Global Outflows")

show(outflows_graph)

#show outflows for US and other high income countries

top_senders_high_income <- total_outflows %>%
  filter(income_group == 'H') %>%
  slice_max(total, n = 5) %>%
  pull(code)

#construct flows to middle and low income countries
treated_countries <- c('JP', 'KR', 'SG')


flows_joined <- flows %>%
  filter(!user_cc %in% treated_countries,
         user_cc != user_cc2) %>%
  left_join(country_data, by = c('user_cc2' = 'alpha.2'))

outflows_lm <- flows_joined %>%
  filter(income_group %in% c('L', 'LM', 'UM')) %>%
  group_by(user_cc, time) %>%
  summarize(volume = sum(volume)) %>%
  left_join(country_data, by = c('user_cc' = 'alpha.2'))

top_senders_high_income <- outflows_lm %>%
  filter(income_group == 'H',
         time >= '2019-01-01' & time <= '2020-12-31') %>%
  group_by(user_cc) %>%
  summarize(total = sum(volume)) %>%
  slice_max(total, n = 6) %>%
  pull(user_cc)
  
  
outflows_lm_high_income_graph <- outflows_lm %>%
  filter(user_cc %in% top_senders_high_income,
         time >= '2019-01-01' & time <= '2020-12-31') %>%
  ggplot(., aes(x=time, y = volume, color = label, linetype=label)) +
  scale_y_log10(breaks = c(10000, 100000, 1000000), labels = c('10,000', '100,000', '1,000,000')) +
  geom_line() +
  scale_x_date(breaks='month', date_labels = '%b %Y') +
  xlab('Date') + 
  ylab('USD') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

show(outflows_lm_high_income_graph)

ggsave('../output/figures_paxful/outflows_us_vs_high_income.png', plot = outflows_lm_high_income_graph, width = 10, height = 6, dpi = 300)



#total flows, broken up by inflows/outflows
total_flows_graph <- total_combined_flows %>%
  slice_max(total, n = 10) %>%
  # select(-non_vehicle) %>%
  pivot_longer(cols = !c(code,label, total), names_to = "type", values_to = "flow") %>%
  mutate(type = factor(type,
                       levels = c("total_outflows", "total_inflows", "domestic", "non_vehicle"),
                       labels = c("Outflows", "Inflows", "Domestic", "Non-Vehicle"))) %>%
  ggplot(aes(fill = type, x = reorder(label, total), y = flow)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() + 
  theme_bw() +
  # ggtitle("Top Crypto Receivers/Senders") +
  xlab("") +
  ylab("Total Flows (USD)") +
  # scale_fill_brewer(palette = "GnBu") +
  scale_fill_viridis_d(option = "C", direction = -1, name = " ") +
  scale_y_continuous(breaks = c(1000000000, 2000000000, 3000000000),
                     labels = c("1B", "2B", "3B"))
# +
#   scale_fill_discrete(" ", breaks = c("total_inflows", "total_outflows"),
#                       labels = c("Inflows", "Outflows"))

show(total_flows_graph)

ggsave('../output/descriptive_graphs/flows_by_country.png', plot = total_flows_graph, width = 11, height = 6, dpi = 300)

###Break flows into proportions
flows_proportion_graph <- total_combined_flows %>%
  slice_max(total, n = 10) %>%
  # select(-non_vehicle) %>%
  pivot_longer(cols = !c(code,label, total), names_to = "type", values_to = "flow") %>%
  mutate(type = factor(type,
                       levels = c("total_outflows", "total_inflows", "domestic", "non_vehicle"),
                       labels = c("Outflows", "Inflows", "Domestic", "Non-Vehicle"))) %>%
  ggplot(aes(fill = type, x = reorder(label, total), y = flow)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() + 
  theme_bw() +
  # ggtitle("Top Crypto Receivers/Senders") +
  xlab("") +
  ylab("Proportion") +
  # scale_fill_brewer(palette = "GnBu") +
  scale_fill_viridis_d(option = "D", direction = -1, name = " ")
 
show(flows_proportion_graph)

ggsave('../output/descriptive_graphs/flows_proportion_by_country.png', width = 11, height = 6, dpi = 300)

####country pairs

country_pair_graph <- country_pair_flows %>%
  slice_max(combined, n = 10) %>%
  ggplot(aes(x = reorder(combined_name, combined), y = combined)) +
  geom_bar(stat = "identity", fill = "blue4") +
  theme_bw() +
  coord_flip() +
  xlab("Trading Pair") +
  ylab('Total Combined Trading Volume') +
  scale_y_continuous(breaks = c(50000000, 100000000, 150000000),
                     labels = c("50M", "100M", "150M"))

show(country_pair_graph)

ggsave('../output/descriptive_graphs/country_pairs.png', width = 11, height = 6, dpi = 300)

######migrants and US outflows
us_outflows_2019 <- getFlows(matched_trades, amount_usd, 'year') %>%
  filter(user_cc != user_cc2,
         time == '2019-01-01',
         user_cc == 'US') %>%
  left_join(country_data, by = c('user_cc2' = 'alpha.2'))

mod <- lm(log(volume)~log(fb1_per1000), us_outflows_2019)
coefs <- coef(mod)

migrant_us_flows_graph <- us_outflows_2019 %>%
  ggplot(aes(y = log(volume), x = log(fb1_per1000))) +
  geom_point() + 
  geom_smooth(method = 'lm',
              formula = y~x+I(x^2)) +
  geom_text(x = 10.5, y = 16,
            label = paste('coefficient = ', round(coefs[2], 2)),
            size = 5) +
  ylab("Ln(Cryptocurrency Inflows from United States, 2019)") +
  xlab("Ln(Foreign Born Population in US)") +
  theme_bw() +
  theme(axis.title = element_text(size = 15))

show(migrant_us_flows_graph)

ggsave('../output/descriptive_graphs/fb_usoutflows.png', plot = migrant_us_flows_graph, width = 8, height = 6, dpi = 300)

####migration shares
country_data <- country_data %>%
  mutate(fb_share = fb1/sum(fb1, na.rm = TRUE))

migrant_shares_chart <- country_data %>%
  slice_max(fb_share, n = 20) %>%
  ggplot(aes(x=reorder(label, fb_share), y = fb_share, fill = factor(income_group))) + 
  geom_bar(stat = 'identity') +
  xlab("") +
  ylab("Share of Total Foreign Born Population") +
  coord_flip() +
  scale_fill_viridis_d(option = "D", direction = -1, name = "Income Group", breaks = c('H', 'UM', 'LM', 'L'),
                       labels = c('High', 'Upper-Middle', 'Lower-Middle', 'Low')) +
  theme_bw()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11))

show(migrant_shares_chart)

ggsave('../output/descriptive_graphs/migrant_shares.png', width = 11, height = 6, dpi = 300)

####countries in data

total_volume <- total_volume %>% inner_join(country_data, by = c("user_cc" = "alpha.2"))

write.csv(total_volume[, c('user_cc', 'income_group', 'label')], '../temporary/countries_by_income.csv')


