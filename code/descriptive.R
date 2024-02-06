#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)
library(gt)
source('functions.R')

trades <- vroom('../temporary/trades_paxful_cleaned.csv')
matched_trades <- vroom('../temporary/matched_paxful_trades.csv')
flows <- vroom('../temporary/bilateral_flows_balanced.csv')
outflows <- vroom('../temporary/outflows_balanced.csv')



####US outflows graph

stimulus_1 <- ymd('2020-04-12')

stimulus_graph <- outflows %>%
  filter(user_cc == 'US',
         time >= '2019-01-01',
         time <= '2021-01-01') %>%
  ggplot(., aes(x = time, y = outflow)) +
  geom_line(color = 'blue', size = 0.8) +
  geom_vline(xintercept = stimulus_1, color = 'red', size = 0.8) +
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
  scale_x_date(limit = c(as.Date('2017-03-01'), as.Date('2022-09-01'))) +
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
  theme(plot.title = element_text(size = 15, hjust = 0.5),
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

unmatched_stats[['N']] <- nrow(trades)
matched_stats[['N']] <- nrow(matched_trades)

unmatched_stats[['Average Trade Size']] <- mean(trades$amount_usd)
matched_stats[['Average Trade Size']] <- mean(matched_trades$amount_usd)

unmatched_stats[['Maximum Trade Size']] <- max(trades$amount_usd)
matched_stats[['Maximum Trade Size']] <- max(matched_trades$amount_usd)

unmatched_stats[['Total Volume']] <- sum(trades$amount_usd)
matched_stats[['Total Volume']] <- sum(matched_trades$amount_usd)

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
  group_by(user_cc2) %>%
  summarize(total = sum(volume)) %>%
  ungroup() %>%
  mutate(share = total/sum(total))

total_outflows <- flows %>%
  group_by(user_cc) %>%
  summarize(total = sum(volume)) %>%
  ungroup() %>%
  mutate(share = total/sum(total))

total_combined_flows <- flows %>%
  filter(user_cc != user_cc2) %>%
  group_by(user_cc, user_cc2) %>%
  summarize(total = sum(volume)) %>%
  ungroup() %>%
  mutate(share = total/sum(total))



