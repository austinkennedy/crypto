#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)

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
  xlab('Date') +
  ylab("USD") +
  ggtitle("Outflows from US") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.margin = unit(c(1,1,1,1), "cm"))

show(stimulus_graph)
ggsave('../output/figures_paxful/stimulus_timing.png')