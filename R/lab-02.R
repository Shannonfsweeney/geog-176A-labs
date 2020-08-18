library(tidyverse)
library(readr)

install.packages("readxl")
install.packages("knitr")
install.packages("zoo")

home = read_csv("data/landdata-states.csv")
library(readr)
landdata_states <- read_csv("data/landdata-states.csv")

library(readxl)
pop <- read_excel("data/PopulationEstimates.xls",
                                  skip = 2)
p1 = pop %>%
  select(fips = "FIPStxt",state = "State", pop2019 = "POP_ESTIMATE_2019") %>%
  group_by(state) %>%
  slice_max(pop2019, n = 1)

fivenum(home$Land.Value)

home %>%
  filter(State %in% c("HI", "CA", "NY")) %>%
  group_by(State) %>%
  summarize(minLV = min(Land.Value),
            meanLV = mean(Land.Value),
            maxLV = max(Land.Value))
p1 = home %>%
  filter(state %in% c("HI", "TX", "AL")) %>%
  ggplot(aes(x = Date, y = Land.Value )) +
  geom_line(aes(color = state)) +
  facet_wrap(-state, scale = "free_y", ncol = 1) +
  labs(title = "Land Value")

ggsave(p1, file = "img/lv-plot.png")
