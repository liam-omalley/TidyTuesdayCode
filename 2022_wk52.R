library(tidyverse)
library(maps)

tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony
stressor <- tuesdata$stressor

states_map <- map_data("state")

varoa_2020 <- stressor %>%
  mutate(state = tolower(state))%>%
  filter(year == 2020 & stressor == "Varroa mites")

varoa_2020 %>%
  ggplot(aes(map_id = state)) +
  geom_map(aes(fill = stress_pct), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_void() +
  facet_wrap(vars(months)) +
  labs(fill = "Stress Percent From Varoa Mites", title= "Varoa Mite-Caused Stress on Beehives by Season in 2020") +
  theme(legend.position="bottom", plot.title = element_text(hjust =0.5))