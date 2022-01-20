library(tidyverse)
choco <- tidytuesdayR::tt_load('2022-01-18')$chocolate

choco <- choco %>% 
  mutate(cocoa_percent = as.numeric(sub("%","",cocoa_percent))/100)

Countries <- choco %>% 
  group_by(country_of_bean_origin) %>% 
  summarise(Mean_Cocoa = mean(cocoa_percent),
            Mean_Rating = mean(rating),
            n = n())

Top_Countries <- Countries %>%
  filter(Mean_Rating >= 3.25 & n >= 5) %>% 
  arrange(-Mean_Rating) %>% 
  head(10)

choco %>% 
  filter(country_of_bean_origin %in% Top_Countries$country_of_bean_origin) %>% 
  ggplot(aes(x = country_of_bean_origin, y = rating)) +
  geom_violin(aes(fill = country_of_bean_origin, color = country_of_bean_origin)) +
  geom_boxplot(width = 0.2) +
  guides(fill = "none", color = "none") +
  labs(x = "Country of Bean Origin", y = "Chocolate Rating",
       title = "Top Ten Bean-Producing Countries by Mean Chocolate Rating") +
  theme_bw() +
  coord_flip()