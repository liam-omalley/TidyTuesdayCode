library(tidyverse)
chocolate <- tidytuesdayR::tt_load('2022-01-18')$chocolate

choco <- chocolate %>% 
  mutate(cocoa_percent = as.numeric(sub("%","",cocoa_percent))/100)

Countries_By_Rating <- choco %>% 
  group_by(country_of_bean_origin) %>% 
  summarise(Mean_Rating = mean(rating),
            n = n()) %>% 
  arrange(-Mean_Rating)

Top_Countries <- Countries_By_Rating %>%
  filter(n >= 5) %>% 
  head(10) %>% 
  arrange(Mean_Rating) %>% #rearrange in reverse order in order to be correct when plotted
  mutate(Disp = paste("m =", Mean_Rating, ";", "n =", n))

choco <- choco %>% 
  mutate(country_of_bean_origin = factor(country_of_bean_origin, levels = Top_Countries$country_of_bean_origin))

Countries_Cut <- Countries_By_Rating %>% 
  head(10) %>% 
  anti_join(Top_Countries, by = "country_of_bean_origin")
 
choco %>% 
  filter(country_of_bean_origin %in% Top_Countries$country_of_bean_origin) %>% 
  ggplot(aes(x = country_of_bean_origin, y = rating)) +
  geom_violin(aes(fill = country_of_bean_origin, color = country_of_bean_origin)) +
  geom_boxplot(width = 0.2) +
  #geom_text(data = Top_Countries, aes(x = country_of_bean_origin, y = 2, label = Disp)) +
  guides(fill = "none", color = "none") +
  labs(x = "Country of Bean Origin", y = "Chocolate Rating",
       title = "Top Ten Bean-Producing Countries by Mean Chocolate Rating",
       caption = paste(
         "For countries with n >= 5 chocolates produced. Countries cut:",
         paste(Countries_Cut$country_of_bean_origin, collapse = ", "))) +
  theme_bw() +
  coord_flip()
