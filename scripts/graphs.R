# Graphs


# Libraries

library(tidyverse)

# Load the data

employment<-
  read.csv('labour-market-stats.csv')

# Caption

employment_graph_caption <-
  'The graph shows adequate employment as a percent of the labour force across time. Adequate employment refers to workers who earn a wage greater or equal to the prevalent minimum wage at the time of the survey.'

# Graph

employment %>%
  filter(series == 'Adequate Employment') %>% 
  ggplot(aes(year, value))+
  geom_point(colour = '#2F66A9')+
  geom_line(colour = '#189ad3' )+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0))+
  labs( x = 'Year',
        y = 'Adequate employment (% of labour force)',
        caption = str_wrap(employment_graph_caption, 109))