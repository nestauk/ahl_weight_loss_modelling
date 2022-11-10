library(tidyverse)
library(here)
library(reshape2)

df <- read_csv(here("outputs/data/obese_all_years.csv"))

# ecdf

df %>% 
  ggplot(., aes(x = ei, colour = as.factor(year), group = as.factor(year))) + stat_ecdf()

df %>% 
  ggplot(. ,aes(x = as.factor(year), y = ei, group = as.factor(year))) + 
  geom_boxplot() +
  labs(x = "Years",
       y = "kcal/day reduction",
       title = "Distribution of Reduction in Intake") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") 

ggsave(here("outputs/figures/obese_boxplot_year.png"), width = 10, bg='#ffffff')
dev.off()

df %>% 
  group_by(year) %>% 
  summarise(mean = mean(ei),
            sd = sd(ei))

