library(gapminder)
library(tidyverse)

#Portuguese Speaking counties vs World Average? 

LusoPhone <- gapminder_unfiltered %>%
  filter(country %in% c("Brazil", "Portugal", "Angola", "Mozambique", "Equatorial Guinea", "Guinea-Bissau", "Sao Tome and Principe", "Timor-Leste")) %>%
  mutate(gdp = gdpPercap * pop)

LusoPhone %>% 
  filter(year == 2007) %>%
  arrange(desc(pop))

LusoPhone %>%
  filter(year == 2007) %>%
  arrange(gdpPercap)

Lusophone07 <- LusoPhone %>%
  filter(year == 2007)

ggplot(Lusophone07, aes(x = pop, y = gdpPercap, color = continent))+
  geom_point()+
  scale_x_log10()