library(gapminder)
library(tidyverse)
library(ggplot2)
library(broom)
gpu <- gapminder_unfiltered

Lusophone <- c("Brazil", "Portugal", "Angola", "Mozambique", "Equatorial Guinea", "Guinea-Bissau", "Sao Tome and Principe", "Timor-Leste")

Hispanophone <- c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominican Republic",
                  "Ecuador", "El Salvador", "Equatorial Guinea", "Guatemala", "Guatemala", "Mexico", "Nicaragua", 
                  "Panama", "Paraguay", "Peru", "Puerto Rico", "Spain", "Uruguay", "Venezuela")

Francophone <- c("Belgium", "Benin", "Burkina Faso", "Cameroon", "Canada", "Central African Republic", "Chad",
                 "Comoros", "Congo, Rep.", "Congo, Dem. Rep.", "Cote d'Ivoire", "Dijibouti", "France", "French Guiana", 
                 "French Polynesia", "Hati", "Luxembourg", "Madagascar", "Mali", "Monaco", "Niger", "Rwanda", "Senegal", 
                 "Switzerland", "Togo", "Vanuatu", "Vietnam")


gpu <- gpu %>%
  mutate(Lang_Group = ifelse(country %in% Lusophone,"Lusophone",
                             ifelse(country %in% Hispanophone, "Hispanophone",
                                    ifelse(country %in% Francophone, "Francophone", "")))) 

Lusophonedf <- gpu %>%
  filter(Lang_Group == "Lusophone")

Lusophonedf %>% 
  filter(year == 2007) %>%
  arrange(desc(pop))

ggplot(Lusophonedf, aes(x = year, y = gdpPercap)) + geom_col() +
  facet_wrap(~country)

ggplot(Lusophonedf, aes(x = year, y = lifeExp)) + geom_col() +
  facet_wrap(~country)

gdpmodel <- lm(gdpPercap ~ factor(Lang_Group), gpu)

summary(gdpmodel)

lemodel <- lm(lifeExp ~ factor(Lang_Group), gpu)

summary(lemodel)

gpurl <- filter(gpu, Lang_Group != "")

ggplot(gpurl, aes(x = year, y = lifeExp, color = Lang_Group)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(gpurl, aes(x = year, y = gdpPercap, color = Lang_Group)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

 gpurl %>%
  nest(-Lang_Group) %>%
  mutate(models = map(data, ~lm(gdpPercap ~ year, .))) %>%
  mutate(tidied = map(models, tidy)) %>%
  unnest(tidied)

 gpurl %>%
   nest(-Lang_Group) %>%
   mutate(models = map(data, ~lm(lifeExp ~ year, .))) %>%
   mutate(tidied = map(models, tidy)) %>%
   unnest(tidied)


