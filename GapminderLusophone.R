library(gapminder)
library(tidyverse)
library(ggplot2)
library(broom)



#Portuguese Speaking counties vs World Average? 

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
                                    ifelse(country %in% Francophone, "Francophone", "Non-Romance")))) 

Lusophonedf <- gpu %>%
  filter(Lang_Group == "Lusophone")

Lusophonedf %>% 
  filter(year == 2007) %>%
  arrange(desc(pop))

Lusophonedf %>%
  filter(year == 2007) %>%
  arrange(gdpPercap)

ggplot(Lusophonedf, aes(x = year, y = gdpPercap)) + geom_col() +
  facet_wrap(~country)

ggplot(Lusophonedf, aes(x = year, y = lifeExp)) + geom_col() +
  facet_wrap(~country)

ggplot(gpu, aes(x = year, y = gdpPercap)) + geom_point() +
  facet_wrap(~Lang_Group)

gpurl <- filter(gpu, Lang_Group != "Non-Romance")

gdpmodel <- lm(gdpPercap ~ Lang_Group, gpu)

lemodel <- lm(lifeExp ~ Lang_Group, gpu)

gdpmodel2 <- lm(gdpPercap ~ Lang_Group, gpurl)

lemodel2 <- lm(lifeExp ~ Lang_Group, gpurl)

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


