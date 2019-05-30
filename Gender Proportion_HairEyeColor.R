install.packages("epitools")
library(dplyr)
library(ggplot2)
library(epitools)

datasets::HairEyeColor

df <- expand.table(HairEyeColor)

message <- "Hello World!"

ggplot(df, aes(x = "Population", fill = Sex))+
  geom_bar(position = "fill")

ggplot(srs, aes(x = "Simple Random Sample", fill = Sex))+
  geom_bar(position = "fill")

ggplot(strat_sample, aes(x = "Stratified Sample", fill = Sex))+
  geom_bar(position = "fill")

pop_Sex_prop <- df %>% 
  count(Sex)%>%
  mutate(prop = n/sum(n))

srs <- df %>%
  sample_n(size = 100)

srs_Sex_prop <- srs %>% 
  count(Sex)%>%
  mutate(prop = n/sum(n))

strat_sample <- df %>%
  group_by(Sex) %>%
  sample_n(size = 50)
  
strat_Sex_prop <- strat_sample %>%
  count(Sex) %>%
  mutate(prop = n /sum(n))


for(i in 1:nrow(strat_Sex_prop)) {
  strat_Sex_prop[i,3] <- strat_Sex_prop[i,2]/colSums(strat_Sex_prop[2])
}

colnames(pop_Sex_prop)[colnames(pop_Sex_prop) == "prop"] <- "pop_prop"

colnames(srs_Sex_prop)[colnames(srs_Sex_prop) == "prop"] <- "Srs_prop"

colnames(strat_Sex_prop)[colnames(strat_Sex_prop) == "prop"] <- "strat_prop"

Props_compare <- data.frame(pop_Sex_prop[1], pop_Sex_prop[3], srs_Sex_prop[3], strat_Sex_prop[3])

Props_compare

