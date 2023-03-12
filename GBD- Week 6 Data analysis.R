install.packages("tidyverse")
install.packages("hexbin")
install.packages("gapminder")

library("gapminder")
library("tidyverse")
library("hexbin")
gapminder

ggplot(gapminder, mapping = aes(lifeExp)) +
  geom_histogram()

ggplot(gapminder, mapping = aes(lifeExp)) +
  geom_histogram(bins=5)

ggplot(gapminder, mapping = aes(lifeExp)) +
  geom_histogram(binwidth = 10)

ggplot(gapminder, mapping = aes(lifeExp)) +
  geom_histogram(binwidth = 1)

ggplot(gapminder, mapping = aes(lifeExp)) +
  geom_histogram()+
  facet_wrap(~continent)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
ggplot(gapminder, mapping = aes(sample=lifeExp)) +
  stat_qq()+
  stat_qq_line()

download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")
surveys <- read.csv("data/portal_data_joined.csv")
surveys_complete <- surveys %>%
  filter(!is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))                # remove missing sex

surveys_complete %>% 
  filter(species_id == "PB") %>% 
  ggplot(mapping=aes(sample = weight))+
  stat_qq()+
  stat_qq_line()

pb_male <- surveys_complete %>% 
  filter(species_id == "PB", sex == "M") %>% 
  pull(weight)
pb_female <- surveys_complete %>% 
  filter(species_id == "PB", sex == "F") %>% 
  pull(weight)

t.test(pb_female, pb_male)

pb <- surveys_complete %>% 
  filter(species_id == "PB") %>% 
  t.test(pb$weight ~ pb$sex)

b <- tibble(season=c("Winter", "Spring", "Summer", "Fall"),   births=c(258,278,211,253))
chisq.test(b$births)

exp_europe <- gapminder %>% 
  filter(year == 2007, continent=="Europe") %>% 
  pull(lifeExp)

exp_americas <- gapminder %>% 
  filter(year == 2007, continent=="Americas")  %>% 
  pull(lifeExp)

life_exp_europe <- gapminder %>% 
  filter(continent=="Europe") %>% 
  select(country, year, lifeExp) %>% 
  spread(key=year, value=lifeExp)

wilcox.test(life_exp_europe$`2002`, life_exp_europe$`2007`, paired=TRUE)     

lm(brainwt ~ bodywt, data=msleep)

result <-  lm(brainwt ~bodywt, data=msleep)
summary(result)
  
ggplot(msleep, aes(x=bodywt, y=brainwt)) +
  geom_point()+
  geom_smooth(method="lm")
 
ggplot(msleep, aes(x=bodywt, y=brainwt)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method="lm")
