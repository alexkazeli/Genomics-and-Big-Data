install.packages("tidyverse")
install.packages("hexbin")
install.packages("gapminder")

library("gapminder")
library("tidyverse")
library("hexbin")
gapminder

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

#Que 1a
new_doc <- PlantGrowth
ggplot(new_doc, mapping=aes(x=group,y=weight))+
  geom_boxplot(alpha=1, color="blue")+
  theme(text=element_text(size=18))

#Que 1b
#Null hypothesis: tr 1 and tr 2 have the same effect
treatment1 <- PlantGrowth %>% 
  filter(group== "trt1") %>% 
  pull(weight)
treatment2 <- PlantGrowth %>% 
  filter(group== "trt2") %>% 
  pull(weight)
t.test(treatment1, treatment2)

#Que 2a
msleep
#hyp: "eating meat makes you smart, therefore carnivores should have larger brains than herbivores."
herbi_carni <- msleep%>% 
  filter(!is.na(brainwt)) %>% 
  filter(vore=="herbi"| vore=="carni") %>% 
  select(name, vore, brainwt) 

#histograms
ggplot(herbi_carni, mapping = aes(brainwt))+
  geom_histogram()+
  facet_wrap("vore")
#Both histograms do not show normal distribution, since the histograms are not bell-shaped.  

#Q-Q plot
ggplot(herbi_carni, mapping=aes(sample=brainwt))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap("vore")
#Howevr carni q-q plot shows normal distribution, since most point lie on the y=x line

#Que 2b; Wilcoxon test since data is not normally distributed
wilcox.test(herbivores_new$brainwt, carnivores_new$brainwt)
#since p-value = 0.5943>0.05;, ACCEPT Null hypothesis

#Que 2c; "eating meat makes you lazy, therefore carnivores probably sleep longer than herbivores"
#Plot a histogram and a Q-Q plot. Is the data normally distributed?
msleep <- msleep 

herbi_carni_sleep <- msleep %>% 
  filter(!is.na(sleep_total)) %>% 
  filter(vore=="herbi"| vore=="carni") %>% 
  select(name, vore, sleep_total)

#histogram
ggplot(herbi_carni_sleep, mapping = aes(sleep_total))+
  geom_histogram()+
  facet_wrap("vore")
#both histograms not normally distributed, since they do not follow a bell-shaped curve

#qq plots
ggplot(herbi_carni_sleep, mapping = aes(sample=sleep_total))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap("vore")
#Both qq-plots are not normally distributed since most of the points do not lie on the y=x line

#Que 2d;Wilcox test is data not normallyt distributed
wilcox.test(herbi_carni_sleep$sleep_total)
#since p-value = 5.285e-10<0.05 REJECT Null hypothesis

lm(brainwt~bodywt, data=msleep)
result <- lm(brainwt~bodywt, data=msleep)
summary(result)

#Que 3a; What is the difference in intercept and coefficient? Does it make a big difference for the model if you remove these heavy mammals?
mammals <- msleep %>% 
  filter(!is.na(brainwt)) %>% 
  select(brainwt, bodywt)

mammals_2000 <- msleep %>% 
  filter(!is.na(brainwt)) %>% 
  filter(bodywt<2000) %>% 
  select(brainwt, bodywt)

lm(brainwt~bodywt, data=mammals) #mammals
#Coefficients;(Intercept:;0.0859173, bodywt:0.0009639
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

#Que 1a
new_doc <- PlantGrowth
ggplot(new_doc, mapping=aes(x=group,y=weight))+
  geom_boxplot(alpha=1, color="blue")+
  theme(text=element_text(size=18))

#Que 1b
#Null hypothesis: tr 1 and tr 2 have the same effect
treatment1 <- PlantGrowth %>% 
  filter(group== "trt1") %>% 
  pull(weight)
treatment2 <- PlantGrowth %>% 
  filter(group== "trt2") %>% 
  pull(weight)
t.test(treatment1, treatment2)

#Que 2a
msleep
#hyp: "eating meat makes you smart, therefore carnivores should have larger brains than herbivores."
herbi_carni <- msleep%>% 
  filter(!is.na(brainwt)) %>% 
  filter(vore=="herbi"| vore=="carni") %>% 
  select(name, vore, brainwt) 

#histograms
ggplot(herbi_carni, mapping = aes(brainwt))+
  geom_histogram()+
  facet_wrap("vore")
#Both histograms do not show normal distribution, since the histograms are not bell-shaped.  

#Q-Q plot
ggplot(herbi_carni, mapping=aes(sample=brainwt))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap("vore")
#Howevr carni q-q plot shows normal distribution, since most point lie on the y=x line

#Que 2b; Wilcoxon test since data is not normally distributed
wilcox.test(herbivores_new$brainwt, carnivores_new$brainwt)
#since p-value = 0.5943>0.05;, ACCEPT Null hypothesis

#Que 2c; "eating meat makes you lazy, therefore carnivores probably sleep longer than herbivores"
#Plot a histogram and a Q-Q plot. Is the data normally distributed?
msleep <- msleep 

herbi_carni_sleep <- msleep %>% 
  filter(!is.na(sleep_total)) %>% 
  filter(vore=="herbi"| vore=="carni") %>% 
  select(name, vore, sleep_total)

#histogram
ggplot(herbi_carni_sleep, mapping = aes(sleep_total))+
  geom_histogram()+
  facet_wrap("vore")
#both histograms not normally distributed, since they do not follow a bell-shaped curve

#qq plots
ggplot(herbi_carni_sleep, mapping = aes(sample=sleep_total))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap("vore")
#Both qq-plots are not normally distributed since most of the points do not lie on the y=x line

#Que 2d;Wilcox test is data not normallyt distributed
wilcox.test(herbi_carni_sleep$sleep_total)
#since p-value = 5.285e-10<0.05 REJECT Null hypothesis

lm(brainwt~bodywt, data=msleep)
result <- lm(brainwt~bodywt, data=msleep)
summary(result)

#Que 3a; What is the difference in intercept and coefficient? Does it make a big difference for the model if you remove these heavy mammals?
mammals <- msleep %>% 
  filter(!is.na(brainwt)) %>% 
  select(brainwt, bodywt)

mammals_2000 <- msleep %>% 
  filter(!is.na(brainwt)) %>% 
  filter(bodywt<2000) %>% 
  select(brainwt, bodywt)

lm(brainwt~bodywt, data=mammals) #mammals
#Coefficients;(Intercept:;0.0859173, bodywt:0.0009639
ggplot(mammals, aes(x=brainwt, y=bodywt))+
  geom_point()+
  geom_smooth(method="lm")

lm(brainwt~bodywt, data=mammals_2000) #mammals<2000kg
#Coefficients; (Intercept):0.059725, bodywt: 0.001029
ggplot(mammals_2000, aes(x=brainwt, y=bodywt))+
  geom_point()+
  geom_smooth(method="lm")

#the intercept decreases and the coefficient increases(line is less steep). The model differs and the points are more spread when the heavy mammals are removed.

#Que 3b;

lm(brainwt~bodywt, data=mammals_2000) #mammals<2000kg
#Coefficients; (Intercept):0.059725, bodywt: 0.001029
ggplot(mammals_2000, aes(x=brainwt, y=bodywt))+
  geom_point()+
  geom_smooth(method="lm")

#the intercept decreases and the coefficient increases(line is less steep). The model differs and the points are more spread when the heavy mammals are removed.

#Que 3b;
ggplot(mammals, aes(x=brainwt, y=bodywt))+
  geom_point()+
  scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method="lm")

ggplot(mammals_2000, aes(x=brainwt, y=bodywt))+
  geom_point()+
  scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method="lm")

#Que 3c
brain_mammals_2000 <- mammals_2000 %>% 
  pull(brainwt)

body_mammals_2000 <- mammals_2000 %>% 
  pull(bodywt)

cor(body_mammals_2000, brain_mammals_2000, method="pearson")
# 0.5286487

brain_mammals <- mammals %>% 
  pull(brainwt)

body_mammals <- mammals %>% 
  pull(bodywt)

cor(body_mammals, brain_mammals, method = "pearson")
# 0.9337822

#Que 3d
#calculate the correlation between brain weight and body weight of the full data and of the data with the heavy animals removed. 

cor(body_mammals_2000, brain_mammals_2000, method="spearman")
# 0.9522176

cor(body_mammals, brain_mammals, method = "spearman")
#0.9571578

#Que 3e
#Spearman is a non-parametric test
