library(tidyverse)
library(rio)
library(Hmisc)
library(shiny)
library(agricolae)
library(DT) 
 
t<- chickwts %>% 
  group_by(feed) %>% 
  summarise(n=length(feed),avg = mean(weight), SD=sd(weight)) 
datatable(t)

ggplot(chickwts, aes(x = feed, y = weight)) +
  geom_boxplot()

ggplot(chickwts, aes(x = feed, y = weight)) +
  geom_jitter(width = 0.30) +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1), 
               col = "red")


chickwts_lm <- lm(weight ~ feed, data = chickwts)
chickwts_lm

datatable(anova(chickwts_lm))


chickwts.av <-aov(weight ~ feed , data = chickwts)
tukeyTest <- TukeyHSD(chickwts.av)
datatable(tukeyTest$feed)
