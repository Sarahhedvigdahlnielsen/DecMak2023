install.packages("pacman")
pacman::p_load(R2jags, parallel, polspline, ggplot2, glue,tidyverse)

#### load data ####
df <- read.csv("SarahHedvigDahlNielsen#8292/Exam/data/preprocessedData.csv")

#### investigating rho ####
df2 <- df %>%  select(subjectid, period, antisocial)

df3 <- df %>% 
  select(subjectid,period,recpun) %>% 
  mutate(
    period = period + 1
  ) %>% 
  rename(
    recPunLastRound = recpun
  ) %>% 
  inner_join(df2, by=c('subjectid','period'))

df4 <- df3 %>% 
  filter(recPunLastRound != 0) %>% 
  mutate(ratio = antisocial / recPunLastRound)

ratio_over_1 <- df4 %>% filter(ratio > 1)

362 / 4373 * 100

# Investigating alpha
df5 <- df %>%
  filter(period == 1)

ggplot(df5, aes(x = antisocial)) +
  geom_density(color="darkblue") +
  geom_vline(aes(xintercept=mean(antisocial)),
             color="lightblue", linetype="dashed", size=1) +
  xlab("Antisocial punishment delivered on the first trial") +
  theme_bw()

mean(df5$antisocial)
sd(df5$antisocial)
