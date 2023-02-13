#PACKAGES ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

#Linear model for comparing groups
lsmodel0 <- lm(formula= height ~ 1, data = darwin)

lsmodel0

#Summaries for models

broom::tidy()  #summarizes information about model components
broom::glance() #reports information about entire model
broom::augment() #adds informations about individual observations to a dataset and it can be used to model predictions onto a new dataset

broom::tidy(lsmodel0)

#compare means

lsmodel1 <- lm(height ~ type, data = darwin) 

broom::tidy(lsmodel1)

summary(lsmodel1)

darwin %>%
  ggplot(aes(x=type,
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

#confidence intervals

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)


#calculating the other condition mean

darwin%>%
  mutate(type=factor(type))%>%
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>%
  lm(height~type, data=.) %>%
  broom::tidy()

means <- emmeans::emmeans(lsmodel1, specs = ~type)
means


means %>%
  as_tibble() %>%
  ggplot(aes(x=type,
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL,
    ymax=upper.CL))

plot(lsmodel1)

performance::check_model(lsmodel1)


performance::check_model(lsmodel1, check=c("normality","qq"))

