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

# continue intro to lieanr models from the graph bit.
