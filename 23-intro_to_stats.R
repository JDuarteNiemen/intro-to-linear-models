
# PACKAGES ----
library(tidyverse)
library(here)
library(kableExtra)

# READING DATA
darwin <- read_csv(here("Data", "darwin.csv"))

# INTRO TO STATS ----

darwin %>%
  ggplot(aes(x=type,
             y=height))+
  geom_point()

#comparing groups
darwin %>%
  group_by(type) %>%
  summarise(mean = mean(height),
            sd = sd(height))

darwin_summary <- darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))

darwin_summary %>%
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

#kableExtra functions make a nice table
darwin_summary %>%
kbl(caption="Summary statisitics of crossed and selfed maize plants") %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")


#making difference in height coloumn

darwin_wide <- darwin %>%
  pivot_wider(names_from = type, values_from = height) %>%
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>%
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())
difference_summary

difference_summary <- difference_summary %>%
  mutate(se = sd/n^0.5)
difference_summary

# the average difference in height was 2.62 ± 1.22 inches (mean ± SE).



#normal distribution
x <- seq(-4,4, length=100) #creates 100 equal spaced numbers between -4 and 4

y <- dnorm(x) # vector of values that shows the height of the probability distribution for each value in x

plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

#confidence intervals
lowerCI <- 2.62-(2*1.22)
upperCI <- 2.62+(2*1.22)

lowerCI
upperCI
