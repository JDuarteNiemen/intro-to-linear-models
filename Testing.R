#______________________----
#Testing ----
##Packages ----
library(tidyverse)

#T-tests ---- 


x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

degf <- c(1, 3, 8, 30)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()



df <- c(1:30)

#map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>%
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x="Degress of Freedom",
       y=expression(paste("Critical value of ", italic("t"))))

lsmodel1 <- lm(height ~type, data = darwin)

broom::tidy(lsmodel1)


tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]


##Paired T ----

darwin %>%
  mutate(pair = as_factor(pair)) %>%
  lm(height ~ type + pair, data = .) %>%
  broom::tidy(., conf.int=T) %>%
  slice(1:2) # just how irst two rows


m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

###Repeatablility ----


set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments

y %>%
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>%
  group_by(`p value < 0.05`)%>%
  summarise(`number of experiments`=n()) #find number of sinificant and insigificant experiments


y %>%
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y="Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

