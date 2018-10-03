library(nnet)
library(car)
zomato<-readRDS("zomato_phase2.rds")

#small test model
fit.test<-multinom(formula = Rating.text ~ Votes + Average.Cost.for.two.Std + continent, data=zomato)

Anova(fit.test)

#all varibles
fit1<-multinom(formula = Rating.text ~ Has.Table.booking + Has.Online.delivery + 
                 Rating.text + Votes + Average.Cost.for.two.Std + continent +
                 cuisine, data=zomato)

Anova(fit1)
