library(nnet)
library(car)
library(MASS)

zomato<-readRDS("zomato_phase2.rds")

#Reordering factor varialbes to specify bases

#rating text (base=poor)
zomato$Rating.text<-factor(zomato$Rating.text, 
                           levels = c("Poor", "Average", "Good","Very Good", "Excellent")) 
levels(zomato$Rating.text)

#continent (base=rest of world)
zomato$continent<-factor(zomato$continent, 
                           levels = c("Rest of World", "Europe", "Asia","North America", "Oceania")) 
levels(zomato$continent)

#========= Modelling =========#

#nominal
fit.full<-multinom(formula = Rating.text ~ Has.Table.booking + Has.Online.delivery  + Votes + Average.Cost.for.two.Std + continent +
                 Cuisine_Range, data=zomato)

summary(fit.full)

Anova(fit.full)

#all predictors besides cuisine range is highly significant, while cuisine range is moderatly significant

#ordinal
fit.full.ord<-polr(formula = Rating.text ~ Has.Table.booking + Has.Online.delivery  + Votes + Average.Cost.for.two.Std + continent +
                     Cuisine_Range, data=zomato,
                  method = "logistic")

summary(fit.full.ord)

Anova(fit.full.ord)
