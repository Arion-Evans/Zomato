library(nnet)
library(car)
library(MASS)

zomato<-readRDS("zomato_phase2.rds")

#Reordering factor varialbes to specify bases

#rating text (base=poor)
zomato$Rating.text<-factor(zomato$Rating.text, 
                           levels = c("Poor", "Average", "Good","Very Good", "Excellent")) 

#continent (base=rest of world)
zomato$continent<-factor(zomato$continent, 
                           levels = c("Rest of World", "Europe", "Asia","North America", "Oceania")) 


#========= Feature selection =========#

library(glmulti)
#--bic--#
set.seed(123)
search.1.bic <- glmulti(y = Rating.text ~ ., 
                         data = zomato, 
                         fitfunction = "glm", 
                         level = 1, 
                         method = "g", 
                         marginality = TRUE,
                         crit = "bic", 
                         family = binomial(link = "logit"))
#works for multinomial response?

print(search.1.aicc)
aa <- weightable(search.1.aicc)
cbind(model = aa[1:5,1], round(aa[1:5,2:3], digits = 3))

plot(search.1.bic, type = "p")

#picks votes and has online delivery as only predictors...

#--aicc--#
set.seed(123)
search.1.aicc <- glmulti(y = Rating.text ~ ., 
                         data = zomato, 
                         fitfunction = "glm", 
                         level = 1, 
                         method = "g", 
                         marginality = TRUE,
                         crit = "aicc", 
                         family = binomial(link = "logit"))

print(search.1.aicc)
aa <- weightable(search.1.aicc)
cbind(model = aa[1:5,1], round(aa[1:5,2:3], digits = 3))

plot(search.1.aicc, type = "p")

#picks has online delivery, votes, continent, cuisine and cuisine range

#========= Modelling =========#

#nominal
fit.test<-multinom(formula = Rating.text ~ Has.Online.delivery  + Votes +  continent + cuisine+
                 Cuisine_Range, data=zomato)

summary(fit.test)

Anova(fit.test)

#all predictors besides cuisine range is highly significant, while cuisine range is moderatly significant

#ordinal
fit.full.ord<-polr(formula = Rating.text ~ Has.Table.booking + Has.Online.delivery  + Votes + Average.Cost.for.two.Std + continent +
                     Cuisine_Range, data=zomato,
                  method = "logistic")

summary(fit.full.ord)

Anova(fit.full.ord)
