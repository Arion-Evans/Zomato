library(nnet)
library(car)
library(MASS)
library(dplyr)
library(data.table)

zomato<-readRDS("zomato_phase2.rds")

#Reordering factor varialbes to specify bases

#rating text (base=poor)
zomato$Rating.text<-factor(zomato$Rating.text, 
                           levels = c("Poor", "Average", "Good","Very Good", "Excellent")) 

#continent (base=rest of world)
zomato$continent<-factor(zomato$continent, 
                           levels = c("Rest of World", "Europe", "Asia","North America", "Oceania")) 

zomato$ID = seq(1,nrow(zomato),1) # for splitting dataset

set.seed(57364)
zomato.train = zomato %>% group_by(Rating.text) %>% sample_frac(0.8) # stratified sampling
zomato.test = zomato[!(zomato$ID %in% zomato.train$ID),]

zomato.train = as.data.table(zomato.train)
zomato = zomato[,-c("ID")]
zomato.train = zomato.train[,-c("ID")]
zomato.test = zomato.test[,-c("ID")]



#========= Feature selection =========#

library(glmulti)
#--bic--#

# no interaction terms
set.seed(123)
search.1way.bic <- glmulti(y = Rating.text ~ ., 
                           data = zomato.train, 
                           fitfunction = "glm", 
                           level = 1, 
                           method = "h", 
                           marginality = TRUE,
                           crit = "bic", 
                           family = binomial(link = "logit"))
#works for multinomial response?

print(search.1way.bic)
aa <- weightable(search.1way.bic)
cbind(model = aa[1:5,1], round(aa[1:5,2:3], digits = 3))

plot(search.1way.bic, type = "p")

#picks votes and has online delivery as only predictors...

# 2-way interactions
set.seed(123)
search.2way.bic <- glmulti(y = Rating.text ~ ., 
                           data = zomato.train, 
                           fitfunction = "glm", 
                           level = 2, 
                           method = "g", 
                           marginality = TRUE,
                           crit = "bic",
                           conseq = 1,
                           family = binomial(link = "logit"))

print(search.2way.bic)
aa <- weightable(search.2way.bic)
cbind(model = aa[1:5,1], round(aa[1:5,2:3], digits = 3))

plot(search.2way.bic, type = "p")

#--aicc--#

# no interaction terms
set.seed(123)
search.1way.aicc <- glmulti(y = Rating.text ~ ., 
                            data = zomato.train, 
                            fitfunction = "glm", 
                            level = 1, 
                            method = "h", 
                            marginality = TRUE,
                            crit = "aicc", 
                            family = binomial(link = "logit"))
 
print(search.1way.aicc)
aa <- weightable(search.1way.aicc)
cbind(model = aa[1:5,1], round(aa[1:5,2:3], digits = 3))

plot(search.1way.aicc, type = "p")

#picks has online delivery, votes, continent, cuisine and cuisine range


# 2-way interactions
set.seed(123)
search.2way.aicc <- glmulti(y = Rating.text ~ ., 
                            data = zomato.train, 
                            fitfunction = "glm", 
                            level = 1, 
                            method = "g", 
                            marginality = TRUE,
                            crit = "aicc", 
                            family = binomial(link = "logit"))

print(search.2way.aicc)
aa <- weightable(search.2way.aicc)
cbind(model = aa[1:5,1], round(aa[1:5,2:3], digits = 3))

plot(search.2way.aicc, type = "p")

#========= Modelling =========#

#nominal
fit.test<-multinom(formula = Rating.text ~ Has.Online.delivery  + Votes +  continent + cuisine+
                 Cuisine_Range, data=zomato.train)

summary(fit.test)

Anova(fit.test)

#all predictors besides cuisine range is highly significant, while cuisine range is moderatly significant

#ordinal
fit.full.ord<-polr(formula = Rating.text ~ Has.Table.booking + Has.Online.delivery  + Votes + Average.Cost.for.two.Std + continent +
                     Cuisine_Range, data=zomato.train,
                  method = "logistic")

summary(fit.full.ord)

Anova(fit.full.ord)
