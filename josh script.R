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

#saveRDS(search.1way.bic, "Feature Selection/search.1way.bic.rds")

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
fit.bic.nom<-multinom(formula = Rating.text ~ Has.Online.delivery  + Votes, data=zomato)


summary(fit.bic.nom)

Anova(fit.test)

#all predictors besides cuisine range is highly significant, while cuisine range is moderatly significant

#ordinal
fit.bic.ord<-polr(formula = Rating.text ~ Has.Online.delivery  + Votes, data=zomato,
                  method = "logistic")

saveRDS(fit.bic.ord,"Models/bic_ord_TRAIN.rds")

summary(fit.full.ord)

Anova(fit.full.ord)

#plot

#prediction dataframe
dvotes <- data.frame(Has.Online.delivery = rep(c("Yes", "No"), each = 50), 
                     Votes = rep(c(5, 125, 255, 505, 755, 1005, 1205, 1505, 2005, 2505, 3005, 3505, 
                                   4005, 4505, 5005, 5505, 6005, 6505, 7005,
                                   7505, 8005, 8505, 9005, 9505, 10005), 4))

#generate predictions using model
pp.votes <- cbind(dvotes, predict(fit.bic.nom, newdata = dvotes, type = "probs", se = TRUE))
#melt for visualisation
lpp <- melt(pp.votes, id.vars = c("Has.Online.delivery", "Votes"), value.name = "Probability")


ggplot(lpp, aes(x = Votes, y = Probability, colour = Has.Online.delivery)) + 
  geom_line() + facet_grid(variable ~ ., scales = "free") + theme_minimal()+
  labs(title="Probability of Rating Levels by Votes and Online Delivery")
