## home assignment, HT2017, ZK ##

rm(list=ls(all=T)) #cleans environment
graphics.off() #cleans graphs

###########################################################
#                                                         #
#      Assignment 1 - hierarchical regression             #
#                                                         #
###########################################################

#opening dataset
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")

#requiring packages
require(lsr)
require(sciplot)
require(psych) # for describe
require(lm.beta) # for lm.beta
require(dplyr)
require(gsheet)
require(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
require(ggplot2) # for ggplot
require(cAIC4) #for cAIC
require(r2glmm) #for r2beta
require(influence.ME) #for influence
require(lattice) #for qqmath
require(reshape2) #for melt function
# require(lme4)
# require(lmerTest) # masks 'step' function 
# require(rgl) # for scatter3D -> outhashtagged b/c I'm working with a MAC


who(T) #checking form of dataset

### check data set for invalid data (e.g. coding errors)
# descriptive statistics
describe(data_sample_1)

#after checking for coding errors -> pp15 (gender = 3) and pp w/ mindfulness < 1 are excluded

#creating a new dataset without participant 15
data_sample_new <- data_sample_1 [-c(15),]
data_sample_new <- data_sample_new[!data_sample_new$mindfulness<1,]

#checking dataset for invalid data + descriptives
summary(data_sample_new)
describe(data_sample_new)

### checking for assumptions to do a regression
#histograms to check for normality + checking the skew and kurtosis values from describe()
hist(data_sample_new$pain,breaks = 20)
hist(data_sample_new$age,breaks = 20)
hist(data_sample_new$STAI_trait,breaks = 20)
hist(data_sample_new$pain_cat,breaks = 20)
hist(data_sample_new$cortisol_serum,breaks = 20)
hist(data_sample_new$cortisol_saliva,breaks = 20)
hist(data_sample_new$mindfulness,breaks = 20)
# -> looks roughly normally distributed

#scatterplots: checking for a pattern between variables and outcome (pain)
plot(age ~ pain, data = data_sample_new)
plot(STAI_trait ~ pain, data = data_sample_new)
plot(pain_cat ~ pain, data = data_sample_new)
plot(cortisol_serum ~ pain, data = data_sample_new)
plot(cortisol_saliva ~ pain, data = data_sample_new)
plot(mindfulness ~ pain, data = data_sample_new)
plot(weight ~ pain, data = data_sample_new)
# -> there's a relationship between the variables 

######### multiple regression (model 1) ##########
# predicting pain based on sex and age

#fit the regression model
mod_pain1 = lm(pain ~ age + sex, data = data_sample_new)

#plotting scatterplots for each predictor separately incl. regression lines
plot(pain ~ age, data = data_sample_new)
abline(lm(pain ~ age, data = data_sample_new))
plot(pain ~ sex, data = data_sample_new)
abline(lm(pain ~ sex, data = data_sample_new))

# the regression equation: Y = b0 + b1*X1 + b2*X2
  #b0 = intercept 
  #b1 = age
  #b2 = sex

mod_pain1
predict(mod_pain1)

#interpreting the results and regression weights
summary(mod_pain1)
AIC(mod_pain1)
confint(mod_pain1)
lm.beta(mod_pain1)

### checking the model 1 for influential outliers with Cook's distance 
cooks.distance(mod_pain1)
plot(mod_pain1, which = 4)
# --> 112 looks like an influential outlier, sticks out from the other CD values
# first checking other assumptions before making a decision

##### Model diagnostics model 1 - assumption check#####

### normality assumption
# QQ plot
plot(mod_pain1, which = 2)
# skew and kurtosis
describe(residuals(mod_pain1))
# histogram
hist(residuals(mod_pain1), breaks = 20)

### linearity assumption
# predicted values against actual values
pred <- predict( object = mod_pain1 )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain1, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain1)

### homoscedasticty assumption (homogeneity of variance)
plot(mod_pain1, which = 3)
ncvTest(mod_pain1)

### multicollinearity (VIF > 3 or 5 -> high multicollinearity)
vif(mod_pain1)
sqrt(vif(mod_pain1))
pairs.panels(data_sample_new[,c("age", "sex")], col = "red", lm = T)

############ multiple regression (model 2) #############
#predicting pain by age, sex, STAI, pain cata., mindfulness, 2x cortisol measures

#fit the regression model
mod_pain2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_new)

#plotting scatterplots for each predictor separately incl. regression lines

plot(pain ~ age, data = data_sample_new)
abline(lm(pain ~ age, data = data_sample_new))
plot(pain ~ sex, data = data_sample_new)
abline(lm(pain ~ sex, data = data_sample_new))
plot(pain ~ STAI_trait, data = data_sample_new)
abline(lm(pain ~ STAI_trait, data = data_sample_new))
plot(pain ~ pain_cat, data = data_sample_new)
abline(lm(pain ~ pain_cat, data = data_sample_new))
plot(pain ~ cortisol_serum, data = data_sample_new)
abline(lm(pain ~ cortisol_serum, data = data_sample_new))
plot(pain ~ cortisol_saliva, data = data_sample_new)
abline(lm(pain ~ cortisol_saliva, data = data_sample_new))
plot(pain ~ mindfulness, data = data_sample_new)
abline(lm(pain ~ mindfulness, data = data_sample_new))

# the regression equation: Y = b0 + b1*X1 + b2*X2 + b3*X3 + b4*X4 + b5*X5 + b6*X6 + b7*X7
#b0 = intercept 
#b1 = age
#b2 = sex
#b3 = STAI
#b4 = pain catastrophizing
#b5 = mindfulness
#b6 = cortisol serum
#b7 = cortisol saliva

mod_pain2
predict(mod_pain2)

#interpreting the results and regression weights
summary(mod_pain2)
AIC(mod_pain2)
confint(mod_pain2)
lm.beta(mod_pain2)

###### checking assumptions model 2 #######

### influential cases ->  Cook's distance
cooks.distance(mod_pain2)
plot(mod_pain2, which = 4)

### normality assumption
# QQ plot
plot(mod_pain2, which = 2)
# skew and kurtosis
describe(residuals(mod_pain2))
# histogram
hist(residuals(mod_pain2), breaks = 20)

### linearity assumption
# predicted values against actual values
pred <- predict( object = mod_pain2 )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain2, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain2)

### homoscedasticty assumption (homogeneity of variance)
plot(mod_pain2, which = 3)
ncvTest(mod_pain2)

### multicollinearity (VIF > 3 or 5 -> high multicollinearity)
vif(mod_pain2)
pairs.panels(data_sample_new[,c("age", "sex","STAI_trait", "pain_cat", "cortisol_serum", "cortisol_saliva", "mindfulness")], col = "red", lm = T)
###### --> excluding cortisol saliva from the regression equation!!!! (high multicollinearity)

############# multiple regression new (model 2 without cortisol saliva) #############
#final model 2 -> excluding cortisol_saliva (b/c literature suggests that serum is a more reliable predictor

## fit regression equation (model 2) without cortisol_saliva
mod_pain2_final = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_new)
mod_pain2_final
predict(mod_pain2_final)

#interpreting the results and regression weights
summary(mod_pain2_final)
AIC(mod_pain2_final)
confint(mod_pain2_final)
lm.beta(mod_pain2_final)

######### assumption check new model 2 #########

### influential cases -> Cook's distance
cooks.distance(mod_pain2_final)
plot(mod_pain2_final, which = 4)

### normality assumption
# QQ plot
plot(mod_pain2_final, which = 2)
# skew and kurtosis
describe(residuals(mod_pain2_final))
# histogram
hist(residuals(mod_pain2_final), breaks = 20)

### linearity assumption
# predicted values against actual values
pred <- predict( object = mod_pain2_final )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
#predicted values against residuals
plot(mod_pain2_final, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain2_final)

### homoscedasticty assumption (homogeneity of variance)
plot(mod_pain2_final, which = 3)
ncvTest(mod_pain2_final)

### multicollinearity (VIF > 3 or 5 -> high multicollinearity)
vif(mod_pain2_final)
pairs.panels(data_sample_new[,c("age", "sex","STAI_trait", "pain_cat", "cortisol_serum", "mindfulness")], col = "red", lm = T)

############# Comparing models ################
# compare models with the anova function
# can only be used if the models are nested! (all of the predictor used in the
# smaller model are also used in the larger model)
# AND the dataset and the nrow is the same (!)

#comparing mod_pain1 (sex + age) with mod_pain2_final (sex + age + STAI, pain cata., mindfulness, cortisol serum)
anova(mod_pain1, mod_pain2_final)

AIC (mod_pain1)
AIC (mod_pain2_final)

# --> model 2 ist besser zum predicten der variance in pain 

###########################################################
#                                                         #
#      Assignment 2 - backward regression                 #
#                                                         #
###########################################################
## backward regression on the same data as assignment 1 
# dataset = data_sample_new b/c measurement/coding errors still need to be excluded

#fit the regression model for backward regression
mod_back_pain <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight, data = data_sample_new)

summary(mod_back_pain) #getting R^2 + p-values etc. for the model

#backward regression
step(mod_back_pain)

# fit the best regression model after backward regression
mod_back_pain_best <- lm(formula = pain ~ age + pain_cat + cortisol_serum + mindfulness, 
                        data = data_sample_new)
mod_back_pain_best
#results for best model
summary(mod_back_pain_best) #p-values etc. of the model
AIC(mod_back_pain_best) #AIC value of the model 
confint(mod_back_pain_best) #confidence intervals
lm.beta(mod_back_pain_best) #regression weights

########### Assumption check backward model ##############

### influential cases -> Cook's distance
cooks.distance(mod_back_pain_best)
plot(mod_back_pain_best, which = 4)

### normality assumption
# QQ plot
plot(mod_back_pain_best, which = 2)
# skew and kurtosis
describe(residuals(mod_back_pain_best))
# histogram
hist(residuals(mod_back_pain_best), breaks = 20)

### linearity assumption
# predicted values against actual values
pred <- predict( object = mod_back_pain_best )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
#predicted values against residuals
plot(mod_back_pain_best, which = 1)
# residual plot for each predictor 
residualPlots(mod_back_pain_best)

### homoscedasticty assumption (homogeneity of variance)
plot(mod_back_pain_best, which = 3)
ncvTest(mod_back_pain_best)

### multicollinearity (VIF > 3 or 5 -> high multicollinearity)
vif(mod_back_pain_best)
pairs.panels(data_sample_new[,c("age", "pain_cat", "cortisol_serum", "mindfulness")], col = "red", lm = T)

## -> all assumptions are met my mod_back_pain_best

###### Model comparison ###
#comparison of initial model of backward regression with model that was calculated
# mod_back_pain <-> mod_back_pain_best
AIC (mod_back_pain, mod_back_pain_best)
anova(mod_back_pain, mod_back_pain_best)

# mod_back_pain_best (backward regression model) <-> mod_pain2_final (theory-based model)
AIC (mod_back_pain_best, mod_pain2_final)
anova (mod_back_pain_best, mod_pain2_final)
## -> no sig difference between the models, preference of theory-based model!

###### Prediction performance of backward and theory-based model ########

## opening new dataset
home_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")

summary(home_sample_2)
describe(home_sample_2)
# -> mindfulness values below 1, exclusion:

home_sample_new <- home_sample_2[!home_sample_2$mindfulness<1,]

describe(home_sample_new)

### make predictions on pain using regression equations of the backward model and theory-based model

## regression equations:
# mod_pain2: Y (pain) = 
# pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness
# mod_back_pain_best: pain ~ age + pain_cat + cortisol_serum + mindfulness

#test performance of theory-based model and backward model 
pred_test <- predict(mod_pain2_final, home_sample_new)
pred_test_back <- predict(mod_back_pain_best, home_sample_new)

# now calculate the sum of squared residuals

RSS_test = sum((home_sample_new["pain"] - pred_test)^2)
RSS_test_back = sum((home_sample_new["pain"] - pred_test_back)^2)
RSS_test
RSS_test_back
## --> RSS in mod_pain2 is smaller than in mod_back_pain_best when testing regression models on new data

############################################################################
#                                                                          #
#           Assignment 3 - Mixed effect models (repeated measures)         #
#                                                                          #
############################################################################

### Zoltan Home Assingment Part 3 ### 

### Load packages 
require(psych) #for describe
require(car) #for residualPlots, vif, pair.panels. ncvTest
require(ggplot2) #for ggplot
require(cAIC4) #for cAIC
require(r2glmm) #for r2beta
require(influence.ME) #for influence
require(lattice) #for qqmath
require(reshape2) #for melt function
require(lme4)
require(lmerTest)


### Load data file
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")
who(TRUE) # fine
describe(data_sample_3) 
summary(data_sample_3) 

# Histograms to show the distribution 
hist(data_sample_3$pain_cat, breaks = 15)
hist(data_sample_3$cortisol_serum, breaks = 20) 
hist(data_sample_3$STAI_trait, breaks = 20) 
hist(data_sample_3$cortisol_saliva, breaks = 20) 
hist(data_sample_3$mindfulness, breaks = 20) 
hist(data_sample_3$weight, breaks = 20) 
hist(data_sample_3$pain1, breaks = 20) 
hist(data_sample_3$pain2, breaks = 20) 
hist(data_sample_3$pain3, breaks = 20) 
hist(data_sample_3$pain4, breaks = 20) 
# None looks normally distributed, BUT skew + kurtosis are okay 

#variables
names (data_sample_3)

#### transform dataset from wide to long format 

# put days into an object 
# designate which are the repeated varibales
repeated_variables = c("pain1", "pain2", "pain3", "pain4")

### Correlation of repeated variables
cor(data_sample_3[,repeated_variables]) # Correlation lower the more time between ratings

###########################################################
#           Transform wide to long format                 #
###########################################################

# id.vars should be all non-repeated variables
# melt() -> transformation function!
data_pain_long = melt(data_sample_3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating")

# order data frame by participant ID
data_pain_long = data_pain_long[order(data_pain_long[,"ID"]),]
# change the time variable to a numerical vector
data_pain_long$time = as.numeric(data_pain_long$time)

# lets look at how the data looks like in long format
data_pain_long

###########################################################
#                        Analysis                         #
###########################################################
mod_rep_int = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (1|ID), data = data_pain_long)
mod_rep_slope = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (time|ID), data = data_pain_long) # random intercept + random slope 

# information needed for reporting:
summary(mod_rep_int) # model coefficient and significance test results
summary(mod_rep_slope)
r2beta(mod_rep_int, method = "nsj") # explained variance by fixed factors
r2beta(mod_rep_slope, method = "nsj")
#cAIC: calculated further down for model comparison
confint(mod_rep_int) # confidence intervals for coefficients
confint(mod_rep_slope)
# custom function to calculate standardized coefficients
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))}

stdCoef.merMod(mod_rep_int)
stdCoef.merMod(mod_rep_slope)

### model comparison to see whether to use random slope or random intercept models
# save the predictions of both models to variables
data_pain_long$pred_int = predict(mod_rep_int)
data_pain_long$pred_slope = predict(mod_rep_slope)
# random intercept model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# random slope and intercept model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)

# compare models with cAIC
cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic

# -> random intercept + slope describes data better 

# compare models with anova
anova(mod_rep_int, mod_rep_slope)

# -> significantly different! -> random intercept + slope is better 
# X^2(2,13) = 20.73, p < .001

############################################
# adding a ^2 to the random slope + intercept model to account for possibly curved relationship 

mod_rep_slope_quad = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + I(time^2) + (time|ID), data = data_pain_long) # random intercept + random slope 
summary(mod_rep_slope_quad)
r2beta(mod_rep_slope_quad)#, method = "nsj")
confint(mod_rep_slope_quad)
stdCoef.merMod(mod_rep_slope_quad)
?r2beta

## plot the results
# save prediction of the model to new variable
data_pain_long$pred_slope_quad = predict(mod_rep_slope_quad)

# random intercept model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# this looks like a better fit than the others

# compare models with cAIC
cAIC(mod_rep_slope)$caic
cAIC(mod_rep_slope_quad)$caic

# compare models with anova
anova(mod_rep_slope, mod_rep_slope_quad)

# based on the results it seems that the random slope + intercept model
# including the quadratic term of time best describes pain ratings over time
# X^2 (1,14) = 29.89, p < .001


#########################################################
#          Model diagnostics                 
#########################################################

# checking for influential outliers
influence(mod_rep_slope_quad, group = "ID")$alt.fixed
influence(mod_rep_slope_quad, obs = T)$alt.fixed # this can take a minute or so
# ^2 variable no deviant values/outliers -> no influential outlier

######### checking assumptions for quadratic model #############

## normality assumption
# QQ plot
qqmath(mod_rep_slope_quad, id=0.05) 
# -> no violation of normality 

## linearity assumption
# linearity of prediction and standardized residuals
plot(mod_rep_slope_quad)
# linearity of each predictor and the standardized residual
# visualize the linearity of connection of each predictor
predictors=c("sex","age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")

for(i in 1:length(predictors)){
  predictor_to_test = data_pain_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_rep_slope_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw())}
## not taking time^2 into account b/c this var. cannot be linear
## other var. are mostly linear

## homoscedasticty assumption (homogeneity of variance)
plot(mod_rep_slope_quad)
# Levens model for testing heteroscedasticity
summary(lm(residuals(mod_rep_slope_quad)^2 ~ data_pain_long[,"ID"]))
# not significant -> no violation of assumption

## multicollinearity
# correlation matrix
pairs.panels(data_pain_long[,c("sex","age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")], col = "red", lm = T)



