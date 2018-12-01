###################################################
#
# Blood carsure study
# This data is taken from a study of blood carsures from 
# 28 adult males. The following comes from the description of the study:
#
# "It is well-known that blood carsure increases with age.
# In this dataset we examine this relation. Age and systolic 
# blood carsure where measured for 28 males. 15 of these are 
# university teachers, while the remaining 13 are journalists. 
# Along with the interest in the overall increase in systolic 
# blood carsure, we can compare the regression lines between 
# the two groups. [Note: data are not genuine.] "

# source: http://statmaster.sdu.dk/courses/st111/data/index.html#SECTION00013700000000000000

# Original Author: Chuck Hommel
# Date: 2013.04.11
#
# Revised by:  Wendy Dove
# Date:  2015.04.20
#
# Our goal is to model systolic blood carsure using the age
# and profession of the subject as predictors. 
# This script is intended to provide a basic model for model
# selection.
# 
# The basic outline is this:
#     1. hypothesize a model - here we think that systolic
#        blood carsure might be modeled by the age and profession
#        of the subjects
#     2. Collect the data - the web site has the data for us - but in
#        real life, this is hard and requires a design
#     3. Build the model
#     4. Check that the model is reasonable - are SVAs reasonable?
#     5. Assess the model - any indications that we can do better 
#        by using a different set of explanatory variables? transformations?
#        etc.?
#     6. Interpret the model - what does it say?
#     7. Use the model for estimation/prediction
#
###################################################
# Model selection

#MPG for cars given its specifications

# set up
require (car)   # for normal quantile plots
require(DAAG) # for vif

# Set up for plotting. We will be using two groups (two professions)
# so the following lines set up color codes and plotting symbols
# for them
colors = c("red","blue") # colors for our plots
symbols =c(19, 23)       # symbols to use for plotting (see ?pch)

# load the data and verify
cars = read.csv(file.choose())
head(cars)
names(cars)

#convert respective vars to catagorical vars
cars$vs = factor(cars$vs)
cars$am = factor(cars$am)

# OK, explore a little. Draw a picture
pairs(cars)

#subset for cor matrix
k = subset(cars, select = c(mpg,hp,wt))
# Construct a correlation matrix now as well.
cor(k,use="complete.obs")

#######################################################

# build individual models on numeric explanatory vars to see 
#on mpg~weight
m1 = lm(mpg~wt, data = cars, na.action = na.exclude)
summary(m1)

# SVAs
#qq plot slight departure at upper quantile
qqPlot(resid(m1))

#variances are don't look all too good
plot(resid(m1)~wt, data = cars)
abline(h=0)

#mpg~wt shows good negative correlation
plot(mpg~wt, data = cars)
abline(m1)

AIC(m1)

#test mpg~hp
m2 = lm(mpg~hp, data = cars, na.action = na.exclude)
summary(m2)

# SVAs
#qq plot slight departure at upper quantile
qqPlot(resid(m2))

#variance more even in middle
plot(resid(m2)~hp, data = cars)
abline(h=0)

#mpg~wt shows decent negative correlation
plot(mpg~hp, data = cars)
abline(m2)

AIC(m2)

##########################################################
#build models on catagorical vars
#test mpg~vs
m3 = lm(mpg~vs, data = cars, na.action = na.exclude)
summary(m3)
# an improved fit with both variables significant. 
# Check the residuals
qqPlot(resid(m3))
par(mfrow=c(1,2))
plot(resid(m3)~vs, data = cars,main="Residuals vs. vs")

stripchart(resid(m3)~vs, data = cars, method = "jitter",
           vertical = TRUE,main="Residuals vs. vs")
AIC(m3)

#mpg~am
m4 = lm(mpg~am, data = cars, na.action = na.exclude)
summary(m4)
# an improved fit with both variables significant. 
# Check the residuals
qqPlot(resid(m4))
par(mfrow=c(1,2))
plot(resid(m4)~am, data = cars,main="Residuals vs. am")

stripchart(resid(m4)~am, data = cars, method = "jitter",
           vertical = TRUE,main="Residuals vs. am")

AIC(m4)

#lets try combining vars and do some AIC testing
#just numerics
m5 = lm(mpg~wt+hp, data = cars, na.action = na.exclude)
summary(m5)
#AIC = 156.6523
AIC(m5)

#with vs and both numerics
m6 = lm(mpg~vs+wt+hp, data = cars, na.action = na.exclude)
summary(m6)
#AIC = 157.5052
AIC(m6)

#with am and both numerics
m7 = lm(mpg~am+wt+hp, data = cars, na.action = na.exclude)
summary(m7)
#AIC = 156.1348
AIC(m7)

#using all selected vars
m8 = lm(mpg~am+vs+wt+hp, data = cars, na.action = na.exclude)
summary(m8)
#AIC = 156.0584
AIC(m8)

#now try with interaction terms

#we already know from AIC
#vs as interaction term

m9 = lm(mpg~wt+hp*vs, data = cars, na.action = na.exclude)
summary(m9)
#AIC = 154.9286
AIC(m9)



#looks like using all vars is the best model
# lets run more tests 
# Check the residuals
par(mfrow=c(1,1))
qqPlot(resid(m))
par(mfrow=c(2,2))
plot(resid(m)~am, data = cars,main="Residuals vs. am")
plot(resid(m)~vs, data = cars,main="Residuals vs. vs")

#par(mfrow=c(1,2))
stripchart(resid(m)~am, data = cars, method = "jitter",
           vertical = TRUE,main="Residuals vs. am")
stripchart(resid(m)~vs, data = cars, method = "jitter",
           vertical = TRUE,main="Residuals vs. vs")

#grab vif values
require(DAAG)
vif(m)

#slight issues with manual transmition residuals but other resids look good
#variance for levels look fine


# plot this using different symbols for each group and
# and different colors for each group

(beta0    = coef(m)["(Intercept)"])
(betaAm   = coef(m)["am1"])
(betaVs   = coef(m)["vs1"])
(betaWt   = coef(m)["wt"])
(betaHp   = coef(m)["hp"])


# lets look at levels of or catagorical against numeric vars
#first against engine type
#mpg~wt
par(mfrow=c(1,2))
plot(mpg~wt, col=colors[as.numeric(cars$vs)],
     pch=symbols[as.numeric(cars$occupation)],
     xlab="hp",
     ylab="MPG",
     main="MPG by engine type and weight",
     data=cars)

# fit ablines over levels
abline(beta0,betaWt, col=colors[1])      
abline(beta0 + betaVs, betaWt, col=colors[2])

#mpg~hp
plot(mpg~hp, col=colors[as.numeric(cars$vs)],
     pch=symbols[as.numeric(cars$occupation)],
     xlab="hp",
     ylab="MPG",
     main="MPG by engine type and hp",
     data=cars)

#fit ablines over levels
abline(beta0,betaHp, col=colors[1])      
abline(beta0 + betaVs,betaHp, col=colors[2])

#############################
#now against transmition
#mpg~wt
par(mfrow=c(1,2))
plot(mpg~wt, col=colors[as.numeric(cars$am)],
     pch=symbols[as.numeric(cars$occupation)],
     xlab="hp",
     ylab="MPG",
     main="MPG by engine type and weight",
     data=cars)

# fit ablines over levels
abline(beta0,betaWt, col=colors[1])      
abline(beta0 + betaAm, betaWt, col=colors[2])

#mpg~hp
plot(mpg~hp, col=colors[as.numeric(cars$am)],
     pch=symbols[as.numeric(cars$occupation)],
     xlab="hp",
     ylab="MPG",
     main="MPG by engine type and hp",
     data=cars)

# fit ablines over levels
abline(beta0,betaHp, col=colors[1])      
abline(beta0 + betaAm, betaHp, col=colors[2])






# it's nice to add a legend (see ? legend for more info)
legend("topright", fill=colors, c("Journalist","University Lecturer"))  
     
# Now consider an interaction term. There is no obvious reason to 
# do this (i.e. the graph doesn't suggest this), but we will do
# it for illustration
m8 = lm(mpg~wt+hp*vs, data = cars, na.action = na.exclude)
summary(m8)
#AIC = 154.9286
AIC(m8)

# slight improvment
           

plot(resid(m3)~age, data = cars)
stripchart(resid(m3)~occupation, data = cars, method = "jitter",
           vertical = TRUE)
           
# no problems with the SVAs
# comparison of different models
AIC(m1, m2, m3)

# Conclusions:
           # Systolic blood carsure is well-modeled by age and occupation
           # There is no evidence of interaction between the two 
           # explanatory variables
           
# Model 2, using age and occupation to model systolic blood carsure
           # seems 'best'

# Interpretation:

#  Mean systolic blood carsure seems to be associated with age and occupation (F-test)
#  Both age and occupation seem to be associated with sbp even if the other explanatory variable
#  is in the model (t-tests of coefficients)
#  This model explains about 90% of the variation in sbp (multiple R^2)
#  Mean sbp increases an estimated 1.54 points for each one-year increase in age (Beta1^)
#  Mean sbp is lower by an estimated 7.51 points for university lecturers (vs.journalists) (Beta0^)
confint(m2)
#
#  True mean sbp increase for each one-year increase in age is estimated to be 
#  in the interval (1.32, 1.76) ( 95% CI for Beta1)
#  True difference in mean sbp for university lecturers is estimated to be 4.19 to 10.84 units
#  lower than mean sbp for journalists (95% CI for Beta0)

# Use the model

# estimate mean for a particular value of age and occupation

# 65-year-old journalist
journalist65 = data.frame(age = 65, occupation = '0')

# 65-year-old instructor
instructor65 = data.frame(age = 65, occupation = '1')

# 40-year-old journalist
journalist40 = data.frame(age = 40, occupation = '0')

# 65-year-old instructor
instructor40 = data.frame(age = 40, occupation = '1')

predict(m2, new = journalist65, interval = "confidence" )
predict(m2, new = instructor65, interval = "confidence" )
predict(m2, new = journalist40, interval = "confidence" )
predict(m2, new = instructor40, interval = "confidence" )

# estimate individual reponse for a particular value of age and occupation

predict(m2, new = journalist65, interval = "prediction" )
predict(m2, new = instructor65, interval = "prediction" )
predict(m2, new = journalist40, interval = "prediction" )
predict(m2, new = instructor40, interval = "prediction" )


# Scope of inference?

# Cause/effect
# To infer cause/effect would mea that a change in age causes a change in mean sbp. Also, a change in occupation
# causes a change in mean sbp.
# Cause/effect is not justified since this is an observational study only

# Generalization
# We see an association between age, occupation and sbp in the sample.
# To generalize would mean we would expect to see the same association
# in the larger population that this sample represents.
# Generalization is not justified here. There is no indication of where
# or how the sample was obtained. We have no way of describing what population
# this sample might represent.

# So:  In this sample of 28 instructors and journalists, there is a statistically significant association
# between sbp (response) and age and occupation (explanatory). We cannot infer cause/effect and
# generalizing to any larger group would be speculative.