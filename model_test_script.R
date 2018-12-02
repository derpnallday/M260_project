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

#take a look at our respone var
hist(cars$mpg)
summary(cars$mpg)

#convert respective vars to catagorical vars
cars$vs = factor(cars$vs)
cars$am = factor(cars$am)

# OK, explore a little. Draw a picture
pairs(cars)


#looks like we want to remove dips, cyl, and drat
#since they are highly correlated with other vars
#also removing gear since it has lowest correlation

c = subset(cars, select = c(mpg,hp,wt,vs,am))
pairs (c)


#subset for cor matrix of all remaning numeric vars
k = subset(cars, select = c(mpg,cyl,disp,hp,wt,qsec,gear,carb))

# Construct a correlation matrix now as well.
cor(k,use="complete.obs")



#######################################################
#
# MODEL TESTING
#
#######################################################
# build individual models on numeric explanatory vars to
# see how well they produce a model
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


##########################################################
#build models on catagorical vars
#test mpg~vs
m3 = lm(mpg~vs, data = cars, na.action = na.exclude)
summary(m3)

# SVAs
#qq looks good
qqPlot(resid(m3))

#variances are skewed slightly to right for both types
plot(resid(m3)~vs, data = cars)


#mpg~am
m4 = lm(mpg~am, data = cars, na.action = na.exclude)
summary(m4)

# SVAs
#qq looks good
qqPlot(resid(m4))

#variances are skewed slightly to right for manual
plot(resid(m4)~am, data = cars)

###########################################################
#lets try combining vars and do some AIC testing
#just numerics
m5 = lm(mpg~wt+hp, data = cars, na.action = na.exclude)
summary(m5)
#AIC = 156.6523

#with vs and both numerics
m6 = lm(mpg~vs+wt+hp, data = cars, na.action = na.exclude)
summary(m6)
#AIC = 157.5052

#with am and both numerics
m7 = lm(mpg~am+wt+hp, data = cars, na.action = na.exclude)
summary(m7)
#AIC = 156.1348

#using all selected vars
m8 = lm(mpg~am+vs+wt+hp, data = cars, na.action = na.exclude)
summary(m8)
#AIC = 156.0584

##########################################################
#now try with interaction terms

#we already know from AIC model with both hp and wt are better
#with hp*wt with am
m9 = lm(mpg~am+wt*hp, data = cars, na.action = na.exclude)
summary(m9)
#AIC = 150.4317

#try hp*wt with vs
m10 = lm(mpg~vs+wt*hp, data = cars, na.action = na.exclude)
summary(m10)

#try hp*wt with vs and am
m11 = lm(mpg~vs+am+wt*hp, data = cars, na.action = na.exclude)
summary(m11)


#test models
AIC(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)

#best model is m10

m = m10
summary(m)


# lets run more tests 
# Check the residuals
par(mfrow=c(1,1))
qqPlot(resid(m))
par(mfrow=c(1,2))
plot(resid(m)~vs, data = cars,main="Residuals vs. vs")

#par(mfrow=c(1,2))
stripchart(resid(m)~vs, data = cars, method = "jitter",
           vertical = TRUE,main="Residuals vs. vs")

#grab vif values
require(DAAG)
vif(m)

#slight issues with straight engine. also outlier in v engine residuals but still mostly good
#variance for levels look fine

# plot this using different symbols for each group and
# and different colors for each group

(beta0    = coef(m)["(Intercept)"])
(betaVs   = coef(m)["vs1"])
(betaWt   = coef(m)["wt"])
(betaHp   = coef(m)["hp"])
(betaWtHp  = coef(m)["wt:hp"])

# lets look at levels of or catagorical against numeric vars
#check against engine type
#mpg~wt
par(mfrow=c(1,2))
plot(mpg~wt, col=colors[as.numeric(cars$vs)],
     pch=symbols[as.numeric(cars$occupation)],
     xlab="Weight",
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

# it's nice to add a legend (see ? legend for more info)
legend("topright", fill=colors, c("V-shaped","S-Shaped"))  
     

# Test result:
           # Best model takes both HP and Weight with engine type as the interaction term
           

# Interpretation:

#  Mean systolic blood carsure seems to be associated with age and occupation (F-test)
#  Both age and occupation seem to be associated with sbp even if the other explanatory variable
#  is in the model (t-tests of coefficients)
#  This model explains about 90% of the variation in sbp (multiple R^2)
#  Mean sbp increases an estimated 1.54 points for each one-year increase in age (Beta1^)
#  Mean sbp is lower by an estimated 7.51 points for university lecturers (vs.journalists) (Beta0^)
confint(m)
#
#  True mean sbp increase for each one-year increase in age is estimated to be 
#  in the interval (1.32, 1.76) ( 95% CI for Beta1)
#  True difference in mean sbp for university lecturers is estimated to be 4.19 to 10.84 units
#  lower than mean sbp for journalists (95% CI for Beta0)

# Use the model

# estimate mean for a particular value of age and occupation

# V-shaped engine with 110 hp and 2.62 (thousand) as weight
#actual is 21 mpg - prediction 22.39
car1 = data.frame(hp = 110, wt = 2.62, vs = '0')
predict(m, new = car1, interval = "confidence" )
predict(m, new = car1, interval = "prediction" )

# V-shaped engine with 245 hp and 3.57(thousand) as weight
#actual 14.3 - prediction 15.45
car2 = data.frame(hp = 245, wt = 3.57, vs = '0')
predict(m, new = car2, interval = "confidence" )
predict(m, new = car2, interval = "prediction" )

# S-shaped engine with 93 hp and 2.32 (thousand) as weight
#around 22.8 prediction 25.8504
car3 = data.frame(hp = 93, wt = 2.32, vs = '1')
predict(m, new = car3, interval = "confidence" )
predict(m, new = car3, interval = "prediction" )

# S-shaped engine with 66 hp and 2.2 (thousand) as weight
#around 32.4 prediction 27.95208
car4 = data.frame(hp = 66, wt = 2.2, vs = '1')
predict(m, new = car4, interval = "confidence" )
predict(m, new = car4, interval = "prediction" )





# Scope of inference?

# Cause/effect
# We can see that increasing hp negatively affects the MPG value
# We can see increasing the weight negatively affects the MPG value
# V-shaped engines tend to have better MPG than S-shaped engines
# predictions for V-shaped tend to undershoot while S-shapes overshoot real value slightly

# Generalization
# We see an association between hp, weight, and engine type on MPG in the sample.
# To generalize would mean we would expect to see the same association
# in the larger population that this sample represents.
# Generalization is not justified here. This dataset was taken in 1974. Technology
# has changed drastically and some of these factors may influence MPG differently than before
# One noteble example is transmition type, which was left out. Modern day automatic transmitions
# tend to get way better MPG than manual counterparts eventhough our testing said otherwise.

# So:  In this sample of 33 cars, there is a statistically significant association
# between MPG (response) and hp, wt, and engine type (explanatory). We cannot infer cause/effect 
# since this data is using older vehicles and technolgy has change so much since the test date.

########################################################
#Now lets do some more analysis on model















