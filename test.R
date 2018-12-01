###################################################
#
# Blood pressure study
# This data is taken from a study of blood pressures from 
# 28 adult males. The following comes from the description of the study:
#
# "It is well-known that blood pressure increases with age.
# In this dataset we examine this relation. Age and systolic 
# blood pressure where measured for 28 males. 15 of these are 
# university teachers, while the remaining 13 are journalists. 
# Along with the interest in the overall increase in systolic 
# blood pressure, we can compare the regression lines between 
# the two groups. [Note: data are not genuine.] "

# source: http://statmaster.sdu.dk/courses/st111/data/index.html#SECTION00013700000000000000

# Original Author: Chuck Hommel
# Date: 2013.04.11
#
# Revised by:  Wendy Dove
# Date:  2015.04.20
#
# Our goal is to model systolic blood pressure using the age
# and profession of the subject as predictors. 
# This script is intended to provide a basic model for model
# selection.
# 
# The basic outline is this:
#     1. hypothesize a model - here we think that systolic
#        blood pressure might be modeled by the age and profession
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

#Systolic blood pressure for journalists and university teachers by age

# set up
require (car)   # for normal quantile plots

# Set up for plotting. We will be using two groups (two professions)
# so the following lines set up color codes and plotting symbols
# for them
colors = c("red","blue") # colors for our plots
symbols =c(19, 23)       # symbols to use for plotting (see ?pch)

# load the data and verify
press = read.csv(file.choose())
head(press)
names(press)

# occupation is categorical, but coded 0/1. Make it a factor
# Note that 0 = journalist, 1 = university lecturer
press$occupation = factor(press$occupation)

# OK, explore a little. Draw a picture
pairs(press)
cor(press$systolic,press$age)

# a first model with just age as explanatory
m1 = lm(systolic~age, data = press, na.action = na.exclude)
summary(m1)

# diagnostics
qqPlot(resid(m1))
plot(resid(m1)~age, data = press)
# no big problems
# This is a reasonable model.
# Here is the graph
plot(systolic~age, data = press)
abline(m1)


# But we hypothesized that occupation
# might be useful too. Let's try it

# A second model with both explanatory variables
# response: systolic blood pressure
# explanatory: occupation, age
m2 = lm(systolic~occupation+age, data = press, na.action = na.exclude)
summary(m2)
# an improved fit with both variables significant. 
# Check the residuals
qqPlot(resid(m2))
par(mfrow=c(1,2))
plot(resid(m2)~age, data = press,main="Residuals vs. Age")
abline(h=0)
stripchart(resid(m2)~occupation, data = press, method = "jitter",
           vertical = TRUE,main="Residuals vs. Occupation")

require(DAAG)
vif(m2)

# no problems
# plot this using different symbols for each group and
# and different colors for each group

 
(beta0            = coef(m2)["(Intercept)"])
(betaAge          = coef(m2)["age"])
(betaOccupation   = coef(m2)["occupation1"])


# we can again look at what's happening graphically

plot(systolic~age, col=colors[as.numeric(press$occupation)],
     pch=symbols[as.numeric(press$occupation)],
     xlab="Age(years)",
     ylab="Systolic BP",
     main="Systolic Blood Pressure by Age and Occupation",
     data=press)

# putting in the regression lines, we have:
abline(beta0,betaAge, col=colors[1])      # for baseline group ()
abline(beta0 + betaOccupation,         # for professional
       betaAge,
       col=colors[2])


# it's nice to add a legend (see ? legend for more info)
legend("bottomright", fill=colors, c("Journalist","University Lecturer"))  

AIC(m1,m2)
     
# Now consider an interaction term. There is no obvious reason to 
# do this (i.e. the graph doesn't suggest this), but we will do
# it for illustration
m3 = lm(systolic~age*occupation, data = press, na.action = na.exclude)
summary(m3)

# no real evidence that this is any improvement.
           

plot(resid(m3)~age, data = press)
stripchart(resid(m3)~occupation, data = press, method = "jitter",
           vertical = TRUE)
           
# no problems with the SVAs
# comparison of different models
AIC(m1, m2, m3)

# Conclusions:
           # Systolic blood pressure is well-modeled by age and occupation
           # There is no evidence of interaction between the two 
           # explanatory variables
           
# Model 2, using age and occupation to model systolic blood pressure
           # seems 'best'

# Interpretation:

#  Mean systolic blood pressure seems to be associated with age and occupation (F-test)
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

