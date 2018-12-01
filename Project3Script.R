###################################################
#
# Project - Multiple Linear Regression
#
# 
# Author: Derek Albosta
# Date:  11/29/18
#
###################################################



###################################################
#
#    Setup and load data
#
###################################################

require(car)    # for qqPlots

cars= read.csv(file.choose(), header = TRUE, na.string="NA", row.names=1)
# verify loading
head(cars)



###################################################
#
#  Examine response variable (mpg) and create
#  scatterplot and correlation matrices
#
###################################################
hist(cars$mpg)
summary(cars$mpg)

# Start with a plot. We are looking for interesting relationships.
pairs(cars)


# There is just too much here. But we've thought about this problem
# and we know we can pare down the list of candidate predictors

# remove catagorial vars for comparisson
k = subset(cars, select = c(mpg,hp,wt,vs,am,carb))
pairs(k)

#convert respective integer data into catagorical values
cars$vs = factor(cars$vs)
cars$am = factor(cars$am)

# Construct a correlation matrix now as well.
cor(k,use="complete.obs")

#convert respective integer data into catagorical values
cars$vs = factor(cars$vs)
cars$am = factor(cars$am)



###################################################
#
#    Let's just try the full model.
#
###################################################
#

# Note the notation mass~. is a shorthand for "model mass on all other
# variables in the data frame"

model1 = lm(mpg~., data = cars, na.action = na.exclude)
summary(model1)

# Wow! looks good. Check the SVAs

# Normality of errors?

# overall
qqPlot(resid(model1))

# Linearity
plot(resid(model1)~fitted(model1), data = cars)
abline(h = 0)

# Constant variance of errors for all values of explanatory?
plot(resid(model1)~ height, data = k)
plot(resid(model1)~ waist, data = k)
plot(resid(model1)~sittingHeight, data=k)

require(DAAG)
vif(model1)


##############################
#
#  End of script
#
##############################
