## Load some packages that will be useful
install.packages("mgcv") # load the MGCV package with Simon Wood's gam() functions
install.packages("readr") # Hadley Wickham's package for reading in data easily
install.packages("ggplot2") # for plotting with ggplot
install.packages("ggfortify")
library(mgcv) # load the MGCV package with Simon Wood's gam() functions
library(readr) # Hadley Wickham's package for reading in data easily
library(ggplot2) # for plotting with ggplot
library(ggfortify) # for autoplot

##################################################################################################
## We'll first examine the bias-variance tradeoff in a simplified form with a small exercise.
## If you remember, this tradeoff is one of the concepts behind finding the optimal amount of smoothing.
## We'll fit linear regressions to parts of the data, increasing the number of parts from 1 to 10.
##################################################################################################

# First, read in the bioluminescence data.
# Sources is the number of bioluminescent plankton seen in each water sample
# SampleDepth is the depth of the water sample, in m
# Station is the ID of the location at which multiple water samples were taken
ISIT <- read_tsv(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/ISIT.txt')) # tsv for tab-separated values

# Subset it to one station. This is what we'll work with for now.
ISITsub <- subset(ISIT, Station == 8)

# Plot the data
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point()

# Now you can fit a linear regression to the ISITsub dataset, with Sources as the response and SampleDepth as the explanatory variable
# Please call the output "mod1", as I've started to fill in for you
mod1 <- lm (ISITsub$Sources ~ ISITsub$SampleDepth, data = ISITsub)

# Q1: What was your code for fitting a linear regression of Sources vs. SampleDepth?
#see above

# We'll predict the mean response and confidence intervals from this model and save the output
ISITsub$mod1 <- predict(mod1)
ISITsub$mod1lwr <- predict(mod1, interval='confidence', level=0.95)[,'lwr']
ISITsub$mod1upr <- predict(mod1, interval='confidence', level=0.95)[,'upr']

# Now plot the linear model fit to the full dataset on top of the data
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point() +
	geom_line(aes(y=mod1)) + 
	geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3)

# Q2: Use your skills from earlier in this course and evaluate this linear model (autoplot may be useful here). How does this fit look to you? Does a linear regression look reasonable? Any well-justified answer is appropriate. 
autoplot(mod1)
#we want residuals around 0, so the residuals are not very normal; therefore a linear regression is not reasonable. 

# For comparison, let's fit linear models to two halves of the data. This involves a fair bit of coding in R, so please ask for help understanding the code where things are confusing. 
# First, we have to find the halves of the data
quants2 <- quantile(ISITsub$SampleDepth, probs=seq(0, 1, by=0.5)) # define the ends and the half-way point in the data with the quantile function
ISITsub$split2 <- cut(ISITsub$SampleDepth, breaks=quants2, include.lowest=TRUE, labels=1:2) 

# Q3: In your own words, what does the cut() function do? How are we using it here?
#The cut function allows us to split up data into two lables
#here we are using it to split the data along the x axis into two subsets (labeled 1 and 2) based on where they fall (first half or second)

# Next, let's fit a linear model to each subset (half) of the data. We do this in a loop to make it easier.
mod2s <- vector('list', length=2) # A list to hold each of our models in a single, convenient object
ISITsub$mod2 <- ISITsub$mod2lwr <- ISITsub$mod2upr <- NA # Initialize the variables we'll use and fill with NA
for(i in 1:length(mod2s)){ # Loop through each model
	mod2s[[i]] <- lm(Sources ~ SampleDepth, data=ISITsub[ISITsub$split2==i,]) # Fit a linear regression to the appropriate subset of the data. Now see why we made the split2 vector?
	ISITsub$mod2[ISITsub$split2==i] <- predict(mod2s[[i]]) # Now predict from that model. Notice we're only predicting for part of the data.
	ISITsub$mod2lwr[ISITsub$split2==i] <- predict(mod2s[[i]], interval='confidence', level=0.95)[,'lwr'] # Same for the confidence intervals
	ISITsub$mod2upr[ISITsub$split2==i] <- predict(mod2s[[i]], interval='confidence', level=0.95)[,'upr']
}

# Plot the data, the 1-model fit, and the 2-model fit
# There will be an odd 3rd line segment connecting the end of the first model fit to the beginning of the second model fit. Just pretend it's not there.
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point() +
	geom_line(aes(y=mod1)) +
	geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3) +
	geom_line(aes(y=mod2, color='red')) +
	geom_ribbon(aes(ymin=mod2lwr, ymax=mod2upr), alpha=0.3, fill='red')


# Q4: Has the fit between the model and the data improved? What's the downside of continuing the split the data into finer and finer chunks and fitting more models?
#Yes, the fit has improved! However, the downside of continuing to split the data/fit more models is the trap of overfitting
#you're not modelling the general trend, you'll eventually just show the exact data set

# Now, it's your turn. Please fit 10 linear regression models. I'd recommend using the 2-model code above as a guide and tweaking it.

quants10 <- quantile(ISITsub$SampleDepth, probs=seq(0, 1, by=0.1)) # define the ends and the 10 points
ISITsub$split10 <- cut(ISITsub$SampleDepth, breaks=quants10, include.lowest=TRUE, labels=1:10) 
mod10 <- vector('list', length=10) # A list to hold each of our models in a single, convenient object
ISITsub$mod10 <- ISITsub$mod10lwr <- ISITsub$mod10upr <- NA # Initialize the variables we'll use and fill with NA
for(i in 1:length(mod10)){ # Loop through each model
    mod10[[i]] <- lm(Sources ~ SampleDepth, data=ISITsub[ISITsub$split10==i,]) # Fit a linear regression to the appropriate subset of the data. Now see why we made the split2 vector?
    ISITsub$mod10[ISITsub$split10==i] <- predict(mod10[[i]]) # Now predict from that model. Notice we're only predicting for part of the data.
    ISITsub$mod10lwr[ISITsub$split10==i] <- predict(mod10[[i]], interval='confidence', level=0.95)[,'lwr'] # Same for the confidence intervals
    ISITsub$mod10upr[ISITsub$split10==i] <- predict(mod10[[i]], interval='confidence', level=0.95)[,'upr']
}


# Now plot the 10 models on top of the data and on top of the 1- and 2-model fits. The plot with the data, 1-model, and 2-model fit is probably a useful guide here.

ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
    geom_point() +
    geom_line(aes(y=mod10)) +
    geom_ribbon(aes(ymin=mod10lwr, ymax=mod10upr), alpha=0.3) +
    geom_line(aes(y=mod10, color='red')) +
    geom_ribbon(aes(ymin=mod10lwr, ymax=mod10upr), alpha=0.3, fill='red')



# Q5: Compare your 10-model fit to the 1-model fit. Which model's predictions are furthest from the observed data? Which model has the widest confidence bounds? How does this (or does this not) illustrate the bias-variance tradeoff?
#Model 1 was the furthest from observed data, but Model 10 had the widest confidence bounds
#bias-variance tradeoff = increasing bias to reduce variance
#mod1 is biased because we assume a linear fit; mod10 has high variance, but it fits so well



####################################################
## Now you'll fit some GAMs and evaluate your models
####################################################
#install.packages("maps") # if needed
library(maps) # has map data in it

# load the data
spdata <- readRDS(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/gadusmorhua.rds')) # load the spdata data.frame with abundance and environmental data from cod surveys
spdata$presfit01 <- as.numeric(spdata$presfit) # make a vector that's nice for plotting
spdata <- spdata[order(spdata$presfit01),] # order the data from absent to present for ease of plotting

# examine the data
head(spdata) # look at the dataset
summary(spdata)

# make a map of the data
world <- map_data('world')

ggplot() + 
	geom_polygon(data=world, aes(x=long, y=lat, group=group), color='black', fill=NA) + # the map
	xlim(-100, -45) + 
	ylim(23, 62) +
	geom_point(data=spdata, aes(x=lon, y=lat, color=presfit, alpha=0.1), size=0.01) # the data points. note that because the dataframe is sorted from absent to present, the presences are plotted on top of the absences


# Q6: Based on the map, do you think cod prefer warmer or cooler waters? Why do you think that?
#Cod like cooler water. I think that because all of the blue points are in the nothern area = colder water


# Q7: Plot cod presence/absence (the presfit vector) vs. the bottom temperature (SBT.actual). At what range of temperatures have cod been observed?

ggplot(spdata, aes(x=SBT.actual, y=presfit01)) +
    geom_point()
#cod can be found in the range ~0 - 20 Celsius

# Q8: Fit a GAM for presfit against SBT.actual. Check the model. Does it look like the assumptions have been met? Why or why not?

gam1 <- gam(presfit01 ~ s(SBT.actual, k=8), data=spdata)
gam.check(gam1)

#does not meet assumptions, using plots we can see that it is violating normality


# Q9: Fit a new GAM with a more appropriate error structure and save it as "mod2". The gam() function takes the same family= argument as does glm(). Which error structure and link function did you choose?
mod2 <- gam(presfit01 ~ s(SBT.actual, k=8), data=spdata, family=binomial)
gam.check(mod2)


# Q10: Check your mod2 GAM. Does it look like the assumptions have been met? Make sure to read the text output as well as look at the graphs. If something doesn't look right, what would you do to fix it?

#yes, the deviance residuals vs. theoretical quantiles plot follows the 1:1 ratio
#I changed the k value because the output showed a low p-value, indicating that the edf and k value were too close


# Q11: Interpret your mod2 GAM (summary function). How much deviance is explained? Is the SBT.actual term significant at alpha=0.05? How wiggly do you expect the smooth fit to be?
#deviance = 25.4%
#significance = yes, the SBT.actual is significant
#line = pretty curvy, since edf is a 9
summary(mod2)


# Now let's make predictions from our last model (mod2) and plot them.
nd <- data.frame(SBT.actual = seq(-5,30, by=0.5)) # make a data.frame of new explanatory variables
nd$mod2 <- predict(mod2, newdata=nd, type='response') # predict on the scale of the response
nd$mod2se <- predict(mod2, newdata=nd, type='response', se.fit=TRUE)$se.fit # get the standard errors of the fit

ggplot(spdata, aes(x=SBT.actual, y=presfit01)) +
	geom_point() +
	geom_line(data=nd, aes(x=SBT.actual, y=mod2, color='red')) +
	geom_ribbon(data=nd, aes(x=SBT.actual, ymin=mod2-mod2se, ymax=mod2+mod2se), alpha=0.3, fill='red', inherit.aes=FALSE)


# Q12: Does the fit look realistic? Why or why not? Does it look overfit? Why or why not?
#I feel like the fit is not realistc because it has many curves; it looks overfit
#it captures too many of the ups and downs to be realistic 




# Q13: This dataset is actually a concatenation of seven different surveys which each have slightly different abilities to catch cod. Fit a new GAM that includes a categorical predictor for survey (the region vector). Based on AIC, which model would you choose? How confident would you be?
mod3 <- gam(presfit01 ~ s(SBT.actual, k=6) + region, data=spdata, family=binomial)
library(MuMIn) # for model selection
model.sel(mod3, mod2)

#AIC shows that model 3, which incorporates region, is the better model. Pretty confident because it is quite a big difference between models. 

