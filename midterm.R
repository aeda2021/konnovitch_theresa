#Tess Konnovitch

### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands


##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join

b <- read.csv(file = '~/Classes/AEDA/pinelands_bees.csv') #reading files
lc <- read.csv(file = '~/Classes/AEDA/land_cover.csv')
head(b) #seeing files
head(lc)

bysite <- group_by(b, site_name) #now they are organized by site_name
c <- add_count(bysite, site_name) #adds column with n = number of bees at that site

blc <- left_join(c, lc, by = c("site_name" = "ï..site_name")) #uses site as the ID to join the data

## 2 Data picture
# plot the data and figure out what type of model might be best
#we want to know if #bees is a function of land cover

plot(x = blc$Nat1600, y = blc$n) #looks like negative trend
#wanted it to look nicer in ggplot
ggplot(blc, aes(x = blc$Nat1600, y = blc$n))+
    geom_point()

bm1 <- lm(blc$n ~ blc$Nat1600) #linear model
bm2 <- anova(bm1) #anova

## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

autoplot(bm1)
summary(bm1)
mean(blc$n) # a lot of 0 and low mean = poisson; mean is 57 so we're good
hist(bm1$residuals) #shows normally distributed around 0
hist(blc$n)

autoplot(bm2)
summary(bm2)
mean(blc$n) # a lot of 0 and low mean = poisson; mean is 57 so we're good
hist(bm2$residuals)

#I originally thought that an anova would be the better model because we have both numeric (n) and categorical (site) data
#however, the linear model I first use shows normality; also my anova gives me errors when I try to check residual histogram


## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why
summary(bm1)
#Interpretations:
#*Residuals - already looked at, but this summary confirms that they are evenly distributed around 0
#1. Coefficients:shows that the slope is about -.6, supporting my thought that there is a negative relationship between increasing land cover and number of individuals; 
#also shows intercepts and standard error, showing that results can vary by .03256
#2. P value: <2.2e-16, indicating null hypothesis should be rejected/we have significant result statistically (perhaps not biologically)
#3. Explanatory value of model: This model explains that the data is normal (given distribution of residuals) and that increasing land cover is correlated with decreasing number of bees recorded. 
#4. Conclusions: In conclusion: increasing land cover is correlated with decreasing bee abundance/presence, as per collection data.
#5. Magnitude and Biological significance: Correlation does not prove causation. I am unaware of the relationship between bee abundance and land cover.
#Using this data, I would hypothesize that perhaps more land cover = more habitats for other insect = more competition for resources, and perhaps this is why
#there are less bees. Or, perhaps increased land cover does not provide the resources needed for bee habitats (again, I am not too familiar)
#Biological significance cannot be determined using a p-value or model. However, with appropriate rationale and theory, I feel that this data could support a 
#hypothesis that predicts decreasing bee abundance with increasing land cover
#6. Confidence in Analysis: I am quite confident that I successfully conducted a linear model. However, I am not as confident that this was the most 
#appropriate model to use. Regardless of model, I am confident that this data shows that increasing land cover is correlated with a decreased in bee abundance. 


## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want

ggplot(blc, aes(x = blc$Nat1600, y = blc$n))+
    geom_point()+
    geom_smooth(span = 1, color = "red", method = "lm") #using lm method 


## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

#I would use site name as a random effect, because land cover is what we're really interested in; site name was used to just identify land cover amount in this assignment
random_m1 <- lmer(n~Nat1600 + (1|site_name), data=blc)
summary(random_m1)


### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.


# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)
data_mod <- read.csv(file = '~/Classes/AEDA/modSel.csv')
hist(data_mod$observedAbundance) #shows many 0 counts
mean(data_mod$observedAbundance) #mean = 1.9

#given this data I assumed that poisson would be best, because of the many counts of 0
#I decided to use fitdist, as we did in class, and found that nbinom actually had the lower AIC value

fitp <- fitdist(data_mod, "pois")
fitnb <- fitdist(data_mod, "nbinom")
summary(fitp)
summary(fitnb)



# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines
ann_temp <- glm(observedAbundance ~ meanAnnualTemp,
                     data = data_mod, family = negative.binomial(2))
sum_temp <- glm(observedAbundance ~ meanSummerTemp,
                data = data_mod, family = negative.binomial(2))
model.sel(ann_temp, sum_temp) #summer temp is the better model, based on AICc

ann_prec <- glm(observedAbundance ~ annualPrecipitation,
                data = data_mod, family = negative.binomial(2))
sum_prec <- glm(observedAbundance ~ summerPrecipitation,
                data = data_mod, family = negative.binomial(2))
model.sel(ann_prec, sum_prec) #summer precip is the better model, based on AICc

total_e <- glm(observedAbundance ~ totalEdge,
                data = data_mod, family = negative.binomial(2))
dist_e <- glm(observedAbundance ~ distance2edge,
                data = data_mod, family = negative.binomial(2))
model.sel(total_e, dist_e) #total e is the better model, based on AICc



# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

mfull <- glm(observedAbundance ~ totalEdge+summerPrecipitation+meanSummerTemp,
              data = data_mod, family = negative.binomial(2))
mpe <- glm(observedAbundance ~ totalEdge+summerPrecipitation,
             data = data_mod, family = negative.binomial(2))
mpt<- glm(observedAbundance ~ summerPrecipitation+meanSummerTemp,
          data = data_mod, family = negative.binomial(2))
met <- glm(observedAbundance ~ summerPrecipitation+meanSummerTemp,
             data = data_mod, family = negative.binomial(2))
mp <- glm(observedAbundance ~ summerPrecipitation,
          data = data_mod, family = negative.binomial(2))
me <- glm(observedAbundance ~ totalEdge,
               data = data_mod, family = negative.binomial(2))
mt <- glm(observedAbundance ~ meanSummerTemp,
          data = data_mod, family = negative.binomial(2))
model.sel(mfull, mpe, mpt, met, mp, me, mt)

#In conclusion, the best model was the full model, which combined the following:
#TotalEdge, SummerPrecipitation, and MeanSummerTemp. 


# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?

mfull <- glm(observedAbundance ~ totalEdge+summerPrecipitation+meanSummerTemp,
             data = data_mod, family = negative.binomial(2)) #final model being used
summary(mfull)

#based on the summary, my hypotheses are supported.
#I predicted that abundance would be higher in more core areas (= less edge)
#this data summary shows that edge influences abundance negatively (-.546807)
#I predicted that precipitation and temp would increase abundance
#this data summary supports this hypothesis, because both SummerPrecipitation and 
#MeanSummerTemp influence abundance positively (.004287 and 0.533818)
#Based on this summary, I beleive that the predictors are of the following importance,
#ranked from most to least: edge, temperature, precipitation 

#My general conclusion is that species are more abundant in colder, warmer, and more "core" habitats. 







