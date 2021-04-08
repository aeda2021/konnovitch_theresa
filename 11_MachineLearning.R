#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("ranger")
install.packages("pdp")
install.packages("kernlab")
if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")
# Install traitdata package from Github
remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)
## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('C:/Users/tessa/OneDrive/Documents/Classes/AEDA/konnovitch_theresa/ML_helpers.R')

set.seed(100)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

df <- amniote_data %>% filter(litter_or_clutch_size_n!=0)
df <- df %>% filter(Class=='Reptilia')
df <- df %>% select(where(~!all(is.na(.x))))
df$y <- log(df$litter_or_clutch_size_n)


##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.
hist(df$y)


##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.
preprocesses <- preProcess(df, method = 'medianImpute')
df_impute <- predict(preprocesses, df)
dim(df_impute)
names(df_impute)

# Remove taxonomic data
cols=c(7:30,32)

df_impute_data=df_impute[,cols]

dim(df_impute_data)
names(df_impute_data)

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?
#linear model will need to be log-transformed because it assumes a noraml error distribution
par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(df_impute_data)){
            hist(df_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}
#log transformation
log_cols <- c(1:24)

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(df_impute_data)){
            hist(df_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}

df_impute_data[, log_cols] <- log1p(df_impute_data[, log_cols])

##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?

apriori_formula <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + litters_or_clutches_per_y + maximum_longevity_y
df_lm <- train(apriori_formula, data = df_impute_data, method = 'lm', trControl = trcntrl, na.action = na.omit)
plotCV(df_lm)
summary(df_lm$finalModel)

#The mammal model is better based in comparing models function (by 0.01)

##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?
enet_gr <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
df_enet <- train(y ~ ., data = df_impute_data, method = 'enet', tuneGrid = enet_gr, trControl = trcntrl, na.action = na.omit)

plotCV(df_enet)

df_enet$results$Rsquared %>% max

df_enet$results %>%
    ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
    geom_line() +
    geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')
#Yes, seems to be a better fit; comparing models shows its better than linear

##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?

gp_gr <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
df_gp <- train(y ~ ., data = df_impute_data, method = 'gaussprRadial', tuneGrid = gp_gr, trControl = trcntrl, na.action = na.omit)

df_gp$results %>% ggplot(aes(sigma, Rsquared)) +
    geom_line() + geom_point() + xlab('Sigma')

plotCV(df_gp)

#gaussian process is better than lm
df_gp
df_gp$results$Rsquared %>% max

##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model? #smaller node = less noise, sets min size node
##What does the number of random predictors selected indicate about interaction depth? More predictors = more interections/better fit becuase of overfitting?
rf_gr <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
df_rf <- train(y ~ ., data = df_impute_data, method = 'ranger', tuneGrid = rf_gr, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

df_rf$results %>%
    ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
    geom_line() +
    geom_point() +
    labs(colour = 'min.node.size')

plotCV(df_rf)

df_rf
df_rf$results$Rsquared %>% max

##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?

compare_models(mammal_m2_gp, mammal_m3_rf) #.93
compare_models(mammal_m1_enet, mammal_m3_rf) #.88
compare_models(mammal_m0_lm, mammal_m3_rf) #.72

compare_models(df_gp, df_rf) #92
compare_models(df_enet, df_rf) #.98
compare_models(df_lm, df_rf) #.46

#Mammal models on average showed higher correlation (values are #); however the enet in amniota was the highest
#shows that one model isnt universally always better!

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 
varImp(df_enet) 
varImp(df_gp)
varImp(df_rf)
#litter and clutch size, and adult body mass seem to be the most important variables across models
#(I'm assuming litter+clutch is 1 because its in the model? So really adult body mass is best)

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.
##How do they differ?
partial(df_gp, pred.var = c('litter_or_clutch_size_n'), plot = TRUE) 
partial(df_rf, pred.var = c('litter_or_clutch_size_n'), plot = TRUE)
#only differ slightly, rf has is less smooth
##What does this say about the likely relation ship between litter/clutch size and the best predictor variable?
#increasing adult body mass is correlated with increasing litter/clutch size


