############################################# Version control ####################################
### Software ###
# R 4.1.2
# RStudio 2022.07.0
# Windows 11 21H2

### Packages ###
# broom.mixed 0.2.9.4
# dplyr 1.0.9
# forcats 0.5.1
# here 1.0.1
# lme4 1.1.30
# lmerTest 3.1.3
# R.oo 1.25.0
# tictoc 1.0.1
# tidyr 1.2.0
# tidyverse 1.3.1
############################################# Code Setup ####################################
# Loading packages
if(!require(here)) install.packages("here",
                                    repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(lme4)) install.packages("lme4",
                                    repos = "http://cran.us.r-project.org")
if(!require(lmerTest)) install.packages("lmerTest",
                                        repos = "http://cran.us.r-project.org")
if(!require(tictoc)) install.packages("tictoc",
                                      repos = "http://cran.us.r-project.org")
if(!require(R.oo)) install.packages("R.oo",
                                    repos = "http://cran.us.r-project.org")
if(!require(broom.mixed)) install.packages("broom.mixed",
                                           repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork",
                                         repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis",
                                         repos = "http://cran.us.r-project.org")
#library(forcats) core tidyverse
#require(dplyr) core tidyverse
#require(tidyr) core tidyverse
#require(ggplot2) core tidyverse


# Importing a list of referees imported from player history; 
# see Stafford et al. (2014) at https://osf.io/gvm2z/
redcardCont <- read.csv(here('data_raw', 'CrowdstormingDataJuly1st.csv'),
                        stringsAsFactors = FALSE)

# Import the 'contaminated' referee numbers
contRefs <- read.csv(here('data_supl', 'refDecontaminate.csv')) %>% select(refNum)

# Exclude the referees
redcard <- redcardCont[!(redcardCont$refNum %in% contRefs$refNum), ]

# Cleaning up
rm(redcardCont, contRefs)

############################################# Data Cleaning ####################################

# Removing NA values
redcard <- na.omit(redcard)

# Attaching an ID column to track cases
redcard$rownumber <- 1:nrow(redcard)

# Getting an average skin tone rating from 2 raters
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

# Aggregated dyads produce illogical values for redCards,
# collapsing data from dichotomous variable (DV)
redcard["redCards"][redcard["redCards"] == 2] <- 1
summary(as.factor(redcard$redCards))

# refCountry needs to be recoded as a factor as well
redcard$refCountry <- as.factor(redcard$refCountry)

# Calculating age of individual players
# Setting an arbitrary date within the season
redcard$birthday <- as.Date(redcard$birthday, '%d.%m.%Y')
season_date <- as.Date('2013-01-01')
redcard$age <- as.numeric((season_date-redcard$birthday)/365)
rm(season_date)

# Collapse position into 4 categories -  Goalkeeper, Back, Middle, Front
# to reduce thu N of dummy-coded predictors
redcard$position <- 
  fct_recode(redcard$position, 
             "Back" = "Left Fullback",
             "Back" = "Right Fullback",
             "Back" = "Center Fullback",
             "Back" = "Center Back",
             "Middle" = "Left Midfielder",
             "Middle" = "Center Midfielder",
             "Middle" =  "Right Midfielder",
             "Middle" = "Attacking Midfielder",
             "Middle" = "Defensive Midfielder",
             "Front" = "Left Winger",
             "Front" = "Right Winger",
             "Front" = "Center Forward",
             "Goalkeeper" = "Goalkeeper")

redcard$position <- as.factor(redcard$position)
redcard$refCountry <- as.factor(redcard$refCountry)

######################################### Covariate Multiverse Setup ################################

# Create list of potential covariates
covariates_list <- list(avrate = c(NA, 'avrate'),
                        position = c(NA, 'position'),
                        yellowCards = c(NA, 'yellowCards'),
                        height = c(NA, 'height'),
                        weight = c(NA, 'weight'),
                        club = c(NA, 'club'),
                        age = c(NA, 'age'),
                        meanIAT = c(NA, 'meanIAT'),
                        refCountry = c(NA, 'refCountry'),
                        victories = c(NA, 'victories'))

# Creating a list of all possible combinations;
# Making a grid combining the NA and other values.
covariates_list <- expand.grid(covariates_list) 

# Remove an empty row (no covariates)
covariates_list <- covariates_list[-1,]

# Re-indexing covariates_list after removing first row
row.names(covariates_list) <- 1:nrow(covariates_list)

# Creating new grouping variables
covariates_list$rownumber <- row.names(covariates_list)

# Saving covariates_list for visualisations
saveRDS(covariate_grid, file = here('data_output', 'covariates_list.RDs'))

# Moving all covariates to a single column, each separated by a '+' sign
covariate_grid <- covariates_list %>%
  tidyr::unite(formula, avrate:victories, sep = '+', na.rm = TRUE)

# Saving covariate_grid for visualisations
saveRDS(covariate_grid, file = here('data_output', 'covariate_grid.RDs'))

######################################### Main Multiverse Loop ##########################################

# Defining a new variable 'output' as a list to store multiple outputs
output <- list()

# Defining a new variables - NA for now as they will be filled once the loop is run
R2conditional <- NA
R2marginal <- NA
predictorR2 <- NA
predictorPval <- NA
or.avrate <- NA
or.lci <- NA
or.uci <- NA

# Starting the loop
for(i in 1:nrow(covariate_grid)) {
  # Printing [i] just to track progress of analysis
  print(i)
  
  skip_to_next <- FALSE
  
  # Monitoring how long full loop is taking
  tic("Total time")
  
  # Monitoring how long regression is taking
  tic("Regression")
  
  # Each row of covariate_grid is now used as a formula for the regression
  output <- tryCatch(glmer(data = redcard,
                           formula = paste('redCards ~ ',
                                           covariate_grid[i, 'formula'], 
                                           '+ (1 | playerShort) + (1 | refNum)'),
                           family = binomial(link="logit"),
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 0),
                     
                     error = function(e) {
                       skip_to_next <<- TRUE
                     })
  
  if(skip_to_next) { next }
  
  toc()
  
  # Monitoring how long data extraction is taking
  tic("Data extraction")
  
  # Getting an overall model fit for each row of covariate_grid
  R2conditional[i] <- modelsummary::get_gof(output)$r2.conditional
  
  # Getting marginal R2 for each row
  R2marginal[i] <- modelsummary::get_gof(output)$r2.marginal
  
  # Getting individual predictor R2 for each row of covariate_grid
  predictorR2[i] <- as.data.frame(summary(output)$coefficients[,1])
  
  # Getting p values for individual predictors
  predictorPval[i] <- as.data.frame(summary(output)$coefficients[,4])
  
  # Getting odds ratios for avrate
  or.avrate[i] <- as.numeric(tidy(output,conf.int = TRUE, exponentiate = TRUE,
                                  effects = "fixed")[2,3])
  
  # Getting OR lower confidence interval
  or.lci[i] <- as.numeric(tidy(output,conf.int = TRUE, exponentiate = TRUE, 
                               effects = "fixed")[2,7])
  
  # Getting OR upper confidence interval
  or.uci[i] <- as.numeric(tidy(output,conf.int = TRUE, exponentiate = TRUE, 
                               effects = "fixed")[2,8])
  
  toc()
  toc()
}

############################################ Creating Output Dataframe ##########################################

# Turning the list of predictor R2 and P values into a data frame;
# Finding the length of each element of predictor_R2 list
len <- sapply(predictorR2, length)
len2 <- sapply(predictorPval, length)

# The longest length dictates number of rows in data frame
n <- max(len)
n2 <- max(len2)

# Finding the number of NAs required for each row to be of same length to longest
len <- n - len
len2 <- n2 - len2

# Constructing a data frame with predictor R2 and p values, respectively
predR2_df <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), 
                                 predictorR2, len)))
predPval_df <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), 
                                   predictorPval, len2)))
# Cleaning up
rm(len, len2, n, n2)

# Selecting relevant columns only
predR2_df<- subset(predR2_df, select = X2)
predPval_df <- subset(predPval_df, select = X2)

# Adding variable to merge data frames by
predR2_df$rownumber <- row.names(predR2_df)
predPval_df$rownumber <- row.names(predPval_df)

# Renaming column names for interpretability
names(predR2_df)[1]<-paste("R2_first_covariate")
names(predPval_df)[1]<-paste("Pval_first_covariate")

# Turning conditional R2 values into data frame

# Padding R2 and OR values with NA values to avoid errors in the code 
# below if the whole MVA is not run
length(R2conditional) <- nrow(covariate_grid)
length(R2marginal) <- nrow(covariate_grid)
length(or.avrate) <- nrow(covariate_grid)
length(or.lci) <- nrow(covariate_grid)
length(or.uci) <- nrow(covariate_grid)

output_table <- data.frame(covariates = covariate_grid,
                           R2c = R2conditional,
                           R2m = R2marginal,
                           Avrate_OR = or.avrate,
                           Avrate_LCI = or.lci,
                           Avrate_UCI = or.uci)

output_table$rownumber <- row.names(output_table)

# Combining R2 and p value data frames into a single data frame
output_table <- merge(output_table, predR2_df, by = "rownumber", all = TRUE)
output_table <- merge(output_table, predPval_df, by = "rownumber", all = TRUE)

# Setting row numbers as numeric so the data frame can be sorted by the column
output_table$rownumber <- as.numeric(output_table$rownumber)

# Ordering and re-indexing data frame so row names and row numbers align
outtable <- output_table[order(output_table$rownumber),]
row.names(outtable) <- 1:nrow(outtable)

# Saving outtable for for visualisations
saveRDS(outtable, file = here('data_output', 'outtable.RDs'))

# The end of the analysis

############################################# Endnote ####################################
# Visualisation code available at
# https://github.com/yorkshireorange/PSY6009/tree/main/code_main/02multiversePlots.R
########################################################################################### 