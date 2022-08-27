############################################# Version control ############################################# 

### Software ###
# R 4.1.2
# RStudio 2022.07.0
# Windows 11 21H2

### Packages ###
# dplyr 1.0.9
# ggplot2 3.3.6
# here 1.0.1
# jtools 2.2.0
# tidyr 1.2.0
# tidyverse 1.3.1
# viridis 0.6.2

############################################# Code Setup ############################################# 

# Loading packages
if(!require(here)) install.packages("here",
                                    repos = "http://cran.us.r-project.org")
if(!require(jtools)) install.packages("jtools",
                                    repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork",
                                         repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis",
                                       repos = "http://cran.us.r-project.org")
#library(forcats) core tidyverse
#require(dplyr) core tidyverse
#require(tidyr) core tidyverse
#require(ggplot2) core tidyverse

# Load the data needed for visualisations
outtable <- readRDS(here('data_output', 'outtable.RDs'))
covariates_list <- readRDS(here('data_output', 'covariates_list.RDs'))
covariate_grid <- readRDS(here('data_output', 'covariate_grid.RDs'))

############################################# Visualisation ############################################# 

# All plots are accompanied by a .csv file (~/plot_file/) sharing their name 
# so that viewers can identify models of interest from the visualisation.

################################## Plot 1A
### Overall multiverse model performance (according to R2m)

# Counting the number of covariates within a model for the plot
outtable$'Model Covariates' <- as.factor(10 - (rowSums(is.na(covariates_list))))

# Ordering by by R2m and assigning an arbitrary identifier variable
model_perf_data <- outtable[order(outtable[,5]),]
model_perf_data$n <- 1:nrow(outtable)

model_perf_plot <- ggplot(data = model_perf_data, 
                          aes(x = n, y = R2m, colour = `Model Covariates`)) +
  geom_point() +
  scale_colour_viridis_d(option = "H", 
                         breaks = c(1:10),
                         labels = c('1 Covariate','2','3','4','5',
                                    '6','7','8','9','10 Covariates')) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  theme(legend.position = c(0.9,0.21)) +
  theme_bw() + 
  labs(y = 'Marginal R2', x = 'Specification number')

model_perf_plot

# Save plot
ggsave(here("plot_save", "model_perf_plot.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)

################################## Plot 1B
### Overall model performance without categorical variables
### (club, refCountry; performance according to R2m)

# Subsetting the covariates list to remove categorical predictors 
# (club, refCountry)
model_perf_noncat_subset <- subset(covariates_list, (is.na(covariates_list[,c(6)])))
model_perf_noncat_subset <- subset(model_perf_noncat_subset, 
                                   (is.na(model_perf_noncat_subset[,c(9)])))

model_perf_noncat_data <- subset(outtable, 
                                 rownumber %in% model_perf_noncat_subset$rownumber)

# Sort by R2m effect size
model_perf_noncat_data <- model_perf_noncat_data[order(model_perf_noncat_data$R2m),]
# Assigning arbitrary numbering variable
model_perf_noncat_data$n <- 1:nrow(model_perf_noncat_data)

# Initialising the plot
model_perf_noncat_plot <- ggplot(data = model_perf_noncat_data, 
                                 aes(x = n, y = R2m, colour = `Model Covariates`)) +
  geom_point() +
  scale_colour_viridis_d(option = "H", 
                         breaks = c(1:8),
                         labels = c('1 Covariate','2','3','4','5',
                                    '6','7','8 Covariates')) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  theme(legend.position = c(0.9,0.21)) +
  theme_bw() + 
  labs(y = 'Marginal R2', x = 'Specification number')

model_perf_noncat_plot

# Save plot
ggsave(here("plot_save", "model_perf_noncat_plot.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)


################################## End of Plots 1A & 1B


################################## Plot 2A 
### Avrate-only model performance (according to R2m)

# Subsetting the covariates list to only view models with avrate
avrate_subset<-subset(covariates_list, (!is.na(covariates_list[,1])))
avrate_perf_data <- subset(model_perf_data, rownumber %in% avrate_subset$rownumber)

# Ordering by by R2m and assigning an arbitrary identifier variable
avrate_perf_data <- avrate_perf_data[order(avrate_perf_data[,5]),]
avrate_perf_data$avrate.n <- 1:nrow(avrate_perf_data)

# Avrate model performance
avrate_perf_plot <- ggplot(data = avrate_perf_data, 
                           aes(x = avrate.n, y = R2m, colour = `Model Covariates`)) +
  geom_point() +
  scale_colour_viridis_d(option = "H", 
                         breaks = c(1:10),
                         labels = c('1 Covariate','2','3','4','5',
                                    '6','7','8','9','10 Covariates')) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  theme(legend.position = c(0.9,0.21)) +
  theme_bw() + 
  labs(y = 'Marginal R2', x = 'Specification number')

avrate_perf_plot

# Save plot
ggsave(here("plot_save", "avrate_perf_plot.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)


################################## Plot 2B
### Avrate-only model performance without categorical variables
### (club, refCountry; according to R2m)

# Subsetting the covariates list to remove categorical predictors 
# (club, refCountry)
avrate_noncat_subset <- subset(covariates_list, (is.na(covariates_list[,c(6)])))
avrate_noncat_subset <- subset(avrate_noncat_subset, 
                               (is.na(avrate_noncat_subset[,c(9)])))

avrate_noncat_data <- subset(outtable, 
                             rownumber %in% avrate_noncat_subset$rownumber)
rm(avrate_noncat_subset)

# Subsetting the data frame to remove the categorical predictors
# (club, refCountry) from the avrate data frame
avrate_noncat_data <- subset(avrate_perf_data, 
                             rownumber %in% avrate_noncat_data$rownumber)

# Assigning arbitrary numbering variable
avrate_noncat_data$n <- 1:nrow(avrate_noncat_data)

# Initialising the plot
avrate_noncat_plot <- ggplot(data = avrate_noncat_data, 
                             aes(x = n, y = R2m, colour = `Model Covariates`)) +
  geom_point() +
  scale_colour_viridis_d(option = "H", 
                         breaks = c(1:8),
                         labels = c('1 Covariate','2','3','4','5',
                                    '6','7','8 Covariates')) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  theme(legend.position = c(0.9,0.21)) +
  theme_bw() + 
  labs(y = 'Marginal R2', x = 'Specification number')

avrate_noncat_plot

# Save plot
ggsave(here("plot_save", "avrate_noncat_plot.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)

################################## End of Plots 2A & 2B


################################## Plot 3A
### Avrate model odds ratios and significance

# Selecting relevant rows (OR, covariate formula, LCI, UCI, p-value)
avrate_ordata <- avrate_perf_data[c(6,2,7,8,10)]

# Introducing data cut-off based on avrate significance
avrate_ordata <- avrate_ordata %>% mutate(
  signif = case_when(Pval_first_covariate >= 0.05 ~ 'p >= .05',
                     Pval_first_covariate < 0.05 ~ 'p < .05'))

# Ordering data based on OR from smallest to larget
avrate_ordata <- avrate_ordata[order(avrate_ordata[,1]),]
avrate_ordata$avrate.or.n <- 1:nrow(avrate_ordata)

# Constructing the plot
avrate_orplot <- ggplot(data = avrate_ordata, 
                        aes(x = avrate.or.n, y = Avrate_OR, col = signif)) +
  ylim(0.75, 1.75) +
  geom_errorbar(aes(ymin = Avrate_LCI, ymax = Avrate_UCI),
                alpha = .2,
                show.legend = FALSE) +
  geom_point(show.legend = TRUE) +
  geom_hline(yintercept = 1.00, linetype = 'dashed', colour = 'black') +
  scale_colour_manual(name = "Legend",
                      values = c("p >= .05"="red", 
                                 "p < .05" = "blue")) +
  labs(x = 'Specification number',
       y = 'Odds ratio: average skin tone') +
# The odds of dark-skinned players receiving a red card compared to 
# light-skinned players. Lines represent 95% confidence intervals
  theme_bw()

avrate_orplot

# Save plot
ggsave(here("plot_save", "avrate_orplot.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)

################################## Plot 3B
### Avrate model odds ratios and significance (non-categorical predictors)

# Remove player club
ordata_cont_subset <- subset(covariates_list, (is.na(covariates_list[,c(6)])))
# Remove referee country
ordata_cont_subset <- subset(ordata_cont_subset, (is.na(ordata_cont_subset[,c(9)])))
# Include all specifications with avrate 
ordata_cont_subset <- subset(ordata_cont_subset,(!is.na(ordata_cont_subset[,c(1)])))
# Match output with the new data frame
avrate_ordata_cont <- subset(outtable, rownumber %in% ordata_cont_subset$rownumber)
# Order by smallest OR
avrate_ordata_cont <- avrate_ordata_cont[order(avrate_ordata_cont[,6]),]
# Arbitrary numbering for visualisation
avrate_ordata_cont$n <- 1:nrow(avrate_ordata_cont)

# Significance of avrate added
avrate_ordata_cont <- avrate_ordata_cont %>% mutate(
  signif_cont = case_when(Pval_first_covariate >= 0.05 ~ 'p >= .05',
                     Pval_first_covariate < 0.05 ~ 'p < .05'))

# Create plot
avrate_orplot_cont <- ggplot(data = avrate_ordata_cont, 
                             aes(x = n, y = Avrate_OR, col = signif_cont)) +
  ylim(0.95, 1.75) +
  geom_hline(yintercept = 1.00, linetype = 'dashed', colour = 'black') + 
  geom_errorbar(aes(ymin = Avrate_LCI, ymax = Avrate_UCI)) +
  geom_point(show.legend = TRUE) +
  scale_colour_manual(name = "Legend",
                      values = c("p >= .05"="red", 
                                 "p < .05" = "blue")) +
  labs(x = 'Specification number',
       y = 'Odds ratio: average skin tone') +
  # The odds of dark-skinned players receiving a red card compared to 
  # light-skinned players. Lines represent 95% confidence intervals
  theme_bw()

avrate_orplot_cont

# Save plot
ggsave(here("plot_save", "avrate_orplot_cont.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)

################################## End of OR plots 3A & 3B


################################## Plot 4A
### Average variance (R2m) explained per model covariate

# Subsetting outtable by matching rownames between covariates_list subsets and outtable

# avrate
avrate_set<-subset(covariates_list, (!is.na(covariates_list[,1])))
output_avrate <- subset(outtable, rownumber %in% avrate_set$rownumber)

# position
position_set<-subset(covariates_list, (!is.na(covariates_list[,2])))
output_position <- subset(outtable, rownumber %in% position_set$rownumber)

# yellowCards
yellowCards_set<-subset(covariates_list, (!is.na(covariates_list[,3])))
output_yellowCards <- subset(outtable, rownumber %in% yellowCards_set$rownumber)

# height
height_set<-subset(covariates_list, (!is.na(covariates_list[,4])))
output_height <- subset(outtable, rownumber %in% height_set$rownumber)

# weight
weight_set<-subset(covariates_list, (!is.na(covariates_list[,5])))
output_weight <- subset(outtable, rownumber %in% height_set$rownumber)

# club
club_set<-subset(covariates_list, (!is.na(covariates_list[,6])))
output_club <- subset(outtable, rownumber %in% club_set$rownumber)

# age
age_set<-subset(covariates_list, (!is.na(covariates_list[,7])))
output_age <- subset(outtable, rownumber %in% age_set$rownumber)

# meanIAT
meanIAT_set<-subset(covariates_list, (!is.na(covariates_list[,8])))
output_meanIAT <- subset(outtable, rownumber %in% meanIAT_set$rownumber)

# refCountry
refCountry_set<-subset(covariates_list, (!is.na(covariates_list[,9])))
output_refCountry <- subset(outtable, rownumber %in% refCountry_set$rownumber)

# victories
victories_set<-subset(covariates_list, (!is.na(covariates_list[,10])))
output_victories <- subset(outtable, rownumber %in% victories_set$rownumber)

# Create one dataframe of the filtered values to build the plot from
avg_perf_data <- cbind(output_avrate$R2m, output_position$R2m, output_yellowCards$R2m, output_height$R2m,
                       output_weight$R2m, output_club$R2m, output_age$R2m,
                       output_meanIAT$R2m, output_refCountry$R2m,
                       output_victories$R2m)

avg_perf_data <- as.data.frame(avg_perf_data)

colnames(avg_perf_data)[c(1,2,3,4,5,6,7,8,9,10)] <- c('Skin_Tone','Position',
                                                      'Yellow_Cards',
                                                      'Height','Weight','Club',
                                                      'Age','Mean_IAT',
                                                      'Ref_Country','Victories')

# Adding 'nice names'
cov_labels <- c("Players' age","Players' club", "Players' height",
                "Referees' mean IAT scores","Players' position",
                "Referees' country", "Players' skin tone","Players' victories",
                "Players' weight", "Yellow Cards Received")

# Transforming data to pass through ggplot
avg_perf_data <- avg_perf_data %>%
  pivot_longer(everything())

# Initiate the plot
avg_perf_plot <- ggplot(data=avg_perf_data, mapping = aes(x = name, y = value, fill = name),
                        show.legend = FALSE) + 
  geom_violin(scale = 'area', 
              width = 1.3,
              adjust = 0.5,
              bw = 0.0075,
              show.legend = FALSE) +
  scale_x_discrete(labels = cov_labels) + 
  labs(y = 'Average overall Marginal R2',
       x = 'Covariate') +
  theme_bw()

avg_perf_plot

# Save plot
ggsave(here("plot_save", "avg_perf_plot.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)

avg_perf_plot2 <- ggplot(data=avg_perf_data, 
                         mapping = aes(x = name, y = value, colour = name)) +
  geom_dotplot(binaxis = "y", 
               binpositions = 'bygroup', 
               stackdir ="center",
               stackratio = 0.1,
               dotsize = 0.2,
               binwidth = 1/100,
               show.legend = FALSE) +
  scale_x_discrete(labels = cov_labels) + 
  labs(y = 'Average overall Marginal R2',
       x = 'Covariate') + 
  theme_bw()

avg_perf_plot2

# Save plot
ggsave(here("plot_save", "avg_perf_plot2.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)

################################## Plot 4B
### Average variance (R2m) explained per model covariate
## without categorical predictors

# Subsetting categorical variable columns to be removed
cat_rows <- as.data.frame(cbind(refCountry_set$rownumber, club_set$rownumber))
cat_rows <- stack(cat_rows)

# Subsetting outputs for rows that are not shared with analyses
# including refCountry or club
output_avrate_nocat <- as.data.frame(subset(output_avrate,
                                            !(rownumber %in% cat_rows$value)))
output_position_nocat <- as.data.frame(subset(output_position,
                                              !(rownumber %in% cat_rows$value)))
output_yellowCards_nocat <- as.data.frame(subset(output_yellowCards,
                                                 !(rownumber %in% cat_rows$value)))
output_height_nocat <- as.data.frame(subset(output_height,
                                            !(rownumber %in% cat_rows$value)))
output_age_nocat <- as.data.frame(subset(output_age,
                                         !(rownumber %in% cat_rows$value)))
output_weight_nocat <- as.data.frame(subset(output_weight,
                                            !(rownumber %in% cat_rows$value)))
output_meanIAT_nocat <- as.data.frame(subset(output_meanIAT,
                                             !(rownumber %in% cat_rows$value)))
output_victories_nocat <- as.data.frame(subset(output_victories,
                                               !(rownumber %in% cat_rows$value)))

# Build a data frame from the subset
avg_perf_data_noncat <- as.data.frame(cbind(output_avrate_nocat$R2m,
                                            output_position_nocat$R2m, 
                                            output_yellowCards_nocat$R2m,
                                            output_height_nocat$R2m,
                                            output_weight_nocat$R2m,
                                            output_age_nocat$R2m,
                                            output_meanIAT_nocat$R2m,
                                            output_victories_nocat$R2m))

rm(output_age_nocat, output_avrate_nocat)

colnames(avg_perf_data_noncat)[c(1,2,3,4,5,6,7,8)] <- c('Skin_Tone','Position',
                                                        'Yellow_Cards','Height',
                                                        'Weight','Age','Mean_IAT',
                                                        'Victories')

# Adding 'nice names'
cov_labels_noncat <- c("Players' age", "Players' height",
                       "Referees' mean IAT scores","Players' position",
                       "Players' skin tone","Players' victories",
                       "Players' weight", "Yellow Cards Received")

# Transforming data to pass through ggplot
avg_perf_data_noncat <- avg_perf_data_noncat %>%
  pivot_longer(everything())

# Initiate the plot
avg_perf_plot_noncat <- ggplot(data=avg_perf_data_noncat, mapping = aes(x = name, y = value),
                               show.legend = FALSE) + 
  geom_violin(scale = 'area', 
              width = 1.3,
              adjust = 0.5,
              bw = 0.0075,
              show.legend = FALSE) +
  scale_x_discrete(labels = cov_labels_noncat) +
  geom_boxplot(width = 0.2, position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = 'Average overall Marginal R2',
       x = 'Covariate') + 
  theme_apa()

avg_perf_plot_noncat

# Save plot
ggsave(here("plot_save", "avg_perf_plot_noncat.pdf"), scale = 2, width = 1920,
       height = 1080, units ="px", dpi = "retina", bg = NULL)

################################## End of Plot 4A (v1 & v2) and 4B

# The end of visualisation

############################################# Endnote ############################################# 
# Code used to generate source data available at
# https://github.com/yorkshireorange/PSY6009/tree/main/code_main/01redcardMultiverse.R
########################################################################################## 