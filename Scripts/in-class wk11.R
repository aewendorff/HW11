#################
###HOMEWORK 11###
#################

#Authors: Aubrey Wendorff + Data from Cody Scott Quiroz
#Packages: readxl, tidyverse

library(readxl)
library(tidyverse)

# Objective 1 -------------------------------------------------------------


###PART A###----
#Split into 2-factor temperature variable (High and Low, 50 obsv. each)
#Read in dataset
gauge <- read.csv("Data/Gauge_EC_Data.xlsx.csv")

#Set Parameters
set.seed(123) #reproducibility of random generation
n <- 50 #sample size of 50 per factor

#Random uniform predictors
x_low <- runif(n, 0, 10) 
x_high <- runif(n, 0, 10)

#Set up slope/intercept so there is no interaction (same slope, dif int)
intercept_low <- 3
slope_low <- 0.5

intercept_high <- 5
slope_high <- 0.5

#Add random normal error
error_low <-  rnorm(n, 0, 1)
error_high <- rnorm(n, 0, 1)

#Calculate new y
y_low <- intercept_low + slope_low * x_low + error_low
y_high <- intercept_high + slope_high * x_high + error_high

#Make a new dataframe
sim_data <- data.frame(y = c(y_low, y_high),
                       x = c(x_low, x_high),
                       temperature = factor(rep(c("Low", "High"), each = n)))

#Create ANCOVA Model
model_temp <- lm(y ~ x * temperature, data = sim_data)
summary(model_temp)

#Rename columns of df
sim_data <- sim_data %>%
  rename(Cond_uscm = x,
         Temperature_C = temperature,
         Cl_mgL = y)

#Save df as csv
write.csv(sim_data, "Outputs/simulated_data.csv")

#Basic plot to check interaction outcome was desired
ggplot(sim_data, aes(x = Cond_uscm, 
                     y = Cl_mgL,
                     color = Temperature_C)) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_bw()


###PART B###---
#Conductivity (Cond, µS/cm), is often used as a proxy to predict chloride concentrations (Cl, mg/L) in freshwater ecosystems. 
#However, this relationship may vary based on in situ water temperature (Low or High), as this factor heavily impacts ion mobility, with higher temperatures increasing ion mobility. 
#Therefore, I explored the question: Does the relationship between Cond and Cl differ in low and high temperature conditions?



# Objective 2 -------------------------------------------------------------
###PART A###---
#Does the relationship between DOC and Methane differ in free-flowing and impounded reaches of a stream?
partner_data <- read.csv("Data/simulated_data_csq.csv")


###PART B###---
#Plot -slopes and intercepts are different
ggplot(partner_data, aes(x = DOC_mgL, 
                         y = CH4_flux_umol,
                         color = ReachType)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

#Interaction model - This is SIGNIFICANT, so other models are not needed
full_model <- lm(CH4_flux_umol ~ DOC_mgL * ReachType, data = partner_data)
summary(full_model)

#No interaction - not necessary
mid_model <- lm(CH4_flux_umol ~ DOC_mgL + ReachType, data = partner_data)
summary(mid_model)

#Linear model - not necessary
lm_model <- lm(CH4_flux_umol ~ DOC_mgL, data = partner_data)
summary(lm_model)


###PART C###---
    #The ANCOVA model shows that DOC and ReachType (impounded or free-flowing) are both influencing methane flux
    #The interaction term is significant, so the DOC and CH4 relationship differs between these two types of stream reaches.
    #Impounded streams have a steeper slope than free-flowing streams, meaning DOC influences CH4 flux more strongly in impounded streams.


###PART D###---
    #From Model:
        #Free-flowing: intercept = 19.44, slope = 3.06
        #Impounded (interaction value + baseline value): intercept = 30.62, slope = 4.47
    #Actual:
        ##Free-flowing: intercept = 20, slope = 3
        #Impounded: intercept = 30, slope = 4.5
    #The true values and the values from my ANCOVA model are almost identical to one another.
