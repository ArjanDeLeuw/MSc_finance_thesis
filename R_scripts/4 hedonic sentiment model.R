
################################################################################
############################ STEP 0: Preparations ##############################
################################################################################
# in 0 merging indices.R the indices for Equation 1 are imported and merged 
#   based on date. 
# in 1 explore and mutate indices the dataset ALL.indices is prepared for 
#   the AR and VAR model: seasonality, logs, differencing, outliers,
#   and descriptives are produced: table 1, figure 1, and plots/correlation 
#   matrices for each index that are not presented in the thesis. 
# in 2 estimate models 1 to 4.R models one to four are estimated and exported 
#   (equation 1, table 4). 3 models with 3, 6, and 12 lags are estimated (not 
#   represented in the thesis). the VAR model is estimated and figure 2, 
#   figure 3, Appendix A.1, Appendix D are produced
# in 3 hedonic model.R the transaction data is explored and mutated and the 
#   hedonic models without sentiment are estimated (equation 3). Table 2/5 and  
#   Appendix A.2/C.1 are produced.
# in 4 hedonic sentiment model.R the transaction data is merged the indices 
#   based on transaction month t.  As a result, we will have a file with the 
#   level of non-fundamental sentiment for each transaction i in month t. Ten 
#   deciles are created based on non-fundamental sentiment and the corresponding 
#   Figures 4, 5, 6 and Appendix B are produced. In addition, hedonic sentiment
#   models are estimated to produce table 5, 6, and 7 and Appendix C.2. 
#   (equation 4 and 5)


### packages
# install.packages("BurStMisc")
# install.packages("MuMIn")
# install.packages("ggpubr")
# install.packages("rstatix")
# install.packages("gridExtra")
# install.packages("sjPlot")
# install.packages("sjmisc")
# install.packages("glmmTMB")


### libraries
library(tidyverse)
library(plyr)
library(stargazer)
library(dplyr)
library(plm)
library(MuMIn)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(glmmTMB)
library(sjPlot)
library(sjmisc)



### prepare environment
cat("\014") # clear console
if(!is.null(dev.list())) dev.off() # clear plots
rm(list=ls()[-match("WF.BWD012.df", ls())])
setwd("V:/GVA/MA_Maatwerk en Advies/Publicaties en Presentaties/2021/2021 - Stage Arjan de Leuw/R_project_sentiment")
gc()


################################################################################
####################### STEP 1: IMPORT AND MERGE DATA ##########################
################################################################################
### import data
WF.BWD012.df <- WF.BWD012.df
str(WF.BWD012.df)

indices.df <- read.csv("subset_all_indices.csv",  
                       header = T, 
                       sep = ",",
                       stringsAsFactors = FALSE)


### subset indices.df
# keep period, sentiment index, and non-fundamental sentiment (residuals)
indices.df <- indices.df[, c(2:3, 63:67)]

# rename
names(indices.df) [1] <- c("transaction_month")

# convert to dates to date format and characters to numeric
indices.df$transaction_month <- as.Date(indices.df$transaction_month)

indices.df[sapply(indices.df, is.character)] <- 
  data.frame(lapply(indices.df[sapply(indices.df, is.character)], as.numeric))

str(indices.df)


### merge
# explore the variables used to merge
str(WF.BWD012.df)
summary(WF.BWD012.df$transaction_month)
WF.BWD012.df$transaction_month

str(indices.df)
summary(indices.df$transaction_month)
indices.df$transaction_month

# merge using join_all 
WF.BWD012.df <- plyr::join_all(list(WF.BWD012.df,
                                    indices.df),
                               by = "transaction_month",
                               type = "inner",
                               match = "all")

rm(indices.df)

# verify manually 
tail(WF.BWD012.df[,c("transaction_month", "residuals_baseline_model")])


################################################################################
########### STEP 2: PORTFOLIO'S BASED ON NON-FUNDAMENTAL SENTIMENT #############
################################################################################
attach(WF.BWD012.df)


### 10 deciles based on non fundamental sentiment (NFM)
# create 10 deciles
WF.BWD012.df$deciles_ten <- dplyr::ntile(WF.BWD012.df$residuals_baseline_model, 10)

# plot 
#plot(WF.BWD012.df$deciles_a, WF.BWD012.df$residuals_baseline_model, 
#     type = "p",
#     data = WF.BWD012.df)


### statistics per decile
# volume per municipality per month  
deciles.volume <- WF.BWD012.df %>%
  dplyr::group_by(transaction_month) %>%
  tally()

names(deciles.volume) <- c("transaction_month", "volume_per_month_mun")  

WF.BWD012.df <- plyr::join_all(list(WF.BWD012.df,
                                    deciles.volume),
                               by = c("transaction_month"),
                               type = "inner",
                               match = "all")

rm(deciles.volume)


### statistics per decile for all observations
deciles.NFM <- WF.BWD012.df %>%
  dplyr::filter(G4_indicator == 1) %>%
  dplyr::group_by(deciles_ten) %>%
  dplyr::summarize(mean(residuals_baseline_model, na.rm = TRUE), 
                   min(residuals_baseline_model, na.rm = TRUE),
                   max(residuals_baseline_model, na.rm = TRUE),
                   
                   mean(transaction_price, na.rm = TRUE),
                   sd(transaction_price, na.rm = TRUE), 
                   mean(transaction_price_per_sqm, na.rm = TRUE),
                   mean(delta_real_predicted_tp_lti, na.rm = TRUE),
                   
                   mean(tp_ranked_monthly, na.rm = TRUE),
                   mean(volume_per_month_mun, na.rm = TRUE),
                   
                   mean(indicator_first_time_buyer, na.rm = TRUE),
                   mean(investor_seller, na.rm = TRUE),
                   mean(private_investor_seller, na.rm = TRUE),
                   mean(non_private_investor_seller, na.rm = TRUE),
                   mean(owner_occupant_seller, na.rm = TRUE),
                   mean(second_house_seller, na.rm = TRUE),
                   mean(private_investor_buyer, na.rm = TRUE),
                   mean(second_house_buyer, na.rm = TRUE),
                   mean(owner_occupant_buyer, na.rm = TRUE),
                   mean(number_of_houses_buyer, na.rm = TRUE),
                   mean(year_of_construction, na.rm = TRUE),
                   
                   mean(property_size, na.rm = TRUE),
                   mean(parcel_size, na.rm = TRUE,)) %>%
  dplyr::ungroup() %>%
  t() %>%
  data.frame() 

names(deciles.NFM) = deciles.NFM[1,]
deciles.NFM <- deciles.NFM[2:22,]


### produce output
stargazer(deciles.NFM, 
          type = "latex", 
          summary = FALSE,
          digits = 3)
                               
write.csv2(deciles.NFM, "deciles_NFM.csv")

### t-test between each decile
# the t.test function and dplyr don't play together nicely.
# manually test each decile (2 vs 1, ..., 10 vs 9):
WF.BWD012.df %>%
  dplyr::select(residuals_baseline_model, deciles_ten) %>%
  dplyr::filter(deciles_ten == 7 | deciles_ten == 8) -> temp

t.test(residuals_baseline_model ~ deciles_ten, data = temp, var.equal = FALSE)


### one-way ANOVA
#to test for a signficance difference between groups  
summary(aov(residuals_baseline_model ~ deciles_ten, WF.BWD012.df))

# or: 
WF.BWD012.df %>%
  anova_test(residuals_baseline_model ~ deciles_ten)


################################################################################
############################# STEP 2A: deciles G4/G40 ##########################
################################################################################

### statistics per decile for G4
deciles.NFM.G4 <- WF.BWD012.df %>%
  dplyr::filter(G4_indicator == 1) %>%
  dplyr::group_by(deciles_ten) %>%
  dplyr::summarize(mean(residuals_baseline_model, na.rm = TRUE), 
                   min(residuals_baseline_model, na.rm = TRUE),
                   max(residuals_baseline_model, na.rm = TRUE),
                   
                   mean(transaction_price, na.rm = TRUE),
                   sd(transaction_price, na.rm = TRUE), 
                   mean(transaction_price_per_sqm, na.rm = TRUE),
                   mean(delta_real_predicted_tp_lti, na.rm = TRUE),
                   
                   mean(tp_ranked_monthly, na.rm = TRUE),
                   mean(volume_per_month_mun, na.rm = TRUE),
                   
                   mean(indicator_first_time_buyer, na.rm = TRUE),
                   mean(investor_seller, na.rm = TRUE),
                   mean(private_investor_seller, na.rm = TRUE),
                   mean(non_private_investor_seller, na.rm = TRUE),
                   mean(owner_occupant_seller, na.rm = TRUE),
                   mean(second_house_seller, na.rm = TRUE),
                   mean(private_investor_buyer, na.rm = TRUE),
                   mean(second_house_buyer, na.rm = TRUE),
                   mean(owner_occupant_buyer, na.rm = TRUE),
                   mean(number_of_houses_buyer, na.rm = TRUE),
                   mean(year_of_construction, na.rm = TRUE),
                   
                   mean(property_size, na.rm = TRUE),
                   mean(parcel_size, na.rm = TRUE,)) %>%
  dplyr::ungroup() %>%
  t() %>%
  data.frame() 

names(deciles.NFM.G4) = deciles.NFM.G4[1,]
deciles.NFM.G4 <- deciles.NFM.G4[2:22,]


### statistics per decile for G40
deciles.NFM.G40 <- WF.BWD012.df %>%
  dplyr::filter(G40_indicator == 1) %>%
  dplyr::group_by(deciles_ten) %>%
  dplyr::summarize(mean(residuals_baseline_model, na.rm = TRUE), 
                   min(residuals_baseline_model, na.rm = TRUE),
                   max(residuals_baseline_model, na.rm = TRUE),
                   
                   mean(transaction_price, na.rm = TRUE),
                   sd(transaction_price, na.rm = TRUE), 
                   mean(transaction_price_per_sqm, na.rm = TRUE),
                   mean(delta_real_predicted_tp_lti, na.rm = TRUE),
                   
                   mean(tp_ranked_monthly, na.rm = TRUE),
                   mean(volume_per_month_mun, na.rm = TRUE),
                   
                   mean(indicator_first_time_buyer, na.rm = TRUE),
                   mean(investor_seller, na.rm = TRUE),
                   mean(private_investor_seller, na.rm = TRUE),
                   mean(non_private_investor_seller, na.rm = TRUE),
                   mean(owner_occupant_seller, na.rm = TRUE),
                   mean(second_house_seller, na.rm = TRUE),
                   mean(private_investor_buyer, na.rm = TRUE),
                   mean(second_house_buyer, na.rm = TRUE),
                   mean(owner_occupant_buyer, na.rm = TRUE),
                   mean(number_of_houses_buyer, na.rm = TRUE),
                   mean(year_of_construction, na.rm = TRUE),
                   
                   mean(property_size, na.rm = TRUE),
                   mean(parcel_size, na.rm = TRUE,)) %>%
  dplyr::ungroup() %>%
  t() %>%
  data.frame() 

names(deciles.NFM.G40) = deciles.NFM.G40[1,]
deciles.NFM.G40 <- deciles.NFM.G40[2:22,]


################################################################################
################################ STEP 2B: plots ################################
################################################################################
attach(WF.BWD012.df)


##################  plot transaction prices and property size  #################
plot_tp <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) +
  geom_line(aes(y = transaction_price), 
            stat  = "summary", 
            fun   = "mean",
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = transaction_price), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = transaction_price), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) 
  labs(y = "Price in Euro's", 
       x = "Deciles",
       title ="Mean Transaction Price") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# -----------------------------------------------------------------------------
plot_tp_sqm <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y =  transaction_price_per_sqm), 
            stat  = "summary", 
            fun   = "mean",
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = transaction_price_per_sqm), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = transaction_price_per_sqm), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "Price in Euro's", 
       x = "Deciles",
       title ="Mean Transaction Price per Square Meter") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# -----------------------------------------------------------------------------
plot_tp_delta <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = delta_real_predicted_tp_lti), 
            stat  = "summary", 
            fun   = "mean", 
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = delta_real_predicted_tp_lti), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = delta_real_predicted_tp_lti), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "Price in Euro's", 
       x = "Deciles",
       title = "Mean Deviation Fundamental Value") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# -----------------------------------------------------------------------------
plot_size <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = property_size), 
            stat  = "summary", 
            fun   = "mean", 
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = property_size), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = property_size), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "Square Meter", 
       x = "Deciles",
       title ="Mean Property Size") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


Figure_three <- grid.arrange(plot_tp, 
                             plot_tp_sqm, 
                             plot_tp_delta, 
                             plot_size,
                             ncol = 3, nrow = 3)


###########################  plots seller indicators ###########################
plot_np_seller <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = non_private_investor_seller), 
            stat = "summary", 
            fun = "mean",
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = non_private_investor_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = non_private_investor_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title ="Mean % Non-Private Investor sellers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# -----------------------------------------------------------------------------
plot_pi_seller <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = private_investor_seller), 
            stat = "summary", 
            fun = "mean", 
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = private_investor_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = private_investor_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title ="Mean % Private Investor Sellers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# -----------------------------------------------------------------------------
plot_oo_seller <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = owner_occupant_seller), 
            stat = "summary", 
            fun = "mean", 
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = owner_occupant_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = owner_occupant_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title = "Mean % Owner-Occupant Sellers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# -----------------------------------------------------------------------------
plot_sh_seller <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = second_house_seller), 
            stat = "summary", 
            fun = "mean",
            color = "black") +
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = second_house_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = second_house_seller), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title ="Mean % Second House Sellers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


Figure_four <- grid.arrange(plot_np_seller, 
                            plot_pi_seller, 
                            plot_oo_seller,
                            plot_sh_seller,
                            ncol = 2, nrow = 2)


###########################  plots buyers indicators ###########################
plot_ft_buyer <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = indicator_first_time_buyer), 
            stat = "summary", 
            fun = "mean",
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = indicator_first_time_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = indicator_first_time_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title ="Mean % First-Time Buyers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# -----------------------------------------------------------------------------
plot_pi_buyer <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = private_investor_buyer), 
            stat = "summary", 
            fun = "mean",
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = private_investor_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = private_investor_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title ="Mean % Private Investor Buyers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# -----------------------------------------------------------------------------
plot_oo_buyer <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = owner_occupant_buyer), 
            stat = "summary", 
            fun = "mean",
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = owner_occupant_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = owner_occupant_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title = "Mean % Owner-Occupant Buyers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# -----------------------------------------------------------------------------
plot_sh_buyer <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(aes(y = second_house_buyer), 
            stat = "summary", 
            fun = "mean",
            color = "black") + 
  geom_line(data  = subset(WF.BWD012.df,G4_indicator %in% c("1")), 
            aes(y = second_house_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "blue") + 
  geom_line(data  = subset(WF.BWD012.df,G40_indicator %in% c("1")), 
            aes(y = second_house_buyer), 
            stat  = "summary", 
            fun   = "mean", 
            color = "green") + 
  
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  labs(y = "%", 
       x = "Deciles",
       title ="Mean % Second House Buyers") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



Figure_five <- grid.arrange(plot_ft_buyer, 
                            plot_pi_buyer, 
                            plot_oo_buyer, 
                            plot_sh_buyer, 
                            ncol = 2, nrow = 2)


############################### plots housetype  ##############################
plot_tp_housetype <- ggplot(WF.BWD012.df, aes(x= deciles_ten)) + 
  geom_line(data  = subset(WF.BWD012.df,housetype %in% c("A")), 
            aes(y = transaction_price_per_sqm, colour = "Apartments"), 
            stat  = "summary", 
            fun   = "mean", 
            size  = 0.8) + 
  geom_line(data  = subset(WF.BWD012.df,housetype %in% c("T")), 
            aes(y = transaction_price_per_sqm, colour = "Chained House"), 
            stat  = "summary", 
            fun   = "mean", 
            size  = 0.8) + 
  geom_line(data  = subset(WF.BWD012.df,housetype %in% c("H")), 
            aes(y = transaction_price_per_sqm, colour = "Corner House"), 
            stat  = "summary", 
            fun   = "mean", 
            size  = 0.8) + 
  geom_line(data  = subset(WF.BWD012.df,housetype %in% c("K")), 
            aes(y = transaction_price_per_sqm, colour = "Semi-Detached House"), 
            stat  = "summary", 
            fun   = "mean", 
            size  = 0.8) + 
  geom_line(data  = subset(WF.BWD012.df,housetype %in% c("V")), 
            aes(y = transaction_price_per_sqm, colour = "Detached House"), 
            stat  = "summary", 
            fun   = "mean", 
            size  = 0.8) + 
  
  scale_x_continuous(breaks=seq(0, 100, by = 1)) +
  scale_color_manual(values = c("Apartments" = "black", 
                                "Chained House" = "blue", 
                                "Corner House" = "green", 
                                "Semi-Detached House" = "gray",
                                "Detached House" = "purple"), 
                     name = "") +
  labs(y = "Price in Euro's", 
       x = "Deciles",
       title ="Mean Transaction Price per Square Meter") + 
  
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        legend.position = "bottom")

plot_tp_housetype

### remove all but WF.BWD012.df and clear RAM
rm(list=ls()[-match("WF.BWD012.df", ls())])
gc()


################################################################################
############ STEP 3:Deciles BASED ON EXPLANATORY POWER OF SENTIMENT ############
################################################################################


# fixed effects (FE) hedonic model, within
mod3.FE.within.lti <- lme4::lmer(LN_transaction_price ~ LN_property_size + 
                                   year_of_construction_1945 +
                                   year_of_construction_1945_1980 +
                                   year_of_construction_1980_2000 + 
                                   housetype +
                                   (1 | COROP_code) +
                                   (1 | transaction_month) + 
                                   (1 | COROP_code:transaction_month),
                                 REML = FALSE,
                                 data = WF.BWD012.df) 


# fixed effects (FE) hedonic model, within
mod.4.FE.within.lti.NFM <- lme4::lmer(LN_transaction_price ~ residuals_baseline_model +
                                       LN_property_size + 
                                       year_of_construction_1945 +
                                       year_of_construction_1945_1980 +
                                       year_of_construction_1980_2000 + 
                                       housetype +
                                       (1 | COROP_code) +
                                       (residuals_baseline_model + 1 | transaction_month) + 
                                       (1 | COROP_code:transaction_month),
                                     REML = FALSE,
                                     data = WF.BWD012.df) 

summary(mod4.FE.within.lti.NFM)

### assumptions
# linearity
plot(resid(mod4.FE.within.lti.NFM), LN_transaction_price)

# homogeneity of variance across groups
plot(mod4.FE.within.lti.NFM)
WF.BWD012.df$abs.residuals.squared <- (abs(residuals(mod4.FE.within.lti.NFM)))^2

test_corop <- lm(abs.residuals.squared ~ COROP_code, data = WF.BWD012.df)
anova(test_corop)

test_transaction_month <- lm(abs.residuals.squared ~ transaction_month, data = WF.BWD012.df)
anova(test_transaction_month)

test_corop_transaction_month <- lm(abs.residuals.squared ~ COROP_code:transaction_month, data = WF.BWD012.df)
anova(test_corop_transaction_month)

# distribution of residuals
qqnorm(resid(mod4.FE.within.lti.NFM)) 
qqline(resid(mod4.FE.within.lti.NFM), col = "red")

hist(abs.residuals.squared, data = WF.BWD012.df)


### anova between model 3 and 4 and plots random slope and effects model 4 (appendix D.2)
anova(mod3.FE.within.lti, mod4.FE.within.lti.NFM)
RE.plots <- plot_model(mod4.FE.within.lti.NFM, type = "re", xlim = 10, ylim = 10)


### output
output_sentiment <- stargazer(mod3.FE.within.lti, mod4.FE.within.lti.NFM,
                              type = 'text', 
                              font.size = "tiny", 
                              dep.var.caption = "",
                              title = "Hedonic model", 
                              digits = 2, 
                              single.row = TRUE, 
                              no.space = TRUE, 
                              keep = c("LN_property_size", "LN_parcel_size", 
                                       "year_of_construction_1945", 
                                       "year_of_construction_1945_1980",  
                                       "year_of_construction_1980_2000", "housetype",
                                       "OTB_Eigen_Huis_Markt_Indicator", 
                                       "residuals_baseline_model"),
                              add.lines = list(c("Transaction year dummies:", "YES","YES"), 
                                               c("Neighborhood dummies:", "YES", "YES"),
                                               c("R2c", round(r.squaredGLMM(mod3.FE.within.lti)[1,2], 3),
                                                 round(r.squaredGLMM(mod4.FE.within.lti.NFM)[1,2], 3))),
                              out = "output_sentiment.doc")


########### Deciles based on difference in residuals of both models ############
### 10 deciles based on explanatory power of sentiment
# calculate difference between the residuals of both models 
WF.BWD012.df$delta_residuals <- residuals(mod3.FE.within.lti) - residuals(mod4.FE.within.lti.NFM) 
summary(WF.BWD012.df$delta_residuals)


# create 10 deciles
WF.BWD012.df$deciles_delta_residuals <- ntile(WF.BWD012.df$delta_residuals, 10)
summary(WF.BWD012.df$deciles_delta_residuals)


### plot 
#plot(WF.BWD012.df$deciles_b, WF.BWD012.df$delta_residuals, 
#     type = "p",
#     data = WF.BWD012.df)


### statistics per decile
deciles.delta.residuals <- WF.BWD012.df %>%
  dplyr::group_by(deciles_delta_residuals) %>%
  dplyr::summarize(mean(transaction_price, na.rm = TRUE),
                   sd(transaction_price, na.rm = TRUE), 
                   mean(transaction_price_per_sqm, na.rm = TRUE),
                   mean(delta_real_predicted_tp_lti, na.rm = TRUE),
                   mean(indicator_first_time_buyer, na.rm = TRUE),
                   mean(investor_seller, na.rm = TRUE),
                   mean(private_investor_seller, na.rm = TRUE),
                   mean(non_private_investor_seller, na.rm = TRUE),
                   mean(owner_occupant_seller, na.rm = TRUE),
                   mean(second_house_seller, na.rm = TRUE),
                   mean(private_investor_buyer, na.rm = TRUE),
                   mean(second_house_buyer, na.rm = TRUE),
                   mean(owner_occupant_buyer, na.rm = TRUE),
                   mean(number_of_houses_buyer, na.rm = TRUE),
                   mean(year_of_construction, na.rm = TRUE),
                   mean(property_size, na.rm = TRUE),
                   mean(parcel_size, na.rm = TRUE)) %>%
  t() %>%
  data.frame() 

names(deciles.delta.residuals) = deciles.delta.residuals[1,]
deciles.delta.residuals <- deciles.delta.residuals[2:17,]


### t-test between each decile
# the t.test function and dplyr don't play together nicely 
# manually test each decile (2 vs 1, ..., 10 vs 9):
WF.BWD012.df %>%
  dplyr::select(transaction_price, deciles_delta_residuals) %>%
  dplyr::filter(deciles_delta_residuals == 5 | deciles_delta_residuals == 6)
t.test(transaction_price ~ deciles_delta_residuals, data = testxs, var.equal = FALSE)


# produce output
deciles.delta.residuals <- stargazer(deciles.delta.residuals, 
                                     type = "latex",
                                     summary = FALSE,
                                     digits = 3,
                                     out = "deciles_delta_residuals.doc")

#format(deciles.delta.residuals, digits = 3)
#write.csv(deciles.delta.residuals, "deciles_delta_residuals.csv")

rm(mod3.FE.within.lti, mod4.FE.within.lti.NFM)


################################################################################
###################### STEP 4: explantory power per group ######################
################################################################################

# FE hedonic model buyer dummies
mod5.FE.within.lti.IB <- lme4::lmer(LN_transaction_price ~ 
                                      indicator_first_time_buyer + 
                                      indicator_first_time_buyer:residuals_baseline_model +
                                      private_investor_buyer +
                                      private_investor_buyer:residuals_baseline_model +
                                      second_house_buyer +
                                      second_house_buyer:residuals_baseline_model +
                                      LN_property_size + 
                                      year_of_construction_1945 +
                                      year_of_construction_1945_1980 +
                                      year_of_construction_1980_2000 + 
                                      housetype +
                                      (1 | COROP_code) +
                                      (residuals_baseline_model + 1 | transaction_month) +
                                      (1 | COROP_code:transaction_month),
                                    data = WF.BWD012.df) 


# FE hedonic model seller dummies
mod6.FE.within.lti.IS <- lme4::lmer(LN_transaction_price ~ 
                                      private_investor_seller +
                                      private_investor_seller:residuals_baseline_model +
                                      second_house_seller +
                                      second_house_seller:residuals_baseline_model +
                                      non_private_investor_seller + 
                                      non_private_investor_seller:residuals_baseline_model +
                                      LN_property_size + 
                                      year_of_construction_1945 +
                                      year_of_construction_1945_1980 +
                                      year_of_construction_1980_2000 + 
                                      housetype +
                                      (1 | COROP_code) +
                                      (residuals_baseline_model + 1 | transaction_month) +
                                      (1 | COROP_code:transaction_month),
                                    data = subset(WF.BWD012.df, indicator_seller_1 != c("Onbekend", "Overig", "Woco"))) 


### output
output_sentiment_indicators <- stargazer(mod5.FE.within.lti.IB, mod6.FE.within.lti.IS,
                                         type = 'latex', 
                                         font.size = "tiny", 
                                         dep.var.caption = "",
                                         title = "Hedonic model", 
                                         digits = 3, 
                                         single.row = TRUE, 
                                         no.space = TRUE, 
                                         keep = c("LN_property_size", 
                                                  "year_of_construction_1945", 
                                                  "year_of_construction_1945_1980",  
                                                  "year_of_construction_1980_2000", 
                                                  "housetype",
                                                  "OTB_Eigen_Huis_Markt_Indicator", 
                                                  "residuals_baseline_model", 
                                                  "indicator_first_time_buyer", 
                                                  "private_investor_buyer", 
                                                  "second_house_buyer", 
                                                  "private_investor_seller",
                                                  "second_house_seller", 
                                                  "non_private_investor_seller"),
                                         add.lines = list(c("Transaction year dummies:", "YES","YES", "YES"), 
                                                          c("Neighborhood dummies:", "YES", "YES", "YES"),
                                                          c("R2c",
                                                            round(r.squaredGLMM(mod5.FE.within.lti.IB)[1,2], 3),
                                                            round(r.squaredGLMM(mod6.FE.within.lti.IS)[1,2], 3))))


### remove models
rm(mod5.FE.within.lti.IB, mod6.FE.within.lti.IS); gc()

################################################################################
###################### STEP 5: explanatory power g40 / g4  ######################
################################################################################

# FE hedonic model buyer dummies 
mod7.FE.within.lti.G40 <- lme4::lmer(LN_transaction_price ~
                                       G40_indicator +
                                       G40_indicator:residuals_baseline_model +
                                       LN_property_size + 
                                       year_of_construction_1945 +
                                       year_of_construction_1945_1980 +
                                       year_of_construction_1980_2000 + 
                                       housetype +
                                       (1 | COROP_code) +
                                       (residuals_baseline_model + 1 | transaction_month) +
                                       (1 | COROP_code:transaction_month),
                                     data = WF.BWD012.df) 


# FE hedonic model seller dummies
mod8.FE.within.lti.G4 <- lme4::lmer(LN_transaction_price ~ 
                                      G4_indicator +
                                      G4_indicator:residuals_baseline_model +
                                      LN_property_size + 
                                      year_of_construction_1945 +
                                      year_of_construction_1945_1980 +
                                      year_of_construction_1980_2000 + 
                                      housetype +
                                      (1 | COROP_code) +
                                      (residuals_baseline_model + 1 | transaction_month) +
                                      (1 | COROP_code:transaction_month),
                                    data = WF.BWD012.df) 


# FE hedonic model seller dummies
#mod9.FE.within.lti.G44 <- lme4::lmer(LN_transaction_price ~ 
#                                      G44_indicator:residuals_baseline_model +
#                                      LN_property_size + 
#                                      year_of_construction_1945 +
#                                      year_of_construction_1945_1980 +
#                                      year_of_construction_1980_2000 + 
#                                      housetype +
#                                      (1 | COROP_code) +
#                                      (residuals_baseline_model - 1 | transaction_month) +
#                                      (1 | COROP_code:transaction_month),
#                                    data = WF.BWD012.df) 


### output
output_sentiment_g <- stargazer(mod7.FE.within.lti.G40, mod8.FE.within.lti.G4, #mod9.FE.within.lti.G44,
                                type = 'text', 
                                font.size = "tiny", 
                                dep.var.caption = "",
                                title = "Hedonic model", 
                                digits = 3, 
                                single.row = TRUE, 
                                no.space = TRUE, 
                                keep = c("LN_property_size", 
                                         "year_of_construction_1945", 
                                         "year_of_construction_1945_1980",  
                                         "year_of_construction_1980_2000", 
                                         "housetype",
                                         "residuals_baseline_model", 
                                         "G40_indicator",
                                         "G4_indicator"),
                                add.lines = list(c("Transaction year dummies:", "YES","YES", "YES"), 
                                                 c("Neighborhood dummies:", "YES", "YES", "YES"),
                                                 c("R2c",
                                                   round(r.squaredGLMM(mod7.FE.within.lti.G40)[1,2], 3),
                                                   round(r.squaredGLMM(mod8.FE.within.lti.G4)[1,2], 3)
                                                 )))


rm(mod7.FE.within.lti.G40, mod8.FE.within.lti.G4, mod9.FE.within.lti.G44); gc()


################################################################################
######################## STEP 5: Obselete/unused models ########################
################################################################################
attach(WF.BWD012.df)


mod.wi.FE.within.lti.NFM <- lme4::lmer(LN_transaction_price ~ residuals_baseline_model +
                                         LN_property_size + 
                                         year_of_construction_1945 +
                                         year_of_construction_1945_1980 +
                                         year_of_construction_1980_2000 + 
                                         housetype +
                                         (1 | COROP_code) +
                                         (residuals_baseline_model - 1 | transaction_month), 
                                       REML = FALSE,
                                       data = WF.BWD012.df) 


mod4.abs.FE.within.lti.NFM <- lme4::lmer(LN_transaction_price ~ abs(residuals_baseline_model) +
                                           LN_property_size + 
                                           year_of_construction_1945 +
                                           year_of_construction_1945_1980 +
                                           year_of_construction_1980_2000 + 
                                           housetype +
                                           (1 | COROP_code) +
                                           (residuals_baseline_model - 1 | transaction_month) + 
                                           (1 | COROP_code:transaction_month),
                                         REML = FALSE,
                                         data = WF.BWD012.df) 


WF.BWD012.df$low_high_sentiment <- 0
WF.BWD012.df$low_high_sentiment <- ifelse(WF.BWD012.df$delta_residuals < 0, 1, 0)


mod6.FE.within.Y.NFM.HL <- lme4::lmer(LN_transaction_price ~ residuals_baseline_model :
                                        low_high_sentiment +
                                        LN_property_size + 
                                        LN_parcel_size + 
                                        year_of_construction_1945 +
                                        year_of_construction_1945_1980 +
                                        year_of_construction_1980_2000 + 
                                        housetype +
                                        (1 | COROP_code) +
                                        (1 | transaction_year),
                                      data = WF.BWD012.df) 


mod7.FE.within.Y.HL <- lme4::lmer(LN_transaction_price ~ low_high_sentiment +
                                    LN_property_size + 
                                    LN_parcel_size + 
                                    year_of_construction_1945 +
                                    year_of_construction_1945_1980 +
                                    year_of_construction_1980_2000 + 
                                    housetype +
                                    (1 | COROP_code) +
                                    (1 | transaction_year),
                                  data = WF.BWD012.df) 


mod8.FE.within.Y.deciles_ten <- lme4::lmer(LN_transaction_price ~ factor(deciles_ten_abs) +
                                             LN_property_size + 
                                             LN_parcel_size + 
                                             year_of_construction_1945 +
                                             year_of_construction_1945_1980 +
                                             year_of_construction_1980_2000 + 
                                             housetype +
                                             (1 | COROP_code) +
                                             (1 | transaction_year),
                                           data = WF.BWD012.df) 


mod9.FE.within.Y.deciles_five <- lme4::lmer(LN_transaction_price ~ factor(deciles_five_abs) +
                                              LN_property_size + 
                                              LN_parcel_size + 
                                              year_of_construction_1945 +
                                              year_of_construction_1945_1980 +
                                              year_of_construction_1980_2000 + 
                                              housetype +
                                              (1 | COROP_code) +
                                              (1 | transaction_year),
                                            data = WF.BWD012.df) 


# FE hedonic model with non-fundamental sentiment (NFM)
mod.FE.within.lti.NFM <- lme4::lmer(LN_transaction_price ~ residuals_baseline_model +
                                      LN_property_size + 
                                      LN_parcel_size + 
                                      year_of_construction_1945 +
                                      year_of_construction_1945_1980 +
                                      year_of_construction_1980_2000 + 
                                      housetype +
                                      (1 | COROP_code) +
                                      (1 | transaction_month) + 
                                      (1 | COROP_code:transaction_month),
                                    data = WF.BWD012.df) 


# FE hedonic model with fundamental sentiment (FM) i.e., the original index
mod.FE.within.lti.FM <- lme4::lmer(LN_transaction_price ~ OTB_Eigen_Huis_Markt_Indicator + 
                                     LN_property_size + 
                                     LN_parcel_size + 
                                     year_of_construction_1945 +
                                     year_of_construction_1945_1980 +
                                     year_of_construction_1980_2000 + 
                                     housetype +
                                     (1 | COROP_code) +
                                     (1 | transaction_month) + 
                                     (1 | COROP_code:transaction_month),
                                   data = WF.BWD012.df) 


### output_sentiment
output_sentiment_two <- stargazer(mod6.FE.within.Y.NFM.HL, 
                                  mod7.FE.within.Y.HL, 
                                  mod8.FE.within.Y.deciles_ten, 
                                  mod9.FE.within.Y.deciles_five,
                                  type = 'latex', 
                                  font.size = "tiny")


rm(mod6.FE.within.Y.NFM.HL, mod7.FE.within.Y.HL, mod8.FE.within.Y.deciles_ten, 
   mod9.FE.within.Y.deciles_five)


################################################################################
################################## REFERENCES ##################################
################################################################################

# https://stat.ethz.ch/~meier/teaching/anova/random-and-mixed-effects-models.html
# https://www.rensvandeschoot.com/tutorials/lme4/
# https://ademos.people.uic.edu/Chapter18.html#622_what_do_the_results_mean
