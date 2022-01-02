
################################################################################
#################################### STEP 0 ####################################
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


### load packages 
#install.packages("xts")
#install.packages("Hmisc")
#install.packages("corrplot")
#install.packages("forecast")
#install.packages("mFilter")
#install.packages("gridExtra")

### libraries
library(corrplot)
library(Hmisc)
library(stargazer)
library(zoo)
library(xts)
library(tidyverse)
library(dplyr)
library(modelr)
library(forecast)
library(mFilter)
library(vars)
library(tseries)
library(time_series)
library(gridExtra)
library(vars)
library(tseries)


### remove all but ALL.indices
rm(list=ls()[-match(c("ALL.indices"), ls())])


################################################################################
############### STEP 1: REGRESSIONS HOUSING FUNDAMENTALS, Eq. 1 ################
################################################################################
detach()
attach(ALL.indices)
require(dynlm)

################################# regression ###################################
### create ts 
ALL.indices.ts <- ts(ALL.indices, 
                     start = c(1995, 1),
                     end = c(2021, 8), 
                     frequency = 12)
# ["2004-04-01/2021-08-01"]


### model 1: only TPI and volume
mod1.base.model <- 
  dynlm(OTB_Eigen_Huis_Markt_Indicator ~ 
          #L(OTB_Eigen_Huis_Markt_Indicator, 1) +
          L(CBS_transaction_price_index, 1:7) +
          L(log(CBS_volume_SA), 1:3),
        start = c(2004, 1),
        end = c(2021, 8), 
        data = ALL.indices.ts)


### model 2: including loans and interests
mod2.base.model <- 
  dynlm(OTB_Eigen_Huis_Markt_Indicator ~ 
          #L(OTB_Eigen_Huis_Markt_Indicator, 1) +
          L(CBS_transaction_price_index, 1:7) +
          L(log(CBS_volume_SA), 1:3) +
          L(log(DNB_newly_made_loans_SA), 1:7) +
          L(DNB_interest_newly_made_loans, 1:2) +
          L(ECB_share_of_variable_rate_mortgages, 1),
        start = c(2004, 1),
        end = c(2021, 8), 
        data = ALL.indices.ts)


### model 3: supply and construction costs
mod3.base.model <- 
  dynlm(OTB_Eigen_Huis_Markt_Indicator ~ 
          #L(OTB_Eigen_Huis_Markt_Indicator, 1) +
          L(CBS_transaction_price_index, 1:7) +
          L(log(CBS_volume_SA), 1:3) +
          L(log(DNB_newly_made_loans_SA), 1:7) +
          L(DNB_interest_newly_made_loans, 1:2) +
          L(ECB_share_of_variable_rate_mortgages, 1) +
          L(EUROSTAT_CCI_newly_built_residential, 1:7) +
          L(log(woningbouwers_newly_built_houses_SA), 1:3),
        start = c(2004, 1),
        end = c(2021, 8), 
        data = ALL.indices.ts)

summary(mod3.base.model)
par(mfrow = c(2,2), mex = 0.6, mar = c(3,3,2,1) + 0.1, omi = rep(.3, 4))
plot(mod3.base.model)


### model 4: macro-economic fundamentals
mod4.base.model <- 
  dynlm(OTB_Eigen_Huis_Markt_Indicator ~ 
          #L(OTB_Eigen_Huis_Markt_Indicator, 1) +
          L(CBS_transaction_price_index, 1:7) +
          L(log(CBS_volume_SA), 1:3) +
          L(log(DNB_newly_made_loans_SA), 1:7) +
          L(DNB_interest_newly_made_loans, 1:2) +
          L(ECB_share_of_variable_rate_mortgages, 1) +
          L(EUROSTAT_CCI_newly_built_residential, 1:7) +
          L(log(woningbouwers_newly_built_houses_SA), 1:3) + 
          L(ECB_HCIP_NL_SA, 1) + 
          L(FRED_ten_year_long_term_bond_yield, 1:2) + 
          L(CBS_population_growth_SA, 1:4),
        start = c(2004, 1),
        end = c(2021, 8), 
        data = ALL.indices.ts)


### output 
stargazer(mod1.base.model, mod2.base.model, mod3.base.model, mod4.base.model,
          type = "text", 
          font.size = "tiny", 
          dep.var.caption = "", 
          title = "OLS estimation for Equation 1", 
          digits = 3, 
          single.row = TRUE)


################################# prediction ###################################
# subset to 2004-01-01 / 2021-08-01
ALL.indices <- ALL.indices[112:320,]

# store predictiona nd join to ALL.indices
prediction_mod_1 <- predict(mod1.base.model)
prediction_mod_2 <- predict(mod2.base.model)
prediction_mod_3 <- predict(mod3.base.model)
prediction_mod_4 <- predict(mod4.base.model)

ALL.predictions <- as.data.frame(cbind(prediction_mod_1, 
                                       prediction_mod_2, 
                                       prediction_mod_3,
                                       prediction_mod_4))

ALL.predictions$period <- seq(as.Date("2004-04-01"), 
                              by = "month", 
                              length.out = 209)

ALL.indices <- plyr::join_all(list(ALL.indices, 
                                   ALL.predictions),
                              by = "period",
                              type = "left")


# plot prediction
g <- ggplot(ALL.indices, aes(x = period))
g <- g + geom_line(aes(y = prediction_mod_1,
                   colour = "Prediction Model 1"), 
                   size = 0.6)
g <- g + geom_line(aes(y = prediction_mod_2, 
                   colour = "Prediction Model 2"), 
                   size = 0.6)
g <- g + geom_line(aes(y = prediction_mod_4, 
                   colour = "Prediction Model 4"), 
                   size = 0.6)
g <- g + geom_line(aes(y = prediction_mod_3, 
                   colour = "Prediction Model 3"),  
                   size = 0.6)
g <- g + geom_line(aes(y = OTB_Eigen_Huis_Markt_Indicator, 
                   colour = "Sentiment Index"), 
                   size = 0.6)

g <- g + scale_x_date(date_breaks = "1 year", 
                      date_labels = "%Y")

g <- g + xlab("Transaction Year")
g <- g + ylab("Index")
g <- g + scale_color_manual(values = c("Prediction Model 1" = "purple", 
                                       "Prediction Model 2" = "green",
                                       "Prediction Model 3" = "blue",
                                       "Prediction Model 4" = "gray",
                                       "Sentiment Index" = "black"), 
                            name = "") 
  
g <- g + theme_minimal(base_size = 12)
g <- g + theme(legend.position = "bottom")

dev.off()
prediction.plot <- g; prediction.plot

rm(prediction, g, ALL.predictions, prediction.plot, prediction_mod_1, 
   prediction_mod_2, prediction_mod_3, prediction_mod_4)


################################# residuals ####################################
# store residuals in ALL.indices
ALL.indices$residuals_baseline_model<- resid(mod4.base.model)

ALL.indices$residuals__model_one <- resid(mod1.base.model)
ALL.indices$residuals__model_two <- resid(mod2.base.model)
ALL.indices$residuals_baseline_model_four <- resid(mod4.base.model)


# summarize and make simpel plots
summary(ALL.indices$residuals_baseline_model)

plot(residuals_baseline_model, 
     data = ALL.indices,
     type = "l",
     main = "residuals baseline model - line")
plot(residuals_baseline_model, 
     data = ALL.indices,
     type = "h",
     main = "residuals baseline model - histogram")

hist(residuals_baseline_model, data = ALL.indices)


### ggplot 
g <- ggplot(ALL.indices, aes(x = period))  
g <- g + geom_bar(aes(y = residuals_baseline_model), 
                  color = "blue", 
                  stat = "identity")

g <- g + scale_x_date(date_breaks = "1 year", 
                      date_labels = "%Y")
g <- g + scale_y_continuous(expand = c(0.01,0))

g <- g + geom_vline(xintercept = as.numeric(ALL.indices$period[41]), 
                    linetype = 1, 
                    color = "gray", 
                    size = 0.6)
g <- g + geom_vline(xintercept = as.numeric(ALL.indices$period[192]), 
                    linetype = 1, 
                    color = "gray", 
                    size = 0.6)
g <- g + geom_label(x = as.numeric(ALL.indices$period[41]), 
                    y = 16.7, 
                    label ="GFC", 
                    size = 3) # 17.8
g <- g + geom_label(x = as.numeric(ALL.indices$period[192]), 
                    y = 16.7, 
                    label ="COVID-19", 
                    size = 3)

g <- g + xlab("Transaction Year")
g <- g + ylab("Residuals") 
g <- g + theme_minimal(base_size = 12) # classic is nice as well 

dev.off()
residuals.plot <- g; residuals.plot

rm(g, residuals.plot, mod1.base.model, 
   mod2.base.model, mod3.base.model, mod4.base.model)


################################################################################
########################## model 3 with 3, 6, 12 lags ##########################
################################################################################

### model 5: 3 lags
mod5.base.model <- 
  dynlm(OTB_Eigen_Huis_Markt_Indicator ~ 
          #L(OTB_Eigen_Huis_Markt_Indicator, 1) +
          L(CBS_transaction_price_index, 1:3) +
          L(log(CBS_volume_SA), 1:3) +
          L(log(DNB_newly_made_loans_SA), 1:3) +
          L(DNB_interest_newly_made_loans, 1:3) +
          L(ECB_share_of_variable_rate_mortgages, 1:3) +
          L(EUROSTAT_CCI_newly_built_residential, 1:3) +
          L(log(woningbouwers_newly_built_houses_SA), 1:3),
        start = c(2004, 1),
        end = c(2021, 8), 
        data = ALL.indices.ts)


### model 6: 12 lags
mod6.base.model <- 
  dynlm(OTB_Eigen_Huis_Markt_Indicator ~ 
          #L(OTB_Eigen_Huis_Markt_Indicator, 1) +
          L(CBS_transaction_price_index, 1:6) +
          L(log(CBS_volume_SA), 1:6) +
          L(log(DNB_newly_made_loans_SA), 1:6) +
          L(DNB_interest_newly_made_loans, 1:6) +
          L(ECB_share_of_variable_rate_mortgages, 1:6) +
          L(EUROSTAT_CCI_newly_built_residential, 1:6) +
          L(log(woningbouwers_newly_built_houses_SA), 1:6),
        start = c(2004, 1),
        end = c(2021, 8), 
        data = ALL.indices.ts)


### model 7: 12 lags
mod7.base.model <- 
  dynlm(OTB_Eigen_Huis_Markt_Indicator ~ 
          #L(OTB_Eigen_Huis_Markt_Indicator, 1) +
          L(CBS_transaction_price_index, 1:12) +
          L(log(CBS_volume_SA), 1:12) +
          L(log(DNB_newly_made_loans_SA), 1:12) +
          L(DNB_interest_newly_made_loans, 1:12) +
          L(ECB_share_of_variable_rate_mortgages, 1:12) +
          L(EUROSTAT_CCI_newly_built_residential, 1:12) +
          L(log(woningbouwers_newly_built_houses_SA), 1:12),
        start = c(2004, 1),
        end = c(2021, 8), 
        data = ALL.indices.ts)


### output 
stargazer(mod3.base.model, mod5.base.model, mod6.base.model, mod7.base.model,
          type = 'text', 
          font.size = "tiny", 
          dep.var.caption = "", 
          title = "original values - OLS estimaton", 
          digits = 3, 
          single.row = TRUE)


########################### prediction model 5,6,7 #############################

# store predictiona nd join to ALL.indices
prediction_mod_5 <- predict(mod5.base.model)
prediction_mod_6 <- predict(mod6.base.model)
prediction_mod_7 <- predict(mod7.base.model)

ALL.predictions.two <- as.data.frame(cbind(prediction_mod_5, 
                                       prediction_mod_6,
                                       prediction_mod_7))

ALL.predictions.two$period <- seq(as.Date("2004-04-01"), 
                              by = "month", 
                              length.out = 209)

ALL.indices <- plyr::join_all(list(ALL.indices, 
                                   ALL.predictions.two),
                              by = "period",
                              type = "left")


# plot prediction
g <- ggplot(ALL.indices, aes(x = period))
g <- g + geom_line(aes(y = prediction_mod_5),
                   color = "blue", 
                   size = 0.6)
g <- g + geom_line(aes(y = prediction_mod_6), 
                   color = "green", 
                   size = 0.6)
g <- g + geom_line(aes(y = prediction_mod_3), 
                   color = "gray", 
                   size = 0.6)
g <- g + geom_line(aes(y = prediction_mod_7), 
                   color = "purple", 
                   size = 0.6)
g <- g + geom_line(aes(y = OTB_Eigen_Huis_Markt_Indicator), 
                   color = "black", 
                   size = 0.6)

g <- g + scale_x_date(date_breaks = "1 year", 
                      date_labels = "%Y")

g <- g + xlab("Transaction Year")
g <- g + ylab("(Predicted) Sentiment Index")

g <- g + theme_minimal(base_size = 10)

dev.off()
prediction.plot <- g; prediction.plot

rm(prediction, g, prediction_mod_5, prediction_mod_6, prediction_mod_7, 
   prediction.plot, mod5.base.model, mod6.base.model, mod7.base.model, 
   ALL.predictions.two)


################################################################################
############################## Reduced form VAR ################################
################################################################################
#### steps: 
#  I(0) --> integration process (stationary)
#  lags --> how many lags to include (dimensionality curse, auto cor. 
#   if you choose too many)
#  estimate two equations --> for sentiment, volume, transactiopn pirce 
#   --> endogenously determined, one uses OLS for each equation.
#  Granger causality test --> not tested
#  IRF's --> not estiamted


### create dataframes 
endogen <- ALL.indices[2:209, c("OTB_change_Eigen_Huis_Markt_Indicator", 
                       "CBS_change_transaction_price_index",
                       "CBS_change_volume_SA_LN")] 

exogen <- ALL.indices[2:209, c("DNB_change_newly_made_loans_SA_LN",
                      "DNB_change_interest_newly_made_loans", 
                      "ECB_share_of_variable_rate_mortgages", 
                      "EUROSTAT_change_CCI_newly_built_residential",
                      "woningbouwers_change_newly_built_houses_SA_LN")] 


### Stationarity 
ADF.endogen <- lapply(endogen, function(x) try (adf.test(x)))
ADF.exogen <- lapply(exogen, function(x) try (adf.test(x)))

plot(CBS_change_transaction_price_index, type = "l")


### model 
VARselect(endogen, lag.max = 10)

mod.var <- VAR(y = endogen, 
               p = 7, 
               exogen = exogen)

stargazer(mod.var$varresult, type = 'text' ) 
summary(mod.var) 


### residuals
temp <- as.data.frame(residuals(mod.var)[,1])
names(temp) <- c("residuals_var")
temp$period <- seq(as.Date("2004-11-01"), 
                   by = "month", 
                   length.out = 201)

# ggplot 
g <- ggplot(temp, aes(x = period))  
g <- g + geom_bar(aes(y = residuals_var), 
                  color = "blue", 
                  stat = "identity")

g <- g + scale_x_date(date_breaks = "1 year", 
                      date_labels = "%Y")
g <- g + scale_y_continuous(expand = c(0.01,0))

g <- g + geom_vline(xintercept = as.numeric(temp$period[41]), 
                    linetype = 1, 
                    color = "gray", 
                    size = 0.6)
g <- g + geom_vline(xintercept = as.numeric(temp$period[192]), 
                    linetype = 1, 
                    color = "gray", 
                    size = 0.6)
g <- g + geom_label(x = as.numeric(temp$period[41]), 
                    y = 12.3, 
                    label ="GFC", 
                    size = 3)
g <- g + geom_label(x = as.numeric(temp$period[192]), 
                    y = 12.3, 
                    label ="COVID-19", 
                    size = 3)

g <- g + xlab("Transaction Year")
g <- g + ylab("Residuals") 
g <- g + theme_minimal(base_size = 10) # classic is nice as well 

dev.off()
residuals.plot <- g; residuals.plot


rm(adf_y1, endogen, exogen, g, mod.var, prediction, residuals.plot, temp)


################################################################################
############################## Other Descriptives ##############################
################################################################################


####################### plot indicators survey questions #######################
plot_buying <- ggplot(ALL.indices, aes(x= period)) +
  geom_line(aes(y = Bijdrage_algemeen_kopen_afgelopen_twaalf_maanden, 
                color = "Buying preceding 12 months")) + 
  geom_line(aes(y = Bijdrage_algemeen_kopen_komende_twaalf_maanden, 
                color = "Buying succeeding 12 months")) +
  
  labs(y = "Index", 
       x = "Transaction Month",
       title ="Panel A: Indicator Good Time to Buy") + 
  scale_color_manual(values = c("Buying preceding 12 months" = "blue", 
                                "Buying succeeding 12 months" = "grey0"), 
                     name = "") +
  
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y") + 
  scale_y_continuous(expand = c(0.01,0)) +
  
  theme_minimal(base_size = 7) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")


# ------------------------------------------------------------------------------
plot_prices <- ggplot(ALL.indices, aes(x= period)) +
  geom_line(aes(y = Bijdrage_koopprijzen_afgelopen_twaalf_maanden, 
                colour = "Prices preceding 12 months")) +
  geom_line(aes(y = Bijdrage_koopprijzen_komende_twaalf_maanden, 
                colour = "Prices succeeding 12 months")) +
  labs(y = "Index", 
       x = "Transaction Month",
       title = "Panel B: Indicator Attractive Transaction Prices ") +
  
  scale_color_manual(values = c("Prices preceding 12 months" = "blue", 
                                "Prices succeeding 12 months" = "grey0"), 
                     name = "") +
  
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y") + 
  scale_y_continuous(expand = c(0.01,0)) +
  
  theme_minimal(base_size = 7) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),         
        legend.position = "bottom")

# ------------------------------------------------------------------------------
plot_interest <- ggplot(ALL.indices, aes(x= period)) +
  
  geom_line(aes(y = Bijdrage_rente_afgelopen_twaalf_maanden, 
                colour = "Interest preceding 12 months")) + 
  geom_line(aes(y = Bijdrage_rente_komende_twaalf_maanden, 
                colour = "Interest succeeding 12 months")) +
  
  labs(y = "Index", 
       x = "Transaction Month", 
       title = "Panel C: Indicators Attractive Interest Rates") +
  scale_color_manual(values = c("Interest preceding 12 months" = "blue", 
                                "Interest succeeding 12 months" = "grey0"), 
                     name = "") +
  
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y") + 
  scale_y_continuous(expand = c(0.01,0)) +
  
  theme_minimal(base_size = 7) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        legend.position = "bottom")


# ------------------------------------------------------------------------------
Figure_x <- grid.arrange(plot_buying, 
                         plot_prices, 
                         plot_interest,
                         ncol=1, nrow =3)


### explort 
write.csv(ALL.indices, "ALL_indices.csv")
