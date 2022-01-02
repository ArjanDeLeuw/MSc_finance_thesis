
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
# in 5 spatial analysis.R data is aggregated to a municipality level to produce
#   maps in Arc.GIS (figure and Appendix A.3).


### prepare environment
cat("\014") # clear console
if(!is.null(dev.list())) dev.off() # clear plots
rm(list=ls()[-match("WF.BWD012.df", ls())])
setwd("V:/GVA/MA_Maatwerk en Advies/Publicaties en Presentaties/2021/2021 - Stage Arjan de Leuw/R_project_sentiment")

library(tidyverse)

################################################################################
######################### STEP 1: prepare data   ###############################
################################################################################
attach(WF.BWD012.df)

### count volume
volume.per.municipality.df <- WF.BWD012.df %>%
  group_by(municipality_code) %>%
  tally() %>% 
  ungroup() %>% 
  data.frame() %>%
  write.csv2("volume_mun_count.csv")


### mean volume per month per municipality
WF.BWD012.df %>%
  group_by(municipality_code, transaction_month) %>%
  tally()%>%
  data.frame() -> volume_mon_mun.df

volume_mun_mean <- aggregate(n ~ municipality_code,  volume_mon_mun.df, mean)
write.csv2(volume_mun_mean, "volume_mun_mean.csv")


### statistics per municipality
sentiment.per.municipality.df <- WF.BWD012.df %>%
  group_by(municipality_code) %>%
  dplyr::summarize(mean(residuals_baseline_model, na.rm = TRUE), 
                   min(residuals_baseline_model, na.rm = TRUE),
                   max(residuals_baseline_model, na.rm = TRUE),
                   mean(transaction_price, na.rm = TRUE),
                   mean(deciles_ten, na.rm = TRUE),
                   
                   mean(transaction_price, na.rm = TRUE),
                   mean(transaction_price_per_sqm, na.rm = TRUE),
                   #mean(delta_real_predicted_tp_lti, na.rm = TRUE),
                   
                   mean(investor_seller, na.rm = TRUE),
                   mean(private_investor_seller, na.rm = TRUE),
                   mean(non_private_investor_seller, na.rm = TRUE),
                   mean(owner_occupant_seller, na.rm = TRUE),
                   mean(second_house_seller, na.rm = TRUE),
                   
                   mean(private_investor_buyer, na.rm = TRUE),
                   mean(indicator_first_time_buyer, na.rm = TRUE),
                   mean(second_house_buyer, na.rm = TRUE),
                   mean(owner_occupant_buyer, na.rm = TRUE)) %>%
  data.frame() %>%
  write.csv2("sentiment_mun_V2.csv")
                   
                   
                   