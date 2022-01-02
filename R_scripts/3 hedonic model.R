
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


### prepare environment
cat("\014") # clear console
if(!is.null(dev.list())) dev.off() # clear plots
rm(list=ls()) # Clean workspace
gc()


setwd("V:/GVA/MA_Maatwerk en Advies/Publicaties en Presentaties/2021/2021 - Stage Arjan de Leuw/R_project_sentiment")


### install.packages
#install.packages("microbenchmark")
#install.packages("foreign")
#install.packages("tidyverse")
#install.packages("Hmisc") 
#install.packages("reshape2")
#install.packages("moments")
#install.packages("ggplot2")
#install.packages("kableExtra")
#install.packages("tseries")
#install.packages("vars")
#install.packages("stargazer")
#install.packages("broom")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("knitr")
#install.packages("estimatr")
#install.packages("SjPlot")
#install.packages("lubridate")
#install.packages("dynlm")
#install.packages("plm")
#install.packages("nlme")
#install.packages("lme4")
#install.packages('fastDummies')
#install.packages("stringr")
#install.packages("statmod")
#install.packages("MuMIn")


### libraries
#library(microbenchmark)
#library(foreign)
library(tidyverse)
#library(reshape2)
library(moments)
library(ggplot2)
#library(kableExtra)
#library(tseries)
#library(vars)
library(stargazer)
library(lmtest)
library(broom)
library(sandwich)
library(knitr)
library(estimatr)
library(car)
library(sandwich)
library(estimatr)
library(lubridate)
#library(dynlm) 
#library(plm)
library(lme4)
library(fastDummies)
library(stringr)
library(statmod)
library(MuMIn)


################################################################################
############################ STEP 1: IMPORT DATA  ##############################
################################################################################

### Import
# transaction data 
BWD012 <- read.csv2("20210804_BWD_012_PART_WONINGTRANS.txt")

# COROP regions
COROP.df <- read.csv2("uitvoer_gem_indeling_COROP_gebieden_g40_g4.csv")

# subject information 
#buyers.experience.df <- read.csv2("20210921_verkrijgers.txt")

 
### working file
WF.BWD012.df <- BWD012
attach(WF.BWD012.df)


### Remove BWD012
rm(BWD012) ; gc()


### match COROP with BWD012
names(COROP.df) <- c("GM_CODE", "COROP_code", "COROP_name", "G40_indicator", 
                     "G4_indicator", "G44_indicator")

WF.BWD012.df <- plyr::join_all(list(WF.BWD012.df, 
                                    COROP.df), 
                               by = "GM_CODE", 
                               type = "left")

rm(COROP.df); gc()


### match COROP with BWD012
buyers.experience.df <- buyers.experience.df[,c("OVER_ID", "SOORT_NP_NNP", 
                                                "AANTAL_WONING_VK", 
                                                "AANTAL_WONING_VK_D")]
names(buyers.experience.df) <- c("OVER_ID", 
                                 "SOORT_NP_NNP", 
                                 "properties_owned", 
                                 "properies_owned_D")

WF.BWD012.df <- dplyr::right_join(WF.BWD012.df, 
                                    buyers.experience.df, 
                               by = "OVER_ID")


rm(buyers.experience.df); gc()

### explore data
#WF.BWD012.df[0,10]
#summary(WF.BWD012.df)

#ncol(WF.BWD012.df)
#nrow(WF.BWD012.df)
#dim(WF.BWD012.df)

#is.matrix(WF.BWD012.df) 
#is.data.frame(WF.BWD012.df)


### translate NL --> EN 
ls(WF.BWD012.df)
names(WF.BWD012.df) <- c("object_ID", "subject_ID", "certificate_number", 
                         "receivement_date", "object_number", "parcel_size", 
                         "birth_date_buyer", "number_of_houses_buyer",
                         "number_of_houses_buyer_d", "number_of_houses_seller",
                         "VBO_ID", "PHT_code", "year_of_construction", 
                         "housetype","property_size", "transaction_price", 
                         "indicator_mog", "number_of_houses_transacted", 
                         "municipality_code", "municipality name", 
                         "district_code", "district_name", "neighborhood_code", 
                         "neighborhood_name", "bel_buyer_ID","bel_seller_ID", 
                         "indicator_newly_built", "indicator_buyer",
                         "indicator_buyer_1", "indicator_seller", 
                         "indicator_seller_1", "indicator_reliable_TP",
                         "indicator_OV03", "indicator_first_time_buyer", 
                         "COROP_code","COROP_name", "G40_indicator", 
                         "G4_indicator", "G44_indicator")

WF.BWD012.df <- WF.BWD012.df[,c("receivement_date", "parcel_size", 
                                "number_of_houses_buyer", "number_of_houses_buyer_d", 
                                "number_of_houses_seller", "year_of_construction", 
                                "housetype", "property_size", "transaction_price", 
                                "number_of_houses_transacted","municipality_code", 
                                "municipality name", "district_code", "district_name", 
                                "neighborhood_code", "neighborhood_name",  
                                "indicator_newly_built", "indicator_buyer",
                                "indicator_buyer_1", "indicator_seller", 
                                "indicator_seller_1", "indicator_reliable_TP",
                                "indicator_OV03", "indicator_first_time_buyer", 
                                "COROP_code","COROP_name", "G40_indicator", 
                                "G4_indicator", "G44_indicator")]


################################################################################
############################# STEP 2: Mutations ################################
################################################################################
attach(WF.BWD012.df)

############################## Date-formats ####################################
# transaction date and birht_date_buyer
# format the transaction date and birth_date_buyer to a YYYY-MM-DD format.
WF.BWD012.df$receivement_date <- as.Date(as.character(receivement_date), 
                                         format = "%Y%m%d")
#WF.BWD012.df$birth_date_buyer <- as.Date(as.character(birth_date_buyer), 
#                                         format = "%Y%m%d")


### transaction year (Y)
# transaction_year as character
WF.BWD012.df$transaction_year <- 
  format(WF.BWD012.df$receivement_date, 
         format = "%Y")

# transaction_year as date format
WF.BWD012.df$temp <- parse_date_time(WF.BWD012.df$transaction_year, "y")
WF.BWD012.df$transaction_year <- as.Date.POSIXct(WF.BWD012.df$temp, 
                                                 format = "%Y")

# as character and numerical 
WF.BWD012.df$transaction_year_num <- as.numeric(WF.BWD012.df$transaction_year)


### transaction month (YM)
# transaction_month as character
WF.BWD012.df$transaction_month <- 
  format(WF.BWD012.df$receivement_date, format = "%Y%m")

# transaction_month in date format
WF.BWD012.df$temp <- parse_date_time(WF.BWD012.df$transaction_month, "ym")
WF.BWD012.df$transaction_month <- as.Date.POSIXct(WF.BWD012.df$temp, 
                                                  format = "%Y%m")

# as character and numerical 
WF.BWD012.df$transaction_month_num <- as.numeric(WF.BWD012.df$transaction_month)

# remove temp 
WF.BWD012.df <- WF.BWD012.df[, c(1:37, 39:43)]


### transaction quarter 
WF.BWD012.df$transaction_quarter <- zoo::as.yearqtr(WF.BWD012.df$transaction_month, 
                                                    format = "%Y%m%d")
WF.BWD012.df$transaction_quarter <- lubridate::quarter(ymd(transaction_month), 
                                                       with_year = TRUE)

########################## log(skewed variables) ###############################
attach(WF.BWD012.df)
attach(WF.BWD012.df)
### Transaction price
# log 
hist(transaction_price)
WF.BWD012.df$LN_transaction_price <- log(transaction_price)
hist(LN_transaction_price)

# rename NaN and inf
WF.BWD012.df$LN_transaction_price[which(is.nan(WF.BWD012.df$LN_transaction_price))]= NA
WF.BWD012.df$LN_transaction_price[which(WF.BWD012.df$LN_transaction_price==-Inf)] = NA


### parcel size in m2
hist(parcel_size)
WF.BWD012.df$LN_parcel_size <- log(parcel_size + 0.001)
hist(LN_parcel_size)

# rename NaN and inf
WF.BWD012.df$LN_parcel_size[which(is.nan(WF.BWD012.df$LN_parcel_size))] = NA
WF.BWD012.df$LN_parcel_size[which(WF.BWD012.df$LN_parcel_size == -Inf)] = NA


### property size 
hist(property_size)
WF.BWD012.df$LN_property_size <- log(property_size + 0.001)
hist(LN_property_size)

# rename NaN and inf
WF.BWD012.df$LN_property_size[which(is.nan(WF.BWD012.df$LN_property_size))] = NA
WF.BWD012.df$LN_property_size[which(WF.BWD012.df$LN_property_size == -Inf)] = NA


############################ Dummies (factors) #################################
attach(WF.BWD012.df)

### Year of construction dummies
WF.BWD012.df$year_of_construction_1945 <- 
  factor(ifelse(year_of_construction <= 1945, 1, 0)) 

WF.BWD012.df$year_of_construction_1945_1980 <- 
  factor(ifelse(year_of_construction > 1945 & year_of_construction <= 1980, 1, 0))

WF.BWD012.df$year_of_construction_1980_2000 <- 
  factor(ifelse(year_of_construction > 1980 & year_of_construction <= 2000, 1, 0))

WF.BWD012.df$year_of_construction_2000 <- 
  factor(ifelse(year_of_construction >= 1980, 1, 0))


contrasts(year_of_construction_1945)
contrasts(year_of_construction_1945_1980)
contrasts(year_of_construction_1980_2000)
contrasts(year_of_construction_2000)


### other indicators
# convert the factor variables to factors. 
WF.BWD012.df$indicator_buyer <- factor(indicator_buyer) 
WF.BWD012.df$indicator_seller <- factor(indicator_seller)
WF.BWD012.df$indicator_reliable_TP <- factor(indicator_reliable_TP)
WF.BWD012.df$housetype <- factor(housetype)

# inspect new variables
contrasts(WF.BWD012.df$indicator_buyer)
contrasts(WF.BWD012.df$indicator_seller)
contrasts(WF.BWD012.df$indicator_reliable_TP)
contrasts(WF.BWD012.df$housetype)


### type of buyer and seller dummies
# buyer dummies
summary(indicator_buyer_1)

WF.BWD012.df$owner_occupant_buyer <- ifelse(WF.BWD012.df$indicator_buyer_1 
                                            == "Eigenaar bewoner", 1, 0)
WF.BWD012.df$private_investor_buyer <- ifelse(WF.BWD012.df$indicator_buyer_1
                                              == "Particuliere Investeerder", 1, 0)
WF.BWD012.df$second_house_buyer <- ifelse(WF.BWD012.df$indicator_buyer_1 
                                          == "Tweede woning", 1, 0)

# seller dummies
summary(indicator_seller_1)

WF.BWD012.df$owner_occupant_seller <- ifelse(WF.BWD012.df$indicator_seller_1 
                                             == "Eigenaar bewoner", 1, 0)
WF.BWD012.df$private_investor_seller <- ifelse(WF.BWD012.df$indicator_seller_1 
                                               == "Particuliere Investeerder", 1, 0)
WF.BWD012.df$second_house_seller <- ifelse(WF.BWD012.df$indicator_seller_1
                                           == "Tweede woning", 1, 0)
WF.BWD012.df$non_private_investor_seller <- ifelse(WF.BWD012.df$indicator_seller_1 %in% c(
  "Bedrijfsmatig", 
  "Bedrijfsmatig institutioneel"),
  1, 0)
WF.BWD012.df$investor_seller <- ifelse(WF.BWD012.df$indicator_seller_1 %in% c( 
  "Bedrijfsmatig", 
  "Particuliere Investeerder",
  "Bedrijfsmatig institutioneel"),
  1, 0)


### factor number of properties owned
WF.BWD012.df$properties_owned_0 <- ifelse(WF.BWD012.df$AANTAL_WONING_VK == 0, 1, 0)
WF.BWD012.df$properties_owned_1 <- ifelse(WF.BWD012.df$AANTAL_WONING_VK == 1, 1, 0)
WF.BWD012.df$properties_owned_25 <- ifelse(WF.BWD012.df$AANTAL_WONING_VK %in% c(2:5), 1, 0)
WF.BWD012.df$properties_owned_610 <- ifelse(WF.BWD012.df$AANTAL_WONING_VK %in% c(6:10), 1, 0 )
WF.BWD012.df$properties_owned_10plus <- ifelse(WF.BWD012.df$AANTAL_WONING_VK %in% c(11:1000), 1, 0)


################################## Misc ###################################### 
# price per sqm
WF.BWD012.df$transaction_price_per_sqm <- transaction_price / property_size

# rename NaN and inf
WF.BWD012.df$transaction_price_per_sqm[which(WF.BWD012.df$transaction_price_per_sqm == Inf)] = NA


################################## Filter ###################################### 
### drop: 
#     newly built housing, 
#     unreliable transaction prices, 
#     transactions in which more than 1 property is transacted
#     unknown property type 

WF.BWD012.df %>% filter(indicator_newly_built == 0 & 
                          indicator_reliable_TP == 1 & 
                          housetype != "" &
                          number_of_houses_transacted == 1 & 
                          ! is.na(LN_property_size) &
                          ! is.na(LN_transaction_price) & 
                          ! is.na(COROP_code) & 
                          housetype != "O") -> WF.BWD012.df


### check for NA, inf, NaN Values
# returns TRUE if there are any of such values 
apply(WF.BWD012.df, 2, function(x) any(is.na(x) | is.infinite(x) | is.nan(x)))


### export and remove all but WF.BWD012.df
# write.csv(WF.BWD012.df, file = "BWD012_v2.csv")
rm(list=ls()[-match("WF.BWD012.df", ls())])

detach(WF.BWD012.df)
gc()


################################## ranking ##################################### 
attach(WF.BWD012.df)

### use ntile to create 100 intervals and group by quarters 
WF.BWD012.df %>%
  group_by(transaction_month) %>%
  mutate(tp_ranked_monthly = dplyr::ntile(transaction_price, 100)) %>%
  as.data.frame() -> WF.BWD012.df

detach(WF.BWD012.df)


################################################################################
####################### STEP 3: DESCRIPTIVE STATISTICS #########################
################################################################################

### summary statistics
# create data.frame
summary.df <- WF.BWD012.df[c("transaction_price", 
                             "transaction_year", 
                             "property_size", 
                             "year_of_construction", 
                             "housetype",
                             "indicator_buyer", 
                             "indicator_seller", 
                             "G40_indicator",
                             "G4_indicator")]

# convert variables in the right format
attach(summary.df)

summary.df$transaction_year <- as.numeric(str_sub(as.character(transaction_year), 1, 4))
summary.df <- (dummy_cols(summary.df, select_columns = "housetype"))
summary.df <- dummy_cols(summary.df, select_columns = "indicator_buyer")
summary.df <- dummy_cols(summary.df, select_columns = "indicator_seller")


### use stargazer for summary statistics:
summary <- stargazer(summary.df, 
                     type = 'latex', 
                     summary = TRUE,
                     na.rm = TRUE,
                     title = "Descriptive statistics", 
                     digits = 3, 
                     omit.summary.stat = c("p25", "p75"))


### Plot Median transaction price 
g <- ggplot(WF.BWD012.df, aes(x = transaction_month, y = transaction_price)) 
g <- g + geom_smooth(stat = "summary", fun = "median", color = "black", group = 1)

g <- g + scale_x_date(date_breaks = "2 year", date_labels = "%Y")
g <- g + scale_y_continuous(labels = function(x) paste0("€" , x/1000))

g <- g + xlab("Transaction Year")
g <- g + ylab("Median Transaction Price (x1000)") 

g <- g + theme_minimal(base_size = 10) # classic is nice as well 
dev.off()

g

rm(summary.df, summary, g); gc()


################################################################################
############################# STEP 4: REGRESSIONS ##############################
################################################################################
# COROP (EU Nuts-3) is the best location dummy. The variation in 
#   characteristics per municipality e.g., population, volume, transaction price
#   is too low. COROP regions (EU Nuts-3) are defined in a statistically 
#   correct way for data-analysis. In addition, if neighborhood and month dummies 
#   are used, zero's are produced in our matrix, because there are neighborhoods 
#   only a few annual transactions. 
# The level of the time dummies however are of interest. The month dummies are 
#   intuitively the best measure, because the sentiment index is on a monthly 
#   basis as well. It is thus logical to use month dummies to correct for any 
#   monthly variation. Below we will estimate a model with month and year dummies. 

attach(WF.BWD012.df)

### hedonic models 1-4
# model without FE
mod0.pooled <- lm(LN_transaction_price ~ LN_property_size + 
                    year_of_construction_1945 +
                    year_of_construction_1945_1980 +
                    year_of_construction_1980_2000 + 
                    housetype,
                  data = WF.BWD012.df)


################################ month dummies ##################################
# fixed effects LSDV, with lm 
#mod1.FE.LSDV.M <- lm(LN_transaction_price ~ LN_property_size + 
#                       #LN_parcel_size + 
#                       year_of_construction_1945 +
#                      year_of_construction_1945_1980 +
#                      year_of_construction_1980_2000 + 
#                      housetype +  
#                      factor(x = as.character(COROP_code)) + 
#                      factor(x = as.character(transaction_quarter)), 
#                    data = WF.BWD012.df)

# the lsdv appraoch in OLS yields the same results compared to the within appraoch
# and approximately the same results for RE using ML (models below)

# fixed effects, location
mod1.FE.within.l <- lme4::lmer(LN_transaction_price ~ LN_property_size + 
                                 year_of_construction_1945 +
                                 year_of_construction_1945_1980 +
                                 year_of_construction_1980_2000 + 
                                 housetype +
                                 (1 | COROP_code),
                                REML = FALSE,
                                data = WF.BWD012.df) 


# fixed effects, location and time
mod2.FE.within.lt <- lme4::lmer(LN_transaction_price ~ LN_property_size + 
                                  year_of_construction_1945 +
                                  year_of_construction_1945_1980 +
                                  year_of_construction_1980_2000 + 
                                  housetype +
                                  (1 | COROP_code) +
                                  (1 | transaction_month),
                                REML = FALSE,
                                data = WF.BWD012.df) 

phtest(mod1.FE.LSDV.Q, mod2.FE.within.lt)

# fixed effects, location, time, and interaction 
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


### output
output_M <- stargazer(mod0.pooled, mod2.FE.within.lt, mod3.FE.within.lti, mod4.FE.within.lti.NFM,
                      type = 'latex', 
                      font.size = "tiny", 
                      dep.var.caption = "",
                      title = "Hedonic model", 
                      digits = 3, 
                      single.row = TRUE, 
                      no.space = TRUE, 
                      keep = c("residuals_baseline_model", "LN_property_size", "LN_parcel_size", 
                               "year_of_construction_1945", 
                               "year_of_construction_1945_1980",  
                               "year_of_construction_1980_2000", "housetype"),
                      add.lines = list(c("Transaction Quarter Dummies:", "NO", "NO",
                                         "YES", "YES"), 
                                       c("COROP Dummies:", "NO", "YES", "YES", "YES"),
                                       c("R2c", "", 
                                         round(r.squaredGLMM(mod2.FE.within.lt)[1,2],2),
                                         round(r.squaredGLMM(mod3.FE.within.lti)[1,2],2), 
                                         round(r.squaredGLMM(mod.plus.FE.within.lti.NFM)[1,2], 3))))


### conclusion 
# Statistically the LSDV or within approach is equal, this is also observed in 
#   both models. the results of the FE (LSDV using LM) is comparable to the RE 
#   model 
# the R2 and in-sample prediction improves for each model. However the difference
#   between a model with interactions between time and location is minor. 


################################################################################
######################## STEP 5: predicted vs real tp ##########################
################################################################################
attach(WF.BWD012.df)

### predict and calculate delta real vs predicted
WF.BWD012.df$predicted_tp_pooled <- exp(predict(mod0.pooled))
WF.BWD012.df$delta_real_predicted_tp_pooled <- transaction_price - WF.BWD012.df$predicted_tp_pooled

### predict and calculate delta real vs predicted
WF.BWD012.df$predicted_tp_lti_nfm <- exp(predict(mod4.FE.within.lti.NFM))
WF.BWD012.df$delta_real_predicted_tp_lti_nfm <- transaction_price - WF.BWD012.df$predicted_tp_lti_nfm


### predict and calculate delta real vs predicted
WF.BWD012.df$predicted_tp_lt <- exp(predict(mod2.FE.within.lt))
WF.BWD012.df$delta_real_predicted_tp_lt <- transaction_price - WF.BWD012.df$predicted_tp_lt


### predict and calculate delta real vs predicted
WF.BWD012.df$predicted_tp_lti <- exp(predict(mod3.FE.within.lti))
WF.BWD012.df$delta_real_predicted_tp_lti <- transaction_price - WF.BWD012.df$predicted_tp_lti


### Plot predicted and real transaction price (tp)
g <- ggplot(WF.BWD012.df, aes(x = transaction_month)) 
g <- g + geom_smooth(aes(y = transaction_price), 
                     stat = "summary", 
                     fun = "median", 
                     color = "blue", 
                     size = 1, 
                     group = 1)
g <- g + geom_smooth(aes(y = predicted_tp_pooled), 
                     stat = "summary", 
                     fun = "median",
                     color = "purple", 
                     size = 1, 
                     group = 1)
g <- g + geom_smooth(aes(y = predicted_tp_lti_nfm), 
                     stat = "summary", 
                     fun = "median", 
                     color = "red", 
                     size = 1.1, 
                     group = 1)
g <- g + geom_smooth(aes(y = predicted_tp_lt), 
                     stat = "summary",
                     fun = "median", 
                     color = "green", 
                     size = 1, 
                     group = 1)
g <- g + geom_smooth(aes(y = predicted_tp_lti),
                     stat = "summary", 
                     fun = "median", 
                     color = "black", 
                     size = 1, 
                     group = 1)

g <- g + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
g <- g + scale_y_continuous(labels = function(x) paste0("??? " , x/1000))

g <- g + xlab("Transaction Year")
g <- g + ylab("Predicted Transaction Price (x1000)")

g <- g + theme_minimal(base_size = 10)
dev.off()

predicted.tp <- g
predicted.tp


### plot difference
g <- ggplot(WF.BWD012.df, aes(x = transaction_month)) 
g <- g + geom_smooth(aes(y = delta_real_predicted_tp_lt), 
                     stat = "summary", 
                     fun = "median", 
                     color = "blue", 
                     size = 1, 
                     group = 1)
g <- g + geom_smooth(aes(y = delta_real_predicted_tp_lti), 
                     stat = "summary", 
                     fun = "median", 
                     color = "black", 
                     size = 1, 
                     group = 1)

g <- g + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
g <- g + scale_y_continuous(labels = function(x) paste0("??? " , x))

g <- g + xlab("Transaction Year")
g <- g + ylab("price")

g <- g + theme_minimal(base_size = 10)
dev.off()

delta.real.predicted <- g 
delta.real.predicted

