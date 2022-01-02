
################################################################################
#################################### STEP 0 ####################################
################################################################################
# in 0 merging indices.R the indices for Equation 1 are imported and merged 
#   based on date. 


rm(list=ls()) # Clean workspace
cat("\014") # clear console
if(!is.null(dev.list())) dev.off()
gc()

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
#install.packages("dynlm")
#install.packages("plm")
#install.packages("plyr")

library(plyr)
library(lubridate)
library(foreign)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(kableExtra)
library(tseries)
library(vars)
library(broom)
library(sandwich)
library(knitr)
library(estimatr)
library(car)
library(sandwich)
library(estimatr)
library(dynlm)


################################################################################
############################# STEP 1: IMPORT DATA ##############################
################################################################################


setwd ("/Users/arjandeleuw/R projects/MSc_Finance/indices")


############################ VEH markt-indicator ################################
OTB.VEH.sentiment.index <- read.csv2("Gemiddelde score EHMI en onderliggende factoren 2004-2021 (6-10-2021).csv", 
    header = T, sep = ",")


OTB.VEH.sentiment.index$period 
OTB.VEH.sentiment.index$period = (seq(as.Date("2004-04-01"), 
                                      by = "month", 
                                      length.out = 211))
OTB.VEH.sentiment.index$period 

####################### CBS population house indices ###########################
### CBS housing data
CBS.bestaande.koopwoningen.en.verkoopprijzen <- read.csv2("CBS M Bestaande koopwoningen en verkoopprijzen.csv", 
    header = T, sep = ";")

names(CBS.bestaande.koopwoningen.en.verkoopprijzen) <- c("CBS_period",
                                                         "CBS_transaction_price_index_existing_houses_baseyear_2015",
                                                         "CBS_monthly_percentage_change_transaction_price_index",
                                                         "CBS_annual_percentage_change_index",
                                                         "CBS_volume","CBS_monthly_change_in_volume",
                                                         "CBS_annual_change_in_volume",
                                                         "CBS_average_transaction_price_in_euros",
                                                         "CBS_total_value_transaction_prices_mln_euros")

# set date
attach(CBS.bestaande.koopwoningen.en.verkoopprijzen)
CBS_period
CBS.bestaande.koopwoningen.en.verkoopprijzen$period = (seq(as.Date("1995-01-01"), 
                                                           by = "month", 
                                                           length.out = 320))
period
detach(CBS.bestaande.koopwoningen.en.verkoopprijzen)

# remove CBS_period 
CBS.bestaande.koopwoningen.en.verkoopprijzen <- CBS.bestaande.koopwoningen.en.verkoopprijzen[, 2:10]


### CBS popluation data
CBS.bevolkingsontwikkeling <- read.csv2("CBS M Bevolkingsontwikkeling.csv",
    header = T, sep = ";")

names(CBS.bevolkingsontwikkeling) <- c("CBS_period",
                                       "CBS_population_begin_period",
                                       "CBS_number_of_births",
                                       "CBS_number_of_deaths",
                                       "CBS_immigration_counts",
                                       "CBS_emigration_counts",
                                       "CBS_corrections_counts",
                                       "CBS_population_growth",
                                       "CBS_population_end_period")


# set date 
attach(CBS.bevolkingsontwikkeling)
CBS_period
CBS.bevolkingsontwikkeling$period = (seq(as.Date("1995-01-01"), 
                                         by = "month",
                                         length.out = 320))
period
detach(CBS.bevolkingsontwikkeling)

# remove CBS_period 
CBS.bevolkingsontwikkeling <- CBS.bevolkingsontwikkeling[, 2:10]


### CBS Unemployment data 
CBS.unemployment.data <- read.csv2("CBS M Unemployment.csv", 
                                   header = T, sep = ";")

# keep period and unemployment rate 
CBS.unemployment.data <- CBS.unemployment.data[,c("Periods","Unemployment.rate.Seasonally.adjusted....")]
names(CBS.unemployment.data) <- c("period", "CBS_unemployment_rate")

# convert date to the correct format
CBS.unemployment.data$period 
CBS.unemployment.data$period  <- parse_date_time(CBS.unemployment.data$period , 
                                                 "%Y%B")
CBS.unemployment.data$period  <- as.Date(CBS.unemployment.data$period )
CBS.unemployment.data$period 


### CBS Newly built houses from 2012 onwards
CBS.housing.supply.data <- read.csv2("CBS M housing supply.csv", 
                                                          header = T, sep = ";")
CBS.housing.supply.data <- CBS.housing.supply.data[,c("Perioden", "Nieuwbouw..aantal.")]


# set date 
attach(CBS.housing.supply.data)
Perioden
CBS.housing.supply.data$Perioden = (seq(as.Date("2012-01-01"), 
                                        by = "month", 
                                        length.out = 116))
Perioden
detach(CBS.housing.supply.data)

names(CBS.housing.supply.data) <- c("period", "CBS_newly_built_houses")


############################## DNB mortgages  ##################################
### key indicators monetary statistics 
DNB.kernindicatoren.monetaire.statistieken <- read.csv2("DNB M Kernindicatoren monetaire statistieken (Maand).csv",
    header = T, sep = ",")

### set date
attach(DNB.kernindicatoren.monetaire.statistieken)
Periode
DNB.kernindicatoren.monetaire.statistieken$period = as.character(paste0(Periode, "-01"))
DNB.kernindicatoren.monetaire.statistieken$period <- as.Date(DNB.kernindicatoren.monetaire.statistieken$period, format = "%Y-%m-%d")
period
detach(DNB.kernindicatoren.monetaire.statistieken)

### mortgages in millions of euro's or counts
temp1 <- filter(DNB.kernindicatoren.monetaire.statistieken,
                Instrument == "Woninghypotheken " &
                InstrumentSub == "Uitstaande bedragen (mln euro's) " &
                Sector == "Huishoudens ")

# keep period and waarde, set to numeric, and convert name
temp1 <- temp1[c("period","waarde")]
temp1$waarde <- as.numeric(temp1$waarde)
names(temp1) <- c("period", "DNB_woninghypotheken_huishoudens_uitstaand_in_mln") 


### mortgages in millions of euro's or counts
temp2 <- filter(DNB.kernindicatoren.monetaire.statistieken,
                Instrument == "Woninghypotheken " &
                InstrumentSub == "Nieuwe contracten, inclusief heronderhandelingen (mln euro's) " &
                Sector == "Huishoudens * ")

# keep period and waarde, set to numeric, and convert name
temp2 <- temp2[c("period","waarde")]
temp2$waarde <- as.numeric(temp2$waarde)
names(temp2) <- c("period","DNB_woninghypotheken_huishoudens_nieuwe_contracten_inclusief_heronderhandelingen") 

### New loans for house purchase
# vanaf 2014!!!
temp3 <- filter(DNB.kernindicatoren.monetaire.statistieken,
                Instrument == "Woninghypotheken " &
                InstrumentSub == "Zuiver nieuwe leningen (d.w.z. exclusief heronderhandelingen) (mln euro’s) " &
                Sector == "Huishoudens * ")

# keep period and waarde, set to numeric, and convert name
temp3 <- temp3[c("period","waarde")]
temp3$waarde <- as.numeric(temp3$waarde)
names(temp3) <- c("period","DNB_woninghypotheken_huishoudens_zuiver_nieuwe_leningen") 


### interest on current loans (%)
temp4 <- filter(DNB.kernindicatoren.monetaire.statistieken,
                Instrument == "Woninghypotheken " &
                InstrumentSub == "Rente op uitstaande bedragen (percentages) " &
                Sector == "Huishoudens ")

# keep period and waarde, set to numeric, and convert name
temp4 <- temp4[c("period","waarde")]
temp4$waarde <- as.numeric(temp4$waarde)
names(temp4) <- c("period","DNB_R_woninghypotheken_huishoudens_uitstaande_rente")

### interest on newly made loans and renegotiated contracts
temp5 <- filter(DNB.kernindicatoren.monetaire.statistieken,
                Instrument == "Woninghypotheken " &
                InstrumentSub == "Rente op nieuwe contracten, inclusief heronderhandelingen (percentages) " &
                Sector == "Huishoudens * ")

# keep period and waarde, set to numeric, and convert name
temp5 <- temp5[c("period","waarde")]
temp5$waarde <- as.numeric(temp5$waarde)
names(temp5) <- c("period","DNB_R_woninghypotheken_huishoudens_nieuwe_contracten_inclusief_heronderhandelingen_rente")


### interest on clean newly made loans (zuiver)
temp6 <- filter(DNB.kernindicatoren.monetaire.statistieken,
                Instrument == "Woninghypotheken " &
                InstrumentSub == "Rente op zuiver nieuwe leningen (d.w.z. exclusief heronderhandelingen) (percentages) " &
                Sector == "Huishoudens ")

# keep period and waarde, set to numeric, and convert name
temp6 <- temp6[c("period","waarde")]
temp6$waarde <- as.numeric(temp6$waarde)
names(temp6) <- c("period","DNB_R_woninghypotheken_huishoudens_zuiver_nieuwe_leningen_rente")


### merge data to DNB.indices
# create sequence of dates in the format that corresponds to the imported files: 
DNB.indices <- data.frame(period = seq(as.Date("1995-01-01"), 
                                       by = "month", 
                                       length.out = 324))

# left_join all indices and remove temp*
DNB.indices <- join_all(list(temp1, temp2, temp3, temp4, temp5, temp6),
                             by = "period",
                             type = "left")
                        
rm(temp1, temp2, temp3, temp4, temp5, temp6, 
   DNB.kernindicatoren.monetaire.statistieken)     


################################### ECB ########################################
### AAR households NL 
ECB.AAR.rate.households.netherlands <- read.csv2("ECB M AAR rate households NL .csv",
    header = T, sep = ",")

### growth rate new loans to non-financials NL
ECB.annual.growth.rate.new.loans.households.non.financials.netherlands <- read.csv2("ECB M Annual growth rate of new loans to households and non-financial corporations.csv",
    header = T, sep = ",")

### HCIP - overall index EU  
ECB.HCIP.overall.index.EU <- read.csv2("ECB M HCIP EU .csv",
    header = T, sep = ",")
ECB.HCIP.overall.index.EU <- ECB.HCIP.overall.index.EU[,0:2]

### HCIP Netherlands, 2015 = 100
ECB.HCIP.overall.index.NL <- read.csv2("ECB M HCIP Netherlands.csv",
    header = T, sep = ",")

### HCIP.Netherlands.rental 
ECB.HCIP.housing.rentals.index.NL <- read.csv2("ECB M HCIP housing rentals.csv",
                                               header = T, sep = ",")

### interest rate component of euro area m/m change 
ECB.interest.rate.component.euro.area <- read.csv2("ECB M Interest rate component of euro area month-to-month level change .csv",
    header = T, sep = ",")

### variable rate loans / total loans for house purchase
ECB.share.variable.rate.mortgages <- read.csv2("ECB M Netherlands, share of variable rate loans in total loans for house purchase .csv",
    header = T, sep = ",")


### merge indices
# create sequence of dates in the format that corresponds to the imported files: 
ECB.indices <- data.frame(period = seq(as.Date("1995-01-01"), 
                                       by = "month", 
                                       length.out = 324))
ECB.indices$period <- as.character(ECB.indices$period, format = "%Y%b")


# left_join all indices
ECB.indices <- join_all(list(ECB.indices, 
                             ECB.interest.rate.component.euro.area, 
                             ECB.AAR.rate.households.netherlands, 
                             ECB.HCIP.overall.index.EU, 
                             ECB.HCIP.overall.index.NL,
                             ECB.HCIP.housing.rentals.index.NL,
                             ECB.share.variable.rate.mortgages, 
                             ECB.annual.growth.rate.new.loans.households.non.financials.netherlands),
                        by = "period",
                        type = "left")


# convert date to the correct format
ECB.indices$period 
ECB.indices$period <- parse_date_time(ECB.indices$period, "%Y%b")
ECB.indices$period <- as.Date(ECB.indices$period)
ECB.indices$period


# remove unnecessary variables
rm(ECB.interest.rate.component.euro.area, 
   ECB.AAR.rate.households.netherlands, 
   ECB.HCIP.overall.index.EU, 
   ECB.HCIP.overall.index.NL, 
   ECB.share.variable.rate.mortgages, 
   ECB.annual.growth.rate.new.loans.households.non.financials.netherlands)


################################### FRED #######################################
### M long term bond yields NL 
FRED.long.term.bond.yields.NL <- read.csv2("FRED M  Long term bond yields NL.csv",
    header = T, sep = ",")

# set correct date for left_join
FRED.long.term.bond.yields.NL$period <- as.Date(FRED.long.term.bond.yields.NL$period, 
                                                format = "%d/%m/%Y")
FRED.long.term.bond.yields.NL$period <- as.character(FRED.long.term.bond.yields.NL$period)

### FRED CPI NL 
FRED.CPI.NL <- read.csv2("FRED M CPI netherlands .csv",
    header = T, sep = ",")

### FRED GDP NL 
FRED.GDP.NL <- read.csv2("FRED M GDP.csv",
    header = T, sep = ",")

### FRED Unemployment rate NL 
FRED.unemployment.rate.NL <- read.csv2("FRED M Unemployment rate.csv",
    header = T, sep = ",")

### merge indices
# create sequence of dates in the format that corresponds to the imported files: 
FRED.indices <- data.frame(period = seq(as.Date("1995-01-01"), 
                                        by = "month", 
                                        length.out = 324))
FRED.indices$period <- as.character(ECB.indices$period)


# left_join all indices
FRED.indices <- join_all(list(FRED.indices, 
                              FRED.long.term.bond.yields.NL,
                              FRED.CPI.NL,
                              FRED.GDP.NL,
                              FRED.unemployment.rate.NL),
                         by = "period",
                         type = "left")


# convert date to the correct format
FRED.indices$period 
FRED.indices$period <- parse_date_time(FRED.indices$period, "%Y-%m-%d")
FRED.indices$period <- as.Date(FRED.indices$period)
FRED.indices$period


# remove unnecessary variables
rm(FRED.CPI.NL, 
   FRED.GDP.NL, 
   FRED.long.term.bond.yields.NL, 
   FRED.unemployment.rate.NL)


################################### OECD #######################################

### OECD long term interest rates
OECD.long.term.interest.rates <- read.csv2("OECD M Long term interest rates.csv",
    header = T, sep = ",")

### OECD short term interest rates
OECD.short.term.interest.rates <- read.csv2("OECD M Short term interest rates.csv",
    header = T, sep = ",")

### merge indices 
# create sequence of dates in the format that corresponds to the imported files: 
OECD.indices <- data.frame(period = seq(as.Date("1995-01-01"), 
                                        by = "month", 
                                        length.out = 324))
OECD.indices$period <- as.character(OECD.indices$period)

# left_join all indices
OECD.indices <- join_all(list(OECD.long.term.interest.rates,
                              OECD.short.term.interest.rates),
                         by = "period",
                         type = "left")


# remove unnecessary variables
rm(OECD.long.term.interest.rates,
   OECD.short.term.interest.rates)


################################# EUROSTAT #####################################
## Construction Cost Index (CCI)
EUROSTAT_CCI <- read.csv2("EUROSTAT_M_construction_cost_index_NL.csv",
                          header = T, sep = ",")

## set correct date format 
EUROSTAT_CCI$period 
EUROSTAT_CCI$period <- parse_date_time(EUROSTAT_CCI$period, "%Y-%m")
EUROSTAT_CCI$period <- as.Date(EUROSTAT_CCI$period)
EUROSTAT_CCI$period


########################### WONINGBOUWCIJFERSNL ################################
## Construction Cost Index (CCI)
WONINGBOUWCIJFERSNL_newly_built_houses_sales <- read.csv2("WONINGBOUWERSNL M Newly Built Houses Sales.csv",
                          header = T, sep = ",")

WONINGBOUWCIJFERSNL_newly_built_houses_sales$period
WONINGBOUWCIJFERSNL_newly_built_houses_sales$period <- parse_date_time(WONINGBOUWCIJFERSNL_newly_built_houses_sales$period, "%d/%m/%Y")
WONINGBOUWCIJFERSNL_newly_built_houses_sales$period <- as.Date(WONINGBOUWCIJFERSNL_newly_built_houses_sales$period)
WONINGBOUWCIJFERSNL_newly_built_houses_sales$period


################################################################################
########################### STEP 2: MERGE INDICES ##############################
################################################################################
### create frame
ALL.indices <- data.frame(value = seq(as.Date("1995-01-01"), 
                                      by = "month", 
                                      length.out = 324))

# left_join all indices
ALL.indices <- join_all(list(CBS.bestaande.koopwoningen.en.verkoopprijzen,
                             CBS.bevolkingsontwikkeling,
                             CBS.unemployment.data,
                             CBS.housing.supply.data,
                             DNB.indices,
                             ECB.indices,
                             FRED.indices,
                             OECD.indices,
                             OTB.VEH.sentiment.index,
                             EUROSTAT_CCI,
                             WONINGBOUWCIJFERSNL_newly_built_houses_sales),
                        by = "period",
                        type = "left")

# convert character variables to integers
str(ALL.indices)
ALL.indices[sapply(ALL.indices, is.character)] <- 
  data.frame(lapply(ALL.indices[sapply(ALL.indices, is.character)], as.numeric)) # remember: sapply = (vector)apply, lapply = (list)apply
ALL.indices[sapply(ALL.indices, is.integer)] <- 
  data.frame(lapply(ALL.indices[sapply(ALL.indices, is.integer)], as.numeric))
str(ALL.indices)
 

################################################################################
################################# References ###################################
################################################################################
# CBSwoningen"  https://opendata.cbs.nl/#/CBS/nl/dataset/83906NED/table?ts=1633936601490
# CBS populatie: https://opendata-cbs-nl.proxy-ub.rug.nl/statline/#/CBS/nl/dataset/83474NED/table?ts=1634026723497
# CBS woningvorrad (nieuwbouw): https://opendata-cbs-nl.proxy-ub.rug.nl/#/CBS/nl/dataset/81955NED/table?dl=18232
# DNB kernindicatoren: https://www.dnb.nl/statistieken/data-zoeken/#/details/kernindicatoren-monetaire-statistieken-maand/dataset/b698ca40-9cae-435b-954e-4fe2c5651370/resource/a8df8430-d941-4706-907b-efd5a9c0bc00
# ECB AAR households: https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=124.MIR.M.NL.B.A2C.AM.R.A.2250.EUR.N
# ECB new loans:  https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=304.RAI.M.NL.GRNLHHNFC.EUR.MIR.Z
# ECB HCIP EU: https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=122.ICP.M.U2.N.000000.4.ANR
# ECB HCIP NL: https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=122.ICP.M.NL.N.000000.4.INX
# ECB HCIP Rental inex NL: https://sdw.ecb.europa.eu/quickview.do;jsessionid=78FC4D5C7D31F2871AC4059437EEDDD3?SERIES_KEY=122.ICP.M.NL.N.041000.4.INX
# ECB interest rate euro: https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=124.MIR.M.U2.B.A20.A.I.A.2240.EUR.O
# ECB variable rate loans / total loans home purchase: https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=304.RAI.M.NL.SVLHPHH.EUR.MIR.Z
# FRED long term bond yields nL: https://fred.stlouisfed.org/series/IRLTLT01NLM156N
# FRED: CPI NL: https://fred.stlouisfed.org/series/NLDCPIALLMINMEI
# FRED: GDP NL: https://fred.stlouisfed.org/tags/series?t=gdp%3Bnetherlands
# FRED Unemployment rate: https://fred.stlouisfed.org/tags/series?t=netherlands%3Bunemployment
# OECD long-term interest rates: https://data.oecd.org/interest/long-term-interest-rates.htm
# OECD short-term interest rates: https://data.oecd.org/interest/short-term-interest-rates.htm#indicator-chart






