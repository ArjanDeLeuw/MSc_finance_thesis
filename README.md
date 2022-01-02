# MSc_finance_thesis
 Contains all R- and Latex-scripts for the MSc Finance thesis on sentiment in the housing market.
 Transaction data and the sentiment index are protected by the Dutch Land Registry Office and TU Delft and cannot be distributed. 


# links: 
Overleaf: https://www.overleaf.com/read/fknznnrdqcqj 

Github: https://github.com/ArjanDeLeuw/MSc_finance_thesis


# R-scripts:  
in 0 merging indices.R the indices for Equation 1 are imported and merged based on date. 

in 1 explore and mutate indices the dataset ALL.indices is prepared for the AR and VAR model: seasonality, logs, differencing, outliers, and descriptives are produced: table 1, figure 1, and plots/correlation matrices for each index that are not presented in the thesis. 

in 2 estimate models 1 to 4.R models one to four are estimated and exported  (equation 1, table 4). 3 models with 3, 6, and 12 lags are estimated (not represented in the thesis). the VAR model is estimated and figure 2, figure 3, Appendix A.1, Appendix D are produced.

in 3 hedonic model.R the transaction data is explored and mutated and the hedonic models without sentiment are estimated (equation 3). Table 2/5 and  Appendix A.2/C.1 are produced.

in 4 hedonic sentiment model.R the transaction data is merged the indices based on transaction month t.  As a result, we will have a file with the level of non-fundamental sentiment for each transaction i in month t. Ten deciles are created based on non-fundamental sentiment and the corresponding Figures 4, 5, 6 and Appendix B are produced. In addition, hedonic sentiment models are estimated to produce table 5, 6, and 7 and Appendix C.2 (equation 4 and 5).

in 5 spatial analysis.R data is aggregated to a municipality level to produce maps in Arc.GIS (figure and Appendix A.3).


# LaTex MSc Finance Thesis: 
contains all writings and figures. 


# Indices: 
All indices used in this study except the sentiment index. 

