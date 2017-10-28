#VOCERA VOICE STATES
#Gustavo Gonzalez
#Generates summary data into a file on Vocera Voice usage details
# install.packages("RMySQL")
library("RMySQL")
library("dplyr")
library("reshape2")

#GLOBAL VARIABLES#

#DB Connection Details
dbhandle <- dbConnect(MySQL(), user='vocera', password='vocera', dbname = 'vocera_reports', host='10.134.21.14')

#Get Call Data
rs <- dbSendQuery(dbhandle, "select * from Calls where TxDate > '2017-09-10'")
CRG_ds_voice_calls <- fetch(rs, n=-1)
rs <- dbSendQuery(dbhandle, "select * from Calls where TxDate > '2017-09-25'")
CVH_ds_voice_calls <- fetch(rs, n=-1)


#2017-09-21 - Gina. S advised that this extract will no longer be needed, commenting out
# #Get Login Logout Data
# rs <- dbSendQuery(dbhandle, "select * from loginlogout where loginlogout.TxDate > '2017-09-09' AND loginlogout.UserSite = 'CRG' AND loginlogout.Operation = 'login'")
# ds_voice_loginlogout <- fetch(rs, n=-1)

#Close DB Connection
dbDisconnect(dbhandle)

#Include only accepted calls for each site listed below.
CRG_daily_voice_call_summary <- filter(CRG_ds_voice_calls, CRG_ds_voice_calls$Accepted==1 & CRG_ds_voice_calls$UserSite=="CRG" & !CRG_ds_voice_calls$DeviceType=="B2000")
CVH_daily_voice_call_summary <- filter(CVH_ds_voice_calls, CVH_ds_voice_calls$Accepted==1 & CVH_ds_voice_calls$UserSite=="CVH"& !CVH_ds_voice_calls$DeviceType=="B2000")


                                                                                                          
#Generate summary table
CRG_daily_voice_call_summary <- data.frame(table(CRG_daily_voice_call_summary$TxDate, CRG_daily_voice_call_summary$DeviceType))
CVH_daily_voice_call_summary <- data.frame(table(CVH_daily_voice_call_summary$TxDate, CVH_daily_voice_call_summary$DeviceType))

#Pivot and Add Totals
################# CRG ##############################################
CRG_daily_voice_call_summary <- dcast(CRG_daily_voice_call_summary, Var1 ~ Var2)
CRG_daily_voice_call_summary$daily_total <- rowSums(CRG_daily_voice_call_summary[,2:ncol(CRG_daily_voice_call_summary)])
CRG_daily_voice_call_summary[nrow(CRG_daily_voice_call_summary)+1,] <- c(NA, sum(CRG_daily_voice_call_summary$Apple), sum(CRG_daily_voice_call_summary$B3000), sum(CRG_daily_voice_call_summary$Phone), sum(CRG_daily_voice_call_summary$daily_total))
write.csv(CRG_daily_voice_call_summary, paste('C:/Users/Public/NIH-Reporting/CRG_daily_voice_call_summary_', format(Sys.time(), "%Y_%m_%d"), ".csv"))
################# CVH ##############################################
CVH_daily_voice_call_summary <- dcast(CVH_daily_voice_call_summary, Var1 ~ Var2)
CVH_daily_voice_call_summary$daily_total <- rowSums(CVH_daily_voice_call_summary[,2:ncol(CVH_daily_voice_call_summary)])
CVH_daily_voice_call_summary[nrow(CVH_daily_voice_call_summary)+1,] <- c(NA, sum(CVH_daily_voice_call_summary$Apple), sum(CVH_daily_voice_call_summary$B3000), sum(CVH_daily_voice_call_summary$Phone), sum(CVH_daily_voice_call_summary$daily_total))
write.csv(CVH_daily_voice_call_summary, paste('C:/Users/Public/NIH-Reporting/CVH_daily_voice_call_summary_', format(Sys.time(), "%Y_%m_%d"), ".csv"))


#2017-09-21 - Gina. S advised that this extract will no longer be needed, commenting out
#Find unique logins by user by date
# daily_loginlogout_summary <- ds_voice_loginlogout
# daily_loginlogout_summary <- unique(daily_loginlogout_summary[c("TxDate", "UserID")])
# 
# #Summarize and create log in file summary
# daily_loginlogout_summary <- data.frame(table(daily_loginlogout_summary$TxDate))
# colnames(daily_loginlogout_summary) <- c("date", "#_login_logout")
# daily_loginlogout_summary[nrow(daily_loginlogout_summary)+1,] <- c(NA, sum(daily_loginlogout_summary$`#_login_logout`))
# write.csv(daily_loginlogout_summary, paste('reporting/daily_voice_loginlogout_summary_', format(Sys.time(), "%Y_%m_%d"), ".csv"))
