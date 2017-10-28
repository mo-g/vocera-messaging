#VOCERA VOICE STATES
#Gustavo Gonzalez
#Generates summary data into a file on Vocera alert details
# install.packages("RMySQL")
library("RMySQL")
library("dplyr")
library("reshape2")
library("data.table")
library("plotly")

#GLOBAL VARIABLES#

#DB Connection Details
dbhandle <- dbConnect(MySQL(), user='vocera', password='vocera', dbname = 'vocera_reports', host='10.134.21.14')

#Get Call Data


rs <- dbSendQuery(dbhandle, "SELECT vmi.TxMillis, vmi.TxDateTime, vmi.TxDate, vmi.ClientID, vmi.DestinationType, vmi.DestinationSite, vmi.DestinationID, vmi.DestinationName, vmi.MessageID, vmi.Priority, vmi.MessageText, vma.ActionType, vma.ActionDetail
FROM vmimessages vmi 
                  INNER JOIN vmiactions vma
                  ON vmi.TxMillis = vma.TxMillis
                  WHERE vmi.TXDate > '2017-10-05'")
All_alerts <- fetch(rs, n=-1)

#Close DB Connection
dbDisconnect(dbhandle)

#Convert dataframes into a data table
CVH_alerts_summary <- filter(All_alerts, All_alerts$DestinationSite =="CVH" & All_alerts$DestinationName == "Code Blue Team")
CRG_alerts_summary <- filter(All_alerts, All_alerts$DestinationSite =="CRG" & All_alerts$DestinationName == "Code Blue Team")
RJH_alerts_summary <- filter(All_alerts, All_alerts$DestinationSite =="RJH" & grepl("Code Blue", MessageText))
All_alerts_summary <- filter(All_alerts, All_alerts$DestinationName == "Code Blue Team" | grepl("Code Blue", MessageText))

#CVH summary breakdown by code and code type
CVH_alerts_summary <- data.frame(table(CVH_alerts_summary$TxDate, CVH_alerts_summary$DestinationName, CVH_alerts_summary$MessageText ,CVH_alerts_summary$ActionType))
CVH_alerts_summary <- dcast(CVH_alerts_summary, Var1 + Var2 ~ Var3, value.var = 'Freq')
CVH_alerts_summary$Daily_Total <- rowSums(CVH_alerts_summary[,3:ncol(CVH_alerts_summary)])
CVH_alerts_summary[nrow(CVH_alerts_summary)+1,] <- c(NA,NA,colSums(CVH_alerts_summary[,3:ncol(CVH_alerts_summary)]))
names(CVH_alerts_summary)[1]<-"Date"
names(CVH_alerts_summary)[2]<-"Group"
write.csv(CVH_alerts_summary, paste('C:/Users/Public/NIH-Reporting/CVH_daily_detailed_alert_summary_', format(Sys.time(), "%Y_%m_%d"), ".csv"))

#CRG summary breakdown by code and code type
CRG_alerts_summary <- data.frame(table(CRG_alerts_summary$TxDate, CRG_alerts_summary$DestinationName, CRG_alerts_summary$MessageText ,CRG_alerts_summary$ActionType))
CRG_alerts_summary <- dcast(CRG_alerts_summary, Var1 + Var2 ~ Var3, value.var = 'Freq')
CRG_alerts_summary$Daily_Total <- rowSums(CRG_alerts_summary[,3:ncol(CRG_alerts_summary)])
CRG_alerts_summary[nrow(CRG_alerts_summary)+1,] <- c(NA,NA,colSums(CRG_alerts_summary[,3:ncol(CRG_alerts_summary)]))
names(CRG_alerts_summary)[1]<-"Date"
names(CRG_alerts_summary)[2]<-"Group"
write.csv(CRG_alerts_summary, paste('C:/Users/Public/NIH-Reporting/CRG_daily_detailed_alert_summary_', format(Sys.time(), "%Y_%m_%d"), ".csv"))

#RJH summary breakdown by code and code type
RJH_alerts_summary <- data.frame(table(RJH_alerts_summary$TxDate, RJH_alerts_summary$DestinationName, RJH_alerts_summary$MessageText))
RJH_alerts_summary <- dcast(RJH_alerts_summary, Var1 + Var2 ~ Var3, value.var = 'Freq')
RJH_alerts_summary$Daily_Total <- rowSums(RJH_alerts_summary[,3:ncol(RJH_alerts_summary)])
RJH_alerts_summary[nrow(RJH_alerts_summary)+1,] <- c(NA,NA,colSums(RJH_alerts_summary[,3:ncol(RJH_alerts_summary)]))
names(RJH_alerts_summary)[1]<-"Date"
names(RJH_alerts_summary)[2]<-"Group"
write.csv(RJH_alerts_summary, paste('C:/Users/Public/NIH-Reporting/RJH_daily_detailed_alert_summary_', format(Sys.time(), "%Y_%m_%d"), ".csv"))

#All Alerts breakdown
All_alerts_summary_Complete <- data.frame(table(All_alerts_summary$DestinationSite, All_alerts_summary$TxDate))
All_alerts_summary <- dcast(All_alerts_summary_Complete, Var2 ~ Var1, value.var = 'Freq')
All_alerts_summary$Daily_Total <- rowSums(All_alerts_summary[,2:ncol(All_alerts_summary)])
All_alerts_summary[nrow(All_alerts_summary)+1,] <- c(NA,colSums(All_alerts_summary[,2:ncol(All_alerts_summary)]))
names(All_alerts_summary)[1]<-"Date"
write.csv(All_alerts_summary, paste('C:/Users/Public/NIH-Reporting/All_daily_alert_summary_', format(Sys.time(), "%Y_%m_%d"), ".csv"))

#plot with plotly
x <- list(
  title = ""
)
y <- list(
  title = "Number of Alerts"
)
p <- plot_ly(x= All_alerts_summary_Complete$Var2, y= All_alerts_summary_Complete$Freq, type="bar", color = All_alerts_summary_Complete$Var1, split = All_alerts_summary_Complete$Var1)%>%
  layout(xaxis = x, yaxis = y, title = "Code Blue Alerts by Site")
htmlwidgets::saveWidget(p, file = "C:/Users/Public/NIH-Reporting/Vocera-Code-Blue-by-Site.html")
p
