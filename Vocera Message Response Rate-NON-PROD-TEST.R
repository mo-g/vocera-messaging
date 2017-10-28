#Library install and setup
# install.packages("ggplot2")
# install.packages("ggbeeswarm")

library(ggplot2)
library(ggbeeswarm)
library("RODBC")
library("beeswarm")

#GLOBAL VARIABLES#
filesource = FALSE
dummydata = TRUE
dbhandle <- odbcDriverConnect('driver={SQL Server};server=10.134.13.36;database=WICMASTER;uid=saVoc;pwd=saVoc45')


#Create datasets from DB with list of consult id's, consult and consult response details.
if (filesource == FALSE){
  write.csv(ds_users <- sqlQuery(dbhandle, "SELECT * FROM [WICMASTER].[dbo].[Users] WHERE ID > 0 AND Login NOT LIKE '%Android%' AND login NOT LIKE 'iOS%' AND Active = 'Y'"), 'dataset/ds_users.csv')
  write.csv(ds_consult_ids <- sqlQuery(dbhandle, "SELECT [ID] FROM [WICMASTER].[dbo].[TextConversations] WHERE Subject IN ('! Urgent Clinical Consultation !', '* Clinical Consultation *')"), 'dataset/ds_consult_ids.csv')
  # ds_consult_ids <- paste(ds_consult_ids$ID, collapse=",")
  write.csv(ds_consult_details <- sqlQuery(dbhandle, paste("SELECT * FROM [WICMASTER].[dbo].[TextMessages] WHERE ConversationID IN (",paste(ds_consult_ids$ID, collapse=","),") AND SeqNo =0", sep="")), 'dataset/ds_consult_details.csv')
  write.csv(ds_consult_response_details <- sqlQuery(dbhandle, paste("SELECT * FROM [WICMASTER].[dbo].[TextMessages] WHERE ConversationID IN (",paste(ds_consult_ids$ID, collapse=","),") AND SeqNo =1", sep="")), 'dataset/ds_consult_response_details.csv')
  odbcCloseAll()
} else{
  ds_users <- read.csv('dataset/ds_users.csv', fileEncoding="UTF-8-BOM")
  ds_consult_ids <- read.csv('dataset/ds_consult_ids.csv', fileEncoding="UTF-8-BOM")
  ds_consult_details <- read.csv('dataset/ds_consult_details.csv', fileEncoding="UTF-8-BOM")
  ds_consult_response_details <- read.csv('dataset/ds_consult_response_details.csv', fileEncoding="UTF-8-BOM")
}

#create message response data set
ds_response_times <- data.frame(conversationID= numeric(0), 
                    severity= numeric(0), 
                    consult_requestor_id= numeric(0),
                    consult_requestor_title= character(0),
                    consult_responder_id = numeric(0),
                    consult_responder_title= character(0),
                    request_datetime = character(0), 
                    response_datetime = character(0),
                    response_time = character(0),
                    stringsAsFactors=FALSE)

#Cycle through the ds_consult_details data set, and populate the ds_response_times with consult and response details
for (i in 1:nrow(ds_consult_details)){
  if (ds_consult_details$ConversationID[i] %in% ds_consult_response_details$ConversationID)
    ds_response_times[nrow(ds_response_times)+1,] <- c(ds_consult_details$ConversationID[i], ds_consult_details$Severity[i], ds_consult_details$CreatorUserID[i], toString(ds_users$Title[match(ds_consult_details$CreatorUserID[i], ds_users$ID)]), ds_consult_response_details$CreatorUserID[i], toString(ds_users$Title[match(ds_consult_response_details$CreatorUserID[i], ds_users$ID)]), as.Date(ds_consult_details$InTime[i], "%Y-%M-%D %H:%M:%S"), as.Date(ds_consult_response_details$InTime[i], "%Y-%M-%D %H:%M:%S"), difftime(ds_consult_response_details$InTime[i],ds_consult_details$InTime[i], units="min"))
}
ds_response_times$severity[ds_response_times$severity ==0] <-"Regular Consult"
ds_response_times$severity[ds_response_times$severity ==2] <-"Urgent Consult"
ds_response_times$response_time <- round(as.numeric(ds_response_times$response_time), digits = 1)
write.csv(ds_response_times, 'dataset/ds_response_times.csv')

#
if (dummydata == TRUE){
  ds_response_times <- read.csv("dataset/ds_dummy_response_times.csv")
}
#Plot the results using ggplot using BeeSwarm
# ggplot(ds_response_times, aes(severity, response_time)) + geom_beeswarm(dodge.width=0, show.legend = TRUE)

#Plot the resuklts using the Beeswarm box plot
 beeswarm(response_time ~ severity, data= ds_response_times,
          method = 'swarm',
          pch=16, pwcol = as.numeric(consult_responder_id),
          xlim = c(0, 4), ylim = NULL,
          xlab = '', ylab = 'Consult Response Time (Mins)')
          legend("bottomright", legend = unique(ds_response_times$consult_responder_title),
                 title = 'Consult Responders', pch = 16, col=unique(as.numeric(ds_response_times$consult_responder_id)))

 boxplot(response_time ~ consult_responder_title, data= ds_response_times, add = T, names = c("",""), col="#0000ff22",
         main = "Vocera Messaging Consult Response Times", range =0)

#Plot results with plotly
# install.packages("plotly")
library(plotly)
p <- plot_ly(ds_response_times, y = ~response_time, x=~consult_responder_title, color = ~severity, type = "box", split = ~severity)
p

p <- plot_ly(ds_response_times, y = ~response_time, x=~consult_responder_title, color = ~consult_responder_title, type = "box")
p

