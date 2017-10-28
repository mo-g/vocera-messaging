####WINDOWS ENVIRONMENT####
#install.packages("RODBC")
#install.packages("networkD3")
library("networkD3")
library("RODBC")
library("plotly")
library("dplyr")
library("plyr")


#GLOBAL VARIABLES#
filesource = FALSE
dbhandle <- odbcDriverConnect('driver={SQL Server};server=VICVOCMSQLP01;database=WICMASTER;uid=saVoc;pwd=Flinkle87')
titlevisualization = TRUE

#Import Dataset using odbc connection and create files from DB query if filesource is TRUE
if (filesource == FALSE){
  write.csv(ds_users <- sqlQuery(dbhandle, "SELECT * FROM [WICMASTER].[dbo].[Users]"), 'dataset/ds_users.csv')
  write.csv(ds_distribution_lists <- sqlQuery(dbhandle, 'SELECT *FROM [WICMASTER].[dbo].[DistLists]'),'dataset/ds_distribution-list.csv')
  write.csv(ds_TextConversationRcpDistLists <- sqlQuery(dbhandle, "SELECT * FROM [WICMASTER].[dbo].[TextConversationRcpDistLists] where InTime > '2017-09-10'"), 'dataset/ds_TextConversationRcpDistLists.csv')
  write.csv(ds_TextConversationRcpUsers <- sqlQuery(dbhandle, "SELECT * FROM [WICMASTER].[dbo].[TextConversationRcpUsers] where InTime > '2017-09-10'"), 'dataset/ds_TextConversationRcpUsers.csv')
  write.csv(ds_TextConversationParticipants <- sqlQuery(dbhandle, "SELECT * FROM [WICMASTER].[dbo].[TextConversationParticipants]where InTime > '2017-09-10'"), 'dataset/ds_TextConversationParticipants.csv')
  write.csv(ds_TextConversations <- sqlQuery(dbhandle, "SELECT * FROM [WICMASTER].[dbo].[TextConversations] where Created > '2017-09-10'"), 'dataset/ds_TextConversations.csv')
  odbcCloseAll()
} else{
  ds_users <- read.csv('dataset/ds_users.csv', fileEncoding="UTF-8-BOM")
  ds_distribution_lists <- read.csv ('dataset/ds_distribution-list.csv', fileEncoding="UTF-8-BOM")
  ds_TextConversationParticipants <- read.csv ('dataset/ds_TextConversationParticipants.csv', fileEncoding="UTF-8-BOM")
  ds_TextConversationRcpDistLists <- read.csv ('dataset/ds_TextConversationRcpDistLists.csv', fileEncoding="UTF-8-BOM")
  ds_TextConversationRcpUsers <- read.csv ('dataset/ds_TextConversationRcpUsers.csv', fileEncoding="UTF-8-BOM")
  ds_TextConversations <- read.csv ('dataset/ds_TextConversations.csv', fileEncoding="UTF-8-BOM")
}

#Add unique identifiers to datasets (Vocera identifiers are not unique across users,groups and distribution lists)
#Required to fit this data into the edges dataframe
ds_users <- transform(ds_users, ID = sprintf('u%s', ID))
ds_distribution_lists <- transform(ds_distribution_lists, ID = sprintf('dl%s', ID))
ds_TextConversationParticipants <- transform(ds_TextConversationParticipants, UserID = sprintf('u%s', UserID))
ds_TextConversationRcpDistLists<- transform(ds_TextConversationRcpDistLists, DistListID = sprintf('dl%s', DistListID))
ds_TextConversationRcpUsers<- transform(ds_TextConversationRcpUsers, UserID = sprintf('u%s', UserID))
ds_TextConversations<- transform(ds_TextConversations, CreatorUserID = sprintf('u%s', CreatorUserID))

#Add conversation type context to ds_TextConversations dataset
for (i in 1:nrow(ds_TextConversations)){
  if (ds_TextConversations$ID[i] %in% ds_TextConversationRcpDistLists$ConversationID){
    ds_TextConversations$Convotype[i] = "User to Distribution Group"
  }else if (ds_TextConversations$ID[i] %in% ds_TextConversationRcpUsers$ConversationID){
    ds_TextConversations$Convotype[i] = "User to User(s)"
  }
}


#BUILD NODES WITH ALL USERS AND DISTRIBUTION LISTS
nodes <- data.frame(type= character(0), 
                    name= character(0), 
                    title= character(0), 
                    uniqueID = character(0),
                    stringsAsFactors=FALSE)

for (i in 1:nrow(ds_users)){
  if (ds_users$ID[i] %in% ds_TextConversationRcpUsers$UserID || ds_users$ID[i] %in% ds_TextConversationParticipants$UserID || ds_users$ID[i] %in% ds_TextConversations$CreatorUserID)
    if(!is.na(ds_users$Title[i])){
      nodes[nrow(nodes)+1,] <- c("user", toString(ds_users$DisplayName[i]), toString(ds_users$Title[i]), ds_users$ID[i])
    }else{
      nodes[nrow(nodes)+1,] <- c("user", toString(ds_users$DisplayName[i]), "Unknown", ds_users$ID[i])
    }
}
for (i in 1:nrow(ds_distribution_lists)){
  if (ds_distribution_lists$ID[i] %in% ds_TextConversationRcpDistLists$DistListID)
    nodes[nrow(nodes)+1,] <- c("dl", toString(ds_distribution_lists$Name[i]), paste(toString(ds_distribution_lists$Name[i]), " Call Group"), ds_distribution_lists$ID[i])
}

#Manually replace titles
nodes$title[nodes$uniqueID == "u-2"] <- "Paging Gateway"
nodes$title[nodes$uniqueID == "u-3"] <- "Partner Alerts" 


#BUILD RELATIONSHIPS FOR EDGES based on distribution lists and and add Unique identifier from the nodes dataframe for each distribution list matching the unique id against the source distribution list
links <- data.frame(convoid = numeric(0),
                    convotype = character(0),
                    creatoruniqueid =character(0),
                    creatorname = character(0), 
                    creatortitle = character(0), 
                    targetuniqueid = character(0),
                    targetname = character(0), 
                    targettitle = character(0), 
                    stringsAsFactors=FALSE)

#Iterate through Text conversation data set, and ass the message threads to the link dataset for visualization
for (i in 1:nrow(ds_TextConversations)){
  convoid = ds_TextConversations$ID[i]
  creatorid = ds_TextConversations$CreatorUserID[i]
  convotype = ds_TextConversations$Convotype[match(ds_TextConversationParticipants$ConversationID[i], ds_TextConversations$ID)]
  if (convoid %in% ds_TextConversationRcpUsers$ConversationID){
                               for (i in 1:nrow(ds_TextConversationRcpUsers)){
                                 if (ds_TextConversationRcpUsers$ConversationID[i] == convoid){
                                   links[nrow(links)+1,] <- c(as.integer(convoid),
                                                              toString(convotype),
                                                              toString(creatorid),
                                                              toString(nodes$name[match(creatorid, nodes$uniqueID)]),
                                                              toString(nodes$title[match(creatorid, nodes$uniqueID)]),
                                                              toString(ds_TextConversationRcpUsers$UserID[i]),
                                                              toString(nodes$name[match(ds_TextConversationRcpUsers$UserID[i], nodes$uniqueID)]),
                                                              toString(nodes$title[match(ds_TextConversationRcpUsers$UserID[i], nodes$uniqueID)]))
                                 }
                               }
    }else if(convoid %in% ds_TextConversationRcpDistLists$ConversationID){
      for (i in 1:nrow(ds_TextConversationRcpDistLists)){
        if (ds_TextConversationRcpDistLists$ConversationID[i] == convoid){
          links[nrow(links)+1,] <- c(as.integer(convoid),
                                 toString(convotype),
                                 toString(creatorid),
                                 toString(nodes$name[match(creatorid, nodes$uniqueID)]),
                                 toString(nodes$title[match(creatorid, nodes$uniqueID)]),
                                 toString(ds_TextConversationRcpDistLists$DistListID[i]),
                                 toString(nodes$name[match(ds_TextConversationRcpDistLists$DistListID[i], nodes$uniqueID)]),
                                 toString(nodes$title[match(ds_TextConversationRcpDistLists$DistListID[i], nodes$uniqueID)]))
    }
  }
    }
}

#Clean up nodes and converts "NA"'s to NA and populate generated nodeid
# nodes <- nodes[, c("type", "name", "title", "uniqueID")]
nodes[nodes=="NA"] <-NA

#Find unique links, required for summary and message count
links <- unique(links[,1:8])

#Setup Exclusion dataframe
ds_excluded <- data.frame(uniqueID = character(0), stringsAsFactors = FALSE)
# ds_excluded = data.frame(uniqueID=c("u1", "u2","u4", "u5", "u8", "u17", "u10","u12", "u48", "dl8", "dl19"),  stringsAsFactors=FALSE)

#Cycle through links ds and add id's to exclusion list
for (i in 1:nrow(nodes)){
  if (nodes$uniqueID[i] %in% links$creatoruniqueid || nodes$uniqueID[i] %in% links$targetuniqueid && !(nodes$uniqueID[i] %in% ds_excluded$uniqueID)){
  }else{
    ds_excluded[nrow(ds_excluded)+1,] <- c(nodes$uniqueID[i])
  }
}

#Remove nodes that are part of the exclusion list
nodes<-nodes[!(nodes$uniqueID %in% ds_excluded$uniqueID),]

#Create titlenodes for generating title senkey diagram
titlenodes <- nodes
titlenodes[titlenodes=="NA"] <-NA
titlenodes <- subset(titlenodes, select = -name)
titlenodes <- unique(titlenodes[,1:2])

#Create Summary dataframe to contain title links and total messages
summary_communication <- data.frame(sourceid = numeric(0),
                                    source = character(0),
                                    targetid = numeric(0),
                                    target = character(0),
                                    totalmessages = numeric(0),
                                    stringsAsFactors=FALSE)

#Add identifiers to nodes
nodes$id <- seq.int(nrow(nodes))-1

#cycle through links dataframe to generate total messages, add node id to links and populate summary communcation dataframe
for (i in 1:nrow(links)){
  links$source[i] <- nodes$id[match(links$creatoruniqueid[i], nodes$uniqueID)]
  links$target[i] <- nodes$id[match(links$targetuniqueid[i], nodes$uniqueID)]
  links$totalmessages[i] <- ds_TextConversations$TotalMessages[match(links$convoid[i], ds_TextConversations$ID)]
  summary_communication[nrow(summary_communication)+1,] <- c(NA,toString(links$creatortitle[i]), NA, toString(links$targettitle[i]), as.integer(links$totalmessages[i]))
}
summary_communication <- unique(summary_communication[,1:5])
summary_communication$totalmessages <- as.integer(summary_communication$totalmessages)

#Aggregate and merge rows with the same source-title and sum up the total message counts
summary_communication <- ddply(summary_communication, c("sourceid", "source", "targetid", "target"), summarise, totalmessages=sum(totalmessages))

########Use Top 100 Rows Only######
#Mainly used to scale down large communication volumes#########
#Titles
summary_communication_all <- summary_communication
summary_communication <- top_n(summary_communication, 100, totalmessages)
#Names
links_all <- links
links <- top_n(links, 50, totalmessages)

#########Filter out title nodes that are not in summary communication and rebuild title node frame######
#Titles
titlenodes_1 <- filter(titlenodes, titlenodes$title %in% summary_communication$source)
titlenodes_2 <- filter(titlenodes, titlenodes$title %in% summary_communication$target)
titlenodes <- bind_rows(titlenodes_1,titlenodes_2)
rm(titlenodes_1,titlenodes_2)

titlenodes <- unique(titlenodes[,1:2])
titlenodes$id <- seq.int(nrow(titlenodes))-1
#Names
nodes_1 <- filter(nodes, nodes$uniqueID %in% links$creatoruniqueid)
nodes_2 <- filter(nodes, nodes$uniqueID %in% links$targetuniqueid)
nodes <- bind_rows(nodes_1,nodes_2)
rm(nodes_1,nodes_2)

nodes <- unique(nodes)
nodes$id <- seq.int(nrow(nodes))-1

#########Add titlenodes Id's to Summary dataframe######
#Titles
for (i in 1:nrow(summary_communication)){
  summary_communication$sourceid[i] <- titlenodes$id[match(summary_communication$source[i], titlenodes$title)]
  summary_communication$targetid[i] <- titlenodes$id[match(summary_communication$target[i], titlenodes$title)]
}
#Names
for (i in 1:nrow(links)){
  links$source[i] <- nodes$id[match(links$creatoruniqueid[i], nodes$uniqueID)]
  links$target[i] <- nodes$id[match(links$targetuniqueid[i], nodes$uniqueID)]
}

#Produce .csv files of datasets
write.csv(links, file = "dataset/top_name_communication.csv")
write.csv(summary_communication, file = "dataset/top_title_communications.csv")
write.csv(links_all, file = "dataset/all_name_communication.csv")
write.csv(summary_communication_all, file = "dataset/all_title_communication.csv")


#CREATE SANKEY
sankey = list()

if (titlevisualization == FALSE){
  sankey$nodes <- data.frame(name = nodes$name, stringsAsFactors =FALSE)
  sankey$links <- data.frame(source = as.integer(links$source), target = as.integer(links$target), value = links$totalmessages)
  sankeyNetwork(Links = sankey$links, Nodes = sankey$nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                units = "\n messages sent", fontSize = 20, nodeWidth = 40, sinksRight = TRUE, iterations = 400, nodePadding = 20)
  
}else{
  sankey$nodes <- data.frame(name = titlenodes$title, stringsAsFactors =FALSE)
  sankey$links <- data.frame(source = as.integer(summary_communication$sourceid), target = as.integer(summary_communication$targetid), value = summary_communication$totalmessages)
  sankeyNetwork(Links = sankey$links, Nodes = sankey$nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                units = "\n messages sent", fontSize = 13, nodeWidth = 40, sinksRight = TRUE, iterations = 800, nodePadding = 40)
}


#Plot Titles and message count in a histogram
 # a <- list(
 #   title="Messages Recieved"
 # )
 # b <- list(
 #   title="",
 #   tickangle = 45
 # )
 # library(plotly)
 # p <- plot_ly(top_summary_communication, y = ~totalmessages, x=~target, color = ~target, type = "bar")%>%
 #   layout(title="Top 100 Contacted and Messages Recieved", margin = list(b = 160), xaxis = b, yaxis = a)
 # p
