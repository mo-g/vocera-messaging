####WINDOWS ENVIRONMENT####
.libPaths( c( .libPaths(), "C:/Users/ggonzalez/R") )
#install.packages("DiagrammeR")
#install.packages("knitr")
#install.packages("rmarkdown")
#install.packages("RODBC")
library("DiagrammeR", lib.loc="C:/Users/ggonzalez/R")
library("RODBC") 
library("visNetwork")


####MAC ENVIRONMENT###
#install.packages("DiagrammeR")
#library(DiagrammeR)


#GLOBAL VARIABLES#
filesource = FALSE
dbhandle <- odbcDriverConnect('driver={SQL Server};server=10.134.13.36;database=WICMASTER;uid=saVoc;pwd=saVoc45')

#Import Dataset using odbc connection and create files from DB query if filesource is TRUE
if (filesource == FALSE){
  write.csv(ds_users <- db_users <- sqlQuery(dbhandle, 'SELECT * FROM [WICMASTER].[dbo].[Users] WHERE ID > 0'), 'dataset/ds_users.csv')
  write.csv(ds_users_in_group <- db_users_in_group <- sqlQuery(dbhandle, 'SELECT * FROM [WICMASTER].[dbo].[UsersInGroup]'), 'dataset/ds_users-in-group.csv')
  write.csv(ds_groups <- db_groups <- sqlQuery(dbhandle, 'SELECT * FROM [WICMASTER].[dbo].[Groups]'), 'dataset/ds_groups.csv')
  write.csv(ds_distribution_lists <-db_distribution_lists <- sqlQuery(dbhandle, 'SELECT *FROM [WICMASTER].[dbo].[DistLists]'),'dataset/ds_distribution-list.csv')
  write.csv(ds_users_in_dist <- db_users_in_dist <- sqlQuery(dbhandle, 'SELECT * FROM [WICMASTER].[dbo].[UsersInDistList]'),'dataset/ds_users_in_dist.csv')
  write.csv(ds_distribution_lists_groups <- db_distribution_lists_groups <- sqlQuery(dbhandle, 'SELECT * FROM [WICMASTER].[dbo].[DistListsInGroup]'),'dataset/ds_distribution_lists_groups.csv')
  write.csv(ds_distribution_lists_users <- db_distribution_lists_users <- sqlQuery(dbhandle, 'SELECT * FROM [WICMASTER].[dbo].[DistListsInUser]'), 'dataset/ds_distribution_lists_users.csv')
  odbcCloseAll()
} else{
  ds_users <- read.csv('dataset/ds_users.csv', fileEncoding="UTF-8-BOM")
  ds_users_in_group <- read.csv('dataset/ds_users-in-group.csv', fileEncoding="UTF-8-BOM")
  ds_groups <- read.csv('dataset/ds_groups.csv', fileEncoding="UTF-8-BOM")
  ds_distribution_lists <- read.csv ('dataset/ds_distribution-list.csv', fileEncoding="UTF-8-BOM")
  ds_users_in_dist <- read.csv('dataset/ds_users_in_dist.csv', fileEncoding="UTF-8-BOM")
  ds_distribution_lists_groups <- read.csv('dataset/ds_distribution_lists_groups.csv', fileEncoding="UTF-8-BOM")
  ds_distribution_lists_users <- read.csv('dataset/ds_distribution_lists_users.csv', fileEncoding="UTF-8-BOM")
}



#Add unique identifiers to datasets (Vocera identifiers are not unique across users,groups and distribution lists)
#Convert the identifier into an integer. Required to fit this data into the edges dataframe
#--USERS---#
ds_users <- transform(ds_users, ID = sprintf('u%s', ID))

#--GROUPS---#
ds_groups <- transform(ds_groups, ID = sprintf('g%s', ID))

#--DISTRIBUTION LIST---#
ds_distribution_lists <- transform(ds_distribution_lists, ID = sprintf('dl%s', ID))
ds_distribution_lists_groups <- transform(ds_distribution_lists_groups, DistListID = sprintf('dl%s', DistListID))
ds_distribution_lists_groups <- transform(ds_distribution_lists_groups, GroupID = sprintf('g%s', GroupID))
ds_distribution_lists_users <- transform(ds_distribution_lists_users, DistListID = sprintf('dl%s', DistListID))
ds_distribution_lists_users <- transform(ds_distribution_lists_users, UserID = sprintf('u%s', UserID))
#--USERS IN DISTRIBUTION LIST---#
ds_users_in_dist <- transform(ds_users_in_dist, DistListID = sprintf('dl%s', DistListID))
ds_users_in_dist <- transform(ds_users_in_dist, UserID = sprintf('u%s', UserID))

#--USERS IN GROUPS---#
ds_users_in_group <- transform(ds_users_in_group, GroupID = sprintf('g%s', GroupID))
ds_users_in_group <- transform(ds_users_in_group, UserID = sprintf('u%s', UserID))

#Create node dataframe and begin populating.
#To exclude orphaned nodes, ensure the the if condition under the for loop is commented out
nodes <- data.frame(Type= character(0), Name= character(0), Shape = character(0), Color = character(0), UniqueID = character(0), stringsAsFactors=FALSE)
for (i in 1:nrow(ds_users)){
  if (ds_users$ID[i] %in% ds_distribution_lists_users$UserID || ds_users$ID[i] %in% ds_users_in_dist$UserID || ds_users$ID[i] %in% ds_users_in_group$UserID)
  nodes[nrow(nodes)+1,] <- c("user", toString(ds_users$DisplayName[i]), "triangle", "LightSlateGray", ds_users$ID[i])
}
for (i in 1:nrow(ds_groups)){
  if (ds_groups$ID[i] %in% ds_distribution_lists_groups$GroupID || ds_groups$ID[i] %in% ds_users_in_group$GroupID)
  nodes[nrow(nodes)+1,] <- c("group", toString(ds_groups$Name[i]), "rectangle", "DarkSeaGreen", ds_groups$ID[i])
}
for (i in 1:nrow(ds_distribution_lists)){
  if (ds_distribution_lists$ID[i] %in% ds_distribution_lists_groups$DistListID || ds_distribution_lists$ID[i] %in% ds_distribution_lists_users$DistListID)
  nodes[nrow(nodes)+1,] <- c("dl", toString(ds_distribution_lists$Name[i]), "circle", "orange", ds_distribution_lists$ID[i])
}


#create nodes using DiagrammeR function
nodes <-
  create_node_df(
    n = length(nodes$UniqueID),
    nodes = nodes$Name,
    type = nodes$Type,
    label = nodes$Name,
    color = nodes$Color, 
    shape = "icon",
    uniqueid = nodes$UniqueID,
    style = "filled",
    group = nodes$Type
    )

for (i in 1:nrow(nodes)){
  if (nodes$type[i]=="dl"){
  ds_distribution_lists$NodeId <- nodes$id[match(ds_distribution_lists$ID, nodes$uniqueid)]
  }else if (nodes$type[i]=="group"){
    ds_groups$NodeId <- nodes$id[match(ds_groups$ID, nodes$uniqueid)]
    }else if (nodes$type[i]=="user"){
      ds_users$NodeId <- nodes$id[match(ds_users$ID, nodes$uniqueid)]
    }
}

#BUILD RELATIONSHIPS FOR EDGES based on distribution lists and and add Unique identifier from the nodes dataframe for each distribution list matching the unique id against the source distribution list
edges <- data.frame(FromNodeType = character(0), FromNodeId = character(0), FromNodeName = character(0), RelationshipType = character(0), ToNodeType = character(0), ToNodeId = character(0), ToNodeName = character(0), color = character(0), stringsAsFactors=FALSE)

for (i in 1:nrow(ds_users_in_dist)){
  if (ds_users_in_dist$UserID[i] %in% nodes$uniqueid & ds_users_in_dist$DistListID[i] %in% nodes$uniqueid){
    ds_users_in_dist$DistListNodeId <- nodes$id[match(ds_users_in_dist$DistListID, nodes$uniqueid)]
    ds_users_in_dist$DistListNodeName <- nodes$nodes[match(ds_users_in_dist$DistListID, nodes$uniqueid)]
    ds_users_in_dist$UserNodeId <- nodes$id[match(ds_users_in_dist$UserID, nodes$uniqueid)]
    ds_users_in_dist$UserNodeName <- nodes$nodes[match(ds_users_in_dist$UserID, nodes$uniqueid)]
    edges[nrow(edges)+1,] <- c("user", ds_users_in_dist$UserNodeId[i], ds_users_in_dist$UserNodeName[i], "is listed in", "dis list", ds_users_in_dist$DistListNodeId[i], ds_users_in_dist$DistListNodeName[i], "SteelBlue")
  }
}
for (i in 1:nrow(ds_users_in_group)){
  if (ds_users_in_group$UserID[i] %in% nodes$uniqueid & ds_users_in_group$GroupID[i] %in% nodes$uniqueid){
    ds_users_in_group$GroupNodeID <- nodes$id[match(ds_users_in_group$GroupID, nodes$uniqueid)]
    ds_users_in_group$GroupNodeName <- nodes$nodes[match(ds_users_in_group$GroupID, nodes$uniqueid)]
    ds_users_in_group$UserNodeID <- nodes$id[match(ds_users_in_group$UserID, nodes$uniqueid)]
    ds_users_in_group$UserNodeName <- nodes$nodes[match(ds_users_in_group$UserID, nodes$uniqueid)]
    edges[nrow(edges)+1,] <- c("user", ds_users_in_group$UserNodeID[i], ds_users_in_group$UserNodeName[i], "is a member of", "group", ds_users_in_group$GroupNodeID[i], ds_users_in_group$GroupNodeName[i], "SeaGreen")
  }
}
for (i in 1:nrow(ds_distribution_lists_groups)){
  if (ds_distribution_lists_groups$GroupID[i] %in% nodes$uniqueid & ds_distribution_lists_groups$DistListID[i] %in% nodes$uniqueid){
    ds_distribution_lists_groups$DistListNodeId <- nodes$id[match(ds_distribution_lists_groups$DistListID, nodes$uniqueid)]
    ds_distribution_lists_groups$DistListNodeName <- nodes$nodes[match(ds_distribution_lists_groups$DistListID, nodes$uniqueid)]
    ds_distribution_lists_groups$GroupNodeId <- nodes$id[match(ds_distribution_lists_groups$GroupID, nodes$uniqueid)]
    ds_distribution_lists_groups$GroupNodeName <- nodes$nodes[match(ds_distribution_lists_groups$GroupID, nodes$uniqueid)]
    edges[nrow(edges)+1,] <- c("group", ds_distribution_lists_groups$GroupNodeId[i], ds_distribution_lists_groups$GroupNodeName[i], "can view", "dis list", ds_distribution_lists_groups$DistListNodeId[i], ds_distribution_lists_groups$DistListNodeName[i], "coral")
  }
}
for (i in 1:nrow(ds_distribution_lists_users)){
  if (ds_distribution_lists_users$UserID[i] %in% nodes$uniqueid & ds_distribution_lists_users$DistListID[i] %in% nodes$uniqueid){
    ds_distribution_lists_users$DistListNodeId <- nodes$id[match(ds_distribution_lists_users$DistListID, nodes$uniqueid)]
    ds_distribution_lists_users$DistListNodeName <- nodes$nodes[match(ds_distribution_lists_users$DistListID, nodes$uniqueid)]
    ds_distribution_lists_users$UserNodeId <- nodes$id[match(ds_distribution_lists_users$UserID, nodes$uniqueid)]
    ds_distribution_lists_users$UserNodeName <- nodes$nodes[match(ds_distribution_lists_users$UserID, nodes$uniqueid)]
    edges[nrow(edges)+1,] <- c("user", ds_distribution_lists_users$UserNodeId[i], ds_distribution_lists_users$UserNodeName[i], "can view", "dis list", ds_distribution_lists_users$DistListNodeId[i], ds_distribution_lists_users$DistListNodeName[i], "coral")
}
}
#Create edges dataframe
edges <-
  create_edge_df(
    from = edges$ToNodeId,
    to = edges$FromNodeId,
    rel = edges$RelationshipType,
    data = edges$RelationshipType,
    # label = edges$RelationshipType,
    color = edges$color,
    length = 200
  )

#CREATE AND RENDER GRAPH USING GRAPHVIZ ENGINE
# graph <-
#   create_graph(
#     nodes_df = nodes,
#     edges_df = edges,
#     attr_theme ="default")%>%
#   set_global_graph_attrs(
#     attr = c("layout","rankdir", "splines"),
#     value = c("dot","RL","false"),
#     attr_type = c("graph","graph", "graph"))
# render_graph(graph)
  
#CREATE AND RENDER GRAPH USING VISNETWORK
ledges <- data.frame(color = c("SteelBlue", "SeaGreen", "coral"),
                     label = c("is listed in", "is a member of", "can message"), arrows =c("to", "from", "to"))
visNetwork(nodes, edges, main = "Vocera Message Pathways", width = "100%", height ="100%") %>%
  # visHierarchicalLayout() %>%
  visInteraction(hideEdgesOnDrag = TRUE) %>%
  visEdges(smooth = TRUE, shadow = TRUE,
           arrows =list(from = list(enabled = TRUE), scaleFactor = 2, highlight = "red")) %>%
  visGroups(groupname = "dl", shape = "icon", 
            icon = list(code = "f0ac", color = "gray")) %>%
  visGroups(groupname = "group", shape = "icon", 
            icon = list(code = "f0c0", color = "orange")) %>%
  visGroups(groupname = "user", shape = "icon", 
            icon = list(code = "f007", color = "green")) %>%
  visLegend(addEdges = ledges, useGroups = TRUE) %>%
  visOptions(selectedBy = "type",
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visPhysics(enabled = TRUE) %>%
  addFontAwesome() 

