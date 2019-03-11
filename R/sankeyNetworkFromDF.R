#-------------------------------
# Function to build a sunkeyNetwork from a dataframe
#-------------------------------

#' @
#' @example 
#' graph_df <- data.frame(level1 = c('A','B','C','D'), level2 = c('E','E','E','F'), level3 = c('G','H', 'H', 'H'), values = c(1,2,3,4))
#' sankeyNetworkFromDF(graph_df, value.column = 'values')


library(networkD3)
library(dplyr)

sankeyNetworkFromDF <- function(df.with.volume, value.column) {

  df.with.volume <- df.with.volume %>% ungroup()

  graph_df <- df.with.volume %>% select(-one_of(value.column)) # select graph db without value

  value <- df.with.volume %>% select(one_of(value.column)) # select value

  list_of_nodes_vector <- unique(as.vector(as.matrix(graph_df))) # combine to vector of values for coding purposes

  list_of_nodes_df <- data.frame(name = list_of_nodes_vector)

  #Now we have to unite this to one model
  table_of_edges_all <- {}
  for(i in (1:(dim(graph_df)[2]-1))) {

    sub.table <- cbind(graph_df[,i:(i+1)], value)
    names(sub.table) <- c(1,2)

    if(is.null(table_of_edges_all)) {table_of_edges_all <- sub.table
    } else {
      table_of_edges_all <- rbind(table_of_edges_all, sub.table)
    }

  }


  # table of edges
  table_of_edges_coded  <- cbind(
    as.data.frame(lapply(table_of_edges_all[,1:(dim(table_of_edges_all)[2]-1)],FUN = function(x) {as.integer(factor(x, levels = list_of_nodes_vector))-1}), stringsAsFactors = FALSE),
    X3 = table_of_edges_all[,dim(table_of_edges_all)[2]])


  names(table_of_edges_coded) <- c('source','target', 'value')

  rownames(table_of_edges_all) <- 1:dim(table_of_edges_coded)[1]

  #Now plot the graph
  p <- sankeyNetwork(Links = table_of_edges_coded, Nodes = list_of_nodes_df, Source = "source",
                     Target = "target",#colourScale = JS("d3.scale.category20()"),
                     Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 30, nodePadding = 15)

  return(p)

}
