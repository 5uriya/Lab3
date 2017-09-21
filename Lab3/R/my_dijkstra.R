#' Dijkstra Algorithm
#' 
#' @param vector_1 A vector to store the first column of the data frame as a vector
#' @param vector_2 A vector to store the second column of the data frame as a vector
#' @param vector_3 A vector to store the third column of the data frame as a vector
#' @param unique_values To extract the unique values of the first column
#' @return The shortest distances from the input node to all other nodes 
#' @examples 
#' dijkstra(wiki_graph,2)
#' dijkstra(wiki_graph,1)
dijkstra<- function(graph,init_node)
{
  vector_1 <- as.vector(graph$v1)
  vector_2 <- as.vector(graph$v2)
  vector_3 <- as.vector(graph$w)
  
  unique_values <- unique(vector_1)
  
  mat<- matrix(c(unique_values,rep(NaN,length(unique_values)),rep(0,length(unique_values))),nrow = length(unique_values),ncol = 3)
  
  mat[init_node,2] <- 0
  
  
  while(length(unique_values)>0)
  {
    minimum <- min(mat[unique_values,2],na.rm = TRUE)
  
    index <- which(mat[,2] %in% minimum)
    
    dum<-intersect(unique_values,index)
    
    current_node <- dum[1]

    
    id <- which(vector_1 %in% current_node)
    connected_nodes <- vector_2[id]
    weight <- vector_3[id]
    
    for(i in 1:length(connected_nodes)){
      
      
      if((mat[connected_nodes[i],2]) > (mat[current_node,2] + weight[i]) || is.nan(mat[connected_nodes[i],2])){
        
        mat[connected_nodes[i],2] <- mat[current_node,2]+weight[i]
      }
      
    }
    
    unique_values <- unique_values[-(which(unique_values %in% current_node))] 
    
  }
  mat[,2]
  
}


