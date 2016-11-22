################################################################################
CreateIGraphObject <- function(edge.Table, node.Table, is.Directed = TRUE){
        # Creates an iGraph object from a node and edge table and removes self-loops
        # 
        # Args: 
        #   edge.Table: the data frame of the edge table
        #   node.Table: the data frame of the node table 
        #   is.Directed = TRUE: If the graph is directed (default is TRUE)
        # 
        # Returns:
        #   The igraph object
        
        graph <- graph_from_data_frame(d        = edge.Table, 
                                      vertices = node.Table, 
                                      directed = is.Directed)
        graph <- simplify(name, remove.multiple = F, remove.loops = T)
        graph
}

