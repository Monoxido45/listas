# grau hierarquico
grau_hierarquico <- function(igraph_obj, k = 2){
  vtx_list <- V(igraph_obj)
  graus <- numeric(length(vtx_list))
  for(i in 1:length(vtx_list)){
    current_vtx <- vtx_list[i]
    viz_dict <- igraph_obj[[i]][[1]]
    l <- 1
    while(l < k){
      dict_list <- list()
      j <- 1
      for(vtx in viz_dict){
        # vizinhos do vertice vizinho de k-1 esimo grau
        new_dict <- igraph_obj[[vtx]][[1]]
        # removendo os vizinhos de k-1 esimo grau do dicionario
        new_dict <- new_dict[!(new_dict %in% c(viz_dict, current_vtx))]
        dict_list[[j]] <- new_dict
        j <- j + 1
      }
      dict_vec <- dict_list |> unlist() |> unique()
      l <- l + 1
      if(l < k){
        viz_dict <- dict_vec
    }
    }
    graus[i] <- length(dict_vec)
  }
  return(graus)
}


