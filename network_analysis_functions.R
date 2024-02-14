# MY NETWORK ANALYSIS FUNCTIONS SCRIPT

# NETWORK GENERATION FUNCTIONS ---------------------------------------------------------------

# DATA PREPARATION
# REMOVE SELF LOOPS IN GRAPH FUNCTION
remove_loops <- function(data){
  boolean <- data[1] != data[2]
  data[boolean,]
  
}
# ASSIGN COLNAMES TO DATAFRAME EDGELIST
align_colnames <- function(dataset, my_names) {
  # GET ONLY FIRST 3 COLS
  dataset1 <- dataset %>% 
    select(1:3)
  # ALIGN COL NAMES
  names(dataset1) <- my_names
  # RETURN NEW DATASET
  return(dataset1)
}

# RANDOM GRAPH GENERATION
#  GENERATE RANDOM GRAPH FROM A BASE GRAPH
generate_random_graph <- function(base_graph){
  
  num_nodes <- vcount(base_graph)
  num_edges <- ecount(base_graph)
  random_graph <- sample_gnm(num_nodes, num_edges, directed = TRUE)
  V(random_graph)$name <- paste0("Node ", 1:num_nodes)
  self_loop_edges <- which(is.loop(random_graph))
  if (length(self_loop_edges) > 0) {
    random_graph <- delete_edges(random_graph, self_loop_edges)
  }
  
  min_val <- E(base_graph)$Weight %>% min()
  max_val <-E(base_graph)$Weight %>% max()
  
  # ASSIGN RANDOM WEIGHTS
  random_weights <- runif(num_edges, min = min_val, max = max_val)
  
  # Set the edge attributes to the random weights
  E(random_graph)$Weight <- random_weights
  # ASSIGN IT A NAME
  random_graph$name <-  names(graph)
  return(random_graph)
  
}
generate_random_graph_list <- function(base_graph, no_of_graphs){
  
  random_graph_list <- list()
  
  for (i in seq_along(1:no_of_graphs)) {
    
    num_nodes <- vcount(base_graph)
    num_edges <- ecount(base_graph)
    random_graph <- sample_gnm(num_nodes, num_edges, directed = TRUE)
    V(random_graph)$name <- paste0("Node ", 1:num_nodes)
    self_loop_edges <- which(is.loop(random_graph))
    if (length(self_loop_edges) > 0) {
      random_graph <- delete_edges(random_graph, self_loop_edges)
    }
    
    min_val <- E(base_graph)$Weight %>% min()
    max_val <-E(base_graph)$Weight %>% max()
    
    # ASSIGN RANDOM WEIGHTS
    random_weights <- runif(num_edges, min = min_val, max = max_val)
    
    # Set the edge attributes to the random weights
    E(random_graph)$Weight <- random_weights
    # ASSIGN IT A NAME
    random_graph$name <-  i
    
    random_graph_list[[i]] <- random_graph
    
    names(random_graph_list)[[i]] <- i
    
    
  }
  
  return(random_graph_list)
  
}

# FILTERING
# DISPARITY BACKBONE FUNCTION - TAKES IGRAPH OBJECT
filter_graph <- function(graph, alpha = 0.05, narrative = FALSE) {
  
  
  # CREATE AN ADJACENCY MATRIX FROM THE IGRAPH OBJECT
  adj_matrix <- as_adjacency_matrix(graph, attr = "Weight", sparse = FALSE)
  # CONVERT THE ADJACENCY MATRIX TO A MATRIX"
  adj_matrix <- as.matrix(adj_matrix)
  # USE THE ADJACENCY MATRIX IN THE DISPARITY() FUNCTION
  result <- disparity(adj_matrix, alpha = alpha, class = "igraph",narrative = narrative)
  # GET VECTOR OF EDGE WEIGHTS FOR EDGES IN FILTERED IGRAPH
  Weights <-  E(graph)$Weight[as_ids(E(graph))  %in% as_ids(E(result))]
  # ADD EDGE WEIGHTS TO IGRAPH OBJECT
  result <- result %>% 
    set_edge_attr("Weight",value = Weights)
  # REMOVE UNCONNECTED NODES
  nodes_total_degree <- degree(result, mode = "total")
  unconnected_nodes <- V(result)[nodes_total_degree == 0]
  result <- delete.vertices(result, unconnected_nodes)

  return(result)
  
}


# METRICS AND WORK UP FUNCTIONS -------------------------------------------

# METRICS RELATED
# GLOBAL METRICS FUNCTIONS
global_metrics_as_df <- function(graph, loops = FALSE) {
  
  # NUMBER OF NODES - NODE COUNT
  my_gorder <- gorder(graph)
  # NUMBER OF EDGES
  my_gsize <- gsize(graph)
  # EDGE DENSITY 
  if (loops == TRUE) {
    my_edge_density <- edge_density(graph, loops = TRUE)
  } else {
    my_edge_density <- edge_density(graph, loops = FALSE)
  }
  # GLOBAL EFFICIENCY
  my_global_efficiency <- global_efficiency(graph)
  # AVERAGE LOCAL EFFICIENCY
  average_local_efficiency_in <-  average_local_efficiency(graph, mode = "in")
  # AVERAGE LOCAL EFFICIENCY
  average_local_efficiency_out <-  average_local_efficiency(graph, mode = "out")
  
  # BETWEENNESS CENTRALITY 
  my_betweenness_centrality1 <- graph %>% 
    centr_betw(directed = TRUE)
  my_betweenness_centrality <- my_betweenness_centrality1$centralization
  # IN CLOSENESS CENTRALITY 
  my_in_closeness_centrality1 <- graph %>% 
    centr_clo(mode = "in")
  my_in_closeness_centrality <- my_in_closeness_centrality1$centralization
  # OUT CLOSENESS CENTRALITY 
  my_out_closeness_centrality1 <- graph %>% 
    centr_clo(mode = "out")
  my_out_closeness_centrality <- my_out_closeness_centrality1$centralization
  # IN DEGREE CENTRALITY 
  if (loops == TRUE) {
    my_in_degree_centrality1 <- graph %>% 
      centr_degree(mode = "in",loops =TRUE)
  } else{
    my_in_degree_centrality1 <- graph %>% 
      centr_degree(mode = "in",loops =FALSE) 
  }
  my_in_degree_centrality <- my_in_degree_centrality1$centralization
  # OUT DEGREE CENTRALITY 
  if (loops == TRUE) {
    my_out_degree_centrality1 <- graph %>% 
      centr_degree(mode = "out",loops =TRUE)
  } else{
    my_out_degree_centrality1 <- graph %>% 
      centr_degree(mode = "out",loops =FALSE) 
  }
  my_out_degree_centrality <- my_out_degree_centrality1$centralization
  # EIGEN CENTRALITY 
  my_eigen_centrality1 <- graph %>% 
    centr_eigen(directed = TRUE)
  my_eigen_centrality <- my_eigen_centrality1$value
  my_metrics_list <- list(my_gorder,my_gsize,my_edge_density,my_global_efficiency,
                          average_local_efficiency_in,average_local_efficiency_out,my_betweenness_centrality,
                          my_in_closeness_centrality,my_out_closeness_centrality,my_in_degree_centrality,
                          my_out_degree_centrality,my_eigen_centrality)
  
  names(my_metrics_list) <- c("Node_Count","Edge_Count","Edge_Density","Global_Efficiency",
                              "Average_Local_Efficiency_In","Average_Local_Efficiency_Out","Betweenness_Centrality",
                              "In_closeness_Centrality","Out_closeness_Centrality","In_degree_centrality",
                              "Out_degree_centrality","eigen_centrality")
  
  my_metrics_df <- my_metrics_list %>%
    as.data.frame()%>%
    pivot_longer(cols = 1:ncol(.),names_to = "Metrics") 
  
  
  return(my_metrics_df)
  
}
global_metrics_igraph_list_to_big_df <- function(igraph_list, loops = FALSE) {
  
  # CREATE EMPTY LIST TO STORE METRIC DF IN
  global_metrics_list <- list()
  # LOOP THROUGH IGRAPH LIST TO MAKE METRICS DF
  for (i in seq_along(igraph_list)) {
    
    if(loops == TRUE) {
      
      global_metrics_list[[i]] <- igraph_list[[i]] %>% 
        global_metrics_as_df(loops = TRUE)  
      colnames(global_metrics_list[[i]])[[2]] <- names(igraph_list)[[i]]
      
    } else{
      
      global_metrics_list[[i]] <- igraph_list[[i]] %>% 
        global_metrics_as_df(loops = FALSE) 
      colnames(global_metrics_list[[i]])[[2]] <- names(igraph_list)[[i]]
    }
    
  }
  
  # JOIN ELEMENTS IN LIST TOGETHER INTO DATAFRAME -
  joined_df <- Reduce(function(x, y) merge(x, y, by = "Metrics", all = TRUE), global_metrics_list)
  
  return(joined_df)
  
}
# NODE METRICS FUNCTIONS
my_as.data.frame  <- function(data, metric){
  
  # CONVERT INTO DF
  df <- data %>% 
    as.data.frame() 
  # ASSIGN COL NAME
  colnames(df) <- metric
  # ADD NODES COLUMN
  df <- df %>% 
    mutate(Nodes = row.names(.), .before = 1) 
  # ORDER BY METRIC - DESC
  df <- df[order(df[,2], decreasing = T),]
  # ADD RANKING
  df <- df %>% 
    mutate(row_number())
  # ASSIGN COL NAME
  colnames(df)[3] <- paste0("Ranking ", colnames(df)[2])
  
  
  return(df)
}
node_metrics_df <- function(graph, loops = FALSE) {
  # STORE VERTICES NAMES
  v_names <-  graph %>% V() %>% names()
  # IN DEGREE COUNT 
  if(loops == TRUE) {
    my_in_degree <- degree(graph, mode = "in", loops = TRUE) %>% 
      my_as.data.frame(metric = "in degree")
  } else{
    my_in_degree <- degree(graph, mode = "in", loops = FALSE) %>% 
      my_as.data.frame(metric = "in degree")
  }
  # OUT DEGREE COUNT 
  if(loops == TRUE) {
    my_out_degree <- degree(graph, mode = "out", loops = TRUE) %>% 
      my_as.data.frame(metric = "out degree")
  } else{
    my_out_degree <- degree(graph, mode = "out", loops = FALSE) %>%
      my_as.data.frame(metric = "out degree")
  }
  # CLOSENESS - IN / OUT
  my_in_closeness <- closeness(graph, mode = "in",cutoff = -1, weights = E(graph)$Weight) %>% 
    my_as.data.frame(metric = "in closeness")
  my_out_closeness <- closeness(graph, mode = "out",cutoff = -1,weights = E(graph)$Weight) %>% 
    my_as.data.frame(metric = "out closeness")
  # LOCAL EFFICIENCY - IN / OUT
  my_local_efficiency_in <-  local_efficiency(graph, mode = "in") %>% 
    my_as.data.frame(metric = "in local efficiency")
  my_local_efficiency_out <-  local_efficiency(graph, mode = "out") %>% 
    my_as.data.frame(metric = "out local efficiency")
  # HUB SCORE 
  # my_hub_score1 <- hub_score(graph)
  # my_hub_score <- my_hub_score1$vector %>% 
  #   my_as.data.frame(metric = "hub score")
  # BETWEENNESS CENTRALITY 
  my_betweenness_centrality1 <- graph %>% 
    centr_betw(directed = TRUE)
  my_betweenness_centrality <- my_betweenness_centrality1$res 
  names(my_betweenness_centrality) <- v_names
  my_betweenness_centrality <- my_betweenness_centrality %>% 
    my_as.data.frame(metric = "Betweenness Centrality")
  # EIGEN CENTRALITY 
  my_eigen_centrality1 <- graph %>%
    centr_eigen(directed = TRUE)
  my_eigen_centrality <- my_eigen_centrality1$vector
  names(my_eigen_centrality) <- v_names
  my_eigen_centrality <- my_eigen_centrality %>% 
    my_as.data.frame(metric = "eigen centrality")
  # HARMONIC CENTRALITY IN
  # my_in_harmonic_centrality <- graph %>% 
  #   harmonic_centrality(mode = "in") %>% 
  #   my_as.data.frame(metric = "in harmonic centrality") 
  # HARMONIC CENTRALITY OUT
  # tryCatch({
  #   my_out_harmonic_centrality <- graph %>% 
  #     harmonic_centrality(mode = "out") %>% 
  #     my_as.data.frame(metric = "out harmonic centrality")
  # }, error = function(e){
  #   cat("Error in harmonic centrality calculation:", conditionMessage(e), "\n")
  #   
  #   my_out_harmonic_centrality <- data.frame(Nodes = v_names,"in_harmonic_centrality" = rep(NA, length(v_names)))
  # })
  # PAGE RANK ALGO 
  # my_page_rank1 <- graph %>% 
  #   page_rank()
  # my_page_rank <- my_page_rank1$vector %>% 
  #   my_as.data.frame(metric = "Page Rank")
  # # POWER CENTRALITY 
  # tryCatch({
  #   my_power_centrality <- graph %>% 
  #     power_centrality() %>% 
  #     my_as.data.frame(metric = "Power Centrality")
  # }, error = function(e){
  #   cat("Error power centrality calculation:", conditionMessage(e), "\n")
  #   
  #   my_power_centrality <<- data.frame(Nodes = v_names,"power_centrality" = rep(NA, length(v_names)))
  #   
  # })
  
  metric_df_list <- list(my_in_degree,my_out_degree,
                         my_in_closeness,my_out_closeness,
                         my_local_efficiency_in,my_local_efficiency_out,
                         my_betweenness_centrality,my_eigen_centrality)
  
  metric_df <- Reduce(function(x, y) merge(x, y, by = "Nodes", all = TRUE), metric_df_list)
  
  return(metric_df)
  
}
graphs_list_to_node_metric_df_list <- function(igraph_list,loops = FALSE) {
  
  # CREATE EMPTY LIST TO STORE METRIC DF IN
  node_metrics_list <- list()
  # LOOP THROUGH IGRAPH LIST TO MAKE METRICS DF
  for (i in seq_along(igraph_list)) {
    
    if(loops == TRUE) {
      node_metrics_list[[i]]  <- igraph_list[[i]] %>%
        node_metrics_df(loops = TRUE)
    } else {
      node_metrics_list[[i]]  <- igraph_list[[i]] %>%
        node_metrics_df(loops = FALSE)
    }
    
    
  }
  # ALIGN NAMES
  names(node_metrics_list) <- names(igraph_list)
  
  return(node_metrics_list)
  
}
# TOP 10 NODES BY METRIC - CAN TAKE METRIC LIST
top_bottom_X <- function(data,metric_list,cut = 10, top_bottom = "top") {
  
  top10list <- list()
  
  for (i in seq_along(metric_list)){
    
    if(top_bottom == "top") {
      
      # ORDER BY METRIC - DESC
      data <- data[order(data[,metric_list[[i]]], decreasing = T),]
      # SELECT DESIRED COLS 
      top10list[[i]] <- data[1:cut,] %>%
        select(Nodes, metric_list[[i]], paste0("Ranking ",metric_list[[i]]))
      
    } else {
      
      # ORDER BY METRIC - DESC
      data <- data[order(data[,metric_list[[i]]], decreasing = F),]
      # SELECT DESIRED COLS
      top10list[[i]] <- data[1:cut,] %>%
        select(Nodes, metric_list[[i]], paste0("Ranking ",metric_list[[i]]))
      
    }
    
  }
  
  names(top10list) <- paste0(metric_list)
  
  return(top10list)
  
}
# EDGE METRICS FUNCTIONS
edge_metrics_df <- function(graph, edgelistdf) {
  
  metric_df <- edgelistdf %>% 
    mutate(edge_betweenness = edge_betweenness(graph,directed = TRUE,cutoff = -1,weights = E(graph)$Weight)) %>% 
    arrange(desc(edge_betweenness)) %>% 
    mutate("Ranking_edge_betweenness" = row_number())
  
  
  return(metric_df)
  
}
graphs_list_to_edge_metrics_df_list <- function(igraph_list, edgelist_list) {
  
  edge_metrics_df_list <- list()
  # RUN EDGE METRICS ON IGRAPH OBJECTS IN LIST
  for (i in seq_along(igraph_list)){
    edge_metrics_df_list[[i]] <- edge_metrics_df(igraph_list[[i]],edgelist_list[[i]])
  }
  # ALIGN NAMES
  names(edge_metrics_df_list) <- names(igraph_list)
  
  return(edge_metrics_df_list)
}

# TOP X FOR NODE METRICS - TAKES DF OF NODES, EDGELIST
topX <- function(data, ..., top = 10) {
  
  if(top == "all") {
    
    data %>%
      arrange(across(c(...),desc)) %>%
      select(Nodes, ...) %>%
      mutate(Ranking = row_number())
    
  } else{
    
    data %>%
      arrange(across(c(...),desc)) %>%
      slice(1:top) %>%
      select(Nodes, ...) %>%
      mutate(Ranking = row_number())
    
  }
  
}
topX_wEU <- function(data, ..., top = 10) {
  
  ordered_list <- data %>%
    arrange(across(c(...),desc)) %>%
    select(Nodes, ...) %>%
    mutate(Ranking = row_number())
  
  if("EU"  %in% ordered_list$Nodes[1:top]) {
    
    EU_ranking <- ordered_list %>% 
      filter(Nodes == "EU")
    
    non_EU_ranking <- ordered_list %>% 
      slice(1:top) %>% 
      filter(Nodes != "EU")
    
    bind_rows(EU_ranking,non_EU_ranking)
    
  } else {
    
    EU_ranking <- ordered_list %>% 
      filter(Nodes == "EU")
    
    bind_rows(EU_ranking,ordered_list)
    
  }
  
}
topX_edges <- function(data, ..., top = 10) {
  
  if(top == "all") {
    
    data %>%
      arrange(across(c(...),desc)) %>%
      select(Seller, Purchaser, Weight, ...) %>%
      mutate(Ranking = row_number())
    
  } else {
    
    data %>%
      arrange(across(c(...),desc)) %>%
      slice(1:top) %>%
      select(Seller, Purchaser, Weight, ...) %>%
      mutate(Ranking = row_number())
    
    
  }
  
}
# NODE STRENGTHS AS DF - TAKES GRAPH OBJECT 
my_in_out_strength <- function(data) {
  
  in_strength <- data %>% 
    strength(mode = "in", weights = E(data)$Weight) %>% 
    as.data.frame() %>% 
    arrange(desc(.)) %>% 
    rownames_to_column(var = "Nodes") %>% 
    rename(in_Strength = ".")
  
  out_strength <- data %>% 
    strength(mode = "out", weights = E(data)$Weight) %>% 
    as.data.frame() %>% 
    arrange(desc(.)) %>% 
    rownames_to_column(var = "Nodes") %>% 
    rename(out_Strength = ".")
  
  in_strength %>% 
    right_join(out_strength, by ="Nodes")
  
}

# PLOTTING FUNCTIONS
normalize <- function(x){(x-min(x))/(max(x)-min(x))}
plot_by_centrality <- function(graph, centrality_measure, colours_list = c("lightblue", "darkblue"),
                               vertex.size = 20, vertex.label.color = "white",    
                               vertex.label.cex = 1,edge.color = "gray",           
                               edge.width = 0.1, arrow.width = 0.01,
                               arrow.size = 0.01, dataset_name = "graph",...) { 
  
  V(graph)$centrality_values <- switch(centrality_measure,
                                       "in degree_loops" = degree(graph, mode = "in", loops = TRUE),
                                       "in degree_loopless" = degree(graph, mode = "in", loops = FALSE),
                                       "out degree_loops" = degree(graph, mode = "out", loops = TRUE),
                                       "out degree_loopless" = degree(graph, mode = "out", loops = FALSE),
                                       "in closeness" = closeness(graph, mode = "in",cutoff = -1),
                                       "out closeness" = closeness(graph, mode = "out",cutoff = -1),
                                       "in local efficiency" = local_efficiency(graph, mode = "in"),
                                       "out local efficiency" = local_efficiency(graph, mode = "out"),
                                       "hub score" =  hub_score(graph)$vector,
                                       "Betweenness Centrality" = betweenness(graph,directed = TRUE),
                                       "Eigenvector centrality" = eigen_centrality(graph,directed = TRUE)$vector,
                                       "in harmonic centrality" = harmonic_centrality(graph,mode = "in"),
                                       "out harmonic centrality" = tryCatch({harmonic_centrality(graph,mode = "out")}, 
                                                                            error = function(e){cat("Error in harmonic centrality calculation:",
                                                                                                    conditionMessage(e), "\n")}),
                                       "page_rank" = page_rank(graph)$vector,
                                       "power centrality" = tryCatch({power_centrality(graph)}, error = function(e){
                                         cat("Error power centrality calculation:", conditionMessage(e), "\n")}))
  
  # print(V(graph)$centrality_values)
  
  V(graph)$centrality_values_norm <- round((normalize(V(graph)$centrality_values)*(length(V(graph))-1)) + 1)
  # print(V(graph)$centrality_values_norm)
  V(graph)$node_colors <- colorRampPalette(colours_list)(length(V(graph)))[V(graph)$centrality_values_norm]
  # print(V(graph)$node_colors)
  
  plot <- plot(graph, ...,
               vertex.color = V(graph)$node_colors,       # Vertex color
               vertex.size = vertex.size,                # Vertex size
               vertex.label.color = vertex.label.color,     # Vertex label color
               vertex.label.cex = vertex.label.cex,          # Vertex label size
               edge.color = edge.color,             # Edge color
               edge.width = edge.width,# Edge width
               arrow.width = arrow.width,
               arrow.size = arrow.size,
               main = paste0(dataset_name,"_",centrality_measure))
  
  return(plot) 
  
}
centrality_graph_plots_creater <- function(graph, loops = FALSE,...) {
  
  centrality_measure_list <- c("in degree_loops","out degree_loops",
                               "in degree_loopless","out degree_loopless",
                               "in closeness","out closeness","in local efficiency",
                               "out local efficiency","hub score",
                               "Betweenness Centrality","Eigenvector centrality",
                               "in harmonic centrality","out harmonic centrality",
                               "page_rank","power centrality")
  if (loops == TRUE) {
    centrality_measure_list <- centrality_measure_list[-c(3:4)]
  } else {
    centrality_measure_list <- centrality_measure_list[-c(1:2)]
  }
  
  for (i in seq_along(centrality_measure_list)){ 
    plot_by_centrality(graph, centrality_measure = centrality_measure_list[[i]],
                       ...)
    
    print(i)
  }
}

