# igraph R PACKAGE FUNCTION GLOSSARY
# https://igraph.org/r/doc/index.html
# igraph (R interface)
# https://r.igraph.org/articles/igraph.html
# Introduction to Network Analysis Using R
# https://yunranchen.github.io/intro-net-r/index.html

# LOAD IN DEPENDENCIES
library(pacman)
p_load(tidyverse,igraph, ggnetwork,intergraph, backbone,Matrix, purrr, here)
# SET OPTIONS TO AVOID SCIENTIFIC NOTATION
options(scipen = 999)
# LOAD IN FUNCTIONS
source("C:/Users/js1771/OneDrive - University of York/Documents/Work related/Cascades/CASCADES Network/network_analysis_functions.R")

# PREPARE DATA ------------------------------------------------------------

# LOAD IN DATA - EDGE LISTS 
# CxC_EUAgg_SectorsDisagg1 = read_delim("Country_By_Country_EU_Combined_Sectors_NonCombined_NetworkFormat.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
CxC_EUAgg_SectorsAgg1 = read_delim("Country_By_Country_EUCombined_SectorsCombined_NetworkFormat.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# RENAME DATA - START HERE FOR EASY RE-RUN
# CxC_EUAgg_SectorsDisagg = CxC_EUAgg_SectorsDisagg1
CxC_EUAgg_SectorsAgg = CxC_EUAgg_SectorsAgg1


# MAKE LIST OF DATASETS
#dataset_list <- list( CxC_EUAgg_SectorsAgg,CxC_EUAgg_SectorsDisagg)
dataset_list <- list(CxC_EUAgg_SectorsAgg)

# ALIGN COLUMN NAMES -----------------------------------------------------

# ALIGNED DATA COLNAMES
aligned_dataset_edgelist <- dataset_list %>% 
  map(align_colnames,my_names = c("Seller","Purchaser","Weight"))

# names(aligned_dataset_edgelist) <- c("CxC_EUAgg_SectorsAgg_Loopless","CxC_EUAgg_SectorsDisagg_Loopless")
names(aligned_dataset_edgelist) <- c("CxC_EUAgg_SectorsAgg_Loopless")

# MAKE UNFILTERED IGRAPH OBJECT LIST ------------------------------------------

# MAKE UNFILTERED IGRAPH OBJECTS LIST FROM DATASET EDGELIST
unfiltered_igraph_list <- aligned_dataset_edgelist %>% 
  map(graph_from_data_frame, directed = TRUE)
# ASSING NAMES
names(unfiltered_igraph_list) <- paste0("unfltr_",names(aligned_dataset_edgelist))


# EDGELIST
# RENAME UNFILTERED EDGELIST
unfiltered_edgelist <- aligned_dataset_edgelist
# ADD EDGE ID TO EDGELIST
# EDGES
unfiltered_edgelist <- unfiltered_edgelist %>% 
  map(function(x) {
    x %>% 
      mutate(edge_id = paste0(Seller,"|", Purchaser))
    
  })


# REMOVE LOOPS ------------------------------------------------------------

# REMOVE LOOPS FROM IGRPAH OBJECT
unfiltered_igraph_list <- unfiltered_igraph_list %>% 
  map(simplify, remove.loops = TRUE)

# REMOVE LOOPS FROM EDGELIST
unfiltered_edgelist <- unfiltered_edgelist %>% 
  map(remove_loops)

# ADD WEIGHT ATTRIBUTE BACK TO IGRAPH OBJECT
E(unfiltered_igraph_list[[1]])$Weight <- unfiltered_edgelist[[1]]$Weight
# E(unfiltered_igraph_list[[2]])$Weight <- unfiltered_edgelist[[2]]$Weight

# ASSIGN IDS TO EDGES 
unfiltered_igraph_list <- unfiltered_igraph_list %>% 
  map(function(x) { E(x)$edge_id <- 1:ecount(x)
  return(x)
  })
# ASSIGN IDS TO NODES
unfiltered_igraph_list <- unfiltered_igraph_list %>% 
  map(function(x) { V(x)$node_id <- 1:vcount(x)
  return(x)
  })



# MAKE DISPARITY FILTERED IGRAPH OBJECTS AND EDGELISTS  ------------------------------------

# LIST OF FILTERED IGRAPH OBJECTS BY USING DISPARITY FILTER
disparity_filtered_graphs_list <- unfiltered_igraph_list %>% 
  map(filter_graph,narrative = TRUE)

names(disparity_filtered_graphs_list) <- gsub(x = names(unfiltered_igraph_list), pattern = "un", replacement = "")
# LIST OF FILTERED EDGELISTS 
disparity_filtered_edgelist <- list()
disparity_filtered_edgelist[[1]] <- unfiltered_edgelist[[1]][as_ids(E(unfiltered_igraph_list[[1]]))  %in% as_ids(E(disparity_filtered_graphs_list[[1]])),]
# disparity_filtered_edgelist[[2]] <- unfiltered_edgelist[[2]][as_ids(E(unfiltered_igraph_list[[2]]))  %in% as_ids(E(disparity_filtered_graphs_list[[2]])),]

# GENERATE RANDOM NETWORK TBC -------------------------------------------------

# ERDOS - RENYI RANDOM GRAPHS
# MAKE RANDOM GRAPHS LIST BASED ON FILTERED DISPARITY GRAPHS
random_graphs <- disparity_filtered_graphs_list %>%
  map(generate_random_graph)
names(random_graphs) <- paste0("Random_",names(random_graphs))

random_graphs_list <- disparity_filtered_graphs_list %>%
  map(generate_random_graph_list,2)

# MAKE EDGELISTS OF RANDOM GRAPHS
random_graphs_edgelist <- random_graphs %>%
 map(as_adjacency_matrix,attr = "Weight", sparse = FALSE) %>%
 map(matrix_to_filtered_edgelist)
names(random_graphs_edgelist) <- paste0("Random_",names(random_graphs_edgelist))


# PREFERENTIAL ATTACHMENT

generate_pref_attach_graph <- function(vertices, no_of_graphs, power) {
  
  df <- tribble( ~graph_no, ~ no_vertices,
                 seq(1,no_of_graphs, by = 1), vertices) %>% 
    unnest_longer(col = graph_no) %>% 
    mutate(random_graphs = map(.x = no_vertices, sample_pa,power)) %>% 
    mutate(plots = map(.x = random_graphs, plot))
  
  return(df)
  
}



# SAVE DATA ---------------------------------------------------------------

# SAVE DISPARITY FILTERED IGRAPH OJBECTS LIST
disparity_filtered_graphs_list %>% 
  saveRDS(file = "disparity_filtered_igraphs_list.rds")
# SAVE DISPARITY FILTERED EDGELIST
disparity_filtered_edgelist %>% 
  saveRDS(file = "disparity_filtered_edgelist.rds")

# SAVE UNFILTERED IGRAPH OJBECTS LIST
unfiltered_igraph_list %>% 
  saveRDS(file = "unfiltered_igraph_list.rds")
# SAVE UNFILTERED EDGELIST
unfiltered_edgelist %>% 
  saveRDS(file = "unfiltered_edgelist.rds")


  
  


# FILTERING - VARIOUS ALPHA VALUES  ---------------------------------------

# MAP METHOD
# LIST OF FILTERED IGRAPH OBJECTS BY USING DISPARITY FILTER
disparity_filtered_graphs_list <- unfiltered_igraph_list[[1]] %>% 
  map(function(x){
    
    alpha_values <- c(NA,0.5*10^seq(0,-10,by=-1))
    
    igraph_objects <- list()
    
    for (i in seq_along(alpha_values))  {
      
      if(i == 1) {
        
        igraph_objects[[i]] <- x
      
      } else{
      
      igraph_objects[[i]] <-  x %>% 
        filter_graph(alpha = alpha_values[[i]])
      
      }
      print(i)
    }
    
    return(igraph_objects)
  })

disparity_filtered_graphs_list %>% 
  saveRDS(file = "disparity_filtered_graphs_list_multiple_A_values.rds")

disparity_filtered_graphs_list  <- readRDS(file = "disparity_filtered_graphs_list_multiple_A_values.rds")

# DF MUTATE METHOD
alpha_values <- c(1, 0.5*10^seq(0,-10,by=-1))
alpha_values_list <- c(alpha_values, alpha_values)

df <- unfiltered_igraph_list[[1]] %>% 
  tribble(~ Data, ~ filtered_list,
          ., disparity_filtered_graphs_list[[1]]) %>% 
  unnest_longer(col = c(filtered_list)) %>% 
  mutate(alpha = alpha_values) %>%
  mutate(nodes = map_dbl(filtered_list, function(x){
    return(gorder(x))
  })) %>% 
  mutate(nodes_perc = nodes *100 / 23) %>% 
  mutate(edges = map_dbl(filtered_list, function(x){
    return(gsize(x))
  })) %>% 
  mutate(edges_perc = edges *100 / 506) %>% 
  mutate(edges_weight_perc = map_dbl(filtered_list, function(x){ strength_perc <- sum(E(x)$Weight)* 100 / 8816602 
                                     return(strength_perc)})) %>% 
  mutate(edge_density = map_dbl(filtered_list, function(x){
  return(edge_density(x, loops = FALSE))
})) %>%
  # mutate(global_efficiency = map_dbl(filtered_list, function(x){
  #   return(global_efficiency(x))
  # })) %>%
  mutate(centr_betw = map_dbl(filtered_list, function(x){
    betw <- centr_betw(x, directed = TRUE)
    return(betw$centralization)
})) %>% 
  mutate(centr_indegree = map_dbl(filtered_list, function(x){
    indegree <- centr_degree(x,mode = "in",loops =FALSE)
    return(indegree$centralization)
  })) %>% 
  mutate(centr_outdegree = map_dbl(filtered_list, function(x){
    outdegree <-centr_degree(x,mode = "out",loops =FALSE)
    return(outdegree$centralization)
  })) %>% 
  mutate(diameter = map_dbl(filtered_list, function(x){
    diameter <- diameter(x,directed = TRUE, unconnected = FALSE,weights = E(x)$Weight)
    return(diameter)
  })) %>% 
  mutate(mean_distance = map_dbl(filtered_list, function(x){
    diameter <- mean_distance(x,directed = TRUE, unconnected = FALSE,weights = E(x)$Weight)
    return(diameter)
  }))

"global_efficiency"

vars <- c("nodes_perc","edges_perc","edges_weight_perc","edge_density","centr_betw","centr_indegree","centr_outdegree","diameter", "mean_distance") 
p <- list()
for (i in seq_along(vars)) {
 
  var <- sym(vars[i])
  
p[[i]] <- df %>% 
  ggplot(aes(x = -log10(alpha), y = !!var, group = Data_id, colour = Data_id)) + geom_line() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

print(i)

}

print(p)

# PLOT MANY P VS K PLOTS AS ALPHA CHANGES - LEAVE
df1 <- df %>% 
  mutate(p_vs_k_in_dataset = map2(.x = filtered_list, .y = alpha, ~ {
    
    Probability <- degree_distribution(.x, v = V(.x), mode = "in", loops = FALSE)[-c(1)]
    df <- data.frame(Prob = Probability, Degree = 1:length(Probability), alpha_value = .y)
    return(df)
    
  })) %>% 
  mutate(p_vs_k_inplot = map(.x = p_vs_k_in_dataset, ~ {
    
   p <- .x %>% 
     ggplot(aes(x = Degree, y = Prob)) +
     geom_line()
   
   return(p)
   
  }))


p_vs_in_k_multiple_value <- do.call(bind_rows, df1$p_vs_k_in_dataset)

p_vs_in_k_multiple_value %>% 
  ggplot(aes(x = Degree, y = Prob, group = alpha_value, colour = alpha_value)) +
  geom_line()







         
         