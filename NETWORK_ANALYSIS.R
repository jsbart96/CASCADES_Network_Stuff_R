# igraph R PACKAGE FUNCTION GLOSSARY
# https://igraph.org/r/doc/index.html
# igraph (R interface)
# https://r.igraph.org/articles/igraph.html
# Introduction to Network Analysis Using R
# https://yunranchen.github.io/intro-net-r/index.html

# LOAD IN DEPENDENCIES
library(pacman)
p_load(tidyverse,igraph, ggnetwork,intergraph, backbone,Matrix, purrr, here,patchwork,ggpubr,stringr)
# SET OPTIONS TO AVOID SCIENTIFIC NOTATION
options(scipen = 999)
# LOAD IN FUNCTIONS
source("C:/Users/js1771/OneDrive - University of York/Documents/Work related/Cascades/CASCADES Network/network_analysis_functions.R")

# LOAD IN DATA ------------------------------------------------------------

Production_df  <- read_delim("Country_By_Country_EUCombined_SectorsCombined_NetworkFormat.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
                            filter(`Seller Country` == `Purchaser Country`) %>% 
  select(2,3) %>% 
  rename(Country = `Purchaser Country`, Production = Weighting)

# READ IN DISPARITY FILTERED IGRAPH OJBECTS LIST
disparity_filtered_graphs_list <- readRDS(file = "disparity_filtered_igraphs_list.rds")
# READ IN DISPARITY FILTERED EDGELIST
disparity_filtered_edgelist <- readRDS(file = "disparity_filtered_edgelist.rds")

# READ IN UNFILTERED IGRAPH OJBECTS LIST
unfiltered_igraph_list <- readRDS(file = "unfiltered_igraph_list.rds")
# READ IN UNFILTERED EDGELIST
unfiltered_edgelist <- readRDS(file = "unfiltered_edgelist.rds")

alpha2_country_name_concordance <- read_csv("alpha2_country_name_concordance.csv")

# ASSIGN ATTRIBUTES TO GRAPHS TBC ---------------------------------------------
# ASSIGN EACH EDGE AN ID
E(disparity_filtered_graphs_list[[1]])$edge_id <- seq_len(ecount(disparity_filtered_graphs_list[[1]]))
# ASSIGN PRODUCTION ATTRIBUTE TO NODES
V(disparity_filtered_graphs_list[[1]])$Production <- Production_df$Production

# GIVE GRAPH FULL COUNTRY NAMES
V(disparity_filtered_graphs_list[[1]]) %>% as_ids() %>% data.frame() %>%
  rename(Code = ".") %>% 
  left_join(alpha2_country_name_concordance, by = c("Code")) %>% 
  rename(Country_name = "Name") %>% 
  mutate(Name = case_when(
                        .$Code == "EU", "European Union",
                        .$Code == "WL", "Rest of Oceania",
                        .$Code == "TR", "Turkiye",

  ))

V(disparity_filtered_graphs_list[[1]])$country_names <- 

# GRAPH TOPOLOGY AND DISTRIBUTIONS PLOTS  ---------------------------------------------------

# UNFILTERED GRAPH - RAW DATA - HISTOGRAM DEGREE DISTRIBUTION
subgraph.edges(unfiltered_igraph_list[[2]],E(unfiltered_igraph_list[[2]])[Weight != 0]) %>% degree(mode = "in") %>% hist(xlab = "in degree, k")
subgraph.edges(unfiltered_igraph_list[[2]],E(unfiltered_igraph_list[[2]])[Weight != 0]) %>% degree(mode = "out") %>% hist(xlab = "out degree, k")

subgraph.edges(unfiltered_igraph_list[[2]],E(unfiltered_igraph_list[[2]])[Weight >= 1e-6]) %>% degree(mode = "in") %>% hist(xlab = "in degree, k")
subgraph.edges(unfiltered_igraph_list[[2]],E(unfiltered_igraph_list[[2]])[Weight  >= 1e-6]) %>% degree(mode = "out") %>% hist(xlab = "out degree, k")

# EDGES CUM DISTRUBTION 
df <- disparity_filtered_edgelist[[1]] %>% 
  arrange(desc(Weight)) %>% 
  mutate(cum_sum = cumsum(Weight)) %>% 
  mutate(cum_sum_perc = cum_sum*100/sum(Weight)) %>% 
  mutate(number = row_number()) 


# DEGREE DISTRIBUTIONS
degree_distribution_plot <- function(graph, x,y, mode, x_breaks,y_breaks) {
  
  Probability <- degree_distribution(graph, v = V(graph), mode = mode, loops = FALSE,cumulative = T)[-c(1)]
  df <- data.frame(Prob = Probability, Degree = 1:length(Probability))
  
  p <- ggplot(df,aes(x = {{x}}, y = {{y}})) +
    geom_point() +
    xlab(paste0(mode,"-Degree, k")) +
    ylab("Probability P(k)") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks)
  
  return(p)
  
}
p <- disparity_filtered_graphs_list[[1]] %>% degree_distribution_plot(x = Degree, y = Prob,mode = "in", y_breaks = seq(0,0.25,by = 0.05), x_breaks = seq(1:13))
p2 <- disparity_filtered_graphs_list[[1]] %>% degree_distribution_plot(x = Degree, y = Prob,mode = "out", y_breaks = seq(0,0.4,length.out =5), x_breaks = seq(1:13))
p3 <- disparity_filtered_graphs_list[[2]] %>% degree_distribution_plot(x = Degree, y = Prob,mode = "in", y_breaks = seq(0,0.012,by = 0.004),seq(0,1400,length.out = 8))
p4 <- disparity_filtered_graphs_list[[2]] %>% degree_distribution_plot(x = Degree, y = Prob,mode = "out", y_breaks = seq(0,0.01, by = 0.002),seq(0,1400,length.out = 8))
ggarrange(p,p2,p3,p4, ncol = 2, nrow = 2, labels = c(LETTERS[1:4]))

# IN DEGREE VS RANK
degree_vs_rank_plot <- function(graph, x,y, mode, x_breaks, y_breaks) {
  
  ordered_degree = sort(degree(graph, v = V(graph),mode = mode),decreasing = T)
  df <- data.frame(Degree = ordered_degree, Rank = 1:length(V(graph)))
  p <-ggplot(df,aes(x = {{x}}, y = {{y}})) +
    geom_point() +
    xlab("Rank") +
    ylab(paste0(mode,"-Degree, k")) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks)
  
  return(p)
         
}
p <- disparity_filtered_graphs_list[[1]] %>% degree_vs_rank_plot(x = Rank, y = Degree,mode = "in", y_breaks = seq(1:13), x_breaks = seq(1,23, by = 2))
p2 <- disparity_filtered_graphs_list[[1]] %>% degree_vs_rank_plot(x = Rank, y = Degree,mode = "out", y_breaks = seq(1:13), x_breaks = seq(1,23, by = 2))
p3 <- disparity_filtered_graphs_list[[2]] %>% degree_vs_rank_plot(x = Rank, y = Degree,mode = "in", y_breaks = seq(0,1500, by = 250), x_breaks = seq(0,4000, by = 1000))
p4 <- disparity_filtered_graphs_list[[2]] %>% degree_vs_rank_plot(x = Rank, y = Degree,mode = "out", y_breaks = seq(0,1200, by = 200),x_breaks = seq(0,4000, by = 1000))
ggarrange(p,p2,p3,p4, ncol = 2, nrow = 2, labels = c(LETTERS[1:4]))

# WEIGHTED DEGREE HISTOGRAM DISTRIBUTION
node_strength_hist <- function(graph, mode) {
  
  strength <- strength(graph,mode = mode,weights = E(graph)$Weight, loops = FALSE)
  hist(strength,xlab = paste0(mode,"-strength"))
  
} # could change to ggplot2? 
# WEIGHTED DEGREE CUMULATIVE DISTRIBUTION PLOT
weighted_cum_dist_plot <- function(graph, mode) {
  
  strength <- strength(graph,mode = mode,weights = E(graph)$Weight, loops = FALSE) %>% sort(decreasing = T)
  total_strength <- sum(strength)
  strength_cumsum <- strength %>% cumsum()
  strength_cumsum_perc <- strength_cumsum*100/total_strength
  df <- data.frame(strength = strength, total_strength = total_strength, strength_cumsum, strength_cumsum_perc)
  p <- df %>% 
   ggplot(aes(x = 1:length(strength), y = strength_cumsum_perc)) +
     geom_point() +
    labs(x = "Ranked Nodes", y = paste0("Cumulative Distribution of ",mode, "-strength (%)")) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  

  return(df)
}
# WEIGHTED DEGREE VS RANK
weighted_degree_vs_rank <- function(graph, mode) {
  
  strength <- strength(graph,mode = mode,weights = E(graph)$Weight, loops = FALSE)
  ordered_strength <- sort(strength, decreasing = T)
  df <- data.frame(node_strength = ordered_strength, Rank = c(1:length(strength)))
  p <- df %>% 
    ggplot(aes(y = node_strength , x = Rank)) +
    geom_point() +
    labs(x = "Ranked nodes", y = paste0(mode, "-strength")) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(p)
  
}
# EDGE WEIGHTS CUMULATIVE DISTRIBUTION
edge_weight_cumsum_plot <- function(graph) {
  
  edge_weights <- E(graph)$Weight %>% sort(decreasing = T)
  df <- data.frame(edge_weights = edge_weights, total_weight = sum(edge_weights)) %>% 
    mutate(edge_weight_cum_sum = cumsum(edge_weights), 
           cum_sum_perc = edge_weight_cum_sum*100/total_weight,
           Ranking = row_number())
  
  p <- df %>% 
    ggplot(aes(x = Ranking, y = cum_sum_perc)) +
    geom_point()+
    labs(x = "Ranked Edges", y = "Cumulative Distribution of Edge Weights (%)") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(p)
  
  
}
# IN DEGREE VS OUT DEGREE
in_vs_out_degree <- function(graph) {
  
  in_degree <- degree(graph, v = V(graph),mode = "in")
  out_degree <- degree(graph, v = V(graph),mode = "out")
  df <- tibble(`In_Degree` = in_degree,`Out_Degree` = out_degree)
  p <- df %>% 
    ggplot(aes(x = `In_Degree`, y = `Out_Degree`)) +
    geom_point() +
    xlab("In-Degree, k") +
    ylab("Out-Degree, k") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
    # scale_x_continuous(breaks = seq(min(df$In_Degree),max(df$In_Degree), length.out = 6))+
    # scale_y_continuous(breaks = seq(min(df$Out_Degree), max(df$Out_Degree), length.out = 6))

  return(p)
  
}
# IN WEIGHTED DEGREE VS WEIGHTED DEGREE
in_vs_out_weighted_degree <- function(graph) {
  
  in_strength <- strength(graph,mode = "in",weights = E(graph)$Weight, loops = FALSE)
  out_strength <- strength(graph,mode = "out",weights = E(graph)$Weight, loops = FALSE)
  df <- tibble(`In-strength` = in_strength,`Out-strength` = out_strength)
  p <- df %>% 
    ggplot(aes(x = `In-strength`/1e6, y = `Out-strength`/1e6)) +
    geom_point() +
    xlab("In-Strength, s (million)") +
    ylab("Out-Strength, s (million)") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(p)
  
}
# DEGREE VS WEIGHTED DEGREE
degree_vs_strength <- function(graph, mode) {
  
  degree <- degree(graph, v = V(graph),mode = mode)
  strength <- strength(graph,mode = mode,weights = E(graph)$Weight, loops = FALSE)
  df <- data.frame(degree = degree, strength = strength)
  p <- df %>% 
    ggplot(aes(x = degree, y = strength/1e6)) +
    geom_point() +
    xlab(paste0(mode, "-degree, k")) +
    ylab(paste0(mode, "-Strength, s (million)")) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(p)
}
# EDGE DISTRIBUTION HISTOGRAM 
edge_distribution_hist <- function(graph) {
  
  edge_weights <- E(graph)$Weight
  p <- hist(edge_weights)
  
}
# EDGE WEIGHT VS RANK 
edge_weight_vs_rank_plot <- function(graph) {
  
  ordered_edges = sort(E(graph)$Weight,decreasing = T)
  df <- data.frame(Edges = ordered_edges, Rank = 1:length(E(graph)))
  p <-ggplot(df,aes(x = Rank, y = Edges)) +
    geom_point() +
    xlab("Rank") +
    ylab("Edge Weight") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  return(p)

}

# unfiltered_igraph_list[1]

df <- disparity_filtered_graphs_list[1] %>% 
  tribble(~Data,
          .) %>% 
  unnest_longer(Data) %>% 
  mutate(Weighted_in_degree_dist = map(.x = Data, node_strength_hist, mode = "in"),
         Weighted_out_degree_dist = map(.x = Data, node_strength_hist, mode = "out"),
         weighted_cum_dist_plot_in = map(.x = Data, weighted_cum_dist_plot, mode = "in" ),
         weighted_cum_dist_plot_out = map(.x = Data, weighted_cum_dist_plot, mode = "out" ),
         weighted_in_degree_vs_rank_plot = map(.x = Data, weighted_degree_vs_rank, mode = "in"),
         weighted_out_degree_vs_rank_plot = map(.x = Data, weighted_degree_vs_rank, mode = "out"),
         in_vs_out_degree_plot = map(.x = Data, in_vs_out_degree),
         in_vs_out_weighted_degree_plot = map(.x = Data, in_vs_out_weighted_degree),
         degree_vs_strength_plot = map(.x = Data, degree_vs_strength, "in"),
         degree_vs_strength_plot = map(.x = Data, degree_vs_strength, "out"),
         edge_weight_cum_plot = map(.x = Data, edge_weight_cumsum_plot),
         edge_dist_hist_plot = map(.x = Data, edge_distribution_hist),
         edgeweight_vs_rank_plot = map(.x = Data, edge_weight_vs_rank_plot))

# PLOT UNFILTERED AND FILTERED IN SINGLE PLOT 
ggarrange(df1$edge_weight_cum_plot[[1]],df$edge_weight_cum_plot[[1]], ncol = 2, labels = c(LETTERS[1:2]))
# PLOT FILTERED STRENGTH DISTRIBUTIONS IN SINGLE PLOT
ggarrange(df$weighted_cum_dist_plot_in[[1]],df$weighted_cum_dist_plot_out[[1]], ncol = 2, labels = c(LETTERS[1:2]))





# BASE GRAPH PLOTS AND METRIC PLOTS ----------------------------------

p <- disparity_filtered_graphs_list[[1]] %>% 
  plot( 
    vertex.color = "orange",       # Vertex color
    vertex.size = 20,                # Vertex size
    vertex.label.color = "black",     # Vertex label color
    vertex.label.cex = 1,          # Vertex label size
    edge.color = "gray",             # Edge color
    edge.width = 2,                  # Edge width
    edge.arrow.size = 0.8,           # arrow size
    edge.arrow.width = 0.8)


p2 <- plot(unfiltered_igraph_list[[1]], 
    vertex.color = "orange",       # Vertex color
    vertex.size = 20,                # Vertex size
    vertex.label.color = "black",     # Vertex label color
    vertex.label.cex = 0.8,          # Vertex label size
    edge.color = "gray",             # Edge color
    edge.width = 2,                  # Edge width
    edge.arrow.size = 0.8,           # arrow size
    edge.arrow.width = 0.8,
    main = "unfltr_CxC_EUAgg_SectorsAgg"    # Title for the plot
  )


ggarrange(p,p2, ncol = 2)

# INDICATORS PLOT LOOP
disparity_filtered_graphs_list[[2]] %>% 
  centrality_graph_plots_creater(dataset_name = "Dsp_Fltr_CxC_EUAgg_SectorsAgg_Loopless_Alpha_0.05",
                                 loops = FALSE)



# GENERATE DATAFRAME OF GLOBAL GRAPH METRICS  -------------------------------------------------------

# GLOBAL METRICS BIG DF
disp_global <- disparity_filtered_graphs_list %>% 
  global_metrics_igraph_list_to_big_df()

# LIST OF NODE METRICS DF
disparity_nodes_df_list <- disparity_filtered_graphs_list %>% 
  graphs_list_to_node_metric_df_list() 
# EDGE LIST METRICS
disp_edgelist_metrics <- graphs_list_to_edge_metrics_df_list(disparity_filtered_graphs_list,disparity_filtered_edgelist)
# SECTOR DISAGG
# view(disp_edgelist_metrics[[1]])
# # SECTOR AGG
# view(disp_edgelist_metrics[[2]])


# SAVE THE METRIC DATASETS -----------------------------------------------------------------------

# SAVE DISPARITY FILTERED NODE BASED METRIC DATAFRAMES LIST
disparity_nodes_df_list %>% 
  saveRDS(file = "disparity_nodes_metric_df_list.rds")
# SAVE DISPARITY FILTERED EDGE BASED METRIC DATAFRAMES EDGELIST
disp_edgelist_metrics %>% 
  saveRDS(file = "disp_edgelist_metrics.rds")


# LOAD IN METRIC DATA -----------------------------------------------------

# LOAD DISPARITY FILTERED NODE BASED METRIC DATAFRAMES LIST
disparity_nodes_df_list <-  readRDS(file = "disparity_nodes_metric_df_list.rds")
# LOAD DISPARITY FILTERED EDGE BASED METRIC DATAFRAMES EDGELIST
disp_edgelist_metrics <- readRDS(file = "disp_edgelist_metrics.rds")


# TOP X BY DIFFERENT METRICS  ---------------------------


topXdfs <- disparity_nodes_df_list %>% 
    tribble(~ Node_Data, ~ Edge_Data, ~ igraph_objects,
              .,disp_edgelist_metrics,disparity_filtered_graphs_list) %>% 
    unnest_longer(col = c(Node_Data, Edge_Data,igraph_objects)) %>% 
    select(-c(Node_Data_id,igraph_objects_id)) %>% 
    select(Edge_Data_id,Node_Data, Edge_Data, everything()) %>% 
    rename(Data_id = Edge_Data_id) %>% 
    mutate(Top_In_Degree_Nodes = map(.x = .$Node_Data,topX, `in degree`,top = "all"),
           Top_Out_Degree_Nodes = map(.x = .$Node_Data,topX, `out degree`,top = "all"),
           Top_in_closeness_Nodes = map(.x = .$Node_Data,topX, `in closeness`, top = "all"),
           Top_Out_closeness_Nodes = map(.x = .$Node_Data,topX, `out closeness`, top = "all"),
           Top_Betweenness_Centrality_Nodes = map(.x = .$Node_Data,topX, `Betweenness Centrality`, top = "all"),
           Top_Betweenness_Edges = map(.x = .$Edge_Data,topX_edges, edge_betweenness, top = "all"),
           Top_Weighted_Edges = map(.x = .$Edge_Data,topX_edges, Weight, top = "all"),
           In_out_strength = map(.x = .$igraph_objects, ~ my_in_out_strength(.)))

topXdfs$Top_in_closeness_Nodes[[1]] %>% mutate(edge_betweenness = round(edge_betweenness,digits= 3)) %>% 
  write.csv(file = "edge_betweenness_all_nodes.csv")


# EU NEIGHBOURS PLOTS ---------------------------------------------------------------


# MAKE EU 1ST DEGREE -IN- NEIGHBOURS GRAPH
EU_neighbours <- make_ego_graph(disparity_filtered_graphs_list[[1]],mode = "in", nodes = "EU", order = 1)[[1]]
# SET NODE COLOURS AND TEXT COLOURS
V(EU_neighbours)[V(EU_neighbours)$name == "EU"]$colour <- "#001489"
V(EU_neighbours)[V(EU_neighbours)$name != "EU"]$colour <- "lightblue"
V(EU_neighbours)[V(EU_neighbours)$name == "EU"]$text_colour <- "#FFDD00"
V(EU_neighbours)[V(EU_neighbours)$name != "EU"]$text_colour <- "black"

EU_neighbours %>% 
  plot( 
    vertex.color = V(EU_neighbours)$colour,       # Vertex color
    vertex.size = 20,                # Vertex size
    vertex.label.color = V(EU_neighbours)$text_colour,     # Vertex label color
    vertex.label.cex = 1,          # Vertex label size
    edge.color = adjustcolor("black",alpha = 0.9),             # Edge color
    edge.width = 2,                  # Edge width
    edge.arrow.size = 0.8,           # arrow size
    edge.arrow.width = 0.8,          # arrow width
    edge.arrow.color = adjustcolor("black",alpha = 0.5),
    main = "Dsp_Fltr_CxC_EUAgg_SectorsAgg_Alpha_0.05_EU_1st_order_In_Neighbours")    # Title for the plot

# PLOT FULL GRAPH BUT WITH EU IN NEIGHBOURS COLOUR CODED
# NODE COLOURS
V(disparity_filtered_graphs_list[[1]])[V(disparity_filtered_graphs_list[[1]])$name  %in% V(EU_neighbours)$name]$EU_in_neighbours <- "lightblue"
V(disparity_filtered_graphs_list[[1]])[!V(disparity_filtered_graphs_list[[1]])$name  %in% V(EU_neighbours)$name]$EU_in_neighbours <- "orange"
V(disparity_filtered_graphs_list[[1]])["EU"]$EU_in_neighbours <- "#001489"
# EDGE LINE TYPES
E(disparity_filtered_graphs_list[[1]])[E(disparity_filtered_graphs_list[[1]])$edge_id  %in% E(EU_neighbours)$edge_id]$edge_line_type <- 1
E(disparity_filtered_graphs_list[[1]])[!E(disparity_filtered_graphs_list[[1]])$edge_id  %in% E(EU_neighbours)$edge_id]$edge_line_type <- 2
# EDGE COLOUR
E(disparity_filtered_graphs_list[[1]])[E(disparity_filtered_graphs_list[[1]])$edge_id  %in% E(EU_neighbours)$edge_id]$edge_colour <- adjustcolor("black",alpha = 0.9)
E(disparity_filtered_graphs_list[[1]])[!E(disparity_filtered_graphs_list[[1]])$edge_id  %in% E(EU_neighbours)$edge_id]$edge_colour <- "gray"
# VERTEX LABEL COLOUR
V(disparity_filtered_graphs_list[[1]])[V(disparity_filtered_graphs_list[[1]])$name == "EU"]$text_colour <- "#FFDD00"
V(disparity_filtered_graphs_list[[1]])[V(disparity_filtered_graphs_list[[1]])$name != "EU"]$text_colour <- "black"

disparity_filtered_graphs_list[[1]] %>% 
  plot( 
    vertex.color = V(disparity_filtered_graphs_list[[1]])$EU_in_neighbours,       # Vertex color
    vertex.size = 20,                # Vertex size
    vertex.label.color = V(disparity_filtered_graphs_list[[1]])$text_colour,     # Vertex label color
    vertex.label.cex = 0.8,          # Vertex label size
    edge.color = E(disparity_filtered_graphs_list[[1]])$edge_colour,             # Edge color
    edge.width = 2,                  # Edge width
    edge.lty = E(disparity_filtered_graphs_list[[1]])$edge_line_type,
    edge.arrow.size = 0.8,           # arrow size
    edge.arrow.width = 0.8,          # arrow width
    main = "Dsp_Fltr_CxC_EUAgg_SectorsAgg_Alpha_0.05_EU_1st_order_In_Neighbours")
  
  

# DISTRIBUTION OF EU FIRST ORDER IN-EDGES  --------------------------------


# MAKE FIRST ORDER EU NEIGHBOURS GRAPH
first_order_incident_graph_maker <- function(graph, target = "EU") {
  
  all_edges <- E(graph) %>% as_ids()
  in_edges <- grepl(pattern = paste0("\\|",target), x = all_edges)
  in_edges_total <- E(graph)[in_edges]
  in_neighbours_graph <- subgraph.edges(graph,in_edges_total) 
  
  return(in_neighbours_graph)
  
}
# PLOT STAR GRAPH
EU_in_neighbours <- disparity_filtered_graphs_list[[1]] %>% 
  first_order_incident_graph_maker() %>% plot()
# HISTOGRAM
E(EU_in_neighbours)$Weight %>% hist(breaks = 7)
# EDGE WEIGHT VS RANK PLOT
EU_in_neighbours %>% edge_weight_vs_rank_plot()

# EU FIRST ORDER IN NEIGHBOURS EDGES DATAFRAME
EU_in_neighbours_df <- disparity_filtered_edgelist[[1]] %>% filter(Purchaser == "EU") %>% 
                          arrange(desc(Weight)) %>% 
                          mutate(Ranking = row_number(),
                                 EU_Imports = Weight *100/ sum(Weight),.after = "Weight") %>% 
  select(-edge_id)
 EU_in_neighbours_df %>% write.csv(file = "EU_in_neighbours_df.csv")

# MAKE HIERARCHICAL TREE GRAPH - EU ROOT ----------------------------------

# GET EDGELIST OF FIRST ORDER IN NEIGHBOURS
first_order_EU <- disparity_filtered_edgelist[[1]] %>% 
                    filter(Purchaser == "EU") %>% 
                    mutate(Seller = paste0(Seller,".1"))
# GET NODE LIST
first_order_EU_node_list <- first_order_EU$Seller %>% substr(1,2)

# CREATE FIRST AND SECOND ORDER EDGELIST  
first_second_order_edgelist <- tribble(~Data, ~ first_order_neighbour,
              disparity_filtered_edgelist[[1]], first_order_EU_node_list) %>% 
  unnest_longer(col = first_order_neighbour) %>% 
  mutate(second_order_edgelists = map2(.x = Data, .y = first_order_neighbour, ~ {
    
    df <- .x %>% 
      filter(Purchaser == .y) %>% 
      # filter(Seller != "EU") %>% 
      # mutate(Seller = paste0(Seller,"_",Purchaser))
      mutate(Purchaser = paste0(Purchaser,".1")) %>% 
      mutate(Seller = paste0(Seller,".",2))
    return(df)
    
  })) %>%  # MAKES SECOND ORDER EDGELISTS
  select(second_order_edgelists) %>% 
  as.list() %>% 
  do.call(bind_rows,.) %>% 
  bind_rows(first_order_EU) %>% 
  group_by(Seller) %>% 
  mutate(numbers = n()) %>% 
  mutate(subclass = LETTERS[1:numbers]) %>%
  ungroup() %>% 
  mutate(Seller = ifelse(.$Purchaser == "EU" | .$numbers == 1, Seller,
                         paste0(Seller,subclass))) %>% 
  select(-subclass)

# MAKE SECOND ORDER IGRAPH OBJECT 
first_second_order_graph <- first_second_order_edgelist %>% 
  graph_from_data_frame(directed = TRUE)
# ADD NODE AND EDGE IDS 
V(first_second_order_graph)$node_id <- 1:vcount(first_second_order_graph)
E(first_second_order_graph)$edge_id <- 1:ecount(first_second_order_graph)

# PLOT BASIC
plot(first_second_order_graph,
     vertex.color = V(first_second_order_graph)$node_colour,
     vertex.size = 15,
     vertex.label.cex = 0.5,
     edge.arrow.size = 0.5)

# INDEX THE NODE LAYERS
first_layer_graph <- make_ego_graph(first_second_order_graph,order = 1,nodes = "EU")[[1]]
first_layer_nodes <- V(first_layer_graph)$node_id
second_layer_nodes <- !V(first_second_order_graph)$node_id  %in%  V(first_layer_graph)$node_id

# ASSIGN COLOURS BASED ON LAYER
V(first_second_order_graph)[second_layer_nodes]$colour <- "orange"
V(first_second_order_graph)[first_layer_nodes]$colour <- "lightblue"
V(first_second_order_graph)["EU"]$colour <- "#001489"
V(first_second_order_graph)[V(first_second_order_graph)$name == "EU"]$text_colour <- "#FFDD00"
V(first_second_order_graph)[V(first_second_order_graph)$name != "EU"]$text_colour <- "black"
# PLOT WITH COLOURED LAYERS
plot(first_second_order_graph,
     vertex.color = V(first_second_order_graph)$colour,
     vertex.size = 16,
     vertex.label.color = V(first_second_order_graph)$text_colour,
     vertex.label.cex = 0.50,
     edge.arrow.size = 0.5)


# ASSIGN LEVEL OF NODE LAYERS
V(first_second_order_graph)[second_layer_nodes]$layer <- 3
V(first_second_order_graph)[first_layer_nodes]$layer <- 2
V(first_second_order_graph)["EU"]$layer <- 1
layers <- V(first_second_order_graph)$layer
# PLOT AS TREE
plot(first_second_order_graph, layout = layout_with_sugiyama(first_second_order_graph, layers = layers),
     vertex.size = 15,
     vertex.labels = 3,
     vertex.color = V(first_second_order_graph)$colour,
     vertex.label.color = V(first_second_order_graph)$text_colour,
     edge.arrow.size = 0.5)


# SHOW PATH DISTANCE FROM ALL NODES TO EU
distances_df <- distances(first_second_order_graph, weights = E(first_second_order_graph)$Weight, mode = "out", to = "EU") %>% 
  as.data.frame(row.names = FALSE)
# SHOW PATH FROM ALL NODES TO EU
paths <- shortest_paths(first_second_order_graph, from = "EU", mode = "in", weights = E(first_second_order_graph)$Weight,output = "vpath")
edge_path_strings <- paths$vpath %>% 
  map(function(x){
    
    x %>% 
      rev() %>% 
      as_ids() %>% 
      reduce(paste,sep = "->")
    
  }) # ADD -> SYMBOL FOR EDGES 
# ADD CHARACTER STRING PATHS 
distances_to_EU_df <- distances_df %>% 
  mutate(edge_path = edge_path_strings,.before = "EU") 
  




# FAILED NODE SCENARIOS ---------------------------------------------------

# FAILED NODE SCENARIO FUNCTION - FIND AFFECT ON EU IMPORTS AND EXPORTS
failed_node_EU_effect <- function(graph, failed_node,return_full = FALSE) {
  
  # CREATE COPY OF GRAPH
  graph1 <- graph
  
  # SEARCH FIRST LAYER
  failed_first_layer_edge_id <- E(graph1)[E(graph1) %>% as_ids() == paste0(failed_node,".1|EU")]$edge_id
  failed_first_layer_edge_weight <- E(graph1)[failed_first_layer_edge_id]$Weight
  failed_first_layer_edge_name <- E(graph1)[failed_first_layer_edge_id] %>% as_ids()
  # UPDATE FIRST LAYER EXPORT TO EU
  E(graph1)[failed_first_layer_edge_id]$Weight <- 0
  # CHANGE AND PERC CHANGE IN EXPORT
  change_in_first_layer_failed_edge_weight <- E(graph1)[failed_first_layer_edge_id]$Weight - E(graph)[failed_first_layer_edge_id]$Weight
  change_in_first_layer_failed_edge_weight_perc <- (change_in_first_layer_failed_edge_weight*100) / E(graph)[failed_first_layer_edge_id]$Weight

  # SEARCH SECOND LAYER - FIND FAILED SECOND ORDER EDGE IDS AND WEIGHTS
  failed_second_order_edges <- E(graph1)[str_detect(E(graph1) %>% as_ids(),pattern = paste0(failed_node,".2"))]
  failed_second_order_weight <- E(graph1)[failed_second_order_edges]$Weight
  change_in_failed_second_order_weight <- 0 - failed_second_order_weight
  change_in_failed_second_order_weight_perc <- change_in_failed_second_order_weight*100 / failed_second_order_weight
  # FIND AFFECTED FIRST LAYER NODES
  affected_first_layer_nodes <- head_of(graph1,E(graph1)[failed_second_order_edges]) 
  # MAKE SUBGRAPH OF AFFECTED FIRST LAYER NODES ONLY
  affected_first_layer_nodes_subgraph <- subgraph.edges(graph1,eids = affected_first_layer_nodes)
  # FIND WEIGHTS OF AFFECTED FIRST LAYER EDGES TO EU
  affected_first_layer_nodes_weights <- E(affected_first_layer_nodes_subgraph)$Weight
  # FIND TOTAL IMPORTS OF AFFECTED FIRST LAYER NODES TO EU
  affected_first_order_node_imports <- strength(graph1,vids =affected_first_layer_nodes, mode = "in", weights = E(graph1)$Weight)
  # FIND PRODUCTION OF AFFECTED FIRST LAYER NODES 
  affected_first_order_node_production <- Production_df %>% filter(Country  %in%  substring(affected_first_layer_nodes %>% as_ids(),1,2)) 
  # MAKE DF WITH ABOVE INFORMATION AND CALCULATE NEW EXPORTS TO EU
  df <- tibble(Affected_edges = failed_second_order_edges %>% as_ids(),
           Weight = failed_second_order_weight,
           first_order_node = affected_first_layer_nodes %>% as_ids(),
           first_order_edge_weight = affected_first_layer_nodes_weights,
           imports = affected_first_order_node_imports) %>% 
    bind_cols(affected_first_order_node_production) %>% 
    mutate(Multiplier = 1-(Weight/(Production+imports))) %>% 
    mutate(New_export_to_EU = first_order_edge_weight * Multiplier,
           Change_in_exports = New_export_to_EU-first_order_edge_weight,
           Change_in_exports_perc = Change_in_exports*100/first_order_edge_weight)
  
  # UPDATE AFFECTED FIRST LAYER EXPORTS TO EU
  E(graph1)[affected_first_layer_nodes]$Weight<- df$New_export_to_EU
  # CALCULATE NEW EU IMPORTS AND CHANGE
  new_EU_imports <- strength(graph1,vids ="EU", mode = "in", weights = E(graph1)$Weight)
  # ORIGINAL EU IMPORTS
  original_EU_imports <- strength(graph,vids ="EU", mode = "in", weights = E(graph)$Weight)
  # CHANGE IN IMPORTS AND PERCENTAGE CHANGE
  change_in_imports <- new_EU_imports - original_EU_imports
  change_in_imports_perc <- (change_in_imports*100)/original_EU_imports
  # ERROR CHECKING
  number_match <- round(change_in_imports,digits = 0) == round(x = sum(change_in_first_layer_failed_edge_weight,df$Change_in_exports), digits = 0)
  if(number_match == FALSE)
    stop("WEIGHTS DON'T MATCH")
  
  # GET LIST OF ALL AFFECTED EDGES - SECOND LAYER, KNOCK ON FIRST LAYER, FIRST LAYER
  all_affected_edges <- c(as_ids(failed_second_order_edges),
                          as_ids(E(affected_first_layer_nodes_subgraph)),
                          failed_first_layer_edge_name)
  # GET LIST OF CHANGE IN EDGE WEIGHTS AFTER NODE FAILURE
  change_in_edge_weights <- c(change_in_failed_second_order_weight,
                              df$Change_in_exports,
                              change_in_first_layer_failed_edge_weight)
  names(change_in_edge_weights) <- c(all_affected_edges)
  # PERCENTAGE CHANGES
  change_in_edge_weights_perc <- c(change_in_failed_second_order_weight_perc,
                              df$Change_in_exports_perc,
                              change_in_first_layer_failed_edge_weight_perc)
  names(change_in_edge_weights_perc) <- c(all_affected_edges)
  
  # FIND CHANGES IN EU EXPORTS AND PERCENTAGE CHANGE
  EU_original_exports <- strength(disparity_filtered_graphs_list[[1]],vids ="EU", mode = "out", weights = E(disparity_filtered_graphs_list[[1]])$Weight)
  Imports_from_failed_nodes <- -change_in_imports
  EU_production <- Production_df %>% filter(Country == "EU") %>% pull(Production)
  New_EU_exports <- EU_original_exports * (1 - Imports_from_failed_nodes/(EU_production+original_EU_imports))
  change_in_EU_exports <- New_EU_exports - EU_original_exports
  change_in_EU_exports_perc <- change_in_EU_exports*100/EU_original_exports
  
  # PUT RESULTS IN DATAFRAME
  # MODIFY AFFECTED EDGE LIST FOR DF
  Affected_edges <- Reduce(function(x,y)paste0(x,", ",y), all_affected_edges) %>% str_replace_all(pattern = "\\|","\\->")
  results_df <- data.frame(Failed_node = failed_node,
                           Affected_edges = Affected_edges,
                           Total_No_of_affected_edges = length(all_affected_edges),
                           EU_Import_Losses = change_in_imports,
                           EU_Import_Losses_perc = change_in_imports_perc,
                           EU_Export_Losses = change_in_EU_exports,
                           EU_Export_Losses_perc = change_in_EU_exports_perc)
  
  # PLACE RELEVANT INFO IN RETURN LIST FOR PRINTING - FULL OR JUST DATAFRAME OPTION IN FUNCTION ARGS
  if(return_full == TRUE) {
  return_list <- list(change_in_imports,change_in_imports_perc,change_in_edge_weights,change_in_edge_weights_perc,change_in_EU_exports,change_in_EU_exports_perc,number_match)
  names(return_list) <- c("change_in_EU_imports","change_in_EU_imports_perc","change_in_edge_weights","change_in_edge_weights_perc","change_in_EU_exports","change_in_EU_exports_perc", "number_match")
  } else{return_list <- results_df }
 return(return_list) 
  
  
}

# LOOP THROUGH ALL POSSIBLE NODES - FIND IMPACT ON EU EXP AND IMPS
all_possible_failed_nodes <- V(first_second_order_graph) %>% as_ids() %>% substring(1,2) %>% unique()
results <- list()
for (i in seq_along(all_possible_failed_nodes)) {
  
  print(i)
  
  results[[i]] <- first_second_order_graph %>% 
  failed_node_EU_effect(failed_node = all_possible_failed_nodes[[i]],return_full = TRUE)
  
  names(results)[[i]] <- all_possible_failed_nodes[[i]]
  
}
# CREATE RESULTS DATAFRAME 
results_df <- results %>% 
  do.call(bind_rows,.) %>% 
  remove_rownames() %>% 
  arrange(EU_Import_Losses) %>% 
  mutate(across(where(is.numeric), ~round(.,digits = 1))) 
# SAVE AS CSV
results_df %>% 
  write.csv(file = "failed_node_EU_impact_results_table.csv")


# THIRD ORDER EU NEIGHBOUR EDGELIST ---------------------------------------
# CREATE THIRD ORDER EU NEIGHBOUR EDGELIST 
third_order_EU_nodes <- V(first_second_order_graph) %>% as_ids() %>% keep(function(x) substring(x,4,4) == "2")
third_order_EU_edgelist <- tribble(~Data, ~ Purchaser,
                          disparity_filtered_edgelist[[1]],third_order_EU_nodes) %>% 
  unnest_longer(col = Purchaser) %>% 
  mutate(third_order_EU = map2(.x = Data, .y = Purchaser, ~ {
    
    purchaser_node <- substring(.y,1,2)
    
    df <- .x %>% 
      filter(Purchaser == purchaser_node) %>% 
      filter(Seller != "EU") %>% 
      mutate(Purchaser = paste0(.y)) %>% 
      mutate(Seller = paste0(Seller,".",3))
    
    return(df)
    
  })) %>% 
  select(third_order_EU) %>% 
  as.list() %>% 
  do.call(bind_rows,.) %>% 
  group_by(Seller) %>% 
  mutate(numbers = n()) %>% 
  mutate(subclass = LETTERS[1:numbers]) %>% 
  mutate(Seller = paste0(Seller, subclass)) %>% 
  bind_rows(.,first_second_order_edgelist)


third_order_graph <- third_order_EU_edgelist %>% 
  graph_from_data_frame(directed = TRUE)

# INDEX NODES 
V(third_order_graph)$node_id <- 1:vcount(third_order_graph)
E(third_order_graph)$edge_id <- 1:ecount(third_order_graph)

# INDEX THE NODE LAYERS
first_layer_graph <- make_ego_graph(third_order_graph,order = 1,nodes = "EU")[[1]]
first_layer_nodes <-  V(third_order_graph)[V(third_order_graph) %>% as_ids() %>% substring(4,4) == "1"]$node_id
second_layer_nodes <- V(third_order_graph)[V(third_order_graph) %>% as_ids() %>% substring(4,4) == "2"]$node_id
third_layer_nodes <- V(third_order_graph)[V(third_order_graph) %>% as_ids() %>% substring(4,4) == "3"]$node_id

# ASSIGN COLOURS BASED ON LAYER
V(third_order_graph)[third_layer_nodes]$colour <- "chartreuse3"
V(third_order_graph)[second_layer_nodes]$colour <- "orange"
V(third_order_graph)[first_layer_nodes]$colour <- "lightblue"
V(third_order_graph)["EU"]$colour <- "#001489"
V(third_order_graph)[V(third_order_graph)$name == "EU"]$text_colour <- "#FFDD00"
V(third_order_graph)[V(third_order_graph)$name != "EU"]$text_colour <- "black"

# PLOT 3RD ORDER GRAPH
third_order_graph %>% plot( 
  vertex.color = V(third_order_graph)$colour,       # Vertex color
  vertex.size = 10,                # Vertex size
  vertex.label.color = "black",     # Vertex label color
  vertex.label.cex = 0.35,          # Vertex label size
  edge.color = "gray",             # Edge color
  edge.width = 2,                  # Edge width
  edge.arrow.size = 0.5,           # arrow size
  edge.arrow.width = 0.5)

# CHECKING METRIC CORRELATION ---------------------------------------------

col_names <- disparity_nodes_df_list[[2]] %>% select(-c("Nodes",contains("Ranking "))) %>% names()

col_pair <- combn(col_names,2, simplify = F)

disparity_nodes_df_list[[2]] %>% 
  map2(col_pair[1][1])

# CORRELATION BETWEEN METRICS - NODE LEVEL
disp_node_metrics_loopless_df <- disparity_nodes_df_list[[2]] %>% select(-contains("Ranking "))
node_metrics_correlation_df <- function(metrics_df) {
  metrics_corr_df <- data.frame(matrix(data = NA, nrow = 0, ncol = 0))
  metrics_df <- metrics_df %>% select(-1) 
  for (i in seq_along(1:ncol(metrics_df))) {
    metrics_corr_df1 <- data.frame(matrix(data = NA, nrow = 0, ncol = 0))
    print(paste0("this is ", i))
    for (j in seq_along(1:ncol(metrics_df))) {
      print(paste0("this is ", j))
      corr_i_j <- cor(x = metrics_df[,i], y = metrics_df[,j]) %>% round(2)
      lm_fit <- lm(metrics_df[,j] ~ metrics_df[,i], data = metrics_df)
      r_squared <- summary(lm_fit)$r.squared %>% round(2)
      metric_i <- colnames(metrics_df)[i]
      metric_j <- colnames(metrics_df)[j]
      df <- data.frame(Metric_1 = metric_i, Metric_2 = metric_j,correlation = corr_i_j, R2 = r_squared)
      metrics_corr_df1 <- bind_rows(metrics_corr_df1,df)
    }
    metrics_corr_df <- bind_rows(metrics_corr_df,metrics_corr_df1)
  }
  return(metrics_corr_df %>% filter(Metric_1 != Metric_2))
}
disp_node_metrics_loopless_correlation <- disp_node_metrics_loopless_df %>% node_metrics_correlation_df()


node_metrics_plots <- function(metrics_df) {
  metrics_corr_plots <- list()
  for (i in seq_along(1:(length(metrics_df)-1))) {
    list1 <- list()
    for (j in seq_along(1:(length(metrics_df)-1))) {
      print(paste0("i = ",i, " j = ",j))
      print(i,j)
      col1 <- i + 1
      col2 <- j + 1 
      list1[[j]] <- plot(x = metrics_df[col1], y = metrics_df[col2])
      
      names(list1)[[j]] <- paste0(colnames(metrics_df)[i+1]," vs ",colnames(metrics_df)[j+1])
      
    }
    metrics_corr_plots[[i]] <- list1
  }
  
  metrics_corr_plots <- metrics_corr_plots %>% unlist(use.names = TRUE, recursive = FALSE)
  return(metrics_corr_plots)
}

plot_list <- disp_node_metrics_loopless_df %>% 
  node_metrics_plots()


#   
# df for stats 
# distributions 
# averages, variance, ranges





# PATH LENGTHS STUFF - LEAVE FOR NOW  -------------------------------------------------------


# SINGLE SHORTEST PATH FROM CHOSEN VERTEX - EDGE PATH OUTPUT
disparity_filtered_graphs_list[[1]] %>%
  shortest_paths(from = "EU",mode = "in",output = "epath", algorithm = "dijkstra")

# ALL SHORTEST PATHS FROM CHOSEN VERTEX - EDGE PATH OUTPUT
disparity_filtered_graphs_list[[1]] %>%
  all_shortest_paths(from = 23,mode = "in")

# EDGE CONNECTIVITY
my_edge_connectivity <- edge_connectivity(disparity_filtered_graphs_list[[1]], source = "EU", target = "US")

# CLUSTERING + FLOW METRICS - LEAVE FOR NOW -------------------------------



# LEAVE FOR NOW 
# MINIMUM CUT BETWEEN TWO VERTICES TO SEPARATE THEM - GIVE SOURCE AND TARGET - OUTPUT: CUT VERTICES, NO., PARTITIONS OF CLUSTERS
disparity_filtered_graphs_list[[2]] %>%
  min_cut(source = "ZA", target = "EU", value.only = FALSE)

# MAX FLOW ('BOTTLENECK' VALUE) - GIVES MAX FLOW BETWEEN SOURCE TARGET NODE, FLOW, CUT, PARTITIONS, AND STATS
disparity_filtered_graphs_list[[2]] %>%
  max_flow(source = "CN", target = "EU")

# FIND CLUSTERS - LEAVE FOR NOW 
disparity_filtered_graphs_list[[2]] %>%
  clusters(mode = c("strong"))

disparity_filtered_graphs_list[[2]] %>%
  cluster_optimal()

disparity_filtered_graphs_list[[2]] %>%
  cluster_walktrap()

disparity_filtered_graphs_list[[2]] %>%
  cluster_edge_betweenness()









