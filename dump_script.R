
# DEGREE DISTRIBUTIONS
# IN DEGREE DISTRIBUTION CXC SEC AGG
Probability <- degree_distribution(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]), mode = "in", loops = FALSE)[-c(1)]
df <- data.frame(Prob = Probability, Degree = 1:length(Probability))
p <- ggplot(df,aes(x = Degree, y = Prob)) +
  geom_point() +
  xlab("In-Degree, k") +
  ylab("Probability P(k)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = 1:length(df$Degree)) +
  scale_y_continuous(breaks = seq(0,0.25,by = 0.05))
# OUT DEGREE DISTRIBUTION CXC SEC AGG
Probability <- degree_distribution(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]), mode = "out", loops = FALSE)[-c(1)]
df <- data.frame(Prob = Probability, Degree = 1:length(Probability))
p2 <-ggplot(df,aes(x = Degree, y = Prob)) +
  geom_point() +
  xlab("Out-Degree, k") +
  ylab("Probability P(k)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = 1:length(df$Degree)) +
  scale_y_continuous(breaks = seq(0,0.4,length.out =5))
# IN DEGREE DISTRIBUTION CXC SEC DISAGG
Probability <- degree_distribution(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]), mode = "in", loops = FALSE)[-c(1)]
df <- data.frame(Prob = Probability, Degree = 1:length(Probability))
p3 <- ggplot(df,aes(x = Degree, y = Prob)) +
  geom_point() +
  xlab("In-Degree, k") +
  ylab("Probability P(k)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0,1400,length.out = 8)) +
  scale_y_continuous(breaks = seq(0,0.012,by = 0.004))
# OUT DEGREE DISTRIBUTION CXC SEC DISAGG
Probability <- degree_distribution(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]), mode = "out", loops = FALSE)[-c(1)]
df <- data.frame(Prob = Probability, Degree = 1:length(Probability))
p4 <-ggplot(df,aes(x = Degree, y = Prob)) +
  geom_point() +
  xlab("Out-Degree, k") +
  ylab("Probability P(k)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0,1400,length.out = 8)) +
  scale_y_continuous(breaks = seq(0,0.01, by = 0.002))

ggarrange(p,p2,p3,p4, ncol = 2, nrow = 2, labels = c(LETTERS[1:4]))


# IN DEGREE VS RANK - CXC SEC AGG
ordered_degree = sort(degree(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]),mode = "in"),decreasing = T)
df <- data.frame(Degree = ordered_degree, Rank = 1:length(V(unfiltered_igraph_list[[1]])))
p <-ggplot(df,aes(x = Rank, y = Degree)) +
  geom_point() +
  xlab("Rank") +
  ylab("In-Degree, k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_continuous(breaks = seq(0,length(df$Rank), by = 2 )) +
  scale_y_continuous(breaks = seq(0,length(df$Degree), by = 2))
# OUT DEGREE VS RANK - CXC SEC AGG
ordered_degree = sort(degree(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]),mode = "out"),decreasing = T)
df <- data.frame(Degree = ordered_degree, Rank = 1:length(V(unfiltered_igraph_list[[1]])))
p2 <-ggplot(df,aes(x = Rank, y = Degree)) +
  geom_point() +
  xlab("Rank") +
  ylab("Out-Degree, k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_continuous(breaks = seq(0,length(df$Rank), by = 2 )) +
  scale_y_continuous(breaks = seq(0,length(df$Degree), by = 2))
# IN DEGREE VS RANK - CXC SEC DISAGG
ordered_degree = sort(degree(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]),mode = "in"),decreasing = T)
df <- data.frame(Degree = ordered_degree, Rank = 1:length(V(unfiltered_igraph_list[[2]])))
p3 <-ggplot(df,aes(x = Rank, y = Degree)) +
  geom_point() +
  xlab("Rank") +
  ylab("In-Degree, k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0,4000, by = 1000))+
  scale_y_continuous(breaks = seq(0,1500, by = 250))
# OUT DEGREE VS RANK - CXC SEC DISAGG
ordered_degree = sort(degree(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]),mode = "out"),decreasing = T)
df <- data.frame(Degree = ordered_degree, Rank = 1:length(V(unfiltered_igraph_list[[2]])))
p4 <-ggplot(df,aes(x = Rank, y = Degree)) +
  geom_point() +
  xlab("Rank") +
  ylab("Out-Degree, k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0,4000, by = 1000))+
  scale_y_continuous(breaks = seq(0,1200, by = 200))

ggarrange(p,p2,p3,p4, ncol = 2, nrow = 2, labels = c(LETTERS[1:4]))



# WEIGHTED DEGREE PLOTS 
# IN / OUT WEIGHTED DISTRIBUTION

# IN WEIGHTED DISTRIBUTIONS -  CXC SEC AGG
in_strength <- strength(unfiltered_igraph_list[[1]],mode = "in",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE)
hist(in_strength)
# IN WEIGHTED DISTRIBUTIONS -  CXC SEC AGG
out_strength <- strength(unfiltered_igraph_list[[1]],mode = "out",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE)
hist(out_strength)
# IN WEIGHTED DISTRIBUTIONS -  CXC SEC DISAGG
in_strength <- strength(unfiltered_igraph_list[[2]],mode = "in",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE)
hist(in_strength)
# OUT WEIGHTED DISTRIBUTIONS -  CXC SEC DISAGG
out_strength <- strength(unfiltered_igraph_list[[2]],mode = "out",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE)
hist(out_strength)


# IN WEIGHTED DEGREE VS RANK - CXC SEC AGG
plot(y = sort(strength(unfiltered_igraph_list[[1]],mode = "in",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE),decreasing = T), x = 1:length(V(unfiltered_igraph_list[[1]])))
# OUT WEIGHTED DEGREE VS RANK - CXC SEC AGG
plot(y = sort(strength(unfiltered_igraph_list[[1]],mode = "out",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE),decreasing = T), x = 1:length(V(unfiltered_igraph_list[[1]])))
# IN WEIGHTED DEGREE VS RANK - CXC SEC DISAGG
plot(y = sort(strength(unfiltered_igraph_list[[2]],mode = "in",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE),decreasing = T), x = 1:length(V(unfiltered_igraph_list[[2]])))
# OUT WEIGHTED DEGREE VS RANK - CXC SEC DISAGG
plot(y = sort(strength(unfiltered_igraph_list[[2]],mode = "out",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE),decreasing = T), x = 1:length(V(unfiltered_igraph_list[[2]])))



# WEIGHTED CUMULATIVE DISTRIBUTION
# IN - CXC SEC AGG
in_strength <- strength(unfiltered_igraph_list[[1]],mode = "in",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE) %>% sort(decreasing = T) %>%  cumsum()
plot(x = 1:length(in_strength), y = in_strength/max(in_strength))
# OUT - CXC SEC AGG
out_strength <- strength(unfiltered_igraph_list[[1]],mode = "out",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE) %>% sort(decreasing = T) %>%  cumsum()
plot(x = 1:length(out_strength), y = out_strength/max(out_strength))
# IN - CXC SEC DISAGG
in_strength <- strength(unfiltered_igraph_list[[2]],mode = "in",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE) %>% sort(decreasing = T) %>%  cumsum()
plot(x = 1:length(in_strength), y = in_strength/max(in_strength))
# OUT - CXC SEC DISAGG
out_strength <- strength(unfiltered_igraph_list[[2]],mode = "out",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE) %>% sort(decreasing = T) %>%  cumsum()
plot(x = 1:length(out_strength), y = out_strength/max(out_strength))


# IN DEGREE VS OUT DEGREE - CXC SEC AGG
in_degree <- degree(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]),mode = "in")
out_degree <- degree(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]),mode = "out")
df <- tibble(`In-Degree` = in_degree,`Out-Degree` = out_degree)
p <- df %>% 
  ggplot(aes(x = `In-Degree`, y = `Out-Degree`)) +
  geom_point() +
  xlab("In-Degree, k") +
  ylab("Out-Degree, k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0,max(df$`In-Degree`), by = 2))+
  scale_y_continuous(breaks = seq(0,max(df$`Out-Degree`), by = 2))
# IN DEGREE VS OUT DEGREE - CXC SEC DISAGG
in_degree <- degree(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]),mode = "in")
out_degree <- degree(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]),mode = "out")
df <- tibble(`In-Degree` = in_degree,`Out-Degree` = out_degree)
p2 <- df %>% 
  ggplot(aes(x = `In-Degree`, y = `Out-Degree`)) +
  geom_point() +
  xlab("In-Degree, k") +
  ylab("Out-Degree, k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0,1500, by = 250))+
  scale_y_continuous(breaks = seq(0,1500, by = 250))
#  IN VS OUT WEIGHTED DEGREE - CXC SEC AGG
in_strength <- strength(unfiltered_igraph_list[[1]],mode = "in",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE)
out_strength <- strength(unfiltered_igraph_list[[1]],mode = "out",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE)
df <- tibble(`In-strength` = in_strength,`Out-strength` = out_strength)
p3 <- df %>% 
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
#  IN VS OUT WEIGHTED DEGREE - CXC SEC DISAGG
in_strength <- strength(unfiltered_igraph_list[[2]],mode = "in",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE)
out_strength <- strength(unfiltered_igraph_list[[2]],mode = "out",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE)
df <- tibble(`In-strength` = in_strength,`Out-strength` = out_strength)
p4 <- df %>% 
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

ggarrange(p,p3,p2,p4, ncol = 2, nrow = 2, labels = c(LETTERS[1:4]))



# IN DEGREE VS IN -STRENGTH - CXC SEC AGG
in_degree <- degree(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]),mode = "in")
in_strength <- strength(unfiltered_igraph_list[[1]],mode = "in",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE)
plot(x = in_degree, y = in_strength,xlab = "In-Degree", ylab = "in_strength")
# IN DEGREE VS IN DEGREE - CXC SEC DISAGG
in_degree <- degree(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]),mode = "in")
in_strength <- strength(unfiltered_igraph_list[[2]],mode = "in",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE)
plot(x = in_degree, y = in_strength,xlab = "In-Degree", ylab = "in_strength")
# OUT DEGREE VS OUT -STRENGTH - CXC SEC AGG
out_degree <- degree(unfiltered_igraph_list[[1]], v = V(unfiltered_igraph_list[[1]]),mode = "out")
out_strength <- strength(unfiltered_igraph_list[[1]],mode = "out",weights = E(unfiltered_igraph_list[[1]])$Weight, loops = FALSE)
plot(x = out_degree, y = out_strength,xlab = "Out-Degree", ylab = "Out_strength")
# OUT DEGREE VS OUT DEGREE - CXC SEC DISAGG
out_degree <- degree(unfiltered_igraph_list[[2]], v = V(unfiltered_igraph_list[[2]]),mode = "out")
out_strength <- strength(unfiltered_igraph_list[[2]],mode = "out",weights = E(unfiltered_igraph_list[[2]])$Weight, loops = FALSE)
plot(x = out_degree, y = out_strength,xlab = "Out-Degree", ylab = "Out_strength")


# EDGE DISTRIBUTION
# UNFILTERED - CXC SEC AGG
# HISTOGRAM
hist(unfiltered_edgelist[[1]]$Weight)
# EDGE WEIGHT VS RANK
plot(y = sort(unfiltered_edgelist[[1]]$Weight,decreasing = T), x = 1:length(unfiltered_edgelist[[1]]$Weight))
# UNFILTERED - CXC SEC DISAGG
# HISTOGRAM
hist(unfiltered_edgelist[[2]]$Weight)
# EDGE WEIGHT VS RANK 
plot(y = sort(unfiltered_edgelist[[2]]$Weight,decreasing = T), x = 1:length(unfiltered_edgelist[[2]]$Weight))

# FILTERED - CXC SEC AGG
# HISTOGRAM
hist(disparity_filtered_edgelist[[1]]$Weight)
# EDGE WEIGHT VS RANK
plot(y = sort(disparity_filtered_edgelist[[1]]$Weight,decreasing = T), x = 1:length(disparity_filtered_edgelist[[1]]$Weight))
#  FILTERED - CXC SEC DISAGG
# HISTOGRAM
hist(disparity_filtered_edgelist[[2]]$Weight)
# EDGE WEIGHT VS RANK 
plot(y = sort(disparity_filtered_edgelist[[2]]$Weight,decreasing = T), x = 1:length(disparity_filtered_edgelist[[2]]$Weight))



# DISTRIBUTION OF EU EDGES
# UNFILTERED CXC SEC AGG 
EU_in_neighbours <- unfiltered_edgelist[[1]] %>% filter(Purchaser == "EU") %>% pull(edge_id)
hist(E(unfiltered_igraph_list[[1]])[E(unfiltered_igraph_list[[1]]) %>% as_ids()  %in% EU_in_neighbours]$Weight)
# UNFILTERED CXC SEC DISAGG 
EU_in_neighbours <-  unfiltered_edgelist[[2]] %>% filter(str_detect(Purchaser, "EU")) %>% pull(edge_id)
hist(E(unfiltered_igraph_list[[2]])[E(unfiltered_igraph_list[[2]]) %>% as_ids()  %in% EU_in_neighbours]$Weight)
# FILTERED CXC SEC AGG 
EU_in_neighbours <- disparity_filtered_edgelist[[1]] %>% filter(Purchaser == "EU") %>% pull(edge_id)
hist(E(unfiltered_igraph_list[[1]])[E(unfiltered_igraph_list[[1]]) %>% as_ids()  %in% EU_in_neighbours]$Weight)
# FILTERED CXC SEC DISAGG 
EU_in_neighbours <-  disparity_filtered_edgelist[[2]] %>% filter(str_detect(Purchaser, "EU")) %>% pull(edge_id)
hist(E(unfiltered_igraph_list[[2]])[E(unfiltered_igraph_list[[2]]) %>% as_ids()  %in% EU_in_neighbours]$Weight)




Probability <- sample_gnp(10000, 0.004) %>% 
  degree.distribution()
df <- data.frame(Prob = Probability[-c(1)], Degree = 1:length(Probability[-c(1)]))
ggplot(df,aes(x = Degree, y = Prob)) +
  geom_point() +
  xlab("Out-Degree, k") +
  ylab("Probability P(k)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


hist(axes = F,degree(sample_gnp(10000,0.004)), breaks = 20,xlab = "Degree, k", ylab = "Number of nodes")

abline(h = 0, col = "black")
abline(v = 18, col = "black")


df <- data.frame(Degree = degree(sample_pa(10000,zero.appeal = 2)))
ggplot(df,aes(x = Degree)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7,position = "identity") +
  labs(x = "Degree, k", y = "Number of nodes") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(expand =  expansion(mult = c(0, 0.05))) +
  xlim(0,15)



sample_pa(40,directed = F) %>% 
  plot(vertex.color = "orange",       # Vertex color
    vertex.size = 10,                # Vertex size
    vertex.label = NA,
    edge.color = "gray",             # Edge color
    edge.width = 2,                  # Edge width
    edge.arrow.size = 0.6,           # arrow size
    edge.arrow.width = 0.6)
  
sample_gnp(40,0.10) %>% 
  plot(vertex.color = "orange",       # Vertex color
       vertex.size = 10,                # Vertex size
       vertex.label = NA,
       edge.color = "gray",             # Edge color
       edge.width = 2,                  # Edge width
       edge.arrow.size = 0.6,           # arrow size
       edge.arrow.width = 0.6)

sample_pa(40) %>% ecount()



df <- data.frame(Degree = degree(sample_pa(10000,zero.appeal = 2)))


Probability_pa <- degree_distribution(sample_pa(10000))[-c(1)]
df <- data.frame(Prob = Probability_pa, Degree = 1:length(Probability_pa))

Probability_gnp <- degree_distribution(sample_gnp(10000,0.004))[-c(1)]
df1 <- data.frame(Prob = Probability_gnp, Degree = 1:length(Probability_gnp))

ggplot(df1,aes(x = Degree, y = Prob)) +
  geom_point() +
  xlab("Degree, k") +
  ylab("Probability P(k)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
 
