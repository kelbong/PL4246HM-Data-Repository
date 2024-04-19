# Final Project

# Setting up, creating main network 'ud_g'-----
## Load libraries + set working directory 
library(tidyverse)
library(igraph)
setwd("/Users/kelbong/Documents/Uni/Y4 S2/PL4246HM Network/Research/FINAL FINAL FINAL")

## Load data sets
responses <- read.csv('class-responses-filtered.csv')
names <- read.csv('class-names.csv')

## Merge responses + names (using 'id')
data_m <- merge(responses, names, by='id') # the sequence of merge must be in this sequence

## Explore/inspect network 
summary(responses)
summary(data_m)

## Plot data.frame in to igraph object
g <- graph_from_data_frame(data_m, directed = TRUE) 
summary(g) # sanity check
plot(g, vertex.label = NA, vertex.size=7, edge.arrow.size = 0.3, layout = layout_nicely) # plot

## Retaining only mutual edges (17 edges removed)
recip_g <- subgraph.edges(g, eids = E(g)[which_mutual(g)], delete.vertices = TRUE)
plot(recip_g, vertex.label = NA, vertex.size=7, edge.arrow.size = 0.3, layout = layout_nicely) # plot

## Making network undirected
ud_g <- as.undirected(recip_g, mode = 'collapse') # plot
plot(ud_g, vertex.label = NA, vertex.size=7, edge.arrow.size = 0.3, layout = layout_nicely) # plot

## General graph characteristics 
avg_degree <- mean(degree(ud_g))

## Generating random network (nodes = 54, edges = 98) for comparisons of network properties and characteristics 
random_network <- sample_gnm(n = 54, m = 98, directed = FALSE, loops = FALSE)

# Node attributions for ud_g -----

## Uploading an updated names list that has all names outside of network ud_g removed 
names <- read.csv('class-names-filtered.csv') # ensure that the network ud_g and 'names' have the same number of items

## Arranging data 'names' based on the order defined in g
names <- names %>% arrange(factor(id, levels = V(ud_g)$name)) 

## sanity check to see if the order are the same
identical(names$id, V(ud_g)$name)

## assign all attributes
ud_g <- set_vertex_attr(ud_g, name = 'gender', value = replace_na(names$gender, 'unknown'))
ud_g <- set_vertex_attr(ud_g, name = 'ethnicity', value = names$ethnicity)
ud_g <- set_vertex_attr(ud_g, name = 'HM_or_not', value = names$HM_or_not)
ud_g <- set_vertex_attr(ud_g, name = 'year_of_study', value = names$year_of_study)

# Assessing ASPL and GCC ------
## Global Clustering Coefficient (GCC) for whole network
transitivity(ud_g, type = 'global') 

## Check if there are any hermit clusters that might affect ASPL
components <- clusters(ud_g) ### Identify disconnected components
num_components <- components$no ### Finding number of disconnected components
membership <- components$membership ### Get the membership vector indicating which component each node belongs to
print(num_components) ### Print number of components
print(membership) ### Print membership for all nodes

## Average Shortest Path Length (ASPL)
g_paths <- shortest.paths(ud_g) # function for the shortest path lengths in g
hist(g_paths, main = 'ud_g network') # plotting the graph for the shortest path lengths in the network
mean (g_paths) # Quantifying ASPL 

# Assessing ASPL and GCC for each gender attribute ----
## Subset by node attributes (FEMALE) --
g_female <- induced_subgraph(graph = ud_g,
+ vids = V(ud_g)$gender == 'Female')

## Plot graph
plot(g_female, vertex.label = NA, vertex.size=7, edge.arrow.size = 0.3, layout = layout_nicely) 

## GCC for FEMALE only network
transitivity(g_female, type = 'global')
## ASPL for FEMALE only network
average.path.length(g_female, directed= FALSE)

## Subset by node attributes (MALE) -- 
g_male <- induced_subgraph(graph = ud_g,
+ vids = V(ud_g)$gender == 'Male')

### Plot graph
plot(g_male, vertex.label = NA, vertex.size=7, edge.arrow.size = 0.3, layout = layout_nicely) 

### GCC for MALE only network
transitivity(g_male, type = 'global')
### ASPL for MALE only network
average.path.length(g_male, directed= FALSE)

# Assortativity -----
## By degree 
assort_deg <- assortativity_degree(ud_g, directed = F) 
## By nominal (based on attributes)
assort_gend <- assortativity_nominal(ud_g, types = factor(V(ud_g)$gender), directed = FALSE)
assort_ethn <- assortativity_nominal(ud_g, types = factor(V(ud_g)$ethnicity), directed = FALSE)
assort_HM <- assortativity_nominal(ud_g, types = factor(V(ud_g)$HM_or_not), directed = FALSE) 
assort_yos <- assortativity_nominal(ud_g, types = factor(V(ud_g)$year_of_study), directed = FALSE) 

## Arranging assortativity into table format
assort_methods <- c('Degree', 'Gender','Ethnicity','HM_or_not','Year_of_study')
assort_mods <- data.frame(assort_deg, assort_gend, assort_ethn, assort_HM, assort_yos)
colnames(assort_mods)<-assort_methods
view(assort_mods)

# Community Detection Methods ------

## Setting the template for output, adjusting margins, 1 row, 2 columns
set.seed(12)
par(mar=c(0,0,0,0)+1, mfrow = c(1,2)) # adjust margins, 1 row, 2 columns  

## Louvain Method --
set.seed(1)
### Storing the resulting community structure in the variable cl_lv 
cl_lv <- cluster_louvain(ud_g, weights = E(ud_g)$weight)
### Adding membership
V(ud_g)$membership_lv <- cl_lv$membership
### Plotting network
plot(ud_g, vertex.color=V(ud_g)$membership_lv, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     main = "Louvain")

## Walktrap Method --
set.seed(1)
### Storing the resulting community structure in the variable cl_rw
cl_rw <- cluster_walktrap(ud_g, weights = E(ud_g)$weight)
### Adding membership
V(ud_g)$membership_rw <- cl_rw$membership
### Plotting network
plot(ud_g, vertex.color=V(ud_g)$membership_rw, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     main = "Random Walk")

## Edge Betweenness Method--
set.seed(1)
### Storing the resulting community structure in the variable cl_eb
cl_eb <- cluster_edge_betweenness(ud_g, weights = E(ud_g)$weight)
### Adding membership
V(ud_g)$membership_eb <- cl_eb$membership
### Plotting network
plot(ud_g, vertex.color=V(ud_g)$membership_eb, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     main = "Edge Betweenness")

## Infomap method -- 
set.seed(1)
### Storing the resulting community structure in the variable cl_im
cl_im <- cluster_infomap (ud_g, e.weights = E(ud_g)$weight)
### Adding membership
V(ud_g)$membership_im <- cl_im$membership
### Plotting network
plot(ud_g, vertex.color=V(ud_g)$membership_im, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     main = "Infomap")

# Modularities ------
mod_lv <- modularity(cl_lv)
mod_im <- modularity(cl_im)
mod_eb <- modularity(cl_eb)
mod_rw <- modularity(cl_rw)

## Putting all these data into a table format
mod_methods <- c('Louvain', 'Random Walker ','Edge Betweenness','Infomap')
mod_results <- data.frame(mod_lv, mod_rw, mod_eb, mod_im)
colnames(mod_results)<-mod_methods
view(mod_results)

# Analyzing Edge Betweenness community structure ----

## Adding new attribute 'edge_type' for gender
E(ud_g)$edge_type<- 'same gender'
E(ud_g)$edge_type[E(ud_g)[ V(ud_g)[V(ud_g)$gender == 'Male'] %--% V(ud_g)[V(ud_g)$gender == 'Female']]] <- 'different gender'
E(ud_g)$edge_type

summary(ud_g)

# Color edges differently depending on whether the tie is between same or different genders 
plot(ud_g, vertex.color=V(ud_g)$membership_eb, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     edge.color = c('red','blue')[factor(E(ud_g)$edge_type)],
     main = "Edge Betweenness")

# Extracting Community Membership and making them into attributes -------
## Extracting community memberships from Edge Betweenness community structure
membership <- cl_eb$membership
V(ud_g)$membership <- membership

## Summarizing the community memberships in a table
community_summary <- table(membership)
view(community_summary)

## Sub-setting out communities into own network
comm1 <- induced_subgraph(ud_g, vids = V(ud_g)$membership == 1) 
comm2 <- induced_subgraph(ud_g, vids = V(ud_g)$membership == 2) 
comm3 <- induced_subgraph(ud_g, vids = V(ud_g)$membership == 3) 
comm4 <- induced_subgraph(ud_g, vids = V(ud_g)$membership == 4) 
comm5 <- induced_subgraph(ud_g, vids = V(ud_g)$membership == 5) 
comm6 <- induced_subgraph(ud_g, vids = V(ud_g)$membership == 6) 
comm7 <- induced_subgraph(ud_g, vids = V(ud_g)$membership == 7) 

# Nominal Assortativity within new community structures ----
## Based on GENDER attributes
assort_gend_1 <- assortativity_nominal(comm1, types = factor(V(comm1)$gender), directed = FALSE)
assort_gend_2 <- assortativity_nominal(comm2, types = factor(V(comm2)$gender), directed = FALSE)
assort_gend_3 <- assortativity_nominal(comm3, types = factor(V(comm3)$gender), directed = FALSE)
assort_gend_4 <- assortativity_nominal(comm4, types = factor(V(comm4)$gender), directed = FALSE)
assort_gend_5 <- assortativity_nominal(comm5, types = factor(V(comm5)$gender), directed = FALSE)
assort_gend_6 <- assortativity_nominal(comm5, types = factor(V(comm5)$gender), directed = FALSE)
assort_gend_7 <- assortativity_nominal(comm5, types = factor(V(comm5)$gender), directed = FALSE)

### Presenting all the values into a table format 
community_assortment <- c('Comm1', 'Comm2 ','Comm3','Comm4','Comm5','Comm6','Comm7')
assort_results <- data.frame(assort_gend_1, assort_gend_2, assort_gend_3, assort_gend_4,assort_gend_5, assort_gend_6, assort_gend_7)
colnames(assort_results)<-community_assortment
view(assort_results)

## Based on ETHNICITY attributes
assort_ethn_1 <- assortativity_nominal(comm1, types = factor(V(comm1)$ethnicity), directed = FALSE)
assort_ethn_2 <- assortativity_nominal(comm2, types = factor(V(comm2)$ethnicity), directed = FALSE)
assort_ethn_3 <- assortativity_nominal(comm3, types = factor(V(comm3)$ethnicity), directed = FALSE)
assort_ethn_4 <- assortativity_nominal(comm4, types = factor(V(comm4)$ethnicity), directed = FALSE)
assort_ethn_5 <- assortativity_nominal(comm5, types = factor(V(comm5)$ethnicity), directed = FALSE)
assort_ethn_6 <- assortativity_nominal(comm5, types = factor(V(comm5)$ethnicity), directed = FALSE)
assort_ethn_7 <- assortativity_nominal(comm5, types = factor(V(comm5)$ethnicity), directed = FALSE)

### Presenting all the values into a table format 
community_assortment_ethn <- c('Comm1', 'Comm2 ','Comm3','Comm4','Comm5','Comm6','Comm7')
assort_results_ethn <- data.frame(assort_ethn_1, assort_ethn_2, assort_ethn_3, assort_ethn_4,assort_ethn_5, assort_ethn_6, assort_ethn_7)
colnames(assort_results_ethn)<-community_assortment_ethn
view(assort_results_ethn)


  
