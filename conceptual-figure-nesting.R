# Conceptual figure showing nested structure of fishing activities
# Sets nested in trips nested in vessels

library(ggplot2)
library(ggraph)
library(igraph)
library(dplyr)
library(emoji)

# Create the hierarchical structure
# 3 vessels, 3 trips per vessel, 3 sets per trip

# Create nodes dataframe
nodes <- data.frame(
  id = character(),
  label = character(),
  level = character(),
  parent = character(),
  stringsAsFactors = FALSE
)

# Add root node
nodes <- rbind(nodes, data.frame(
  id = "root",
  label = "Fleet",
  level = "fleet",
  parent = NA,
  stringsAsFactors = FALSE
))

# Add vessels
for(v in 1:3) {
  vessel_id <- paste0("V", v)
  nodes <- rbind(nodes, data.frame(
    id = vessel_id,
    label = paste("Vessel", v),
    level = "vessel",
    parent = "root",
    stringsAsFactors = FALSE
  ))
  
  # Add trips for this vessel
  for(t in 1:3) {
    trip_id <- paste0("V", v, "_T", t)
    nodes <- rbind(nodes, data.frame(
      id = trip_id,
      label = paste("Trip", t),
      level = "trip",
      parent = vessel_id,
      stringsAsFactors = FALSE
    ))
    
    # Add sets for this trip
    for(s in 1:3) {
      set_id <- paste0("V", v, "_T", t, "_S", s)
      nodes <- rbind(nodes, data.frame(
        id = set_id,
        label = paste("Set", s),
        level = "set",
        parent = trip_id,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Create edges dataframe
edges <- nodes %>%
  filter(!is.na(parent)) %>%
  select(from = parent, to = id)

# Create igraph object
g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Create catch data for sets
set_ids <- nodes$id[nodes$level == "set"]
set.seed(123)  # For reproducible results
catch_data <- data.frame(
  set_id = set_ids,
  catch_type = sample(c("shark", "fish"), 
                     length(set_ids), 
                     replace = TRUE, 
                     prob = c(1/3, 2/3)),
  stringsAsFactors = FALSE
)

# Add emoji symbols
catch_data$emoji <- ifelse(catch_data$catch_type == "shark", 
                          emoji("shark"), 
                          emoji("fish"))

# Add catch information to nodes dataframe
nodes$catch_emoji <- ""
nodes$catch_emoji[match(catch_data$set_id, nodes$id)] <- catch_data$emoji

# Set node attributes
V(g)$level <- nodes$level[match(V(g)$name, nodes$id)]
V(g)$label <- nodes$label[match(V(g)$name, nodes$id)]
V(g)$catch_emoji <- nodes$catch_emoji[match(V(g)$name, nodes$id)]

# Create the plot with left-to-right orientation
p <- ggraph(g, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal(alpha = 0.6, width = 1.2, colour = "darkblue") +
  geom_node_point(aes(color = level, size = level)) +
  # Only show labels for non-set nodes
  geom_node_text(aes(label = ifelse(level != "set", label, ""), color = level), 
                 hjust = -0.1, vjust = -2, size = 5) +  # Increased from 3 to 5
  # Add catch emojis for sets only
  geom_node_text(aes(label = ifelse(level == "set", catch_emoji, "")),
                 hjust = -0.2, vjust = 1, size = 10) +  # Increased from 6 to 10
  scale_color_manual(values = c("fleet" = "black", 
                               "vessel" = "darkred", 
                               "trip" = "darkgreen", 
                               "set" = "darkblue")) +
  scale_size_manual(values = c("fleet" = 6, 
                              "vessel" = 5, 
                              "trip" = 4, 
                              "set" = 3)) +  # Increased all sizes
  
  coord_flip() +  # Flip to make it horizontal (left-to-right)
#   xlim(-0.5, 3.5) +  # Compress the x-axis to make branches shorter
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Increased title size
    plot.margin = margin(20, 80, 20, 20)  # Increased right margin for emojis
  ) +
  scale_y_reverse() +  # Reverse x-axis to put root on left
  labs(title = "Nested Structure of Fishing Activities with Catch Types")
p


# Save the plot
ggsave("plots/fishing_structure_dendrogram.png", p, 
       width = 12, height = 8, dpi = 300, bg = "white")


# Print catch data summary
print("Catch data by set:")
print(catch_data)
print(paste("Total sets:", nrow(catch_data)))
print(paste("Shark catches:", sum(catch_data$catch_type == "shark")))
print(paste("Fish catches:", sum(catch_data$catch_type == "fish")))
