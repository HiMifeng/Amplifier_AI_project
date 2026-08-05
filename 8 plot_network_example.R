# Description: Illustrative example network figure (a schematic Erdos-Renyi graph, NOT one of
#   the real AddHealth networks) showing human/LLM agent composition at three different q (AI
#   proportion) values.
# Input: none (network is generated synthetically)
# Output: plots/net illustration.png

library(igraph)
library(qgraph)
library(latex2exp)
library(data.table)
library(dplyr)

project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)


# Set up the output file 
output_folder <- file.path(project_root, "plots")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
png_outfile <- file.path(output_folder, "net illustration.png")
png(png_outfile, width = 15, height = 6, units = "in", res = 200)

# Plot settings for 3-panel figure
par(mfrow = c(1,3), mar = c(4, 0.5, 3.5, 0.5), cex.main = 4.5, font.main = 2)
# Parameters
set.seed(25)
num_nodes <- 20
q = c(0.2,0.6,0.8)
num_humans <- num_nodes-q*num_nodes
num_llms <- q*num_nodes

# Create a random network
g <- erdos.renyi.game(num_nodes, p = 0.3, directed = FALSE)
# Use Fruchterman-Reingold layout to prevent overlapping nodes
coords <- layout_with_fr(g, niter = 500, grid = "nogrid")

# Panel 1: Seeding with black enclosing circles around specific red nodes
# Define node shapes and colors
V(g)$shape <- rep("circle", num_nodes)             # Humans as circles
V(g)$color <-  "#FFAD60"                                 # Humans as blue
V(g)$shape[1:num_llms[1]] <- "square" # LLM agents as squares
V(g)$color[1:num_llms[1]] <- "gray"   # LLMs as gray

plot(g, layout = coords, vertex.size = 15, vertex.label = NA,
     vertex.color = V(g)$color, vertex.shape = V(g)$shape,
     edge.width = 1.5, edge.color = "darkgrey", main = bquote(bold("A  ") ~italic(q) == .(q[1])))

# Add a legend inside the plot area without overlapping nodes
par(xpd = NA)
legend(x = -1.14, y = -1,  legend = c("Human", "LLM"),
       pch = c(21, 22),
       pt.bg = c("#FFAD60", "gray"),
       col = "black",
       pt.cex = 6,
       bty = "n",
       cex = 3)


# Panel 2: Network Alteration
# Define node shapes and colors
V(g)$shape <- rep("circle", num_nodes)             # Humans as circles
V(g)$color <- "#FFAD60"                                  # Humans as blue
V(g)$shape[1:num_llms[2]] <- "square" # LLM agents as squares
V(g)$color[1:num_llms[2]] <- "gray"   # LLMs as gray

plot(g, layout = coords, vertex.size = 15, vertex.label = NA,
     vertex.color = V(g)$color, vertex.shape = V(g)$shape,
     edge.width = 1.5, edge.color = "darkgrey", main =bquote(bold("B  ") ~italic(q) == .(q[2])))

# Panel 3: Generative Recommender System
V(g)$shape <- rep("circle", num_nodes)             # Humans as circles
V(g)$color <-  "#FFAD60"                               # Humans as blue
V(g)$shape[1:num_llms[3]] <- "square" # LLM agents as squares
V(g)$color[1:num_llms[3]] <- "gray"   # LLMs as gray

plot(g, layout = coords, vertex.size = 15, vertex.label = NA,
     vertex.color = V(g)$color, vertex.shape = V(g)$shape,
     edge.width = 1.5, edge.color = "darkgrey", main = bquote(bold("C  ") ~italic(q) == .(q[3])))

dev.off()

