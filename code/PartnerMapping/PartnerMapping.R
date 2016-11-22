################################################################################
# CREATES CSV TABLES AND SELECTION OF GRAPHICS SHOWING PARTNERS IN THE REGION
# AND FOR EACH COUNTRY.

# INPUTS: 
# ../../DataProtectionAssessmentNotPublic/data/PartnerMapping/regional_partners_nodes.csv
# ../../DataProtectionAssessmentNotPublic/data/PartnerMapping/regional_partners_edges.csv

# CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

# INSTALL PACKAGES IF REQUIRED
if(!require(extrafont)){
        install.packages("extrafont")
}
if(!require(igraph)){
        install.packages("igraph")
}
if(!require(devtools)){
        install.packages("devtools")
}

# INSTALL LIBRARIES IF REQUIRED
#!! NEED TO CHANGE THIS BLOCK OF CODE !!#
library(extrafont)
font_import()
loadfonts()
library(igraph)
if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library("ForceAtlas2")
library(plyr)

# LOAD THE DATA
import.Edges <- data.frame(read.csv("../DataProtectionAssessmentNotPublic/data/PartnerMapping/regional_partners_edges.csv", 
                                    header = TRUE, stringsAsFactors = FALSE))
import.Nodes <- data.frame(read.csv("../DataProtectionAssessmentNotPublic/data/PartnerMapping/regional_partners_nodes.csv", 
                                    header = TRUE, stringsAsFactors = FALSE))

# CREATE LIST OF LEVELS TO RUN AT
country.Levels <- c("Regional", "Jordan", "Lebanon")

# CREATE IGRAPH OBJECT
graph.Regional.Partners <- graph_from_data_frame(d=import.Edges, vertices = import.Nodes, directed = F)
graph.Regional.Partners <- simplify(graph.Regional.Partners, remove.multiple = F, remove.loops = T)

# CREATE COUNTRY LEVEL SUB NETWORKS
graph.Jordan.Partners      <- induced.subgraph(graph.Regional.Partners, which(V(graph.Regional.Partners)$jordan == 1))
graph.Jordan.DSA.Partners  <- induced.subgraph(graph.Jordan.Partners, which(V(graph.Jordan.Partners)$DSA_jordan == 1))
graph.Lebanon.Partners     <- induced.subgraph(graph.Regional.Partners, which(V(graph.Regional.Partners)$lebanon == 1))
graph.Lebanon.DSA.Partners <- induced.subgraph(graph.Lebanon.Partners, which(V(graph.Lebanon.Partners)$DSA_lebanon == 1))

# CREATE VECTORS OF OBJECTS CREATED SO FAR
all.Graph.names   <- list("graph.Regional.Partners", "graph.Jordan.Partners", "graph.Jordan.DSA.Partners",
                       "graph.Lebanon.Partners", "graph.Lebanon.DSA.Partners")
all.Graph.Layouts <- list("layout.Regional.Partners", "layout.Jordan.Partners", "layout.Jordan.DSA.Partners",
                       "layout.Lebanon.Partners", "layout.Lebanon.DSA.Partners")
all.Graphs <- data.frame(all.Graph.names, all.Graph.Layouts)

# FIX THE GRAPH PLOT LAYOUT PARAMETERS FOR EACH GRAPH

all.Graphs[1 , 2] <- layout.forceatlas2(all.Graphs[1 , 1], 
                                        directed = FALSE,
                                        iterations = 500,
                                        linlog = FALSE,
                                        pos = NULL, 
                                        nohubs = FALSE, 
                                        k = 200, 
                                        gravity = 0.5, 
                                        ks = 0.1, 
                                        ksmax = 10, 
                                        delta = 1, 
                                        center = NULL, 
                                        #tolerate = 0.1, 
                                        dim = 2, 
                                        plotstep = 500, 
                                        plotlabels = FALSE) 
as.igraph(all.Graphs[1 , 1])
