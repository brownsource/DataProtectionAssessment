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

# CREATE LIST OF GRAPHS CREATED
all.Graphs           <- list(graph.Regional.Partners, graph.Jordan.Partners, graph.Jordan.DSA.Partners,
                             graph.Lebanon.Partners, graph.Lebanon.DSA.Partners)

all.Graphs.Layouts   <- list(5)

# FIX THE GRAPH PLOT LAYOUT PARAMETERS FOR EACH GRAPH
for (i in 1:length(all.Graphs)) {
        all.Graphs.Layouts[[i]] <- layout.forceatlas2(all.Graphs[[i]], 
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
}

# CREATE LIST OF REGIONAL CHARTS TO GENERATE
regional.Graphs.Outputs   <- c("Regional_Partners_All", 
                               "Regional_Partners_All_DSA",
                               "Regional_Partners_All_JDSA",
                               "Regional_Partners_All_LDSA")

# CREATE LIST OF JORDAN CHARTS TO GENERATE 
jordanAll.Graphs.Outputs   <- c("Jordan_Partners_All",
                                "Jordan_Partners_All_DSA")

# CREATE LIST OF JORDAN DSA CHARTS TO GENERATE 
jordanDSA.Graphs.Outputs   <- c("Jordan_Partners_DSA")

# CREATE LIST OF LEBANON CHARTS TO GENERATE 
lebanonAll.Graphs.Outputs   <- c("Lebanon_Partners_All",
                                 "Lebanon_Partners_All_DSA")

# CREATE LIST OF LEBANON DSA CHARTS TO GENERATE 
lebanonDSA.Graphs.Outputs   <- c("Lebanon_Partners_DSA")

# SAVE PNGs OF CHARTS
source("code/PartnerMapping/F.OutputForceAtlas.R")

# Regional
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[1], all.Graphs.Layouts[[1]], graph.Labels = TRUE, highlight.DSA = "No", node.Scale = 1)
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[1], all.Graphs.Layouts[[1]], graph.Labels = FALSE, highlight.DSA = "No", node.Scale = 1)
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[2], all.Graphs.Layouts[[1]], graph.Labels = TRUE, highlight.DSA = "All", node.Scale = 1)
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[2], all.Graphs.Layouts[[1]], graph.Labels = FALSE, highlight.DSA = "All", node.Scale = 1)
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[3], all.Graphs.Layouts[[1]], graph.Labels = TRUE, highlight.DSA = "Jordan", node.Scale = 1)
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[3], all.Graphs.Layouts[[1]], graph.Labels = FALSE, highlight.DSA = "Jordan", node.Scale = 1)
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[4], all.Graphs.Layouts[[1]], graph.Labels = TRUE, highlight.DSA = "Lebanon", node.Scale = 1)
OutputForceAtlas(graph.Regional.Partners, regional.Graphs.Outputs[4], all.Graphs.Layouts[[1]], graph.Labels = FALSE, highlight.DSA = "Lebanon", node.Scale = 1)

# Jordan
OutputForceAtlas(graph.Jordan.Partners, jordanAll.Graphs.Outputs[1], all.Graphs.Layouts[[2]], graph.Labels = TRUE, highlight.DSA = "No", node.Scale = 2)
OutputForceAtlas(graph.Jordan.Partners, jordanAll.Graphs.Outputs[1], all.Graphs.Layouts[[2]], graph.Labels = FALSE, highlight.DSA = "No", node.Scale = 2)
OutputForceAtlas(graph.Jordan.Partners, jordanAll.Graphs.Outputs[2], all.Graphs.Layouts[[2]], graph.Labels = TRUE, highlight.DSA = "Jordan", node.Scale = 2)
OutputForceAtlas(graph.Jordan.Partners, jordanAll.Graphs.Outputs[2], all.Graphs.Layouts[[2]], graph.Labels = FALSE, highlight.DSA = "Jordan", node.Scale = 2)

OutputForceAtlas(graph.Jordan.DSA.Partners, jordanDSA.Graphs.Outputs[1], all.Graphs.Layouts[[3]], graph.Labels = TRUE, highlight.DSA = "No", node.Scale = 2)
OutputForceAtlas(graph.Jordan.DSA.Partners, jordanDSA.Graphs.Outputs[1], all.Graphs.Layouts[[3]], graph.Labels = FALSE, highlight.DSA = "No", node.Scale = 2)

# Lebanon
OutputForceAtlas(graph.Lebanon.Partners, lebanonAll.Graphs.Outputs[1], all.Graphs.Layouts[[4]], graph.Labels = TRUE, highlight.DSA = "No", node.Scale = 2)
OutputForceAtlas(graph.Lebanon.Partners, lebanonAll.Graphs.Outputs[1], all.Graphs.Layouts[[4]], graph.Labels = FALSE, highlight.DSA = "No", node.Scale = 2)
OutputForceAtlas(graph.Lebanon.Partners, lebanonAll.Graphs.Outputs[2], all.Graphs.Layouts[[4]], graph.Labels = TRUE, highlight.DSA = "Lebanon", node.Scale = 2)
OutputForceAtlas(graph.Lebanon.Partners, lebanonAll.Graphs.Outputs[2], all.Graphs.Layouts[[4]], graph.Labels = FALSE, highlight.DSA = "Lebanon", node.Scale = 2)

OutputForceAtlas(graph.Lebanon.DSA.Partners, lebanonAll.Graphs.Outputs[1], all.Graphs.Layouts[[5]], graph.Labels = TRUE, highlight.DSA = "No", node.Scale = 2)
OutputForceAtlas(graph.Lebanon.DSA.Partners, lebanonAll.Graphs.Outputs[1], all.Graphs.Layouts[[5]], graph.Labels = FALSE, highlight.DSA = "No", node.Scale = 2)


# SAVE CSVs OF THE PARTNERS AND WHICH COUNTRIES THEY OPERATE IN
source("code/PartnerMapping/F.OutputCSV.R")

OutputCSV(graph.Jordan.Partners, "Jordan")
OutputCSV(graph.Lebanon.Partners, "Lebanon")