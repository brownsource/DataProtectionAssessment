################################################################################
OutputForceAtlas <- function(graph, graph.Name, graph.Layout, graph.Labels = TRUE, highlight.DSA = "No", node.Scale = 1) {

        # Saves a PNG of the iGraph Object
        # 
        # Args: 
        #   graph: the iGraph Object
        #   graph.Name: the file name to be saved
        #   graph.Layout: the plot parameters
        #   graph.Labels: TRUE = labels, FALSE = Anonymised
        #   highlight.DSA: OPTIONS: No, All, Jordan, Lebanon DEFAULT: No
        #   node.Scale: OPTIONS: 1 = Small, 2 = medium DEFAULT: 1
        # 
        # Returns:
        #   -

        # DEMO VALUES FOR TESTING
        # graph         <- graph.Regional.Partners
        # graph.Name    <- regional.Graphs.Outputs[2]
        # graph.Layout  <- all.Graphs.Layouts[[1]]
        # graph.Labels  <- TRUE
        # highlight.DSA <- "All"
        # node.Scale    <- 1
        
        # Set the colours of the nodes
        V(graph)[V(graph)$type == "Country"]$color            <- "#009460"
        V(graph)[V(graph)$type == "UN Agency"]$color          <- "#0072B6"
        V(graph)[V(graph)$type == "Government"]$color         <- "#FF5300"
        V(graph)[V(graph)$type == "INGO"]$color               <- "#FF8C00"
        V(graph)[V(graph)$type == "Local"]$color              <- "#FFCC00"
        V(graph)[V(graph)$type == "Other"]$color              <- "grey80"
        V(graph)[V(graph)$type == "Not yet classified"]$color <- "grey80"
        # Set labels according to anonymisation
        if (graph.Labels == TRUE) {
                V(graph)$label <- V(graph)$acronym
        } else {
                V(graph)$label <- NA
                
        }
        # set other node attributes
        V(graph)$frame.color   <- rgb(255,255,255, maxColorValue = 255)
        V(graph)$shape         <- "circle"
        V(graph)$label.family  = "Arial"
        V(graph)$label.font    = 1
        # Set edge attributes
        E(graph)$color         <- "grey80"
        E(graph)$width         <- 0.5
        E(graph)$margin        <- 1
        # Calculate the degree for each node
        d.graph                <- degree(graph, mode="all")
        # Set the size of the nodes and labels depending on the degree
        if (node.Scale == 1) { 
                V(graph)$size          <- (d.graph - min(d.graph)) / (max(d.graph) - min(d.graph)) * (25-5)+5
                V(graph)$label.cex     <- (d.graph - min(d.graph)) / (max(d.graph) - min(d.graph)) * (0.75-0.25)+0.25
                V(graph)$cex           <- V(graph)$label.cex                
        } else {
                V(graph)$size          <- (d.graph - min(d.graph)) / (max(d.graph) - min(d.graph)) * (20-10)+10
                V(graph)$label.cex     <- (d.graph - min(d.graph)) / (max(d.graph) - min(d.graph)) * (0.75-0.5)+0.5
                V(graph)$cex           <- V(graph)$label.cex
        }
        # Set highlight attributes if Highlight DSA selected
        if (highlight.DSA == "All") {
                V(graph)$frame.color[which(V(graph)$DSA_jordan == 1 | V(graph)$DSA_lebanon == 1)] <- rgb(0,0,0, maxColorValue = 255)
                V(graph)$color[which(V(graph)$DSA_jordan != 1 & V(graph)$DSA_lebanon !=1 )] <- "grey80"
                
        } else if (highlight.DSA == "Jordan") {
                V(graph)$frame.color[which(V(graph)$DSA_jordan == 1)] <- rgb(0,0,0, maxColorValue = 255)
                V(graph)$color[which(V(graph)$DSA_jordan != 1)] <- "grey80"
                
        } else if (highlight.DSA == "Lebanon") {
                V(graph)$frame.color[which(V(graph)$DSA_lebanon == 1)] <- rgb(0,0,0, maxColorValue = 255)
                V(graph)$color[which(V(graph)$DSA_lebanon !=1 )] <- "grey80"
                
        } else {}
              
        # Set the output directory
        output.Path <- "../DataProtectionAssessmentNotPublic/output/PartnerMapping/"
        if (graph.Labels == TRUE) {
                filename.Path <- paste(output.Path, graph.Name, "_labels.png", sep = "")
        } else {
                filename.Path <- paste(output.Path, graph.Name, "_anon.png", sep = "")
        }
        
        dir.create(dirname(filename.Path), showWarnings = FALSE)

        # Save as PNG
        Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
        png(file = filename.Path, width = 10, height = 8, units = 'in', res = 300, bg = "transparent")        
        par(mar=c(5,3,2,2)+0.1)
        
        plot(graph, layout = graph.Layout)
        legend(x="bottom", 
               title.adj = 0,
               c("Country","UN Agency", "Government", "INGO", "Local", "Other","Not yet classified"), 
               pt.bg=c("#009460","#0072B6","#FF5300","#FF8C00","#FFCC00","grey80", "grey80"),
               pch=21,
               col=NA, 
               pt.cex=1.5, 
               cex=0.75, 
               bty="n", 
               horiz=TRUE)
        
        dev.off()        
}

