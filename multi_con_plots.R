# Plotting pairwise consensus tree distances for four datasets with two levels of cutoff support

library(ggplot2)
library(gridExtra)
library(grid)

setwd('C:/Users/ben/Desktop/covarion/results/crocs/')
crocs_gtr_50 = scan("gtr/gtr_crocs_con_tree_distances.csv", quiet=TRUE)
crocs_gtr_95 = scan("gtr/gtr_crocs_95_con_tree_distances.csv", quiet=TRUE)
crocs_part_50 = scan("partition/part_crocs_con_tree_distances.csv", quiet=TRUE)
crocs_part_95 = scan("partition/part_crocs_95_con_tree_distances.csv", quiet=TRUE)
crocs_cov_50 = scan("cov/cov_crocs_con_tree_distances.csv", quiet=TRUE)
crocs_cov_95 = scan("cov/cov_crocs_95_con_tree_distances.csv", quiet=TRUE)

setwd('C:/Users/ben/Desktop/covarion/results/turtles/')
turtles_gtr_50 = scan("gtr/gtr_turtles_con_tree_distances.csv", quiet=TRUE)
turtles_gtr_95 = scan("gtr/gtr_turtles_95_con_tree_distances.csv", quiet=TRUE)
turtles_part_50 = scan("partition/part_turtles_con_tree_distances.csv", quiet=TRUE)
turtles_part_95 = scan("partition/part_turtles_95_con_tree_distances.csv", quiet=TRUE)
turtles_cov_50 = scan("cov/cov_turtles_con_tree_distances.csv", quiet=TRUE)
turtles_cov_95 = scan("cov/cov_turtles_95_con_tree_distances.csv", quiet=TRUE)

setwd('C:/Users/ben/Desktop/covarion/results/squamates/')
squamates_gtr_50 = scan("gtr/gtr_squamates_con_tree_distances.csv", quiet=TRUE)
squamates_gtr_95 = scan("gtr/gtr_squamates_95_con_tree_distances.csv", quiet=TRUE)
squamates_part_50 = scan("partition/part_squamates_con_tree_distances.csv", quiet=TRUE)
squamates_part_95 = scan("partition/part_squamates_95_con_tree_distances.csv", quiet=TRUE)
squamates_cov_50 = scan("cov/cov_squamates_con_tree_distances.csv", quiet=TRUE)
squamates_cov_95 = scan("cov/cov_squamates_95_con_tree_distances.csv", quiet=TRUE)

setwd('C:/Users/ben/Desktop/covarion/results/amph/')
amph_gtr_50 = scan("gtr/gtr_amph_con_tree_distances.csv", quiet=TRUE)
amph_gtr_95 = scan("gtr/gtr_amph_95_con_tree_distances.csv", quiet=TRUE)
amph_part_50 = scan("partition/part_amph_con_tree_distances.csv", quiet=TRUE)
amph_part_95 = scan("partition/part_amph_95_con_tree_distances.csv", quiet=TRUE)
amph_cov_50 = scan("cov/cov_amph_con_tree_distances.csv", quiet=TRUE)
amph_cov_95 = scan("cov/cov_amph_95_con_tree_distances.csv", quiet=TRUE)

crocs_df = data.frame(Distances = c(crocs_gtr_50, crocs_gtr_95, crocs_part_50, crocs_part_95, crocs_cov_50, crocs_cov_95),
                ID = c(rep("GTR 50", length(crocs_gtr_50)),
                       rep("GTR 95", length(crocs_gtr_95)),
                       rep("Partition 50", length(crocs_part_50)),
                       rep("Partition 95", length(crocs_part_95)),
                       rep("Covarion 50", length(crocs_cov_50)),
                       rep("Covarion 95", length(crocs_cov_95))),
                model = c(rep("GTR", 156), rep("Partition", 156), rep("Cov", 156)))

turtles_df = data.frame(Distances = c(turtles_gtr_50, turtles_gtr_95, turtles_part_50, turtles_part_95, turtles_cov_50, turtles_cov_95),
                      ID = c(rep("GTR 50", length(turtles_gtr_50)),
                             rep("GTR 95", length(turtles_gtr_95)),
                             rep("Partition 50", length(turtles_part_50)),
                             rep("Partition 95", length(turtles_part_95)),
                             rep("Covarion 50", length(turtles_cov_50)),
                             rep("Covarion 95", length(turtles_cov_95))),
                      model = c(rep("GTR", 156), rep("Partition", 156), rep("Cov", 156)))

squamates_df = data.frame(Distances = c(squamates_gtr_50, squamates_gtr_95, squamates_part_50, squamates_part_95, squamates_cov_50, squamates_cov_95),
                      ID = c(rep("GTR 50", length(squamates_gtr_50)),
                             rep("GTR 95", length(squamates_gtr_95)),
                             rep("Partition 50", length(squamates_part_50)),
                             rep("Partition 95", length(squamates_part_95)),
                             rep("Covarion 50", length(squamates_cov_50)),
                             rep("Covarion 95", length(squamates_cov_95))),
                      model = c(rep("GTR", 156), rep("Partition", 156), rep("Cov", 156)))

amph_df = data.frame(Distances = c(amph_gtr_50, amph_gtr_95, amph_part_50, amph_part_95, amph_cov_50, amph_cov_95),
                      ID = c(rep("GTR 50", length(amph_gtr_50)),
                             rep("GTR 95", length(amph_gtr_95)),
                             rep("Partition 50", length(amph_part_50)),
                             rep("Partition 95", length(amph_part_95)),
                             rep("Covarion 50", length(amph_cov_50)),
                             rep("Covarion 95", length(amph_cov_95))),
                      model = c(rep("GTR", 156), rep("Partition", 156), rep("Cov", 156)))

p1 = ggplot(crocs_df, aes(x = ID, y = Distances, fill=model)) + geom_violin() + 
  labs(x = "", y = "", title = "Crocodilians") +
  theme(legend.position="none", text = element_text(size=17), plot.title = element_text(hjust=0.5, size=20)) +
  scale_x_discrete(labels=c("Covarion 50" = "50", "Covarion 95" = "95", "GTR 50" = "50",
                            "GTR 95" = "95", "Partition 50" = "50", "Partition 95" = "95")) + 
  scale_fill_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Cov" = "#D4D4D4")) +
  stat_summary(fun="median", geom="point", size=2, color="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(hjust=0.5, angle=90)) + ylim(0, 170)

p2 = ggplot(turtles_df, aes(x = ID, y = Distances, fill=model)) + geom_violin() + 
  labs(x = "", y = "", title = "Turtles") +
  theme(legend.position="none", text = element_text(size=17), plot.title = element_text(hjust=0.5, size=20)) +
  scale_x_discrete(labels=c("Covarion 50" = "50", "Covarion 95" = "95", "GTR 50" = "50",
                            "GTR 95" = "95", "Partition 50" = "50", "Partition 95" = "95")) + 
  scale_fill_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Cov" = "#D4D4D4")) +
  stat_summary(fun="median", geom="point", size=2, color="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(hjust=0.5, angle=90)) + ylim(0, 170)

p3 = ggplot(squamates_df, aes(x = ID, y = Distances, fill=model)) + geom_violin() + 
  labs(x = "", y = "", title = "Squamates") +
  theme(legend.position="none", text = element_text(size=17), plot.title = element_text(hjust=0.5, size=20)) +
  scale_x_discrete(labels=c("Covarion 50" = "50", "Covarion 95" = "95", "GTR 50" = "50",
                            "GTR 95" = "95", "Partition 50" = "50", "Partition 95" = "95")) + 
  scale_fill_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Cov" = "#D4D4D4")) +
  stat_summary(fun="median", geom="point", size=2, color="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(hjust=0.5, angle=90)) + ylim(0, 170)

p4 = ggplot(amph_df, aes(x = ID, y = Distances, fill=model)) + geom_violin() + 
  labs(x = "", y = "", title = "Amphibians") +
  theme(legend.position="none", text = element_text(size=17), plot.title = element_text(hjust=0.5, size=20)) +
  scale_x_discrete(labels=c("Covarion 50" = "50", "Covarion 95" = "95", "GTR 50" = "50",
                            "GTR 95" = "95", "Partition 50" = "50", "Partition 95" = "95")) + 
  scale_fill_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Cov" = "#D4D4D4")) +
  stat_summary(fun="median", geom="point", size=2, color="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(hjust = 0.5, angle=90)) + ylim(0, 170)

grid.arrange(p1, p2, p3, p4, left=textGrob("Pairwise uRF Distances", vjust = 0.8, rot = 90, gp=gpar(fontsize=25)),
             bottom=textGrob("Consensus Cutoff", vjust = 0.1, gp=gpar(fontsize=25)), nrow = 2)
