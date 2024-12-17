# Plotting differences in marginal likelihoods between three competing models in four datasets

library(ggplot2)
library(gridExtra)
library(grid)

setwd('C:/Users/ben/Desktop/covarion/results/ss')

turtles_diffs = scan('turtles_diffs.csv', quiet=TRUE)
turtles_al = scan('turtles_al.csv', quiet=TRUE)

turtles_df = data.frame(diffs = turtles_diffs, Model = c(rep("GTR", 13), rep("Partition", 13),
                                                       rep("Covarion", 13)),
                       al_size = turtles_al)


crocs_diffs = scan('crocs_diffs.csv', quiet=TRUE)
crocs_al = scan('crocs_al.csv', quiet=TRUE)

crocs_df = data.frame(diffs = crocs_diffs, Model = c(rep("GTR", 13), rep("Partition", 13),
                                                       rep("Covarion", 13)),
                       al_size = crocs_al)

squamates_diffs = scan('squamates_diffs.csv', quiet=TRUE)
squamates_al = scan('squamates_al.csv', quiet=TRUE)

squamates_df = data.frame(diffs = squamates_diffs, Model = c(rep("GTR", 13), rep("Partition", 13),
                                                     rep("Covarion", 13)),
                      al_size = squamates_al)

amph_diffs = scan('amph_diffs.csv', quiet=TRUE)
amph_al = scan('amph_al.csv', quiet=TRUE)

amph_df = data.frame(diffs = amph_diffs, Model = c(rep("GTR", 13), rep("Partition", 13),
                                                     rep("Covarion", 13)),
                      al_size = amph_al)

p1 = ggplot(crocs_df, aes(x=al_size, y=diffs, color=Model)) + geom_point(size=4) +
  labs(x = "", y = "", title="Crocodilians (n=20)") + ylim(-2250, 0) +
  theme(legend.position = c(.2, .3), legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(hjust = 0.5, size=20), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=13), legend.title = element_text(size=13)) +
  scale_color_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Covarion" = "#D4D4D4"))

p2 = ggplot(turtles_df, aes(x=al_size, y=diffs, color=Model)) + geom_point(size=4) +
  labs(x = "", y = "", title="Turtles (n=53)") + ylim(-2250, 0) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=20), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size=15),
        axis.line = element_line(colour = "black"), axis.title = element_text(size=15)) +
  scale_color_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Covarion" = "#D4D4D4"))

p3 = ggplot(squamates_df, aes(x=al_size, y=diffs, color=Model)) + geom_point(size=4) +
  labs(x = "", y = "", title="Squamates (n=120)") + ylim(-2250, 0) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=20), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size=15),
        axis.line = element_line(colour = "black"), axis.title = element_text(size=15)) +
  scale_color_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Covarion" = "#D4D4D4"))

p4 = ggplot(amph_df, aes(x=al_size, y=diffs, color=Model)) + geom_point(size=4) +
  labs(x = "", y = "", title="Amphibians (n=157)") + ylim(-2250, 0) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=20), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.text = element_text(size=15),
        axis.line = element_line(colour = "black"), axis.title = element_text(size=15)) +
  scale_color_manual(values = c("GTR" = "#373D3F", "Partition" = "#8C979A", "Covarion" = "#D4D4D4"))

grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2, left=textGrob("Difference from Max Marginal Lkl", vjust = 0.8, rot = 90, gp=gpar(fontsize=20)),
             bottom=textGrob("Alignment Size (bp)", vjust = 0.1, gp=gpar(fontsize=20)))


