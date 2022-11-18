
# Load any packages
library(nlme)
library(ggplot2)
library(lme4)
library(viridis)
library(scales)

data <- read.csv("Data/MPs_lab_vs_soil.csv")

median(data[which(data$Source == "Lab"), "MPs"])/median(data[which(data$Source == "Soil"), "MPs"])
mean(data[which(data$Source == "Lab"), "MPs"])/mean(data[which(data$Source == "Soil"), "MPs"])


FIG <- 
  ggplot(data = data, aes(y = MPs, x = Source, fill = Source)) +
  geom_jitter(aes(col = Source), width = 0.1, size = 0.3) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, size = 0.2) +
  scale_y_log10(expand = c(0,1),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_x_discrete(labels = c("in vivo studies", "Soil")) +
  scale_color_manual(values = c("#00798c", "#d1495b")) +
  scale_fill_manual(values = c("#00798c", "#d1495b")) +
  ylab("Microplastics (items/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", face = "bold", colour = "black"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


ggsave(FIG,
       width = 3.23, height = 3, units = "in",
       dpi = 600,
       bg = "transparent",
       file="MP_Disconnect.png")
