
# Load any packages
library(nlme)
library(ggplot2)
library(lme4)
library(viridis)
library(scales)
library(gridExtra)

data <- read.csv("Data/MPs_lab_vs_soil.csv")

median(data[which(data$Source == "Lab"), "MPs"])/median(data[which(data$Source == "Soil"), "MPs"])
mean(data[which(data$Source == "Lab"), "MPs"])/mean(data[which(data$Source == "Soil"), "MPs"])

#How many studies had concentrations below the max?
MAX <- max(data[which(data$Source == "Soil"), "MPs"])
data[which(data[which(data$Source == "Lab"), "MPs"] < MAX),]

#How many studies had concentrations below the median?
MED <- median(data[which(data$Source == "Soil"), "MPs"])
data[which(data[which(data$Source == "Lab"), "MPs"] < MED),]

set.seed(1)
A <- 
  ggplot(data = data, aes(y = MPs, x = Source, fill = Source)) +
  ggtitle("A)") +
  geom_jitter(aes(col = Source), width = 0.1, size = 0.3) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, size = 0.2) +
  scale_y_log10(expand = c(0,1),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_x_discrete(labels = c("In vivo studies", "Soil")) +
  scale_color_manual(values = c("#d1495b", "#00798c")) +
  scale_fill_manual(values = c("#d1495b", "#00798c")) +
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


POLYMERS <- read.csv("Data/Polymers.csv")
POLYMERS_SOIL <- POLYMERS[which(POLYMERS$Study == "Soil"),]
POLYMERS_SOIL$Polymer <- factor(POLYMERS_SOIL$Polymer,
                  levels = POLYMERS_SOIL$Polymer[order(POLYMERS_SOIL$Count, decreasing = TRUE)])

POLYMERS_LAB <- POLYMERS[which(POLYMERS$Study == "Lab"),]
POLYMERS_LAB$Polymer <- factor(POLYMERS_LAB$Polymer,
                                levels = POLYMERS_SOIL$Polymer[order(POLYMERS_SOIL$Count, decreasing = TRUE)])

B <- 
  ggplot() +
  ggtitle("B)") +
  geom_bar(data = POLYMERS_SOIL, aes(y = Count, x = Polymer),
           stat = "identity", alpha = 0.7, fill = "#00798c") +
  geom_bar(data = POLYMERS_LAB, aes(y = Count, x = Polymer),
           stat = "identity", alpha = 0.7, fill = "#d1495b") +
  scale_color_manual(values = c("#00798c", "#d1495b")) +
  scale_fill_manual(values = c("#00798c", "#d1495b")) +
  scale_y_continuous(limits = c(0,60), expand = c(0,0.1)) +
  ylab("Number of studies") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(angle = 90, size=6, family = "sans", face = "bold", colour = "black", hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))




lab <- data[which(data$Source == "Lab"),]

fit <- lmer(log10(MPs) ~ Year + (1|DOI), data = lab)
summary(fit)
confint(fit)

hist(residuals(fit))

C <- 
  ggplot(data = lab, aes(y = MPs, x = Year)) +
  ggtitle("C)") +
  geom_hline(aes(yintercept = median(data[which(data$Source == "Soil"), "MPs"])), linetype = "dashed", col = "grey70") +
  geom_smooth(col = "grey40", size = 0.5, method = "lm", se = F) +
  #geom_point(col = "#d1495b", size = 0.6) +
  geom_jitter(col = "#d1495b", size = 0.5, width = 0.05) +
  scale_y_log10(expand = c(0,1),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ylab("Microplastics (items/kg)") +
  xlab("Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", colour = "black"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


TOP <-
    grid.arrange(A,B,
                 ncol=2,
                 nrow=1)

FIG <-
  grid.arrange(TOP,C,
               ncol=1,
               nrow=2,
               heights = c(1, 0.5))

ggsave(FIG,
       width = 6.86, height = 6, units = "in",
       dpi = 600,
       bg = "transparent",
       file="Figures/MP_Disconnect.png")
