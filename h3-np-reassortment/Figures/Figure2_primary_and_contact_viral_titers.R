library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(readxl)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(emmeans)
library(multcomp)
library(multcompView)
library(cowplot)


#laptop
setwd("//las-dfs-01.las.iastate.edu/lss/research/pcgauger-lab/Megan/NP_WGS/animal_study/flu60/results/")

#desktop
#setwd("//iastate/lss/research/pcgauger-lab/Megan/NP_WGS/animal_study/flu60/results/")
c2 <- read_excel("VI_and_VT_results_combined.xlsx", sheet = "Contact Pigs for R")
p2 <- read_excel("VI_and_VT_results_combined.xlsx", sheet = "Primary Pigs for R")
#setwd("//iastate/lss/research/pcgauger-lab/Megan/NP_WGS/animal_study/flu60/results/data_analysis/")
setwd("//las-dfs-01.las.iastate.edu/lss/research/pcgauger-lab/Megan/NP_WGS/animal_study/flu60/results/data_analysis")

min_text_size = 18
line_thickness = 1.5
#custom color palette
#jco_edit <-  c("#3b3b3bFF","#0073C2FF","#EFC000FF","#868686FF","#CD534CFF")

group_colors <- c( "control" = "#3b3b3bFF", "wtNC/19-pdmNP" = "#0073C2FF", "NC/19-trigNP" = "#EFC000FF",
                   "wtMN/18-trigNP" = "#868686FF", "MN/18-pdmNP" = "#CD534CFF")

#make Contacts barchart by group over DPC
sem <- function(x, na.rm = FALSE) {
  out <-sd(x, na.rm = na.rm)/sqrt(length(x))
  return(out)
}

gather_cols <- c("0DPC Titer", "1DPC Titer", "2DPC Titer", "3DPC Titer", "4DPC Titer", "5DPC Titer", "7DPC Titer", "9DPC Titer")
c2_long <- gather(c2, DPC, Titer, gather_cols)

c2_tidy <- c2_long %>%
  group_by(DPC,Group) %>%
  summarize(m = mean(Titer),
            sem = sem(Titer),
            sd = sd(Titer)) %>%
  mutate(DPC = str_remove(DPC, "DPC Titer"))

c2_tidy$DPC <- factor(c2_tidy$DPC)
c2_tidy$Group <- factor(c2_tidy$Group, 
                        levels = c("wtNC/19 pdm NP", "rgNC/19 TRIG NP", "wtMN/18 TRIG NP", "rgMN/18 pdm NP"))

levels(c2_tidy$Group)[levels(c2_tidy$Group)=="wtNC/19 pdm NP"] <- "wtNC/19-pdmNP"
levels(c2_tidy$Group)[levels(c2_tidy$Group)=="rgNC/19 TRIG NP"] <- "NC/19-trigNP"
levels(c2_tidy$Group)[levels(c2_tidy$Group)=="wtMN/18 TRIG NP"] <- "wtMN/18-trigNP"
levels(c2_tidy$Group)[levels(c2_tidy$Group)=="rgMN/18 pdm NP"] <- "MN/18-pdmNP"

#try a line plot
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
c2_line <- ggplot(c2_tidy, aes(x=DPC, y=m, group = Group, color = Group)) +
  geom_line(size = line_thickness) +
  geom_point(size = line_thickness) +
  scale_color_manual(values = group_colors) +
  geom_errorbar(aes(ymin=m-sem, ymax=m+sem), width = 0.2, position = position_dodge(0.05), size = line_thickness-0.5) +
  labs(x="Days Post Contact (DPC)", y="TCID50/ml(log10)") +
  theme(
    axis.text = element_text(size = min_text_size - 2, face = "bold"),
    axis.title = element_text(size = min_text_size - 2, face = "bold"),
    legend.title = element_text(size = min_text_size, face="bold"),
    legend.text = element_text(size=min_text_size), 
    legend.position = "none"
  )
c2_line


tiff("line_graph_all_contact_NS_6-26-23.tif", units = "in", width = 11, height = 5, res = 300, compression = "lzw")
c2_line
dev.off()

#repeat for primary pigs
gather_cols <- c("0DPI Titer", "1DPI Titer", "2DPI Titer", "3DPI Titer", "4DPI Titer", "5DPI Titer")
p2_long <- gather(p2, DPI, Titer, gather_cols)

p2_tidy <- p2_long %>%
  group_by(DPI,Group) %>%
  summarize(m = mean(Titer),
            sem = sem(Titer),
            sd = sd(Titer)) %>%
  mutate(DPI = str_remove(DPI, "DPI Titer"))
p2_tidy$DPC <- factor(p2_tidy$DPI)
p2_tidy$Group <- factor(p2_tidy$Group, 
                        levels = c("control", "wtNC/19 pdm NP", "rgNC/19 TRIG NP", "wtMN/18 TRIG NP", "rgMN/18 pdm NP"))

levels(p2_tidy$Group)[levels(p2_tidy$Group)=="wtNC/19 pdm NP"] <- "wtNC/19-pdmNP"
levels(p2_tidy$Group)[levels(p2_tidy$Group)=="rgNC/19 TRIG NP"] <- "NC/19-trigNP"
levels(p2_tidy$Group)[levels(p2_tidy$Group)=="wtMN/18 TRIG NP"] <- "wtMN/18-trigNP"
levels(p2_tidy$Group)[levels(p2_tidy$Group)=="rgMN/18 pdm NP"] <- "MN/18-pdmNP"

p2_line <- ggplot(p2_tidy, aes(x=DPI, y=m, group = Group, color = Group)) +
  geom_line(size = line_thickness) +
  geom_point(size = line_thickness) +
  scale_color_manual(values = group_colors) +
  geom_errorbar(aes(ymin=m-sem, ymax=m+sem), width = 0.2, position = position_dodge(0.05), size = line_thickness-0.5) +
  labs(x="Days Post Inoculation (DPI)", y="TCID50/ml(log10)") +
  ylim(0,6) +
  theme(
    axis.text = element_text(size = min_text_size - 2, face = "bold"),
    axis.title = element_text(size = min_text_size - 2, face = "bold"),
    legend.title = element_text(size = min_text_size - 2, face="bold"),
    legend.text = element_text(size=min_text_size-2),
    legend.position = "right"
  )
p2_line

tiff("line_graph_all_primary_NS_6-26-23.tif", units = "in", width = 11, height = 5, res = 300, compression = "lzw")
p2_line
dev.off()

combo <- plot_grid(p2_line, c2_line, labels = c("", ""), label_size = min_text_size-5, ncol = 1)
combo

tiff("line_graph_primary_and_contacts_fix_virus_names_8-02-23.tif", units = "in", width = 11, height = 5, res = 300, compression = "lzw")
combo
dev.off()


#one-way ANOVA, doing them by individual days
#ANOVA of 3DPC
dpc3_contacts <- c2_long %>%
  filter(DPC == "3DPC Titer")
DPC3_ANOVA <- lm(Titer ~ Group, data = dpc3_contacts)
anova(DPC3_ANOVA)
lm()
tukey <- TukeyHSD(aov(DPC3_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPC3_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld


dpc4_contacts <- c2_long %>%
  filter(DPC == "4DPC Titer")
DPC4_ANOVA <- lm(Titer ~ Group, data = dpc4_contacts)
anova(DPC4_ANOVA)
tukey <- TukeyHSD(aov(DPC4_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPC4_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

dpc5_contacts <- c2_long %>%
  filter(DPC == "5DPC Titer")
DPC5_ANOVA <- lm(Titer ~ Group, data = dpc5_contacts)
anova(DPC5_ANOVA)
tukey <- TukeyHSD(aov(DPC5_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPC5_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

dpc7_contacts <- c2_long %>%
  filter(DPC == "7DPC Titer")
DPC7_ANOVA <- lm(Titer ~ Group, data = dpc7_contacts)
anova(DPC7_ANOVA)
tukey <- TukeyHSD(aov(DPC7_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPC7_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

dpc9_contacts <- c2_long %>%
  filter(DPC == "9DPC Titer")
DPC9_ANOVA <- lm(Titer ~ Group, data = dpc9_contacts)
anova(DPC9_ANOVA)
tukey <- TukeyHSD(aov(DPC9_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPC9_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

#One-way ANOVA of primaries:
DPI0_primary <- p2_long %>%
  filter(DPI == "0DPI Titer")
DPI0_ANOVA <- lm(Titer ~ Group, data = DPI0_primary)
anova(DPI0_ANOVA)
tukey <- TukeyHSD(aov(DPI0_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPI0_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld
 
DPI1_primary <- p2_long %>%
  filter(DPI == "1DPI Titer")
DPI1_ANOVA <- lm(Titer ~ Group, data = DPI1_primary)
anova(DPI1_ANOVA)
tukey <- TukeyHSD(aov(DPI1_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPI1_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

DPI2_primary <- p2_long %>%
  filter(DPI == "2DPI Titer")
DPI2_ANOVA <- lm(Titer ~ Group, data = DPI2_primary)
anova(DPI2_ANOVA)
tukey <- TukeyHSD(aov(DPI2_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPI2_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

DPI3_primary <- p2_long %>%
  filter(DPI == "3DPI Titer")
DPI3_ANOVA <- lm(Titer ~ Group, data = DPI3_primary)
anova(DPI3_ANOVA)
tukey <- TukeyHSD(aov(DPI3_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPI3_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

DPI4_primary <- p2_long %>%
  filter(DPI == "4DPI Titer")
DPI4_ANOVA <- lm(Titer ~ Group, data = DPI4_primary)
anova(DPI4_ANOVA)
tukey <- TukeyHSD(aov(DPI4_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPI4_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld

DPI5_primary <- p2_long %>%
  filter(DPI == "5DPI Titer")
DPI5_ANOVA <- lm(Titer ~ Group, data = DPI5_primary)
anova(DPI5_ANOVA)
tukey <- TukeyHSD(aov(DPI5_ANOVA))
tukey
plot(tukey, las=1, col="brown")
model_means <- emmeans(object = DPI5_ANOVA, specs = "Group")
model_means_cld <- cld(object = model_means, adjust = "Tukey", Letters = letters, alpha = 0.05)
model_means_cld


#summary of means by DPC
contacts %>%
  group_by(DPC) %>%
  get_summary_stats(Titer, type="mean_sd")
#graph summary boxplots by day
bxp <- ggboxplot(contacts, x = "DPC", y = "Titer", add = "point")
bxp
#identify extreme outliers by day
contacts %>%
  group_by(DPC) %>%
  identify_outliers(Titer)

###2-WAY repeated measures ANOVA

#convert to factor
contacts2 <- contacts %>%
  convert_as_factor(DPC,Pig,Group)
#summary stats
contacts2 %>%
  group_by(Group_Name, DPC) %>%
  get_summary_stats(Titer, type = "mean_sd")
#order Group Name
contacts2$Group_Name <- factor(contacts2$Group_Name, levels = c("wtNC/19 pdm NP", "rgNC/19 TRIG NP", "wtMN/18 TRIG NP", "rgMN/18 pdm NP"))
#visualize contacts with boxplot
bxp2 <- ggboxplot(
  contacts2, x = "DPC", y = "Titer", color = "Group_Name", palette = "jco", size =1) +
  labs(x="DPC", y="Log10 TCID50/mL", color = "Group") +
  theme(
    axis.text = element_text(size = min_text_size - 1, face = "bold"),
    axis.title = element_text(size = min_text_size, face = "bold"),
    legend.title = element_text(size = min_text_size, face="bold"),
    legend.text = element_text(size=min_text_size),
    plot.title = element_text(hjust = 0.5, face = "bold", size = min_text_size +1),
    strip.text.x = element_text(size = min_text_size)
  ) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  grids(linetype = "dashed", color = "darkgrey")
bxp2

tiff("boxplot_all_groups_by_dpc.tif", units = "in", width = 11, height = 5, res = 300, compression = "lzw")
bxp2
dev.off()


#individual day ANOVAs#
# Day 4 is the most significant #
day3 <- contacts2[contacts2$DPC == "3",]
summary(day3)
one.way <- aov(Titer ~ Group, data = day3)
summary(one.way)
tukey.one.way <- TukeyHSD(one.way)
tukey.one.way

day4 <- contacts2[contacts2$DPC == "4",]
summary(day4)
one.way <- aov(Titer ~ Group, data = day4)
summary(one.way)
tukey.one.way <- TukeyHSD(one.way)
tukey.one.way

day5 <- contacts2[contacts2$DPC == "5",]
summary(day5)
one.way <- aov(Titer ~ Group, data = day5)
summary(one.way)
tukey.one.way <- TukeyHSD(one.way)
tukey.one.way
