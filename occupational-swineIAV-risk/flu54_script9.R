################
### This, script9, reads all the data from scripts 1-5 to make odds ratios tables.
### By Garrett Janzen
################

setwd("C:/Users/Working.Directory")

# ##########################

library("ggplot2")
library("gridExtra")
library("readxl")
library("reshape2")
library("dplyr")
library("stringr")
library("ggh4x")
library("car")
library("psych")
library("openxlsx") # to format dates
library("lubridate") # to modify dates, in prep for making Decade
library("epitools")
library("reshape2")
library("lsmeans")

##########################

data <- as.data.frame(read.csv("script6_formatted_data.csv"));head(data)
data$Decade <- as.factor(data$Decade)
data$cohort_group <- ifelse(data$cohort == "Farm" | data$cohort == "Vet", "Ag", "Non-Ag")

data$Agebin2 <- "1968-1976"
data$Agebin2 <- ifelse(data$Age < (2023-1976), "Post-1976", data$Agebin2) # if age less than 47
data$Agebin2 <- ifelse(data$Age > (2023-1968), "Pre-1968", data$Agebin2)  #55

data_plot    <- data[,c(9:21, 1, 5:8, 22:23, 45)]
data_plot_HI <- data_plot[,c(1:5,  14:ncol(data_plot))]
data_plot_BJ <- data_plot[,c(6:9,  14:ncol(data_plot))]
data_plot_HK <- data_plot[,c(10:13,14:ncol(data_plot))]

data_plot_HI_melt <- reshape2::melt(data_plot_HI, id.vars=c("HI19.HuVac","cohort","Age","Decade","Gender","Agebin","Agebin2","cohort_group","Influenza_vaccine_.12_months"))
colnames(data_plot_HI_melt) <- c("vaccine_titer","cohort","age","decade","sex","agebin","agebin2","cohort_group","vacc_status","swine_strain","swine_titer")
data_plot_HI_melt$vaccine <- "HI"

data_plot_BJ_melt <- reshape2::melt(data_plot_BJ, id.vars=c("BJ95.1B.2","cohort","Age","Decade","Gender","Agebin","Agebin2","cohort_group","Influenza_vaccine_.12_months"))
colnames(data_plot_BJ_melt) <- c("vaccine_titer","cohort","age","decade","sex","agebin","agebin2","cohort_group","vacc_status","swine_strain","swine_titer")
data_plot_BJ_melt$vaccine <- "BJ"

data_plot_HK_melt <- reshape2::melt(data_plot_HK, id.vars=c("HK19.HuVac","cohort","Age","Decade","Gender","Agebin","Agebin2","cohort_group","Influenza_vaccine_.12_months"))
colnames(data_plot_HK_melt) <- c("vaccine_titer","cohort","age","decade","sex","agebin","agebin2","cohort_group","vacc_status","swine_strain","swine_titer")
data_plot_HK_melt$vaccine <- "HK"

data_plot_melt <- rbind(data_plot_HI_melt, data_plot_BJ_melt, data_plot_HK_melt)
data_plot_melt$vaccine_titer <- ifelse(data_plot_melt$vaccine_titer == 1, 0, data_plot_melt$vaccine_titer)
data_plot_melt$swine_titer <- ifelse(data_plot_melt$swine_titer == 1, 0, data_plot_melt$swine_titer)

data_plot_melt_alt <- data_plot_melt
data_plot_melt_alt$vaccine_titer <- log(data_plot_melt_alt$vaccine_titer/10, 2)
data_plot_melt_alt$vaccine_titer <- ifelse(data_plot_melt_alt$vaccine_titer == -Inf, NA, data_plot_melt_alt$vaccine_titer)
data_plot_melt_alt$swine_titer <- log(data_plot_melt_alt$swine_titer/10, 2)
data_plot_melt_alt$swine_titer <- ifelse(data_plot_melt_alt$swine_titer == -Inf, NA, data_plot_melt_alt$swine_titer)

hist(data_plot_melt_alt$vaccine_titer, breaks=20)
hist(data_plot_melt_alt$swine_titer, breaks=20)

plot1 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot1
plot2 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer, color=vaccine)) +
  geom_point(aes(colour = vaccine),size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot2
plot2.5 <- ggplot(na.omit(data_plot_melt_alt), aes(x=vaccine_titer, y=swine_titer, color=vacc_status)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot2.5
plot3 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer, color=cohort)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot3
plot3.5 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer, color=cohort_group)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot3.5
plot4 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer, color=decade)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2, alpha = 0.2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot4
plot5 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer, color=agebin)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot5
plot5.5 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer, color=agebin2)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot5.5
plot6 <- ggplot(data_plot_melt_alt, aes(x=vaccine_titer, y=swine_titer, color=sex)) +
  geom_point(size=1) + 
  geom_smooth(method=lm, size=2) +
  xlab("Log2 (Vaccine titer/10)") +
  ylab("Log2 (Swine virus titer/10)") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot6

png("Figures/Correlations/script7_scatter.png");plot1;dev.off()
png("Figures/Correlations/script7_scatter_vaccine.png");plot2;dev.off()
png("Figures/Correlations/script7_scatter_vacc_status.png");plot2.5;dev.off()
png("Figures/Correlations/script7_scatter_cohort.png");plot3;dev.off()
png("Figures/Correlations/script7_scatter_cohort_group.png");plot3.5;dev.off()
png("Figures/Correlations/script7_scatter_decade.png");plot4;dev.off()
png("Figures/Correlations/script7_scatter_agebin.png");plot5;dev.off()
png("Figures/Correlations/script7_scatter_agebin2.png");plot5.5;dev.off()
png("Figures/Correlations/script7_scatter_sex.png");plot6;dev.off()

##########
m.interaction <- lm(swine_titer ~ vaccine_titer*decade, data = data_plot_melt_alt)
anova(m.interaction)
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "decade", var="vaccine_titer")
m.lst.pairs <- pairs(m.lst)

write.csv(m.lst, file="Output/script7_slopes_decade.csv", row.names = F)
write.csv(m.lst.pairs, file="Output/script7_slopes_decade_pairs.csv", row.names = F)

##########
m.interaction <- lm(swine_titer ~ vaccine_titer*agebin2, data = data_plot_melt_alt)
anova(m.interaction)
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "agebin2", var="vaccine_titer")
m.lst.pairs <- pairs(m.lst)

write.csv(m.lst, file="Output/script7_slopes_decade.csv", row.names = F)
write.csv(m.lst.pairs, file="Output/script7_slopes_decade_pairs.csv", row.names = F)

##########
m.interaction <- lm(swine_titer ~ vaccine_titer*cohort, data = data_plot_melt_alt)
anova(m.interaction)
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "cohort", var="vaccine_titer")
m.lst.pairs <- pairs(m.lst)

write.csv(m.lst, file="Output/script7_slopes_decade.csv", row.names = F)
write.csv(m.lst.pairs, file="Output/script7_slopes_decade_pairs.csv", row.names = F)

############







