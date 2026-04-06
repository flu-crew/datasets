
################
### This script reads the data from CEIRRFLU54.xlsx and creates box plots.
### By Garrett Janzen
################

setwd("C:/Users/Working.Directory")

# ##########################

library("ggplot2")
library("readxl")
library("reshape2")
library("dplyr")
library("stringr")
library("ggh4x")
library("car")
library("psych")

##########################
data <- as.data.frame(read_xlsx("CEIRRFLU54.xlsx", sheet=2));dim(data) # This is the original data file, original sheet

colnames(data) <- str_replace_all(colnames(data), " ", "_")
colnames(data) <- str_replace_all(colnames(data), "-", "_")
colnames(data) <- str_replace_all(colnames(data), "/", "_")
colnames(data);colnames(data)[which(colnames(data) == "IA21.1990.4a")] <- "IA21.1990.4.a";colnames(data)
for(i in 1:length(colnames(data))){
  if(!is.na(as.numeric(substr(colnames(data)[i],1,1)))){
    colnames(data)[i] <- paste0("y", colnames(data)[i])
  }
}

# change all values of 0 or <10 to 10.
data[,13:ncol(data)][data[,13:ncol(data)] == 0 | data[,13:ncol(data)] == "<10"] <- 10

data_sub <- data[,c(13:ncol(data))]
data_sub_t <- as.data.frame(t(data_sub))
data_sub_t$clade <- rownames(data_sub_t)
head(data_sub_t)
data_sub_t_melt <- reshape2::melt(data_sub_t, id.vars="clade")
data_sub_t_melt$variable <- NULL
colnames(data_sub_t_melt) <- c("clade","value")
data_sub_t_melt$value <- as.numeric(data_sub_t_melt$value)
plot(density(data_sub_t_melt$value, na.rm=TRUE))
qqnorm(data_sub_t_melt$value)
qqline(data_sub_t_melt$value, col="red",
       distribution=qnorm) #normal distribution, quantile function

gmlabel_1 <- c(
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[1]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[2]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[3]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[4]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[5]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[6]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[7]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[8]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[9]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[10]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[11]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[12]),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt[which(data_sub_t_melt$clade == colnames(data[13:25])[13]),]$value, na.rm=T), digits=0)
);gmlabel_1

data_sub_t_melt$value <- ifelse(data_sub_t_melt$value < 10, 10, data_sub_t_melt$value);table(data_sub_t_melt$value)

data_sub_t_melt$value <- log(data_sub_t_melt$value/10, 2)
data_sub_t_melt$value <- ifelse(data_sub_t_melt$value == -Inf, NA, data_sub_t_melt$value)
data_sub_t_melt$clade <- factor(data_sub_t_melt$clade, levels = colnames(data[13:25]))

plot1 <- ggplot(data_sub_t_melt, aes(x=clade, y=value)) + geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Clade") +
  stat_summary(geom = 'text', label = gmlabel_1, fun.y = max, vjust = -1) +
  scale_y_continuous(breaks=seq(0,10,2));plot1

pdf("Figures/Sheet1/s1_clade_value.pdf", width=12)
plot1
dev.off()

###
design <- "
  ABCDEFG
  HIJKLM#
"

design_threerows <- "
  ABCDE
  FGHI#
  JKLM#
"
###

######################## Decades ######################## 
colnames(data) # 13-25 are the genotypes
data_sub_temp <- data[,c("Decade",colnames(data)[13:25])]
data_sub_temp$Decade <- as.character(data_sub_temp$Decade)
# data_sub_temp[data_sub_temp=="<10"]<-0

colnames(data_sub_temp)[2:ncol(data_sub_temp)]
i <- 1
data1 <- data_sub_temp[,c(1,i+1)]
data2 <- data_sub_temp[,c(1,i+2)]
data3 <- data_sub_temp[,c(1,i+3)]
data4 <- data_sub_temp[,c(1,i+4)]
data5 <- data_sub_temp[,c(1,i+5)]
data6 <- data_sub_temp[,c(1,i+6)]
data7 <- data_sub_temp[,c(1,i+7)]
data8 <- data_sub_temp[,c(1,i+8)]
data9 <- data_sub_temp[,c(1,i+9)]
data10 <- data_sub_temp[,c(1,i+10)]
data11 <- data_sub_temp[,c(1,i+11)]
data12 <- data_sub_temp[,c(1,i+12)]
data13 <- data_sub_temp[,c(1,i+13)]

data1melt <- melt(data1, id="Decade")
data2melt <- melt(data2, id="Decade")
data3melt <- melt(data3, id="Decade")
data4melt <- melt(data4, id="Decade")
data5melt <- melt(data5, id="Decade")
data6melt <- melt(data6, id="Decade")
data7melt <- melt(data7, id="Decade")
data8melt <- melt(data8, id="Decade")
data9melt <- melt(data9, id="Decade")
data10melt <- melt(data10, id="Decade")
data11melt <- melt(data11, id="Decade")
data12melt <- melt(data12, id="Decade")
data13melt <- melt(data13, id="Decade")

data1melt$variable <- colnames(data_sub_temp)[i+1]
data2melt$variable <- colnames(data_sub_temp)[i+2]
data3melt$variable <- colnames(data_sub_temp)[i+3]
data4melt$variable <- colnames(data_sub_temp)[i+4]
data5melt$variable <- colnames(data_sub_temp)[i+5]
data6melt$variable <- colnames(data_sub_temp)[i+6]
data7melt$variable <- colnames(data_sub_temp)[i+7]
data8melt$variable <- colnames(data_sub_temp)[i+8]
data9melt$variable <- colnames(data_sub_temp)[i+9]
data10melt$variable <- colnames(data_sub_temp)[i+10]
data11melt$variable <- colnames(data_sub_temp)[i+11]
data12melt$variable <- colnames(data_sub_temp)[i+12]
data13melt$variable <- colnames(data_sub_temp)[i+13]

colnames(data1melt) <- c("decade","clade","value")
colnames(data2melt) <- c("decade","clade","value")
colnames(data3melt) <- c("decade","clade","value")
colnames(data4melt) <- c("decade","clade","value")
colnames(data5melt) <- c("decade","clade","value")
colnames(data6melt) <- c("decade","clade","value")
colnames(data7melt) <- c("decade","clade","value")
colnames(data8melt) <- c("decade","clade","value")
colnames(data9melt) <- c("decade","clade","value")
colnames(data10melt) <- c("decade","clade","value")
colnames(data11melt) <- c("decade","clade","value")
colnames(data12melt) <- c("decade","clade","value")
colnames(data13melt) <- c("decade","clade","value")

data_sub_t_melt_decade <- rbind(data1melt, data2melt, data3melt, data4melt, data5melt, data6melt, data7melt,
                                data8melt, data9melt, data10melt, data11melt, data12melt, data13melt)
data_sub_t_melt_decade$value <- as.numeric(data_sub_t_melt_decade$value)
data_sub_t_melt_decade$value <- log(data_sub_t_melt_decade$value/10, 2)
data_sub_t_melt_decade$decade <- as.factor(data_sub_t_melt_decade$decade)
data_sub_t_melt_decade$value <- ifelse(data_sub_t_melt_decade$value == -Inf, NA, data_sub_t_melt_decade$value)

plot_decade <- ggplot(data_sub_t_melt_decade, aes(x=decade, y=value)) + 
  facet_grid(rows = vars(clade)) +
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Decade") +
  scale_y_continuous(breaks=seq(0,10,2)); plot_decade

pdf("Figures/Sheet1/s1_decade_value.pdf", width=3.5, height=14)
plot_decade
dev.off()

plot_decadec <- ggplot(data_sub_t_melt_decade, aes(x=decade, y=value)) + 
  facet_manual(vars(clade), design = design_threerows) +
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Decade") +
  geom_jitter(color="black", size=0.3, alpha=0.5) +
  scale_y_continuous(breaks=seq(0,10,2)); plot_decadec

pdf("Figures/Sheet1/s1_decade_value_cols.pdf", width=12, height=6)
plot_decadec
dev.off()

plot_decadenc <- ggplot(data_sub_t_melt_decade, aes(x=decade, y=value)) + 
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Decade") +
  scale_y_continuous(breaks=seq(0,10,2)); plot_decadenc

pdf("Figures/Sheet1/s1_decade_value_noclade.pdf", width=5, height=5)
plot_decadenc
dev.off()

data_sub_t_melt_decade_nona <- data_sub_t_melt_decade[which(data_sub_t_melt_decade$value != -Inf),]
aov_decade <- aov(value ~ clade + decade, data = data_sub_t_melt_decade_nona)
sum.aov_decade <- summary(aov_decade);sum.aov_decade

data_sub_t_melt_decade_no40 <- data_sub_t_melt_decade_nona[which(data_sub_t_melt_decade_nona$decade != "1940"),]
aov_decade_no40 <- aov(value ~ clade + decade, data = data_sub_t_melt_decade_no40)
sum.aov_decade_no40 <- summary(aov_decade_no40);sum.aov_decade_no40

######################## Genders ########################
colnames(data) # 13-25 are the genotypes
data_sub_temp <- data[,c("Gender",colnames(data)[13:25])]
data_sub_temp$Gender <- as.character(data_sub_temp$Gender)
# data_sub_temp[data_sub_temp=="<10"]<-0

colnames(data_sub_temp)[2:ncol(data_sub_temp)]
i <- 1
data1 <- data_sub_temp[,c(1,i+1)]
data2 <- data_sub_temp[,c(1,i+2)]
data3 <- data_sub_temp[,c(1,i+3)]
data4 <- data_sub_temp[,c(1,i+4)]
data5 <- data_sub_temp[,c(1,i+5)]
data6 <- data_sub_temp[,c(1,i+6)]
data7 <- data_sub_temp[,c(1,i+7)]
data8 <- data_sub_temp[,c(1,i+8)]
data9 <- data_sub_temp[,c(1,i+9)]
data10 <- data_sub_temp[,c(1,i+10)]
data11 <- data_sub_temp[,c(1,i+11)]
data12 <- data_sub_temp[,c(1,i+12)]
data13 <- data_sub_temp[,c(1,i+13)]

data1melt <- melt(data1, id="Gender")
data2melt <- melt(data2, id="Gender")
data3melt <- melt(data3, id="Gender")
data4melt <- melt(data4, id="Gender")
data5melt <- melt(data5, id="Gender")
data6melt <- melt(data6, id="Gender")
data7melt <- melt(data7, id="Gender")
data8melt <- melt(data8, id="Gender")
data9melt <- melt(data9, id="Gender")
data10melt <- melt(data10, id="Gender")
data11melt <- melt(data11, id="Gender")
data12melt <- melt(data12, id="Gender")
data13melt <- melt(data13, id="Gender")

data1melt$variable <- colnames(data_sub_temp)[i+1]
data2melt$variable <- colnames(data_sub_temp)[i+2]
data3melt$variable <- colnames(data_sub_temp)[i+3]
data4melt$variable <- colnames(data_sub_temp)[i+4]
data5melt$variable <- colnames(data_sub_temp)[i+5]
data6melt$variable <- colnames(data_sub_temp)[i+6]
data7melt$variable <- colnames(data_sub_temp)[i+7]
data8melt$variable <- colnames(data_sub_temp)[i+8]
data9melt$variable <- colnames(data_sub_temp)[i+9]
data10melt$variable <- colnames(data_sub_temp)[i+10]
data11melt$variable <- colnames(data_sub_temp)[i+11]
data12melt$variable <- colnames(data_sub_temp)[i+12]
data13melt$variable <- colnames(data_sub_temp)[i+13]

colnames(data1melt) <- c("gender","clade","value")
colnames(data2melt) <- c("gender","clade","value")
colnames(data3melt) <- c("gender","clade","value")
colnames(data4melt) <- c("gender","clade","value")
colnames(data5melt) <- c("gender","clade","value")
colnames(data6melt) <- c("gender","clade","value")
colnames(data7melt) <- c("gender","clade","value")
colnames(data8melt) <- c("gender","clade","value")
colnames(data9melt) <- c("gender","clade","value")
colnames(data10melt) <- c("gender","clade","value")
colnames(data11melt) <- c("gender","clade","value")
colnames(data12melt) <- c("gender","clade","value")
colnames(data13melt) <- c("gender","clade","value")

data_sub_t_melt_gender <- rbind(data1melt, data2melt, data3melt, data4melt, data5melt, data6melt, data7melt,
                                data8melt, data9melt, data10melt, data11melt, data12melt, data13melt)
data_sub_t_melt_gender$value <- as.numeric(data_sub_t_melt_gender$value)
data_sub_t_melt_gender$value <- log(data_sub_t_melt_gender$value/10, 2)
data_sub_t_melt_gender$gender <- as.factor(data_sub_t_melt_gender$gender)
data_sub_t_melt_gender$value <- ifelse(data_sub_t_melt_gender$value == -Inf, NA, data_sub_t_melt_gender$value)

plot_gender <- ggplot(data_sub_t_melt_gender, aes(x=gender, y=value)) + 
  facet_grid(rows = vars(clade)) +
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Sex") +
  scale_y_continuous(breaks=seq(0,10,2)); plot_gender

pdf("Figures/Sheet1/s1_gender_value.pdf", width=3.5, height=14)
plot_gender
dev.off()

plot_genderc <- ggplot(data_sub_t_melt_gender, aes(x=gender, y=value)) + 
  facet_manual(vars(clade), design = design_threerows) +
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Sex") +
  geom_jitter(color="black", size=0.3, alpha=0.5) +
  scale_y_continuous(breaks=seq(0,10,2)); plot_genderc

pdf("Figures/Sheet1/s1_gender_value_cols.pdf", width=12, height=6)
plot_genderc
dev.off()

plot_gendernc <- ggplot(data_sub_t_melt_gender, aes(x=gender, y=value)) + 
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Sex") +
  scale_y_continuous(breaks=seq(0,10,2)); plot_gendernc

pdf("Figures/Sheet1/s1_gender_value_noclade.pdf", width=5, height=5)
plot_gendernc
dev.off()

data_sub_t_melt_gender_nona <- data_sub_t_melt_gender[which(data_sub_t_melt_gender$value != -Inf),]
aov_gender <- aov(value ~ clade + gender, data = data_sub_t_melt_gender_nona)
sum.aov_gender <- summary(aov_gender);sum.aov_gender

data_sub_t_melt_gender_no40 <- data_sub_t_melt_gender_nona[which(data_sub_t_melt_gender_nona$gender != "1940"),]
aov_gender_no40 <- aov(value ~ clade + gender, data = data_sub_t_melt_gender_no40)
sum.aov_gender_no40 <- summary(aov_gender_no40);sum.aov_gender_no40

######################## Vaccine Status ######################## 

colnames(data) # 13-25 are the genotypes
data_sub_temp <- data[,c("Influenza_vaccine_<12_months",colnames(data)[13:25])]
colnames(data_sub_temp)[1] <- "Vaccine"
data_sub_temp$Vaccine<- as.character(data_sub_temp$Vaccine)
# data_sub_temp[data_sub_temp=="<10"]<-0
data_sub_temp <- data_sub_temp[-which(data_sub_temp$Vaccine == "unknown"),]

colnames(data_sub_temp)[2:ncol(data_sub_temp)]
i <- 1
data1 <- data_sub_temp[,c(1,i+1)]
data2 <- data_sub_temp[,c(1,i+2)]
data3 <- data_sub_temp[,c(1,i+3)]
data4 <- data_sub_temp[,c(1,i+4)]
data5 <- data_sub_temp[,c(1,i+5)]
data6 <- data_sub_temp[,c(1,i+6)]
data7 <- data_sub_temp[,c(1,i+7)]
data8 <- data_sub_temp[,c(1,i+8)]
data9 <- data_sub_temp[,c(1,i+9)]
data10 <- data_sub_temp[,c(1,i+10)]
data11 <- data_sub_temp[,c(1,i+11)]
data12 <- data_sub_temp[,c(1,i+12)]
data13 <- data_sub_temp[,c(1,i+13)]

data1melt <- melt(data1, id="Vaccine")
data2melt <- melt(data2, id="Vaccine")
data3melt <- melt(data3, id="Vaccine")
data4melt <- melt(data4, id="Vaccine")
data5melt <- melt(data5, id="Vaccine")
data6melt <- melt(data6, id="Vaccine")
data7melt <- melt(data7, id="Vaccine")
data8melt <- melt(data8, id="Vaccine")
data9melt <- melt(data9, id="Vaccine")
data10melt <- melt(data10, id="Vaccine")
data11melt <- melt(data11, id="Vaccine")
data12melt <- melt(data12, id="Vaccine")
data13melt <- melt(data13, id="Vaccine")

data1melt$variable <- colnames(data_sub_temp)[i+1]
data2melt$variable <- colnames(data_sub_temp)[i+2]
data3melt$variable <- colnames(data_sub_temp)[i+3]
data4melt$variable <- colnames(data_sub_temp)[i+4]
data5melt$variable <- colnames(data_sub_temp)[i+5]
data6melt$variable <- colnames(data_sub_temp)[i+6]
data7melt$variable <- colnames(data_sub_temp)[i+7]
data8melt$variable <- colnames(data_sub_temp)[i+8]
data9melt$variable <- colnames(data_sub_temp)[i+9]
data10melt$variable <- colnames(data_sub_temp)[i+10]
data11melt$variable <- colnames(data_sub_temp)[i+11]
data12melt$variable <- colnames(data_sub_temp)[i+12]
data13melt$variable <- colnames(data_sub_temp)[i+13]

colnames(data1melt) <- c("vaccine","clade","value")
colnames(data2melt) <- c("vaccine","clade","value")
colnames(data3melt) <- c("vaccine","clade","value")
colnames(data4melt) <- c("vaccine","clade","value")
colnames(data5melt) <- c("vaccine","clade","value")
colnames(data6melt) <- c("vaccine","clade","value")
colnames(data7melt) <- c("vaccine","clade","value")
colnames(data8melt) <- c("vaccine","clade","value")
colnames(data9melt) <- c("vaccine","clade","value")
colnames(data10melt) <- c("vaccine","clade","value")
colnames(data11melt) <- c("vaccine","clade","value")
colnames(data12melt) <- c("vaccine","clade","value")
colnames(data13melt) <- c("vaccine","clade","value")

data6melt$value <- as.numeric(data6melt$value) # I don't know why this is necessary, but it is.

data1melt[which(data1melt$vaccine=="no"),]$value
log(data1melt[which(data1melt$vaccine=="no"),]$value/10, 2)

data_sub_t_melt_vaccine <- rbind(data1melt, data2melt, data3melt, data4melt, data5melt, data6melt, data7melt,
                                 data8melt, data9melt, data10melt, data11melt, data12melt, data13melt)
data_sub_t_melt_vaccine$value <- as.numeric(data_sub_t_melt_vaccine$value)
data_sub_t_melt_vaccine$vaccine <- as.factor(data_sub_t_melt_vaccine$vaccine)
data_sub_t_melt_vaccine$clade <- factor(data_sub_t_melt_vaccine$clade, levels = colnames(data[13:25]))

gmlabels <- c(
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[1] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[1] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[2] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[2] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[3] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[3] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[4] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[4] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[5] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[5] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[6] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[6] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[7] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[7] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[8] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[8] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[9] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[9] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[10] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[10] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[11] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[11] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[12] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[12] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[13] & data_sub_t_melt_vaccine$vaccine == "no"),]$value, na.rm=T), digits=0),
  round(mean(data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$clade == colnames(data[13:25])[13] & data_sub_t_melt_vaccine$vaccine == "yes"),]$value, na.rm=T), digits=0)
);gmlabels

data_sub_t_melt_vaccine$value <- log(data_sub_t_melt_vaccine$value/10, 2)
data_sub_t_melt_vaccine$value <- ifelse(data_sub_t_melt_vaccine$value == -Inf, NA, data_sub_t_melt_vaccine$value)

plot_vaccine <- ggplot(data_sub_t_melt_vaccine, aes(x=vaccine, y=value)) + 
  facet_grid(rows = vars(clade)) +
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Vaccine") +
  scale_y_continuous(breaks=seq(0,10,2)); plot_vaccine

pdf("Figures/Sheet1/s1_vaccine_value.pdf", width=3.5, height=14)
plot_vaccine
dev.off()

plot_vaccinec <- ggplot(data_sub_t_melt_vaccine, aes(x=vaccine, y=value)) + 
  facet_manual(vars(clade), design = design_threerows) +
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Vaccine") +
  geom_jitter(color="black", size=0.3, alpha=0.5) +
  stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1) +
  scale_y_continuous(breaks=seq(0,100,2), limits=c(0,10.5)); plot_vaccinec

pdf("Figures/Sheet1/s1_vaccine_value_cols.pdf", width=12, height=6)
plot_vaccinec
dev.off()

plot_vaccinenc <- ggplot(data_sub_t_melt_vaccine, aes(x=vaccine, y=value)) + 
  geom_boxplot() +
  labs(y = "Log2 (GMT/10)", x = "Vaccine") +
  scale_y_continuous(breaks=seq(0,10,2)); plot_vaccinenc

pdf("Figures/Sheet1/s1_vaccine_value_noclade.pdf", width=5, height=5)
plot_vaccinenc
dev.off()

data_sub_t_melt_vaccine_nona <- data_sub_t_melt_vaccine[which(data_sub_t_melt_vaccine$value != -Inf),]
aov_vaccine <- aov(value ~ clade + vaccine, data = data_sub_t_melt_vaccine_nona)
sum.aov_vaccine <- summary(aov_vaccine);sum.aov_vaccine

data_sub_t_melt_vaccine_no40 <- data_sub_t_melt_vaccine_nona[which(data_sub_t_melt_vaccine_nona$vaccine != "1940"),]
aov_vaccine_no40 <- aov(value ~ clade + vaccine, data = data_sub_t_melt_vaccine_no40)
sum.aov_vaccine_no40 <- summary(aov_vaccine_no40);sum.aov_vaccine_no40

####################
### Testing normality of distribution:
plot(density(data_sub_t_melt_vaccine_nona$value, na.rm=TRUE))
qqnorm(data_sub_t_melt_vaccine_nona$value)
qqline(data_sub_t_melt_vaccine_nona$value, col="red",
       distribution=qnorm) #normal distribution, quantile function
qqPlot(data_sub_t_melt_vaccine_nona$value, distribution="norm")

pdf("Figures/Sheet1/s1_qqplot_ci.pdf", width=5, height=5)
qqPlot(data_sub_t_melt_vaccine_nona$value, distribution="norm")
dev.off()

####################
### Testing difference between groups, ignoring clades:
aov_decade_noclade <- aov(value ~ decade, data = data_sub_t_melt_decade_nona)
sum.aov_decade_noclade <- as.data.frame(unclass(summary(aov_decade_noclade)))

aov_gender_noclade <- aov(value ~ gender, data = data_sub_t_melt_gender_nona)
sum.aov_gender_noclade <- as.data.frame(unclass(summary(aov_gender_noclade)))

aov_vaccine_noclade <- aov(value ~ vaccine, data = data_sub_t_melt_vaccine_nona)
sum.aov_vaccine_noclade <- as.data.frame(unclass(summary(aov_vaccine_noclade)))

#testing for decade differences without the 40s included:
aov_decade_noclade_no40 <- aov(value ~ decade, data = data_sub_t_melt_decade_no40)
sum.aov_decade_noclade_no40 <- as.data.frame(unclass(summary(aov_decade_noclade_no40)))

#visually inspect the differences between groups to check your expectations
plot_decadenc
plot_gendernc
plot_vaccinenc

#adjust the p-values and see if any are significant
p.adjust(c(sum.aov_decade_noclade$Pr..F.[1],
           sum.aov_gender_noclade$Pr..F.[1],
           sum.aov_vaccine_noclade$Pr..F.[1]), method="bonferroni")

#these were run in the code previously. These one-way ANOVAs include clade.
sum.aov_decade
sum.aov_gender
sum.aov_vaccine

#perform Tukey's "Honest Significant Difference" test, based on studentized t-test
TukeyHSD(aov_decade)$decade;plot((TukeyHSD(aov_decade)$decade)[,4])
TukeyHSD(aov_gender)$gender;plot((TukeyHSD(aov_gender)$gender)[,4])
TukeyHSD(aov_vaccine)$vaccine;plot((TukeyHSD(aov_vaccine)$vaccine)[,4])

###

data$cohort <- "script1"
data_sub_t_melt_vaccine$cohort <- "script1"
data_sub_t_melt_decade$cohort <- "script1"
data_sub_t_melt_gender$cohort <- "script1"

write.csv(data, file="Output/script1_data.csv", row.names = F)
write.csv(data_sub_t_melt_vaccine, file="Output/script1_data_sub_t_melt_vaccine.csv", row.names = F)
write.csv(data_sub_t_melt_decade, file="Output/script1_data_sub_t_melt_decade.csv", row.names = F)
write.csv(data_sub_t_melt_gender, file="Output/script1_data_sub_t_melt_gender.csv", row.names = F)






