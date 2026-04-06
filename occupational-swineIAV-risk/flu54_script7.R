
################
### This, script7, replicates the code from script5 but uses a different input file. Scripts 1-5 and their outputs are not used here.
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

############################################################################
###
data_wi <- as.data.frame(readxl::read_xlsx("Final_Swine_virus_HI_data_Sept23.xlsx"));head(data_wi)
colnames(data_wi) <- c("specid","pid","age","sex",
                       paste(colnames(data_wi)[5],data_wi[1,5],sep="_"),
                       paste(colnames(data_wi)[6],data_wi[1,6],sep="_"),
                       paste(colnames(data_wi)[7],data_wi[1,7],sep="_"),
                       paste(colnames(data_wi)[8],data_wi[1,8],sep="_"),
                       paste(colnames(data_wi)[9],data_wi[1,9],sep="_"),
                       paste(colnames(data_wi)[10],data_wi[1,10],sep="_"),
                       paste(colnames(data_wi)[11],data_wi[1,11],sep="_"),
                       paste(colnames(data_wi)[12],data_wi[1,12],sep="_"),
                       paste(colnames(data_wi)[13],data_wi[1,13],sep="_"),
                       paste(colnames(data_wi)[14],data_wi[1,14],sep="_"))
data_wi <- data_wi[-1,]
viruses_fixed <- colnames(data_wi)[5:ncol(data_wi)]
colnames(data_wi) <- gsub("/","_", colnames(data_wi));colnames(data_wi) <- gsub("-","_", colnames(data_wi))
colnames(data_wi) <- gsub(" ","_", colnames(data_wi));colnames(data_wi) <- gsub("_W","W", colnames(data_wi))
colnames(data_wi) <- gsub(".","_", colnames(data_wi), fixed=TRUE, perl=FALSE)
head(data_wi)

data_wi[] <- lapply(data_wi, function(y) gsub(">1280","1280", y)) #allows us to make this count numeric
# change all values of 0 or <10 to 10.
data_wi[data_wi == "0"] <- "10"

data <- data_wi

############################################################################

colnames(data)

data$Italy_133898_3_1C_2_1 <- as.numeric(data$Italy_133898_3_1C_2_1)
data$ltaly_25675_1C_2_2 <- as.numeric(data$ltaly_25675_1C_2_2)
data$ltaly_8100_1C_2_5 <- as.numeric(data$ltaly_8100_1C_2_5)
data$ltaly_49701_6_1C_2_4 <- as.numeric(data$ltaly_49701_6_1C_2_4)
data$ltaly_262599_4_1A_3_3_2 <- as.numeric(data$ltaly_262599_4_1A_3_3_2)
data$ltaly_236825_1C_2_6 <- as.numeric(data$ltaly_236825_1C_2_6)
data$Eng_048998_1C_2_2 <- as.numeric(data$Eng_048998_1C_2_2)
data$Eng_045393_H1N2_1B1_1 <- as.numeric(data$Eng_045393_H1N2_1B1_1)
data$Eng_260038_H1N2_1B1_1 <- as.numeric(data$Eng_260038_H1N2_1B1_1)
data$Wisconsin_588_seasonal <- as.numeric(data$Wisconsin_588_seasonal)

data$Italy_133898_3_1C_2_1_lfc <- log2(data$Italy_133898_3_1C_2_1) - log2(data$Wisconsin_588_seasonal)
data$ltaly_25675_1C_2_2_lfc <- log2(data$ltaly_25675_1C_2_2) - log2(data$Wisconsin_588_seasonal)
data$ltaly_8100_1C_2_5_lfc <- log2(data$ltaly_8100_1C_2_5) - log2(data$Wisconsin_588_seasonal)
data$ltaly_49701_6_1C_2_4_lfc <- log2(data$ltaly_49701_6_1C_2_4) - log2(data$Wisconsin_588_seasonal)
data$ltaly_262599_4_1A_3_3_2_lfc <- log2(data$ltaly_262599_4_1A_3_3_2) - log2(data$Wisconsin_588_seasonal)
data$ltaly_236825_1C_2_6_lfc <- log2(data$ltaly_236825_1C_2_6) - log2(data$Wisconsin_588_seasonal)
data$Eng_048998_1C_2_2_lfc <- log2(data$Eng_048998_1C_2_2) - log2(data$Wisconsin_588_seasonal)
data$Eng_045393_H1N2_1B1_1_lfc <- log2(data$Eng_045393_H1N2_1B1_1) - log2(data$Wisconsin_588_seasonal)
data$Eng_260038_H1N2_1B1_1_lfc <- log2(data$Eng_260038_H1N2_1B1_1) - log2(data$Wisconsin_588_seasonal)

####################

viruses <- colnames(data)[5:13]
viruses_lfc <- colnames(data)[15:23]
data$cohort <- "HI_Sept_2023"
reference <- viruses_fixed[length(viruses_fixed)]

dfstore <- data.frame()
dfstore_lfc <- data.frame()
plot_list1 = list()
plot_list2 = list()

####################

i <- NULL
for(i in 1:length(viruses)){
  # i <- 1
  virus <- viruses[i]
  virus_fixed <- viruses_fixed[i]
  data_sub <- data[,c("cohort", paste(virus))]
  colnames(data_sub) <- c("cohort","virus")
  
  plot1 <- ggplot(data_sub, aes(x=virus, y=cohort)) +
    geom_boxplot(lwd=1.5) +
    labs(y = "Cohort", x = paste(virus_fixed, " Titer", sep="")) +
    geom_jitter(size=1.5, alpha=0.5) +
    coord_flip() #+
  plot_list1[[i]] = plot1
  
  virus_filename <- str_replace_all(virus, "[.]", "_")
  pdf(paste("Figures/Cohort_Comparison_5.5/", virus_filename, ".pdf", sep=""), width=5, height=6)
  print(ggplot(data_sub, aes(x=virus, y=cohort)) + 
          geom_boxplot(lwd=1.5) +
          labs(y = "Cohort", x = paste(virus_fixed, " Titer", sep="")) +
          geom_jitter(size=1.5, alpha=0.5) +
          coord_flip() #+
  )
  dev.off()
  
  png(paste("Figures/Cohort_Comparison_5.5/", virus_filename, ".png", sep=""), width=600, height=600)
  print(ggplot(data_sub, aes(x=virus, y=cohort)) + 
          geom_boxplot(lwd=1.5) +
          labs(y = "Cohort", x = paste(virus_fixed, " Titer", sep="")) +
          geom_jitter(size=1.5, alpha=0.5) +
          coord_flip() #+
  )
  dev.off()
  
}

i <- NULL
for(i in 1:length(viruses_lfc)){
  # i <- 1
  virus_lfc <- viruses_lfc[i]
  virus_fixed <- viruses_fixed[i]
  
  data_sub <- data[,c("cohort", paste(virus_lfc))]
  colnames(data_sub) <- c("cohort","virus_lfc")
  
  plot1 <- ggplot(data_sub, aes(x=virus_lfc, y=cohort, color=cohort)) +
    ggtitle(paste(virus_fixed))+
    geom_boxplot(lwd=1.5) +
    scale_color_manual(values=c("#8F9FC5", "#BBC5DC", "#7A629F", "#AFA0C5")) +
    labs(y = "Cohort", x = paste("Log2FoldChange from ", paste(reference), sep="")) +
    geom_jitter(size=1.5, alpha=0.5) +
    coord_flip() +
    scale_x_continuous(limits=c(-13,8), breaks=seq(-12,8,2), expand = c(0,0)) +
    theme(axis.title.x=element_blank(), legend.position = "none", text = element_text(size = 23)) +
    geom_vline(xintercept=0, linetype="dashed", color = "black")
  plot_list2[[i]] = plot1
  
  virus_filename <- str_replace_all(virus_lfc, "[.]", "_")
  pdf(paste("Figures/Cohort_Comparison_5.5/", virus_filename, ".pdf", sep=""), width=5, height=5)
  print(ggplot(data_sub, aes(x=virus_lfc, y=cohort, color=cohort)) + 
          ggtitle(paste(substr(virus_lfc,1,nchar(virus_lfc)-4))) +
          geom_boxplot(lwd=1.5) +
          scale_color_manual(values=c("#8F9FC5", "#BBC5DC", "#7A629F", "#AFA0C5")) +
          labs(y = "Cohort", x = paste("Log2FoldChange from ", paste(reference), sep="")) +
          geom_jitter(size=1.5, alpha=0.5) +
          coord_flip() +
          theme(axis.title.x=element_blank(), legend.position = "none", text = element_text(size = 18)) +
          scale_x_continuous(limits=c(-13,8), breaks=seq(-12,8,2), expand = c(0,0))
  )
  dev.off()
  
  png(paste("Figures/Cohort_Comparison_5.5/", virus_filename, ".png", sep=""), width=500, height=500)
  print(ggplot(data_sub, aes(x=virus_lfc, y=cohort, color=cohort)) + 
          ggtitle(paste(substr(virus_lfc,1,nchar(virus_lfc)-4))) +
          geom_boxplot(lwd=1.5) +
          scale_color_manual(values=c("#8F9FC5", "#BBC5DC", "#7A629F", "#AFA0C5")) +
          labs(y = "Cohort", x = paste("Log2FoldChange from ", paste(reference), sep="")) +
          geom_jitter(size=1.5, alpha=0.5) +
          coord_flip() +
          theme(axis.title.x=element_blank(), legend.position = "none", text = element_text(size = 18)) +
          scale_x_continuous(limits=c(-13,8), breaks=seq(-12,8,2), expand = c(0,0))
  )
  dev.off()
  
}

#########

plot_list1
plot_list2
viruses_lfc

png("Figures/Cohort_Comparison_5.5/combined.png", width=2000, height=2000)
p <- grid.arrange(
  grobs = plot_list2,
  layout_matrix = rbind(c(5, 8, 9),
                        c(1, 2, 3),
                        c(4, 6, 7))
)
dev.off()
pdf("Figures/Cohort_Comparison_5.5/combined.pdf", width=20, height=22)
p <- grid.arrange(
  grobs = plot_list2,
  layout_matrix = rbind(c(5, 8, 9),
                        c(1, 2, 3),
                        c(4, 6, 7))
)
dev.off()

png("Figures/Cohort_Comparison_5.5/combined_1a.png", width=2000, height=600)
p <- grid.arrange(
  grobs = plot_list2[1:4],
  layout_matrix = rbind(c(1, 2, 3, 4))
)
dev.off()
pdf("Figures/Cohort_Comparison_5.5/combined_1a.pdf", width=20, height=6)
p <- grid.arrange(
  grobs = plot_list2[1:4],
  layout_matrix = rbind(c(1, 2, 3, 4))
)
dev.off()

png("Figures/Cohort_Comparison_5.5/combined_1b.png", width=1550, height=600)
p <- grid.arrange(
  grobs = plot_list2[5:7],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()
pdf("Figures/Cohort_Comparison_5.5/combined_1b.pdf", width=15.5, height=6)
p <- grid.arrange(
  grobs = plot_list2[5:7],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()

png("Figures/Cohort_Comparison_5.5/combined_h3.png", width=1550, height=600)
p <- grid.arrange(
  grobs = plot_list2[8:10],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()
pdf("Figures/Cohort_Comparison_5.5/combined_h3.pdf", width=15.5, height=6)
p <- grid.arrange(
  grobs = plot_list2[8:10],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()






