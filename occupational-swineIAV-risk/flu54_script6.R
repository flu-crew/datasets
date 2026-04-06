
################
### This, script6, reads all the data from scripts 1-5 to make comparisons between cohorts and generates figures comparing them.
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
library("ggpubr")
library("multcompView")

##########################

data1 <- as.data.frame(read.csv("Output/script1_data.csv"));head(data1)
data2 <- as.data.frame(read.csv("Output/script2_data.csv"));head(data2)
data3 <- as.data.frame(read.csv("Output/script3_data.csv"));head(data3)
data4 <- as.data.frame(read.csv("Output/script4_data.csv"));head(data4)
data5 <- as.data.frame(read.csv("Output/script5_data.csv"));head(data5)

colnames(data1)
colnames(data2)
colnames(data3)
colnames(data4)
colnames(data5)

data1$cohort <- "Vet"
data2$cohort <- "Employee"
data3$cohort <- "Farm"
data4$cohort <- "Philadelphia"
data5$cohort <- "Hong Kong"

data1_sort <- data1[,c("cohort","Number","Study_ID",
         "Primary_state","Age","Decade","Gender","Influenza_vaccine_.12_months",
         "HI19.HuVac","IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3_c1","MN21.1A.3.3.3_c3","BJ95.1B.2","IA21.1B.2.1",
         "IA20.1B.2.2.1","IA20.1B.2.2.2","HK19.HuVac","IA21.1990.4.a","KS20.2010.1","IN21.2010.2")]
data2_sort <- data2[,c("cohort","Number","De_identified_code",
         "Primary_state","Age","Decade","Gender","Influenza_vaccine_.12_months",
         "HI19.HuVac","IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3_c1","MN21.1A.3.3.3_c3","BJ95.1B.2","IA21.1B.2.1",
         "IA20.1B.2.2.1","IA20.1B.2.2.2","HK19.HuVac","IA21.1990.4.a","KS20.2010.1","IN21.2010.2")]
data3_sort <- data3[,c("cohort","Number","De_identified_code",
         "Primary_state","Age","Decade","Gender","Influenza_vaccine_.12_months",
         "HI19.HuVac","IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3_c1","MN21.1A.3.3.3_c3","BJ95.1B.2","IA21.1B.2.1",
         "IA20.1B.2.2.1","IA20.1B.2.2.2","HK19.HuVac","IA21.1990.4.a","KS20.2010.1","IN21.2010.2")]
data4_sort <- data4[,c("cohort","Number","De_identified_code",
         "Primary_state","Age","Decade","Gender","Flu_Vax_2021_2022",
         "HI19.HuVac","IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3_c1","MN21.1A.3.3.3_c3","BJ95.1B.2","IA21.1B.2.1",
         "IA20.1B.2.2.1","IA20.1B.2.2.2","HK19.HuVac","IA21.1990.4.a","KS20.2010.1","IN21.2010.2")]
data5_sort <- data5[,c("cohort","Number","De_identified_code",
                       "Primary_Location","Age","Decade","Gender","Influenza_vaccine_.12_months",
                       "HI19.HuVac","IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3_c1","MN21.1A.3.3.3_c3","BJ95.1B.2","IA21.1B.2.1",
                       "IA20.1B.2.2.1","IA20.1B.2.2.2","HK19.HuVac","IA21.1990.4.a","KS20.2010.1","IN21.2010.2")]

colnames(data1_sort) <- colnames(data2_sort) <- colnames(data3_sort) <- colnames(data4_sort) <- colnames(data5_sort)<- c(
  "cohort","Number","De_identified_code",
  "Primary_state","Age","Decade","Gender","Influenza_vaccine_.12_months",
  "HI19.HuVac","IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3_c1","MN21.1A.3.3.3_c3","BJ95.1B.2","IA21.1B.2.1",
  "IA20.1B.2.2.1","IA20.1B.2.2.2","HK19.HuVac","IA21.1990.4.a","KS20.2010.1","IN21.2010.2" 
)

# data <- rbind(data1_sort, data2_sort, data3_sort, data4_sort)
data <- rbind(data1_sort, data3_sort, data4_sort, data5_sort)

######

colnames(data)

# change all values of 0 or <10 to 10.
data[data == "<10"] <- "10" # pseudocount prevents -inf values in the log2fc
data[data == "0"] <- "10" # pseudocount prevents -inf values in the log2fc

data$HI19.HuVac <- as.numeric(data$HI19.HuVac)
data$IL20.1A.1.1.3 <- as.numeric(data$IL20.1A.1.1.3)
data$IA20.1A.3.3.2 <- as.numeric(data$IA20.1A.3.3.2)
data$IN22.1A.3.3.3_c1 <- as.numeric(data$IN22.1A.3.3.3_c1)
data$MN21.1A.3.3.3_c3 <- as.numeric(data$MN21.1A.3.3.3_c3)
data$BJ95.1B.2 <- as.numeric(data$BJ95.1B.2)
data$IA21.1B.2.1 <- as.numeric(data$IA21.1B.2.1)
data$IA20.1B.2.2.1 <- as.numeric(data$IA20.1B.2.2.1)
data$IA20.1B.2.2.2 <- as.numeric(data$IA20.1B.2.2.2)
data$HK19.HuVac <- as.numeric(data$HK19.HuVac)
data$IA21.1990.4.a <- as.numeric(data$IA21.1990.4.a)
data$KS20.2010.1 <- as.numeric(data$KS20.2010.1)
data$IN21.2010.2 <- as.numeric(data$IN21.2010.2)

data$IL20.1A.1.1.3_lfc <- log2(data$IL20.1A.1.1.3)-log2(data$HI19.HuVac)
data$IA20.1A.3.3.2_lfc <- log2(data$IA20.1A.3.3.2)-log2(data$HI19.HuVac)
data$IN22.1A.3.3.3_c1_lfc <- log2(data$IN22.1A.3.3.3_c1)-log2(data$HI19.HuVac)
data$MN21.1A.3.3.3_c3_lfc <- log2(data$MN21.1A.3.3.3_c3)-log2(data$HI19.HuVac)

data$IA21.1B.2.1_lfc <- log2(data$IA21.1B.2.1)-log2(data$BJ95.1B.2)
data$IA20.1B.2.2.1_lfc <- log2(data$IA20.1B.2.2.1)-log2(data$BJ95.1B.2)
data$IA20.1B.2.2.2_lfc <- log2(data$IA20.1B.2.2.2)-log2(data$BJ95.1B.2)

data$IA21.1990.4.a_lfc <- log2(data$IA21.1990.4.a)-log2(data$HK19.HuVac)
data$KS20.2010.1_lfc <- log2(data$KS20.2010.1)-log2(data$HK19.HuVac)
data$IN21.2010.2_lfc <- log2(data$IN21.2010.2)-log2(data$HK19.HuVac)

####################

viruses <- c("IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3_c1","MN21.1A.3.3.3_c3",
             "IA21.1B.2.1","IA20.1B.2.2.1","IA20.1B.2.2.2",
             "IA21.1990.4.a","KS20.2010.1","IN21.2010.2",
             "HI19.HuVac","BJ95.1B.2","HK19.HuVac")
viruses_lfc <- c("IL20.1A.1.1.3_lfc","IA20.1A.3.3.2_lfc","IN22.1A.3.3.3_c1_lfc","MN21.1A.3.3.3_c3_lfc",
                 "IA21.1B.2.1_lfc","IA20.1B.2.2.1_lfc","IA20.1B.2.2.2_lfc",
                 "IA21.1990.4.a_lfc","KS20.2010.1_lfc","IN21.2010.2_lfc")

dfstore <- data.frame()
dfstore_lfc <- data.frame()
plot_list1 = list()
plot_list2 = list()

####################

i <- NULL
for(i in 1:length(viruses)){
  # i <- 1
  virus <- viruses[i]
  data_sub <- data[,c("cohort", paste(virus))]
  colnames(data_sub) <- c("cohort","virus")
  aovout <- aov(virus ~ cohort, data = data_sub)
  sum.aovout <- as.data.frame(unclass(summary(aovout)))
  dfstore[i,1] <- virus
  dfstore[i,2] <- sum.aovout$Pr..F.[1]
  dfstore[i,3:8] <- as.data.frame(TukeyHSD(aovout)$cohort)[,4]
  
  gmlabels <- c(
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[2]),]$virus, na.rm=T), digits=0),
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[4]),]$virus, na.rm=T), digits=0),
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[3]),]$virus, na.rm=T), digits=0),
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[1]),]$virus, na.rm=T), digits=0)
  );gmlabels
  
  plot1 <- ggplot(data_sub, aes(x=virus, y=cohort)) +
    geom_boxplot(lwd=1.5) +
    labs(y = "Cohort", x = paste(virus, " Titer", sep="")) +
    geom_jitter(size=0.9, alpha=0.5) +
    coord_flip() #+
    #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
  plot_list1[[i]] = plot1
  
  virus_fixed <- str_replace_all(virus, "[.]", "_")
  pdf(paste("Figures/Cohort_Comparison/", virus_fixed, ".pdf", sep=""), width=5, height=6)
  print(ggplot(data_sub, aes(x=virus, y=cohort)) + 
          geom_boxplot(lwd=1.5) +
          labs(y = "Cohort", x = paste(virus, " Titer", sep="")) +
          geom_jitter(size=0.9, alpha=0.5) +
          theme(text = element_text(size = 20)) +
          coord_flip() #+
           #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
           )
  dev.off()
  
  png(paste("Figures/Cohort_Comparison/", virus_fixed, ".png", sep=""), width=600, height=600)
  print(ggplot(data_sub, aes(x=virus, y=cohort)) + 
          geom_boxplot(lwd=1.5) +
          labs(y = "Cohort", x = paste(virus, " Titer", sep="")) +
          geom_jitter(size=0.9, alpha=0.5) +
          theme(text = element_text(size = 20)) +
          coord_flip() #+
           #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
        )
  dev.off()
  
}

i <- NULL
for(i in 1:length(viruses_lfc)){
  # i <- 7
  virus_lfc <- viruses_lfc[i]
  
  reference <- "not found"
  reference <- ifelse(i %in% c(1:4), "HI19.HuVac", reference)
  reference <- ifelse(i %in% c(5:7), "BJ95.1B.2", reference)
  reference <- ifelse(i %in% c(8:10), "HK19.HuVac", reference)
  
  data_sub <- data[,c("cohort", paste(virus_lfc))]
  colnames(data_sub) <- c("cohort","virus_lfc")
  data_sub$cohort <- factor(data_sub$cohort , levels=c("Hong Kong", "Philadelphia", "Farm", "Vet"))
  data_sub$cohort_group <- ifelse(data_sub$cohort == "Vet" | data_sub$cohort == "Farm", "Farmarm/Vet", "HK/Phily")
  
  aovout <- aov(virus_lfc ~ cohort, data = data_sub)
  sum.aovout <- as.data.frame(unclass(summary(aovout)))
  dfstore_lfc[i,1] <- virus_lfc
  dfstore_lfc[i,2] <- sum.aovout$Pr..F.[1]
  dfstore_lfc[i,3:8] <- as.data.frame(TukeyHSD(aovout)$cohort)[,1]
  
  aovout2 <- aov(virus_lfc ~ cohort_group, data = data_sub)
  sum.aovout2 <- as.data.frame(unclass(summary(aovout2)))
  dfstore_lfc[i,9] <- sum.aovout2$Pr..F.[1]
  dfstore_lfc[i,10] <- as.data.frame(TukeyHSD(aovout2)$cohort_group)[,1]
  
  tukey <- TukeyHSD(aovout)
  cld <- multcompLetters4(aovout, tukey)
  cld <- as.data.frame.list(cld$cohort)
  Tk <- group_by(data_sub, cohort) %>%
    summarise(mean=mean(virus_lfc), quant = quantile(virus_lfc, probs = 0.75)) %>%
    arrange(desc(mean))
  Tk$cld <- cld$Letters

  gmlabels <- c(
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[2]),]$virus, na.rm=T), digits=0),
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[4]),]$virus, na.rm=T), digits=0),
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[3]),]$virus, na.rm=T), digits=0),
    round(mean(data_sub[which(data_sub$cohort == unique(data_sub$cohort)[1]),]$virus, na.rm=T), digits=0)
  );gmlabels
  
  plot1 <- ggplot(data_sub, aes(x=virus_lfc, y=cohort, color=cohort)) +
    ggtitle(paste(substr(virus_lfc,1,nchar(virus_lfc)-4))) +
    geom_boxplot(lwd=1.5) +
    # scale_color_brewer(palette="Paired") +
    scale_color_manual(values=c("#8F9FC5", "#BBC5DC", "#7A629F", "#AFA0C5")) +
    labs(y = "Cohort", x = paste("Log2FoldChange from ", paste(reference), sep="")) +
    geom_jitter(size=0.9, alpha=0.5) +
    coord_flip() +
    scale_x_continuous(limits=c(-13,8), breaks=seq(-12,8,2), expand = c(0,0)) +
    #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1) +
    theme(axis.title.x=element_blank(), legend.position = "none", text = element_text(size = 23)) +
    geom_vline(xintercept=0, linetype="dashed", color = "black") +
    geom_text(data = Tk, aes(x = quant, y = cohort, label = cld), size = 6, vjust=-1, hjust =-1);plot1
  plot_list2[[i]] = plot1
  
  virus_fixed <- str_replace_all(virus_lfc, "[.]", "_")
  pdf(paste("Figures/Cohort_Comparison/", virus_fixed, ".pdf", sep=""), width=5, height=5)
  print(ggplot(data_sub, aes(x=virus_lfc, y=cohort, color=cohort)) + 
          ggtitle(paste(substr(virus_lfc,1,nchar(virus_lfc)-4))) +
          geom_boxplot(lwd=1.5) +
          # scale_color_brewer(palette="Paired") +
          scale_color_manual(values=c("#8F9FC5", "#BBC5DC", "#7A629F", "#AFA0C5")) +
          labs(y = "Cohort", x = paste("Log2FoldChange from ", paste(reference), sep="")) +
          geom_jitter(size=0.9, alpha=0.5) +
          coord_flip() +
          #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
          theme(axis.title.x=element_blank(), legend.position = "none", text = element_text(size = 18)) +
          scale_x_continuous(limits=c(-13,8), breaks=seq(-12,8,2), expand = c(0,0))
          )
  dev.off()
  
  png(paste("Figures/Cohort_Comparison/", virus_fixed, ".png", sep=""), width=500, height=500)
  print(ggplot(data_sub, aes(x=virus_lfc, y=cohort, color=cohort)) + 
          ggtitle(paste(substr(virus_lfc,1,nchar(virus_lfc)-4))) +
          geom_boxplot(lwd=1.5) +
          # scale_color_brewer(palette="Paired") +
          scale_color_manual(values=c("#8F9FC5", "#BBC5DC", "#7A629F", "#AFA0C5")) +
          labs(y = "Cohort", x = paste("Log2FoldChange from ", paste(reference), sep="")) +
          geom_jitter(size=0.9, alpha=0.5) +
          coord_flip() +
          #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
          theme(axis.title.x=element_blank(), legend.position = "none", text = element_text(size = 18)) +
          scale_x_continuous(limits=c(-13,8), breaks=seq(-12,8,2), expand = c(0,0))
          )
  dev.off()
  
}

colnames(dfstore_lfc) <- c("viruses","pval", row.names(as.data.frame(TukeyHSD(aovout)$cohort)),
                           "pval_group", row.names(as.data.frame(TukeyHSD(aovout2)$cohort_group)))
dfstore_lfc$pval_adj <- p.adjust(dfstore_lfc$pval, method="bonferroni")
dfstore_lfc$pval_group_adj <- p.adjust(dfstore_lfc$pval_group, method="bonferroni")
dfstore_lfc <- dfstore_lfc[,c(1:2,11,3:9,12,10)] #reorder data frame into more logical column order
write.csv(dfstore_lfc, "Output/script5_cohort_comparison_lfc.csv", row.names=F)

#########

plot_list1
plot_list2
viruses_lfc

png("Figures/Cohort_Comparison/combined.png", width=2000, height=1300)
p <- grid.arrange(
  grobs = plot_list2,
  layout_matrix = rbind(c(1, 2, 3, 4),
                        c(5, 6, 7, NA),
                        c(8, 9, 10, NA))
)
dev.off()
pdf("Figures/Cohort_Comparison/combined.pdf", width=20, height=13)
p <- grid.arrange(
  grobs = plot_list2,
  layout_matrix = rbind(c(1, 2, 3, 4),
                        c(5, 6, 7, NA),
                        c(8, 9, 10, NA))
)
dev.off()

png("Figures/Cohort_Comparison/combined_1a.png", width=2000, height=600)
p <- grid.arrange(
  grobs = plot_list2[1:4],
  layout_matrix = rbind(c(1, 2, 3, 4))
)
dev.off()
pdf("Figures/Cohort_Comparison/combined_1a.pdf", width=20, height=6)
p <- grid.arrange(
  grobs = plot_list2[1:4],
  layout_matrix = rbind(c(1, 2, 3, 4))
)
dev.off()

png("Figures/Cohort_Comparison/combined_1b.png", width=1550, height=600)
p <- grid.arrange(
  grobs = plot_list2[5:7],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()
pdf("Figures/Cohort_Comparison/combined_1b.pdf", width=15.5, height=6)
p <- grid.arrange(
  grobs = plot_list2[5:7],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()

png("Figures/Cohort_Comparison/combined_h3.png", width=1550, height=600)
p <- grid.arrange(
  grobs = plot_list2[8:10],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()
pdf("Figures/Cohort_Comparison/combined_h3.pdf", width=15.5, height=6)
p <- grid.arrange(
  grobs = plot_list2[8:10],
  layout_matrix = rbind(c(1, 2, 3))
)
dev.off()

############################################################################
############################################################################
############################################################################
### Running a boxplot contrasting decades and agebin2s, ignoring cohorts
###

data$Agebin <- ifelse(data$Age > (2023-1976), "Pre-1977","Post-1976")
data$Agebin2 <- "1968-1976"
data$Agebin2 <- ifelse(data$Age < (2023-1976), "Post-1976", data$Agebin2)
data$Agebin2 <- ifelse(data$Age > (2023-1968), "Pre-1968", data$Agebin2)

design_threerows <- "
  ABCDE
  FGHI#
  JKLM#
"
design_threerows_lfc <- "
  ABCD
  EFG#
  HIJ#
"

##############
### Decade ###
data_Decade <- data[,c("Decade",colnames(data)[9:21])]
data_Decade$Decade <- as.character(data_Decade$Decade)
data_Decade_melt <- reshape2::melt(data_Decade, id.vars=c("Decade"))
data_Decade_melt$value_log2 <- log(data_Decade_melt$value/10, 2)
data_Decade_melt$value_log2 <- ifelse(data_Decade_melt$value_log2 < 0, 0, data_Decade_melt$value_log2)

data_Decade_lfc <- data[,c("Decade",colnames(data)[22:31])]
data_Decade_lfc$Decade <- as.character(data_Decade_lfc$Decade)
data_Decade_lfc_melt <- reshape2::melt(data_Decade_lfc, id.vars=c("Decade"))

plot_gmt <- ggplot(data_Decade_melt, aes(x=Decade, y=value_log2)) +
  geom_boxplot() +
  xlab("Decade") +
  ylab("GMT") +
  geom_jitter(size=0.4, alpha=0.5) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_gmt

plot_lfc <- ggplot(data_Decade_lfc_melt, aes(x=Decade, y=value)) +
  geom_boxplot() +
  xlab("Decade") +
  ylab("Log2FoldChange") +
  geom_jitter(size=0.4, alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_lfc

plot_gmt_facet <- ggplot(data_Decade_melt, aes(x=Decade, y=value_log2)) +
  geom_boxplot() +
  xlab("Decade") +
  ylab("GMT") +
  geom_jitter(size=0.4, alpha=0.5) +
  facet_manual(vars(variable), design = design_threerows) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_gmt_facet

plot_lfc_facet <- ggplot(data_Decade_lfc_melt, aes(x=Decade, y=value)) +
  geom_boxplot() +
  xlab("Decade") +
  ylab("Log2FoldChange") +
  geom_jitter(size=0.4, alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  facet_manual(vars(variable), design = design_threerows_lfc) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_lfc_facet

png("Figures/Decade_Comparison/script5_boxplot_Decade.png", width=1200, height=650)
ggarrange(plot_gmt, plot_lfc)
dev.off()

png("Figures/Decade_Comparison/script5_boxplot_Decade_gmt_facet.png", width=1700, height=950)
plot_gmt_facet
dev.off()

png("Figures/Decade_Comparison/script5_boxplot_Decade_lfc_facet.png", width=1700, height=950)
plot_lfc_facet
dev.off()

##############
### Agebin ###
data_Agebin <- data[,c("Agebin",colnames(data)[9:21])]
data_Agebin$Agebin <- as.character(data_Agebin$Agebin)
data_Agebin_melt <- reshape2::melt(data_Agebin, id.vars=c("Agebin"))
data_Agebin_melt$Agebin <- factor(data_Agebin_melt$Agebin, levels=c("Pre-1977","Post-1976"))
data_Agebin_melt$value_log2 <- log(data_Agebin_melt$value/10, 2)
data_Agebin_melt$value_log2 <- ifelse(data_Agebin_melt$value_log2 < 0, 0, data_Agebin_melt$value_log2)

data_Agebin_lfc <- data[,c("Agebin",colnames(data)[22:31])]
data_Agebin_lfc$Agebin <- as.character(data_Agebin_lfc$Agebin)
data_Agebin_lfc_melt <- reshape2::melt(data_Agebin_lfc, id.vars=c("Agebin"))

plot_gmt <- ggplot(data_Agebin_melt, aes(x=Agebin, y=value_log2)) +
  geom_boxplot() +
  xlab("Agebin") +
  ylab("GMT") +
  geom_jitter(size=0.4, alpha=0.5) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_gmt

plot_lfc <- ggplot(data_Agebin_lfc_melt, aes(x=Agebin, y=value)) +
  geom_boxplot() +
  xlab("Agebin") +
  ylab("Log2FoldChange") +
  geom_jitter(size=0.4, alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_lfc

plot_gmt_facet <- ggplot(data_Agebin_melt, aes(x=Agebin, y=value_log2)) +
  geom_boxplot() +
  xlab("Agebin") +
  ylab("GMT") +
  geom_jitter(size=0.4, alpha=0.5) +
  facet_manual(vars(variable), design = design_threerows) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_gmt_facet

plot_lfc_facet <- ggplot(data_Agebin_lfc_melt, aes(x=Agebin, y=value)) +
  geom_boxplot() +
  xlab("Agebin") +
  ylab("Log2FoldChange") +
  geom_jitter(size=0.4, alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  facet_manual(vars(variable), design = design_threerows_lfc) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_lfc_facet

png("Figures/Decade_Comparison/script5_boxplot_Agebin.png", width=1200, height=650)
ggarrange(plot_gmt, plot_lfc)
dev.off()

png("Figures/Decade_Comparison/script5_boxplot_Agebin_gmt_facet.png", width=1200, height=950)
plot_gmt_facet
dev.off()

png("Figures/Decade_Comparison/script5_boxplot_Agebin_lfc_facet.png", width=1200, height=950)
plot_lfc_facet
dev.off()

##############
### Agebin2 ###
data_Agebin2 <- data[,c("Agebin2",colnames(data)[9:21])]
data_Agebin2$Agebin2 <- as.character(data_Agebin2$Agebin2)
data_Agebin2_melt <- reshape2::melt(data_Agebin2, id.vars=c("Agebin2"))
data_Agebin2_melt$Agebin2 <- factor(data_Agebin2_melt$Agebin2, levels=c("Pre-1968","1968-1976","Post-1976"))
data_Agebin2_melt$value_log2 <- log(data_Agebin2_melt$value/10, 2)
data_Agebin2_melt$value_log2 <- ifelse(data_Agebin2_melt$value_log2 < 0, 0, data_Agebin2_melt$value_log2)

data_Agebin2_lfc <- data[,c("Agebin2",colnames(data)[22:31])]
data_Agebin2_lfc$Agebin2 <- as.character(data_Agebin2_lfc$Agebin2)
data_Agebin2_lfc_melt <- reshape2::melt(data_Agebin2_lfc, id.vars=c("Agebin2"))

plot_gmt <- ggplot(data_Agebin2_melt, aes(x=Agebin2, y=value_log2)) +
  geom_boxplot() +
  xlab("Agebin2") +
  ylab("GMT") +
  geom_jitter(size=0.4, alpha=0.5) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_gmt

plot_lfc <- ggplot(data_Agebin2_lfc_melt, aes(x=Agebin2, y=value)) +
  geom_boxplot() +
  xlab("Agebin2") +
  ylab("Log2FoldChange") +
  geom_jitter(size=0.4, alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_lfc

plot_gmt_facet <- ggplot(data_Agebin2_melt, aes(x=Agebin2, y=value_log2)) +
  geom_boxplot() +
  xlab("Agebin2") +
  ylab("GMT") +
  geom_jitter(size=0.4, alpha=0.5) +
  facet_manual(vars(variable), design = design_threerows) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_gmt_facet

plot_lfc_facet <- ggplot(data_Agebin2_lfc_melt, aes(x=Agebin2, y=value)) +
  geom_boxplot() +
  xlab("Agebin2") +
  ylab("Log2FoldChange") +
  geom_jitter(size=0.4, alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  facet_manual(vars(variable), design = design_threerows_lfc) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_lfc_facet

png("Figures/Decade_Comparison/script5_boxplot_Agebin2.png", width=1200, height=650)
ggarrange(plot_gmt, plot_lfc)
dev.off()

png("Figures/Decade_Comparison/script5_boxplot_Agebin2_gmt_facet.png", width=1550, height=950)
plot_gmt_facet
dev.off()

png("Figures/Decade_Comparison/script5_boxplot_Agebin2_lfc_facet.png", width=1550, height=950)
plot_lfc_facet
dev.off()

###################################
### Decade, with Vaccine Status ###
data_Decade <- data[,c("Decade",colnames(data)[9:21],"Influenza_vaccine_.12_months")]
data_Decade$Decade <- as.character(data_Decade$Decade)
data_Decade_melt <- reshape2::melt(data_Decade, id.vars=c("Decade","Influenza_vaccine_.12_months"))
data_Decade_melt$value_log2 <- log(data_Decade_melt$value/10, 2)
data_Decade_melt$value_log2 <- ifelse(data_Decade_melt$value_log2 < 0, 0, data_Decade_melt$value_log2)

data_Decade_melt$Influenza_vaccine_.12_months <- ifelse(data_Decade_melt$Influenza_vaccine_.12_months == "Y" |
                                                          data_Decade_melt$Influenza_vaccine_.12_months == "yes" |
                                                data_Decade_melt$Influenza_vaccine_.12_months == "Yes",
                                              "vaccinated",data_Decade_melt$Influenza_vaccine_.12_months)
data_Decade_melt$Influenza_vaccine_.12_months <- ifelse(data_Decade_melt$Influenza_vaccine_.12_months == "No" |
                                                          data_Decade_melt$Influenza_vaccine_.12_months == "no" |
                                                          data_Decade_melt$Influenza_vaccine_.12_months == "U",
                                              "unvaccinated",data_Decade_melt$Influenza_vaccine_.12_months)
data_Decade_melt$Influenza_vaccine_.12_months <- ifelse(data_Decade_melt$Influenza_vaccine_.12_months == "Do not know" |
                                                          data_Decade_melt$Influenza_vaccine_.12_months == "unknown",
                                              NA, data_Decade_melt$Influenza_vaccine_.12_months)
data_Decade_melt <- data_Decade_melt[!is.na(data_Decade_melt$Influenza_vaccine_.12_months),]
dim(data_Decade_melt);table(data_Decade_melt$Influenza_vaccine_.12_months)

plot_gmt_facet_wvacc <- ggplot(data_Decade_melt, aes(x=Decade, y=value_log2)) +
  geom_boxplot() +
  xlab("Decade") +
  ylab("GMT") +
  geom_jitter(size=0.4, alpha=0.5) +
  facet_grid(variable~Influenza_vaccine_.12_months) +
  theme(axis.text.y = element_text(size = 20),
        text = element_text(size=22),
        panel.grid.minor = element_blank());plot_gmt_facet_wvacc

png("Figures/Decade_Comparison/script5_boxplot_Decade_facet_vaccinated.png", width=750, height=2000)
plot_gmt_facet_wvacc
dev.off()

#####
#Another version, splitting H1 from H3, and grouping boxes by vacc status
data_Decade$Influenza_vaccine_.12_months <- ifelse(data_Decade$Influenza_vaccine_.12_months == "Y" |
                                                     data_Decade$Influenza_vaccine_.12_months == "yes" |
                                                     data_Decade$Influenza_vaccine_.12_months == "Yes",
                                                   "vaccinated",data_Decade$Influenza_vaccine_.12_months)
data_Decade$Influenza_vaccine_.12_months <- ifelse(data_Decade$Influenza_vaccine_.12_months == "No" |
                                                     data_Decade$Influenza_vaccine_.12_months == "no",
                                                   "unvaccinated",data_Decade$Influenza_vaccine_.12_months)
data_Decade$Influenza_vaccine_.12_months <- ifelse(data_Decade$Influenza_vaccine_.12_months == "Do not know" |
                                                     data_Decade$Influenza_vaccine_.12_months == "U" |
                                                     data_Decade$Influenza_vaccine_.12_months == "unknown",
                                                   NA, data_Decade$Influenza_vaccine_.12_months)
data_Decade <- data_Decade[!is.na(data_Decade$Influenza_vaccine_.12_months),]

viruses <- c("IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3-c1","MN21.1A.3.3.3-c3",
             "IA21.1B.2.1","IA20.1B.2.2.1","IA20.1B.2.2.2",
             "IA21.1990.4.a","KS20.2010.1","IN21.2010.2",
             "HI19.HuVac","BJ95.1B.2","HK19.HuVac")
viruses_lfc <- c("IL20.1A.1.1.3_lfc","IA20.1A.3.3.2_lfc","IN22.1A.3.3.3_c1-lfc","MN21.1A.3.3.3_c3-lfc",
                 "IA21.1B.2.1_lfc","IA20.1B.2.2.1_lfc","IA20.1B.2.2.2_lfc",
                 "IA21.1990.4.a_lfc","KS20.2010.1_lfc","IN21.2010.2_lfc")
colnames(data_Decade)[5:6] <- c("IN22.1A.3.3.3-c1","MN21.1A.3.3.3-c3")

colors <- c(rep("black",10),rep("gray",3))

plot_list3 = list()
dfstore3 <- data.frame()

i <- NULL
for(i in 1:length(viruses)){
  # i <- 9
  virus <- viruses[i]
  data_sub <- data_Decade[,c("Decade", "Influenza_vaccine_.12_months", paste(virus))]
  colnames(data_sub) <- c("Decade", "Vaccine_Status", "virus")
  data_sub$virus <- log(data_sub$virus/10, 2)
  data_sub$virus <- ifelse(data_sub$virus < 0, 0, data_sub$virus)
  

  aovout <- aov(virus ~ Decade, data = data_sub)
  sum.aovout <- as.data.frame(unclass(summary(aovout)))
  dfstore3[i,1] <- virus
  dfstore3[i,2] <- sum.aovout$Pr..F.[1]
  dfstore3[i,3:23] <- as.data.frame(TukeyHSD(aovout)$Decade)[,4]
  
  plot1 <- ggplot(data_sub, aes(x=Decade, y=virus)) +
    geom_boxplot(lwd=1.2, color=paste(colors[i]), aes(fill = Vaccine_Status)) +
    labs(x = "Decade", y = paste(virus, " GMT", sep="")) +
    scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,10)) +
    geom_hline(yintercept=2, linetype ="dashed") +
    theme(legend.position = "none", text = element_text(size=20),
          axis.title=element_text(size=24,face="bold")) +
    geom_jitter(size=0.9, alpha=0.5);plot1
  #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
  plot_list3[[i]] = plot1
  
  virus_fixed <- str_replace_all(virus, "[.]", "_")
  pdf(paste("Figures/Decade_Comparison/paired_boxplot_", virus_fixed, ".pdf", sep=""), width=5, height=6)
  print(ggplot(data_sub, aes(x=Decade, y=virus)) +
          geom_boxplot(lwd=1.2, color=paste(colors[i]), aes(fill = Vaccine_Status)) +
          labs(x = "Decade", y = paste(virus, " GMT", sep="")) +
          scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,10)) +
          geom_hline(yintercept=2, linetype ="dashed") +
          geom_jitter(size=0.9, alpha=0.5) #+
        #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
  )
  dev.off()
  
  png(paste("Figures/Decade_Comparison/paired_boxplot_", virus_fixed, ".png", sep=""), width=600, height=600)
  print(ggplot(data_sub, aes(x=Decade, y=virus)) +
          geom_boxplot(lwd=1.2, color=paste(colors[i]), aes(fill = Vaccine_Status)) +
          labs(x = "Decade", y = paste(virus, " GMT", sep="")) +
          scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,10)) +
          geom_hline(yintercept=2, linetype ="dashed") +
          geom_jitter(size=0.9, alpha=0.5) #+
        #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
  )
  dev.off()
  
  if(i==1){
    plot_temp <- ggplot(data_sub, aes(x=Decade, y=virus)) +
      geom_boxplot(lwd=1.2, color=paste(colors[i]), aes(fill = Vaccine_Status)) +
      labs(x = "Decade", y = paste(virus, " GMT", sep="")) +
      scale_fill_discrete(name = "Vaccination Status") +
      theme(text = element_text(size=30))
    legend <- get_legend(plot_temp)
    legend_plot <- as_ggplot(legend)
    rm(plot_temp, legend)
  }
}

plot_list3[[14]] <- legend_plot

png("Figures/Decade_Comparison/decade_combined_with_vaccs_hires.png", width=8900, height=5750, res=300)
p <- grid.arrange(
  grobs = plot_list3,
  layout_matrix = rbind(c(11, 1, 2, 3, 4),
                        c(12, 5, 6, 7, 14),
                        c(13, 8, 9, 10, NA))
)
dev.off()

png("Figures/Decade_Comparison/decade_combined_with_vaccs.png", width=2000, height=1300)
p <- grid.arrange(
  grobs = plot_list3,
  layout_matrix = rbind(c(11, 1, 2, 3, 4),
                        c(12, 5, 6, 7, 14),
                        c(13, 8, 9, 10, NA))
)
dev.off()

pdf(paste("Figures/Cohort_Comparison/", virus_fixed, ".pdf", sep=""), width=5, height=6)
print(ggplot(data_sub, aes(x=virus, y=cohort)) + 
        geom_boxplot(lwd=1.5) +
        labs(y = "Cohort", x = paste(virus, " Titer", sep="")) +
        geom_jitter(size=0.9, alpha=0.5) +
        theme(text = element_text(size = 20)) +
        coord_flip() #+
      #stat_summary(geom = 'text', label = gmlabels, fun.y = max, vjust = -1)
)
dev.off()


png("Figures/Decade_Comparison/decade_combined.png", width=2000, height=1300)
p <- grid.arrange(
  grobs = plot_list3[c(1:10,14)],
  layout_matrix = rbind(c(1, 2, 3, 4),
                        c(5, 6, 7, 14),
                        c(8, 9, 10, NA))
)
dev.off()

#######################################################
### testing vaccination status by gender and cohort ###
data_test <- data[,c("Gender","Influenza_vaccine_.12_months","cohort")]
data_test_melt <- reshape2::melt(data_test, id.vars=c("Gender","Influenza_vaccine_.12_months","cohort"))
data_test_melt$Influenza_vaccine_.12_months <- ifelse(data_test_melt$Influenza_vaccine_.12_months == "Y" |
                                                        data_test_melt$Influenza_vaccine_.12_months == "yes" |
                                                        data_test_melt$Influenza_vaccine_.12_months == "Yes",
                                                      "vaccinated",data_test_melt$Influenza_vaccine_.12_months)
data_test_melt$Influenza_vaccine_.12_months <- ifelse(data_test_melt$Influenza_vaccine_.12_months == "No" |
                                                        data_test_melt$Influenza_vaccine_.12_months == "no",
                                                      "unvaccinated",data_test_melt$Influenza_vaccine_.12_months)
data_test_melt$Influenza_vaccine_.12_months <- ifelse(data_test_melt$Influenza_vaccine_.12_months == "Do not know" |
                                                        data_test_melt$Influenza_vaccine_.12_months == "U" |
                                                        data_test_melt$Influenza_vaccine_.12_months == "unknown",
                                                      "unknown", data_test_melt$Influenza_vaccine_.12_months)
data_test_melt$Gender <- ifelse(data_test_melt$Gender == "F" | data_test_melt$Gender == "Female", "Female", "Male")
# data_test_melt$cohort <- ifelse(data_test_melt$cohort == "Farm" | data_test_melt$cohort == "Vet", "Ag", "Non-Ag")
# data_test_melt <- data_test_melt[!is.na(data_test_melt$Influenza_vaccine_.12_months),]

rate_vacc_by_gender <- table(data_test_melt$Influenza_vaccine_.12_months, data_test_melt$Gender) #similar vacc rates among m and f
rate_vacc_by_gender[3,1]/sum(rate_vacc_by_gender[,1]) # Female vacc rate
rate_vacc_by_gender[3,2]/sum(rate_vacc_by_gender[,2]) # Male vacc rate

rate_vacc_by_cohort <- table(data_test_melt$Influenza_vaccine_.12_months, data_test_melt$cohort) #similar vacc rates among m and f
rate_vacc_by_cohort[3,1]/sum(rate_vacc_by_cohort[,1]) # Farm vacc rate
rate_vacc_by_cohort[3,2]/sum(rate_vacc_by_cohort[,2]) # Hong Kong vacc rate
rate_vacc_by_cohort[3,3]/sum(rate_vacc_by_cohort[,3]) # Philadelphia vacc rate
rate_vacc_by_cohort[3,4]/sum(rate_vacc_by_cohort[,4]) # Vet vacc rate
(rate_vacc_by_cohort[3,1] + rate_vacc_by_cohort[3,4])/
  (sum(sum(rate_vacc_by_cohort[,1]) + sum(sum(rate_vacc_by_cohort[,4])))) # Ag vacc rate
(rate_vacc_by_cohort[3,2] + rate_vacc_by_cohort[3,3])/
  (sum(sum(rate_vacc_by_cohort[,2]) + sum(sum(rate_vacc_by_cohort[,3])))) # Non-ag vacc rate

rm(data_test, data_test_melt, rate_vacc_by_gender, rate_vacc_by_cohort)
############################################




