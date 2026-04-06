
################
### This script is modified from script 8 to try a subset analysis suggested during peer review.
### I will be removing all non-vaccinated data, and compare occupationally-exposed vs non-occupationally-exposed samples, for each strain individually.
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
library("openxlsx")
library("lubridate")
library("epitools")
library("ggforestplot")
library("tidyverse")
library("ggforce")
library("ggpubr")
library("scales")

##########################

data1 <- as.data.frame(read.csv("Output/script1_data.csv"));head(data1)
data2 <- as.data.frame(read.csv("Output/script2_data.csv"));head(data2)
data3 <- as.data.frame(read.csv("Output/script3_data.csv"));head(data3)
data4 <- as.data.frame(read.csv("Output/script4_data.csv"));head(data4)
data5 <- as.data.frame(read.csv("Output/script5_data.csv"));head(data5)

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
  "HI19.HuVac","IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3-c1","MN21.1A.3.3.3-c3",
  "BJ95.1B.2","IA21.1B.2.1","IA20.1B.2.2.1","IA20.1B.2.2.2",
  "HK19.HuVac","IA21.1990.4.a","KS20.2010.1","IN21.2010.2" 
)

data <- rbind(data1_sort, data3_sort, data4_sort, data5_sort)

data$Influenza_vaccine_.12_months <- tolower(data$Influenza_vaccine_.12_months)
data$Influenza_vaccine_.12_months <- ifelse(data$Influenza_vaccine_.12_months == "y", "yes", data$Influenza_vaccine_.12_months)
data$Influenza_vaccine_.12_months <- ifelse(data$Influenza_vaccine_.12_months == "n", "no", data$Influenza_vaccine_.12_months)
data$`Influenza_vaccine_.12_months` <- ifelse(data$Influenza_vaccine_.12_months != "yes" &
                                                data$Influenza_vaccine_.12_months != "no",
                                              NA, data$Influenza_vaccine_.12_months)
table(data$Influenza_vaccine_.12_months)

data$Gender <- ifelse(data$Gender == "Female", "F", data$Gender)
data$Gender <- ifelse(data$Gender == "Male", "M", data$Gender)
table(data$Gender)

data$Agebin <- ifelse(data$Age > (2023-1976), "Pre-1977","Post-1976")
data$Agebin2 <- "1968-1976"
data$Agebin2 <- ifelse(data$Age < (2023-1976), "Post-1976", data$Agebin2)
data$Agebin2 <- ifelse(data$Age > (2023-1968), "Pre-1968", data$Agebin2)

####

# change all values of 0 or <10 to 10.
data[data == "<10"] <- "10" # pseudocount prevents -inf values in the log2fc
data[data == "0"] <- "10" # pseudocount prevents -inf values in the log2fc
data$HI19.HuVac <- as.numeric(data$HI19.HuVac)
data$IL20.1A.1.1.3 <- as.numeric(data$IL20.1A.1.1.3)
data$IA20.1A.3.3.2 <- as.numeric(data$IA20.1A.3.3.2)
data$IN22.1A.3.3.3_c1 <- as.numeric(data$'IN22.1A.3.3.3-c1')
data$MN21.1A.3.3.3_c3 <- as.numeric(data$'MN21.1A.3.3.3-c3')
data$BJ95.1B.2 <- as.numeric(data$BJ95.1B.2)
data$IA21.1B.2.1 <- as.numeric(data$IA21.1B.2.1)
data$IA20.1B.2.2.1 <- as.numeric(data$IA20.1B.2.2.1)
data$IA20.1B.2.2.2 <- as.numeric(data$IA20.1B.2.2.2)
data$HK19.HuVac <- as.numeric(data$HK19.HuVac)
data$IA21.1990.4.a <- as.numeric(data$IA21.1990.4.a)
data$KS20.2010.1 <- as.numeric(data$KS20.2010.1)
data$IN21.2010.2 <- as.numeric(data$IN21.2010.2)

#combine 1As and 1Bs into single mean measures
data$mean1A <- (data$IL20.1A.1.1.3+data$IA20.1A.3.3.2+data$IN22.1A.3.3.3_c1+data$'MN21.1A.3.3.3-c3')/4
data$mean1B <- (data$IA21.1B.2.1+data$IA20.1B.2.2.1+data$IA20.1B.2.2.2)/3
#combine H1s and H3s into single mean measures
data$meanH1 <- (data$IL20.1A.1.1.3+data$IA20.1A.3.3.2+data$IN22.1A.3.3.3_c1+data$'MN21.1A.3.3.3-c3'+
                  data$IA21.1B.2.1+data$IA20.1B.2.2.1+data$IA20.1B.2.2.2)/7
data$meanH3 <- (data$IA21.1990.4.a+data$KS20.2010.1+data$IA20.1B.2.2.1+data$IN21.2010.2)/4

####################

data$HK19.HuVac_titer40 <- ifelse(data$HK19.HuVac >= 40, "yes", "no")
data$HI19.HuVac_titer40 <- ifelse(data$HI19.HuVac >= 40, "yes", "no")
data$IL20.1A.1.1.3_titer40 <- ifelse(data$IL20.1A.1.1.3 >= 40, "yes", "no")
data$IA20.1A.3.3.2_titer40 <- ifelse(data$IA20.1A.3.3.2 >= 40, "yes", "no")
data$IN22.1A.3.3.3_c1_titer40 <- ifelse(data$'IN22.1A.3.3.3-c1' >= 40, "yes", "no")
data$MN21.1A.3.3.3_c3_titer40 <- ifelse(data$'MN21.1A.3.3.3-c3' >= 40, "yes", "no")
data$BJ95.1B.2_titer40 <- ifelse(data$BJ95.1B.2 >= 40, "yes", "no")
data$IA21.1B.2.1_titer40 <- ifelse(data$IA21.1B.2.1 >= 40, "yes", "no")
data$IA20.1B.2.2.1_titer40 <- ifelse(data$IA20.1B.2.2.1 >= 40, "yes", "no")
data$IA20.1B.2.2.2_titer40 <- ifelse(data$IA20.1B.2.2.2 >= 40, "yes", "no")
data$IA21.1990.4.a_titer40 <- ifelse(data$IA21.1990.4.a >= 40, "yes", "no")
data$KS20.2010.1_titer40 <- ifelse(data$KS20.2010.1 >= 40, "yes", "no")
data$IN21.2010.2_titer40 <- ifelse(data$IN21.2010.2 >= 40, "yes", "no")

data$mean1A_titer40 <- ifelse(data$mean1A >= 40, "yes", "no")
data$mean1B_titer40 <- ifelse(data$mean1B >= 40, "yes", "no")
data$meanH1_titer40 <- ifelse(data$meanH1 >= 40, "yes", "no")
data$meanH3_titer40 <- ifelse(data$meanH3 >= 40, "yes", "no")

####################

data_ag <- data[which(data$cohort == "Farm" | data$cohort == "Vet"),];nrow(data_ag)
data_non_ag <- data[which(data$cohort == "Philadelphia" | data$cohort == "Hong Kong"),];nrow(data_non_ag)

rm(data1, data2, data3, data4, data5)
rm(data1_sort, data2_sort, data3_sort, data4_sort, data5_sort)

####################
#Create OR output placeholders to sub in when the data do not permit calculation of actual OR results
dat <- matrix(c(1,1,1,1),2,2,byrow=TRUE)
tapw <- c("Unvaccinated", "Vaccinated")
outc <- c("<40", ">=40")
dimnames(dat) <- list("Contrast" = tapw, "Outcome" = outc);dat
or_out_empty_vacc <- oddsratio(dat, rev="both")

dat <- matrix(c(1,1,1,1),2,2,byrow=TRUE)
tapw <- c("Female", "Male")
outc <- c("<40", ">=40")
dimnames(dat) <- list("Contrast" = tapw, "Outcome" = outc);dat
or_out_empty_gender <- oddsratio(dat, rev="c")

dat <- matrix(c(1,1,1,1),2,2,byrow=TRUE)
tapw <- c("Ag", "Non-Ag")
outc <- c("<40", ">=40")
dimnames(dat) <- list("Contrast" = tapw, "Outcome" = outc);dat
or_out_empty_cohort <- oddsratio(dat, rev="both")

dat <- matrix(c(1,1,1,1,1,1,1,
                1,1,1,1,1,1,1),7,2,byrow=TRUE)
tapw <- c("2000","1990","1980","1970","1960","1950","1940")
outc <- c("<40", ">=40")
dimnames(dat) <- list("Contrast" = tapw, "Outcome" = outc);dat
or_out_empty_decade <- oddsratio(dat, rev="c")

dat <- matrix(c(1,1,1,1),2,2,byrow=TRUE)
tapw <- c("Pre-1977","Post-1976")
outc <- c("<40", ">=40")
dimnames(dat) <- list("Contrast" = tapw, "Outcome" = outc);dat
or_out_empty_agebin <- oddsratio(dat, rev="both")

or_list <- list()
titer_groups <- c("IL20.1A.1.1.3_titer40","IA20.1A.3.3.2_titer40","IN22.1A.3.3.3_c1_titer40","MN21.1A.3.3.3_c3_titer40",
                  "IA21.1B.2.1_titer40","IA20.1B.2.2.1_titer40","IA20.1B.2.2.2_titer40",
                  "IA21.1990.4.a_titer40","KS20.2010.1_titer40","IN21.2010.2_titer40")
variables <- c("cohort","Agebin","Gender","Influenza_vaccine_.12_months")
data_or <- data[c(variables, titer_groups)];head(data_or)
data_or$cohort <- ifelse(data$cohort == "Philadelphia" | data$cohort == "Hong Kong", "non-ag","ag");table(data_or$cohort)

################
### This is the modification, where non-vaccinated samples are removed.
nrow(data_or)
data_or <- data_or[which(data_or$Influenza_vaccine_.12_months == "yes"),]
nrow(data_or)
################
j <- NULL
counter <- 1
variable <- variables[1];variable
for (j in 1:length(titer_groups)){
  titer_group <- titer_groups[j];titer_group
  data_temp <- data_or[c(variable,titer_group)];nrow(data_temp)
  data_temp_colname_storage <- colnames(data_temp)
  colnames(data_temp) <- c("variable","titer_group")
  data_temp <- data_temp[!is.na(data_temp$variable),];nrow(data_temp)
  or_out <- NULL

  class1 <- table(factor(data_temp[which(data_temp$variable==as.character(unique(factor(data_temp$variable))[1])),]$titer_group,
                         levels=c("yes","no")));class1
  class2 <- table(factor(data_temp[which(data_temp$variable==as.character(unique(factor(data_temp$variable))[2])),]$titer_group,
                         levels=c("yes","no")));class2
  
  tapw <- c(as.character(unique(factor(data_temp$variable))[1]), as.character(unique(factor(data_temp$variable))[2]))
  outc <- c(">=40", "<40")
  dat <- matrix(c(class1, class2),2,2,byrow=TRUE)
  dimnames(dat) <- list("Contrast" = tapw, "Outcome" = outc);dat
  if(any(dat == 0)){
    dat <- dat+0.51
  };dat
  or_out_empty_var <- NA
  or_out_empty_var <- ifelse(variable == "cohort", or_out_empty_cohort, or_out_empty_var)
  or_out_empty_var <- ifelse(variable == "Gender", or_out_empty_gender, or_out_empty_var)
  or_out_empty_var <- ifelse(variable == "Influenza_vaccine_.12_months", or_out_empty_vacc, or_out_empty_var)
  or_out_empty_var
  
  tryCatch({
    or_out <- oddsratio(dat, rev="c")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #if the variable is cohort, I want to reverse the row names, so non-ag is the reference.
  if(variable=="cohort"){
    tryCatch({
      or_out <- oddsratio(dat, rev="both")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

  or_list[[counter]] <- or_out
  names(or_list)[[counter]] <- paste0(titer_group, "_", variable)
  counter <- counter+1
}

counter

###########################
### Combine the Results ###

group_mapping <- c("IL20.1A.1.1.3","IA20.1A.3.3.2","IN22.1A.3.3.3-c1","MN21.1A.3.3.3-c3",
                                "IA21.1B.2.1","IA20.1B.2.2.1","IA20.1B.2.2.2",
                                "IA21.1990.4.a","KS20.2010.1","IN21.2010.2")
i <- j <- NULL
results_df <- do.call(rbind, lapply(seq_along(or_list), function(i) {
  x <- or_list[[i]]  # Extract current oddsratio output
  group <- group_mapping[i]  # Assign group based on run number
  data.frame(
    Group = group,
    Run = paste("Run", i),  # Identify which run the results came from
    Comparison = rownames(x$measure),  # Factor level comparisons
    Odds_Ratio = x$measure[, 1],  # Extract odds ratios
    Lower_CI = x$measure[, 2],  # Extract lower confidence intervals
    Upper_CI = x$measure[, 3],  # Extract upper confidence intervals
    p_value = if (!is.null(x$p.value)) round(x$p.value[, 1], 5) else NA  # Round p-values
  )
}))
results_df$Formatted_OR <- sprintf("%.2f (%.2f-%.2f)", 
                                   results_df$Odds_Ratio, 
                                   results_df$Lower_CI, 
                                   results_df$Upper_CI)
results_table <- results_df[, c("Group", "Comparison", "Formatted_OR", "p_value")]
results_table <- reshape(results_table, 
                         idvar = "Comparison", 
                         timevar = "Group", 
                         direction = "wide")
results_table$Comparison <- factor(results_table$Comparison, levels = c("non-ag","ag")) 
results_table <- results_table[order(results_table$Comparison), ];results_table
write.csv(results_table, "Output/review_script8_odds_ratios_table.csv", row.names = FALSE, fileEncoding = "UTF-8")

results_df_plot <- results_df[, c("Group", "Comparison", "Odds_Ratio", "Lower_CI","Upper_CI","p_value")]
results_df_plot$Odds_Ratio_Log <- log(results_df_plot$Odds_Ratio)
results_df_plot$Lower_CI_Log <- log(results_df_plot$Lower_CI)
results_df_plot$Upper_CI_Log <- log(results_df_plot$Upper_CI)

write.csv(results_df_plot, "Output/review_script8_odds_ratios_data_for_plot.csv", row.names = FALSE, fileEncoding = "UTF-8")





