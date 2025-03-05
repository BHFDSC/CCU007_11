Analysis and curation code related to the analysis conducted for this project.


install.packages("finalfit")
library("finalfit")
Valve<-subset(Readmit8,Readmit8$VALVE== "1. Yes"& CABG=="0. No" & MAJOR_AORTIC=="0. No" & OTHER_CARDIAC_PROCS=="0. No")
#1.Count the number of readmission
table(is.na(Valve$DIAG_4_CONCAT_ALL_1))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_2))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_3))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_4))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_5))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_6))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_7))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_8))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_9))
summary(is.na(Valve$DIAG_4_CONCAT_ALL_10))

#2. Number of 30 days readmission
Valve$datefromdischarge_1<- difftime(Valve$ADMIDATE_1,Valve$DATE_DISCHARGE_OR_HOSP_DEATH , units = c("days"))
table(sum(Valve$datefromdischarge_1 < 30, na.rm = TRUE))
summary(as.numeric(Valve$datefromdischarge_1))

#number of admission per month/year
yymm <- substr(Valve$ADMIDATE_1, 1, 7)
yymm_counts <- table(yymm)
print(yymm_counts)

#number of discharge per month/year
Valve1 <- Valve %>%
    mutate(
        def = format(DATE_DISCHARGE_OR_HOSP_DEATH, "%Y-%m"))

table(Valve1$def)

#number of non readmission = discharge -yes admission

#causes of admission
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_1 = substr(DIAG_4_CONCAT_ALL_1, 1, 5))
frequency_table1 <- table(Valve$DIAG_4_CONCAT_01_1)
sorted_table1 <- sort(frequency_table1, decreasing = TRUE)
print(head(sorted_table1,20))

Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_2 = substr(DIAG_4_CONCAT_ALL_2, 1, 5))
frequency_table2 <- table(Valve$DIAG_4_CONCAT_01_2)
sorted_table2 <- sort(frequency_table2, decreasing = TRUE)
print(head(sorted_table2,20))

Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_3 = substr(DIAG_4_CONCAT_ALL_3, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_4 = substr(DIAG_4_CONCAT_ALL_4, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_5 = substr(DIAG_4_CONCAT_ALL_5, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_6 = substr(DIAG_4_CONCAT_ALL_6, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_7 = substr(DIAG_4_CONCAT_ALL_7, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_8 = substr(DIAG_4_CONCAT_ALL_8, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_9 = substr(DIAG_4_CONCAT_ALL_9, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_10 = substr(DIAG_4_CONCAT_ALL_10, 1, 5))

Valve1<-select(Valve,DIAG_4_CONCAT_01_1,DIAG_4_CONCAT_01_2,DIAG_4_CONCAT_01_3,DIAG_4_CONCAT_01_4,DIAG_4_CONCAT_01_5,DIAG_4_CONCAT_01_6,DIAG_4_CONCAT_01_7,DIAG_4_CONCAT_01_8,DIAG_4_CONCAT_01_9,DIAG_4_CONCAT_01_10)
# Combine all columns into a single vector
long_df <- Valve1 %>%
    pivot_longer(cols = everything(), names_to = "DIAG_4_CONCAT_01", values_to = "code")

# Count occurrences of each code
code_counts <- long_df %>%
    count(code, sort = TRUE)
print(code_counts,25)

#Secondary diagnosis
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_1))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_2))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_3))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_4))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_5))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_6))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_7))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_8))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_9))
length(grep("I61",Valve$DIAG_4_CONCAT_ALL_10))

##procedure
## Counting procedure

length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_1))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_2))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_3))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_4))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_5))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_6))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_7))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_8))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_9))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_10))

#table for pre op characteristics


#Age
Valve$date1_parsed <-as.Date(paste0(Valve$MONTH_YEAR_OF_BIRTH,"-01"))
Valve$date2_parsed <-as.POSIXct(Valve$DATE_AND_TIME_OF_OPERATION,format="%Y-%m-%d %H:%M:%S",tz="UTC")
Valve$date2_parsed <-as.Date(Valve$date2_parsed)
Valve$Age<- difftime(Valve$date2_parsed,Valve$date1_parsed , units = "weeks")
Valve$Age <- Valve$Age/52

#BMI
Valve <- Valve %>%
    mutate(HEIGHT = ifelse(HEIGHT == 7179, 179, HEIGHT))
table(Valve$HEIGHT)
Valve$HEIGHT[which(is.na(Valve$HEIGHT))] = median(Valve$HEIGHT, na.rm=TRUE)
Valve$WEIGHT[which(is.na(Valve$WEIGHT))] = median(Valve$WEIGHT, na.rm=TRUE)
Valve$BMI <- Valve$WEIGHT / (Valve$HEIGHT/100 * Valve$HEIGHT/100)
Valve$BMI[which(is.na(Valve$BMI))] = median(Valve$BMI, na.rm=TRUE)
median_value <- median(Valve$BMI[!is.infinite(Valve$BMI)], na.rm = TRUE)
# Replace infinite values with the median
Valve$BMI[is.infinite(Valve$BMI)] <- median_value



#Risk factors predicting 30 days and 12 month re-admission
Valve$ReadmissionYN <-ifelse(is.na(Valve$ADMIDATE_1),0,1)

#Clean data Method 1

# Convert to factor if not already
Valve$OPERATIVE_URGENCY <- as.factor(Valve$OPERATIVE_URGENCY)

# Set new levels
levels(Valve$OPERATIVE_URGENCY) <- c("1", "2", "3", "4")

# Check the result
table(Valve$OPERATIVE_URGENCY)
 Valve$CARDIOGENIC_SHOCK_PRE_OP <- as.factor(Valve$CARDIOGENIC_SHOCK_PRE_OP)
 # Set new levels
 levels(Valve$CARDIOGENIC_SHOCK_PRE_OP) <- c("0", "1")
 # Check the result
 table(Valve$CARDIOGENIC_SHOCK_PRE_OP)

 Valve$ANGINA_STATUS_PRE_SURGERY <- as.factor(Valve$ANGINA_STATUS_PRE_SURGERY)
 # Set new levels
 levels(Valve$ANGINA_STATUS_PRE_SURGERY) <- c("0", "1", "2", "3", "4")
 # Check the result
 table(Valve$ANGINA_STATUS_PRE_SURGERY)
 
 Valve$DYSPNOEA_STATUS_PRE_SURGERY <- as.factor(Valve$DYSPNOEA_STATUS_PRE_SURGERY)
 # Set new levels
 levels(Valve$DYSPNOEA_STATUS_PRE_SURGERY) <- c("1", "2", "3", "4")
 # Check the result
 table(Valve$DYSPNOEA_STATUS_PRE_SURGERY)


 Valve$EJECTION_FRACTION_CATEGORY <- as.factor(Valve$EJECTION_FRACTION_CATEGORY)
 # Set new levels
 levels(Valve$EJECTION_FRACTION_CATEGORY) <- c("1", "2","2", "3","3", "4")
 # Check the result
 table(Valve$EJECTION_FRACTION_CATEGORY)


 Valve$EXTRACARDIAC_ARTERIOPATHY <- as.factor(Valve$EXTRACARDIAC_ARTERIOPATHY)
 # Set new levels
 levels(Valve$EXTRACARDIAC_ARTERIOPATHY) <- c("0", "1")
 # Check the result
 table(Valve$EXTRACARDIAC_ARTERIOPATHY)


 
 Valve$DIABETES_MANAGEMENT <- as.factor(Valve$DIABETES_MANAGEMENT)
 # Set new levels
 levels(Valve$DIABETES_MANAGEMENT) <- c("0", "1","2","3")
 # Check the result
 table(Valve$DIABETES_MANAGEMENT)

 Valve$HISTORY_OF_HYPERTENSION <- as.factor(Valve$HISTORY_OF_HYPERTENSION)
 # Set new levels
 levels(Valve$HISTORY_OF_HYPERTENSION) <- c("0", "1","9")
 # Check the result
 table(Valve$HISTORY_OF_HYPERTENSION)

 Valve$HISTORY_OF_NEUROLOGICAL_DYSFN <- as.factor(Valve$HISTORY_OF_NEUROLOGICAL_DYSFN)
 # Set new levels
 levels(Valve$HISTORY_OF_NEUROLOGICAL_DYSFN) <- c("0", "1")
 # Check the result
 table(Valve$HISTORY_OF_NEUROLOGICAL_DYSFN)

 Valve$HISTORY_OF_PULMONARY_DISEASE <- as.factor(Valve$HISTORY_OF_PULMONARY_DISEASE)
 # Set new levels
 levels(Valve$HISTORY_OF_PULMONARY_DISEASE) <- c("0","0","1", "1")
 # Check the result
 table(Valve$HISTORY_OF_PULMONARY_DISEASE)

 Valve$CIGARETTE_SMOKING_HISTORY <- as.factor(Valve$CIGARETTE_SMOKING_HISTORY)
 # Set new levels
 levels(Valve$CIGARETTE_SMOKING_HISTORY) <- c("0", "1","2")
 # Check the result
 table(Valve$CIGARETTE_SMOKING_HISTORY)

 Valve$RENAL_FUNCTION_DIALYSIS <- as.factor(Valve$RENAL_FUNCTION_DIALYSIS)
 # Set new levels
 levels(Valve$RENAL_FUNCTION_DIALYSIS) <- c("0", "1","2","3","3")
 # Check the result
 table(Valve$RENAL_FUNCTION_DIALYSIS)

 Valve$SEX <- as.factor(Valve$SEX)
 # Set new levels
 levels(Valve$SEX) <- c("1","2")
 # Check the result
 table(Valve$SEX)

 
 Valve$INTERVAL_SURGERY_AND_LAST_MI <- as.factor(Valve$INTERVAL_SURGERY_AND_LAST_MI)
 # Set new levels
 levels(Valve$INTERVAL_SURGERY_AND_LAST_MI) <- c("0","1","2","3","4","5")
 # Check the result
 table(Valve$INTERVAL_SURGERY_AND_LAST_MI)
 
 Valve$DSWI <- as.factor(Valve$DSWI)
 # Set new levels
 levels(Valve$DSWI) <- c("0", "1")
 # Check the result
 table(Valve$DSWI)


 Valve$NEW_HAEMOFILT_OR_DIAL_POST_OP <- as.factor(Valve$NEW_HAEMOFILT_OR_DIAL_POST_OP)
 # Set new levels
 levels(Valve$NEW_HAEMOFILT_OR_DIAL_POST_OP) <- c("0", "1")
 # Check the result
table(Valve$NEW_HAEMOFILT_OR_DIAL_POST_OP)



 Valve$NEW_POST_OP_NEUROLOGICAL_DYSF <- as.factor(Valve$NEW_POST_OP_NEUROLOGICAL_DYSF)
 # Set new levels
 levels(Valve$NEW_POST_OP_NEUROLOGICAL_DYSF) <- c("0","2","1","1","2","2","2","2","2","2")
 # Check the result
 table(Valve$NEW_POST_OP_NEUROLOGICAL_DYSF)



#Predict
library(stats)

Valve$ReadmissionYN <-factor(Valve$ReadmissionYN)
Valve$Age <-as.numeric(Valve$Age)
Valve$BMI <-as.numeric(Valve$BMI)
Valve$SEX <-factor(Valve$SEX)
Valve$OPERATIVE_URGENCY <- factor(Valve$OPERATIVE_URGENCY)
Valve$ANGINA_STATUS_PRE_SURGERY <-factor(Valve$ANGINA_STATUS_PRE_SURGERY)
Valve$CARDIOGENIC_SHOCK_PRE_OP <-factor(Valve$CARDIOGENIC_SHOCK_PRE_OP )
Valve$DYSPNOEA_STATUS_PRE_SURGERY <-factor(Valve$DYSPNOEA_STATUS_PRE_SURGERY )
Valve$DIABETES_MANAGEMENT <-factor(Valve$DIABETES_MANAGEMENT )
Valve$EJECTION_FRACTION_CATEGORY <-factor(Valve$EJECTION_FRACTION_CATEGORY )
Valve$EXTRACARDIAC_ARTERIOPATHY <-factor(Valve$EXTRACARDIAC_ARTERIOPATHY ) 
Valve$HISTORY_OF_HYPERTENSION <-factor(Valve$HISTORY_OF_HYPERTENSION )
Valve$HISTORY_OF_NEUROLOGICAL_DYSFN <-factor(Valve$HISTORY_OF_NEUROLOGICAL_DYSFN ) 
Valve$HISTORY_OF_PULMONARY_DISEASE <-factor(Valve$HISTORY_OF_PULMONARY_DISEASE )
Valve$CIGARETTE_SMOKING_HISTORY <-factor(Valve$CIGARETTE_SMOKING_HISTORY )
Valve$RENAL_FUNCTION_DIALYSIS <-factor(Valve$RENAL_FUNCTION_DIALYSIS)
Valve$INTERVAL_SURGERY_AND_LAST_MI <-factor(Valve$INTERVAL_SURGERY_AND_LAST_MI) 
Valve$NEW_HAEMOFILT_OR_DIAL_POST_OP <-factor(Valve$NEW_HAEMOFILT_OR_DIAL_POST_OP )
Valve$DSWI <-factor(Valve$DSWI )
Valve$NEW_POST_OP_NEUROLOGICAL_DYSF <-factor(Valve$NEW_POST_OP_NEUROLOGICAL_DYSF)

Predictreadmission <- glm(ReadmissionYN ~ Age + BMI + SEX+OPERATIVE_URGENCY+ANGINA_STATUS_PRE_SURGERY+CARDIOGENIC_SHOCK_PRE_OP+DYSPNOEA_STATUS_PRE_SURGERY+DIABETES_MANAGEMENT+
EJECTION_FRACTION_CATEGORY+EXTRACARDIAC_ARTERIOPATHY+HISTORY_OF_HYPERTENSION+HISTORY_OF_NEUROLOGICAL_DYSFN+HISTORY_OF_PULMONARY_DISEASE+CIGARETTE_SMOKING_HISTORY+RENAL_FUNCTION_DIALYSIS
+INTERVAL_SURGERY_AND_LAST_MI+NEW_HAEMOFILT_OR_DIAL_POST_OP+DSWI+NEW_POST_OP_NEUROLOGICAL_DYSF
,data=Valve,family="binomial")
class(Predictreadmission)
summary(Predictreadmission)

explanatory <- c(  "Age" , "BMI" , "SEX" , "OPERATIVE_URGENCY" , 
                  "ANGINA_STATUS_PRE_SURGERY" , "CARDIOGENIC_SHOCK_PRE_OP" , "DYSPNOEA_STATUS_PRE_SURGERY" , 
                  "DIABETES_MANAGEMENT" , "EJECTION_FRACTION_CATEGORY" , "EXTRACARDIAC_ARTERIOPATHY",
                  "HISTORY_OF_HYPERTENSION" , "HISTORY_OF_NEUROLOGICAL_DYSFN",
                  "HISTORY_OF_PULMONARY_DISEASE" , "CIGARETTE_SMOKING_HISTORY" , 
                  "RENAL_FUNCTION_DIALYSIS" , "INTERVAL_SURGERY_AND_LAST_MI","NEW_HAEMOFILT_OR_DIAL_POST_OP")
dependent <- "ReadmissionYN"
table2 <- Valve %>% 
    finalfit(dependent, explanatory, 
             dependent_label_prefix = "")
table2

#gtsummary compare readmission vs no readmission
Preop <- select(Valve, Age , BMI , SEX , OPERATIVE_URGENCY , 
    ANGINA_STATUS_PRE_SURGERY , CARDIOGENIC_SHOCK_PRE_OP , DYSPNOEA_STATUS_PRE_SURGERY , 
    DIABETES_MANAGEMENT , EJECTION_FRACTION_CATEGORY , EXTRACARDIAC_ARTERIOPATHY,
    HISTORY_OF_HYPERTENSION , HISTORY_OF_NEUROLOGICAL_DYSFN,
    HISTORY_OF_PULMONARY_DISEASE , CIGARETTE_SMOKING_HISTORY , 
    RENAL_FUNCTION_DIALYSIS , INTERVAL_SURGERY_AND_LAST_MI,ReadmissionYN)


###########################################################################################################Survival
library(lubridate)
library(tidyverse)
library(ggsurvfit)
Valve1 <-Valve

Valve1$REG_DATE_OF_DEATH <- as.Date(as.character(Valve1$REG_DATE_OF_DEATH), format = "%Y%m%d")
summary(Valve1$REG_DATE_OF_DEATH)

Valve1$DATE_AND_TIME_OF_OPERATION <- as.Date(Valve1$DATE_AND_TIME_OF_OPERATION)
summary(Valve1$DATE_AND_TIME_OF_OPERATION)


Valve1 <- Valve1 %>%
  filter(is.na(REG_DATE_OF_DEATH) | REG_DATE_OF_DEATH >= DATE_AND_TIME_OF_OPERATION)
#filter 7


Valve1 <-
    Valve1 %>% 
    mutate(
        os_months = as.duration(DATE_AND_TIME_OF_OPERATION %--% REG_DATE_OF_DEATH) / dmonths(1)
    )
summary(Valve1$os_months)


Valve1$Status <- NA

Valve1 <- Valve1 %>%
  mutate(os_months = replace_na(os_months, 0)) %>%
  mutate(Status = if_else(os_months > 0, 1, 0))


Valve1$REG_DATE_OF_DEATH[is.na(Valve1$REG_DATE_OF_DEATH)] <- "2023-06-28"

Valve1 <- Valve1 %>%
  filter(is.na(REG_DATE_OF_DEATH) | REG_DATE_OF_DEATH >= DATE_AND_TIME_OF_OPERATION)

Valve1 <-
    Valve1 %>% 
    mutate(
        os_months = as.duration(DATE_AND_TIME_OF_OPERATION  %--% REG_DATE_OF_DEATH) / dmonths(1)
    )


survfit2(Surv(os_months, Status) ~ Valve1$ReadmissionYN, data = Valve1) %>%
    ggsurvfit() +
    labs(
        y = "Percentage Survival",
        title = "12 month survival in patients with/without readmission",
        x= "Time,Months",
    ) +
    scale_color_manual(values = c('brown1', 'cyan'),
                       labels = c('No Readmission', 'Readmission')) +
    scale_fill_manual(values = c('brown1', 'cyan'),
                      labels = c('No Readmission', 'Readmission')) +
    add_pvalue(caption = "Log-rank {p.value}")+
    add_legend_title("Admission")+
    add_risktable(risktable_stats = "{n.risk} ({cum.event})")

######################################################################################################
Valve2 <- Valve1 %>%
    filter(is.na(REG_DATE_OF_DEATH) | REG_DATE_OF_DEATH >= DATE_AND_TIME_OF_OPERATION)
Valve2 <- subset(Valve2, PATIENT_STATUS_AT_DISCHARGE == "0. Alive")


#1.Count the number of readmission
table(is.na(Valve2$DIAG_4_CONCAT_ALL_1))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_2))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_3))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_4))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_5))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_6))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_7))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_8))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_9))
summary(is.na(Valve2$DIAG_4_CONCAT_ALL_10))

#2. Number of 365 days readmission
Valve2$DATE_AND_TIME_OF_OPERATION <- as.Date(Valve2$DATE_AND_TIME_OF_OPERATION, format = "%Y-%m-%d")
Valve2$ADMIDATE_1 <- as.Date(Valve2$ADMIDATE_1, format = "%Y-%m-%d")
Valve2$REG_DATE_OF_DEATH <- as.Date(Valve2$REG_DATE_OF_DEATH, format = "%Y-%m-%d")

# Filter patients who had their operation in 2013
patients_2013 <- Valve2 %>% filter(format(DATE_AND_TIME_OF_OPERATION, "%Y") == "2013")

# Calculate the difference between operation date and readmission date in days
patients_2013 <- patients_2013 %>%
    mutate(DaysToReadmission = as.numeric(ADMIDATE_1 - DATE_AND_TIME_OF_OPERATION))

# Calculate the cutoff date for 12 months after the operation
patients_2013 <- patients_2013 %>%
    mutate(TwelveMonthCutoff = DATE_AND_TIME_OF_OPERATION + 365)

# Exclude patients who died before they could be readmitted (within 12 months)
patients_eligible_for_readmission <- patients_2013 %>%
    filter(Status == 0 | (Status == 1 & REG_DATE_OF_DEATH > TwelveMonthCutoff))

# Filter patients who had a readmission within 12 months after operation
readmitted_within_12_months <- patients_eligible_for_readmission %>%
    filter(!is.na(ADMIDATE_1) & DaysToReadmission <= 365)

### Calculate the readmission rate
Valve2 <- Valve2 %>%
    mutate(
        Cutoff_30days = DATE_AND_TIME_OF_OPERATION + 30,
        Cutoff_3months = DATE_AND_TIME_OF_OPERATION + 90,
        Cutoff_6months = DATE_AND_TIME_OF_OPERATION + 180,
        Cutoff_12months = DATE_AND_TIME_OF_OPERATION + 365
    )

calculate_rates <- function(df, cutoff_date, time_period) {
    # Filter patients who were readmitted within the given time period
    readmitted <- df %>%
        filter(!is.na(ADMIDATE_1) & ADMIDATE_1 <= !!sym(cutoff_date) & 
                   (Status == 0 | (Status == 1 & ADMIDATE_1 <= REG_DATE_OF_DEATH)))
    
    # Filter patients who died within the given time period
    mortality <- df %>%
        filter(Status == 1 & REG_DATE_OF_DEATH <= !!sym(cutoff_date))
    
    # Calculate the number of patients readmitted and those who died
    num_readmitted <- nrow(readmitted)
    num_died <- nrow(mortality)
    
    # Calculate the number of patients who had an operation and were eligible for readmission
    num_eligible <- nrow(df)
    
    # Calculate the readmission rate
    readmission_rate <- num_readmitted / num_eligible
    
    # Calculate mortality rate
    mortality_rate <- num_died / num_eligible
    
    # Print results
    cat("For", time_period, ":\n")
    cat("Number of patients readmitted within", time_period, ":", num_readmitted, "\n")
    cat("Number of patients who died within", time_period, ":", num_died, "\n")
    cat("Number of patients eligible for readmission:", num_eligible, "\n")
    cat("Readmission rate within", time_period, ":", readmission_rate, "\n")
    cat("Mortality rate within", time_period, ":", mortality_rate, "\n\n")
}

# Calculate and print readmission and mortality rates for each time period
calculate_rates(Valve2, "Cutoff_30days", "30 days")
calculate_rates(Valve2, "Cutoff_3months", "3 months")
calculate_rates(Valve2, "Cutoff_6months", "6 months")
calculate_rates(Valve2, "Cutoff_12months", "12 months")

#####causes of admission
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_1 = substr(DIAG_4_CONCAT_ALL_1, 1, 5))
frequency_table1 <- table(Valve$DIAG_4_CONCAT_01_1)
sorted_table1 <- sort(frequency_table1, decreasing = TRUE)
print(head(sorted_table1,20))

Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_2 = substr(DIAG_4_CONCAT_ALL_2, 1, 5))
frequency_table2 <- table(Valve2$DIAG_4_CONCAT_01_2)
sorted_table2 <- sort(frequency_table2, decreasing = TRUE)
print(head(sorted_table2,20))

Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_3 = substr(DIAG_4_CONCAT_ALL_3, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_4 = substr(DIAG_4_CONCAT_ALL_4, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_5 = substr(DIAG_4_CONCAT_ALL_5, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_6 = substr(DIAG_4_CONCAT_ALL_6, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_7 = substr(DIAG_4_CONCAT_ALL_7, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_8 = substr(DIAG_4_CONCAT_ALL_8, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_9 = substr(DIAG_4_CONCAT_ALL_9, 1, 5))
Valve <- Valve %>% mutate(DIAG_4_CONCAT_01_10 =substr(DIAG_4_CONCAT_ALL_10, 1, 5))

Valve3<-select(Valve,DIAG_4_CONCAT_01_1,DIAG_4_CONCAT_01_2,DIAG_4_CONCAT_01_3,DIAG_4_CONCAT_01_4,DIAG_4_CONCAT_01_5,DIAG_4_CONCAT_01_6,DIAG_4_CONCAT_01_7,DIAG_4_CONCAT_01_8,DIAG_4_CONCAT_01_9,DIAG_4_CONCAT_01_10)
# Combine all columns into a single vector
long_df <- Valve3 %>%
    pivot_longer(cols = everything(), names_to = "DIAG_4_CONCAT_01", values_to = "code")

# Count occurrences of each code
code_counts <- long_df %>%
    count(code, sort = TRUE)
print(code_counts,25)

#find out how many primary diagnosis of a specific diagnosis
specific_code <- 'I489,'
abc <- code_counts %>%
    filter(code == specific_code) %>%
    pull(n)

print(abc)
#Secondary diagnosis
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_1))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_2))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_3))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_4))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_5))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_6))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_7))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_8))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_9))
length(grep("I61",Valve2$DIAG_4_CONCAT_ALL_10))

##procedure
## Counting procedure

length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_1))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_2))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_3))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_4))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_5))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_6))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_7))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_8))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_9))
length(grep("K59",Valve2$OPERTN_4_CONCAT_ALL_10))

#Risk factors predicting 30 days and 12 month re-admission
Valve$ReadmissionYN <-ifelse(is.na(Valve$ADMIDATE_1),0,1)
Valve2$ReadmissionYN <-ifelse(is.na(Valve2$ADMIDATE_1),0,1)
#Predict
library(stats)
names(Valve2)[names(Valve2) == "3_96_SURGICAL_INCISION"] <- "SURGICAL_INCISION"
Predictreadmission <- glm(ReadmissionYN ~ Age + BMI + SEX+OPERATIVE_URGENCY+ANGINA_STATUS_PRE_SURGERY+CARDIOGENIC_SHOCK_PRE_OP+DYSPNOEA_STATUS_PRE_SURGERY+DIABETES_MANAGEMENT+
EJECTION_FRACTION_CATEGORY+EXTRACARDIAC_ARTERIOPATHY+HISTORY_OF_HYPERTENSION+HISTORY_OF_NEUROLOGICAL_DYSFN+HISTORY_OF_PULMONARY_DISEASE+CIGARETTE_SMOKING_HISTORY+RENAL_FUNCTION_DIALYSIS
+INTERVAL_SURGERY_AND_LAST_MI+NEW_HAEMOFILT_OR_DIAL_POST_OP+DSWI+NEW_POST_OP_NEUROLOGICAL_DYSF+CUMULATIVE_BYPASS_TIME+CUMULATIVE_CROSS_CLAMP_TIME+
VALVES_REPLACED_REPAIRED+NEW_HAEMOFILT_OR_DIAL_POST_OP+SURGICAL_INCISION+RETURN_TO_THEATRE+NEW_POST_OP_NEUROLOGICAL_DYSF+DSWI
,data=Valve2,family="binomial")
class(Predictreadmission)
summary(Predictreadmission)

#gtsummary compare readmission vs no readmission
Preop <- select(Valve, Age , BMI , SEX , OPERATIVE_URGENCY , 
    ANGINA_STATUS_PRE_SURGERY , CARDIOGENIC_SHOCK_PRE_OP , DYSPNOEA_STATUS_PRE_SURGERY , 
    DIABETES_MANAGEMENT , EJECTION_FRACTION_CATEGORY , EXTRACARDIAC_ARTERIOPATHY,
    HISTORY_OF_HYPERTENSION , HISTORY_OF_NEUROLOGICAL_DYSFN,
    HISTORY_OF_PULMONARY_DISEASE , CIGARETTE_SMOKING_HISTORY , 
    RENAL_FUNCTION_DIALYSIS , INTERVAL_SURGERY_AND_LAST_MI,ReadmissionYN)


## Post op
names(Valve)[names(Valve) == "3_96_SURGICAL_INCISION"] <- "SURGICAL_INCISION"
postop <- c( "CUMULATIVE_BYPASS_TIME","CUMULATIVE_CROSS_CLAMP_TIME","VALVES_REPLACED_REPAIRED","PATIENT_STATUS_AT_DISCHARGE","NEW_HAEMOFILT_OR_DIAL_POST_OP","SURGICAL_INCISION","RETURN_TO_THEATRE","NEW_POST_OP_NEUROLOGICAL_DYSF","DSWI")
dependent <- "ReadmissionYN"
table3 <- Valve %>% 
    finalfit(dependent, postop, 
             dependent_label_prefix = "")
table3

Valve2$CUMULATIVE_BYPASS_TIME <-as.numeric(Valve2$CUMULATIVE_BYPASS_TIME)
Valve2$CUMULATIVE_CROSS_CLAMP_TIME <-as.numeric(Valve2$CUMULATIVE_CROSS_CLAMP_TIME)
Valve2$VALVES_REPLACED_REPAIRED <-as.factor(Valve2$VALVES_REPLACED_REPAIRED)

 # Set new levels
 levels(Valve2$PATIENT_STATUS_AT_DISCHARGE) <- c("0", "1")
 # Check the result
table(Valve2$PATIENT_STATUS_AT_DISCHARGE)

Valve2$"SURGICAL_INCISION" <- as.factor(Valve2$"SURGICAL_INCISION")
# Set new levels
levels(Valve2$"SURGICAL_INCISION") <- c("1", "1","1","1","1","1","1","2","2","3","3","3","3","3","3","3","3","3","3","3","3","3","3","4","4","4","4","4")
# Check the result
table(Valve2$"SURGICAL_INCISION")

Valve2$RETURN_TO_THEATRE <- as.factor(Valve2$RETURN_TO_THEATRE)
# Set new levels
levels(Valve2$RETURN_TO_THEATRE) <- c("0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")
# Check the result
table(Valve2$RETURN_TO_THEATRE)


##gtsummary
library(gtsummary)
Preop <- select(Valve2, Age , BMI , SEX , OPERATIVE_URGENCY , 
    ANGINA_STATUS_PRE_SURGERY , CARDIOGENIC_SHOCK_PRE_OP , DYSPNOEA_STATUS_PRE_SURGERY , 
    DIABETES_MANAGEMENT , EJECTION_FRACTION_CATEGORY , EXTRACARDIAC_ARTERIOPATHY,
    HISTORY_OF_HYPERTENSION , HISTORY_OF_NEUROLOGICAL_DYSFN,
    HISTORY_OF_PULMONARY_DISEASE , CIGARETTE_SMOKING_HISTORY , 
    RENAL_FUNCTION_DIALYSIS , INTERVAL_SURGERY_AND_LAST_MI,ReadmissionYN)

Postop <- select(Valve2,"CUMULATIVE_BYPASS_TIME","CUMULATIVE_CROSS_CLAMP_TIME","VALVES_REPLACED_REPAIRED","NEW_HAEMOFILT_OR_DIAL_POST_OP",
"SURGICAL_INCISION","RETURN_TO_THEATRE","NEW_POST_OP_NEUROLOGICAL_DYSF","DSWI",ReadmissionYN)

Preop %>%
    tbl_summary(by = ReadmissionYN, statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)"), digits = all_continuous() ~ 2,) %>%
    add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
    add_overall() %>%
    add_n() %>%
    modify_header(label ~ "**Variable**") %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") %>%
    modify_caption("**Table 1. Patient Characteristics**") %>%
    bold_labels()

Postop %>%
    tbl_summary(by = ReadmissionYN, statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)"), digits = all_continuous() ~ 2,) %>%
    add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
    add_overall() %>%
    add_n() %>%
    modify_header(label ~ "**Variable**") %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") %>%
    modify_caption("**Table 1. Patient Characteristics**") %>%
    bold_labels()
###############################################################COVID time period#########################################################################
Prelockdown <- Valve1[Valve1$DATE_AND_TIME_OF_OPERATION >= "2010-01-01" & Valve1$DATE_AND_TIME_OF_OPERATION <= "2020-03-22", ]
table(Prelockdown$ReadmissionYN)
Firstlockdown <- Valve1[Valve1$DATE_AND_TIME_OF_OPERATION >= "2020-03-23" & Valve1$DATE_AND_TIME_OF_OPERATION <= "2020-06-23", ]
table(Firstlockdown$ReadmissionYN)
Firstrelaxation <- Valve1[Valve1$DATE_AND_TIME_OF_OPERATION >= "2020-06-24" & Valve1$DATE_AND_TIME_OF_OPERATION <= "2020-11-04", ]
table(Firstrelaxation$ReadmissionYN)
Secondlockdown <- Valve1[Valve1$DATE_AND_TIME_OF_OPERATION >= "2020-11-05" & Valve1$DATE_AND_TIME_OF_OPERATION <= "2020-12-02", ]
table(Secondlockdown$ReadmissionYN)
Firstrelaxation <- Valve1[Valve1$DATE_AND_TIME_OF_OPERATION >= "2020-06-24" & Valve1$DATE_AND_TIME_OF_OPERATION <= "2020-11-04", ]
table(Firstrelaxation$ReadmissionYN)
Primary and secondary analysis
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

Valve5<-select(Valve2,DIAG_4_CONCAT_ALL_1,DIAG_4_CONCAT_ALL_2,DIAG_4_CONCAT_ALL_3,DIAG_4_CONCAT_ALL_4,DIAG_4_CONCAT_ALL_5,DIAG_4_CONCAT_ALL_6,DIAG_4_CONCAT_ALL_7,DIAG_4_CONCAT_ALL_8,DIAG_4_CONCAT_ALL_9,DIAG_4_CONCAT_ALL_10)
# Combine all columns into a single vector
long_df1 <- Valve5 %>%
    pivot_longer(cols = everything(), names_to = "DIAG_4_CONCAT_ALL_1", values_to = "code")

# Count occurrences of each code
code_counts <- long_df1 %>%
    count(code, sort = TRUE)
print(code_counts,25)

# Step 1: Separate Primary and Secondary Diagnoses
Valve6 <- long_df1 %>%
  mutate(
    Primary_Diag = str_split(code, ",|\\s") %>% map_chr(~ .[1]), # Extract primary diagnosis from DIAG_4_CONCAT_ALL_1
    Secondary_Diag = str_split(code, ",|\\s") %>% map(~ .[-1])   # Extract secondary diagnoses from DIAG_4_CONCAT_ALL_1
  )

# Step 2: Count the frequency of each Primary Diagnosis
primary_diagnosis_counts <- Valve6 %>%
  group_by(Primary_Diag) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Sort by the most common diagnoses

# Step 3: Get the Top 100 Most Common Primary Diagnoses
top_100_primary_diagnoses <- primary_diagnosis_counts %>%
  slice_head(n = 101)

# View the Top 100 Primary Diagnoses
print(top_100_primary_diagnoses)

# Step 4: Unnest Secondary Diagnoses for separate analysis
Valve_secondary <- Valve6 %>%
  unnest(Secondary_Diag)  # Each secondary diagnosis in its own row

# Step 5: Count the frequency of each Secondary Diagnosis
secondary_diagnosis_counts <- Valve_secondary %>%
  group_by(Secondary_Diag) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Step 6: Get the Top 100 Most Common Secondary Diagnoses (optional)
top_100_secondary_diagnoses <- secondary_diagnosis_counts %>%
  slice_head(n = 100)

# View the top 100 secondary diagnoses (if needed)
print(top_100_secondary_diagnoses)

matching_rows <- grep("I61", primary_diagnosis_counts$Primary_Diag) 
total_count <- sum(primary_diagnosis_counts$count[matching_rows]) 
total_count
length(grep("I61",Valve_secondary$Secondary_Diag))

#####calculate cardiovascular related primary diagnosis
#Remove first row in primary diagnosis
primary_diagnosis_counts <- primary_diagnosis_counts[-1, ]
#calculate the number of row with I and no I
#consist of I (10318)
matching_rows <- grep("I", primary_diagnosis_counts$Primary_Diag) 
total_count <- sum(primary_diagnosis_counts$count[matching_rows]) 
total_count
#calculate sum of column and minus 10318 (n=42430, 42430-10318=32112)
total_sum <- sum(primary_diagnosis_counts$count, na.rm = TRUE)
#Cardiovascular secondary diagnosis (Total row 429212)
length(grep("I",Valve_secondary$Secondary_Diag))
#107151 (Non cardiovascular secondary diagnosis = 429212-107151=322061)

Procedure
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_1))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_2))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_3))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_4))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_5))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_6))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_7))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_8))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_9))
length(grep("K59",Valve$OPERTN_4_CONCAT_ALL_10))


