#######################################################################################################
##########   Program Name: Utilization Paper Stats    ################
###Project Name: ICDCode Utilization Paper 
##Purpose: 1. Getting age mean and stdev for the Utilization Paper
#          2. Creating plots of: ICDCode Counts over time; Prevalence of ICDCode and Comorbs over time; Number of ICDCode codes assigned per location
#         
##Programmer: Andy Dey
##Datasets Used: Cohort Datasets created in SQL
##Results Locations: output_filepath

##Date Last Updated: 8/2050

######################################################################################################

library(RODBC)##RODBC is for SQL Databases, this is how to connect to my database
ch=odbcDriverConnect('driver={SQL Server Native Client 11.0};
                     server=servername;
                     database=dbname;
                     trusted_connection=yes')
library(tidyverse)
library(ggplot2)
library(Hmisc)


######################Table 1#################################################
######Table 1: Age Mean and Standard Deviation########
####T1 part 1:a. Everyone who had a ICDCode Code and OtherDisease####
##T1 part 1.a.iBringing in the correct dataset for the cohort
AllICDCodequery<-'SELECT distinct IDVar, ageVarName FROM schema.tablename'
AllICDCodeDemo <- sqlQuery(ch,AllICDCodequery)#SQL Query function literally queries the table


###T1 Part 1.a.ii. Getting Average and Standard Deviation for Age
mean(AllICDCodeDemo$ageVarName)
sd(AllICDCodeDemo$ageVarName)


####T1 part 1b: Everyone who had a ICDCode Code and OtherDisease####
##T1 part 1.b.i.Bringing in the correct dataset for the cohort
AllICDCodequery<-'SELECT distinct IDVar, ageVarName FROM schema.tablename where ageVarName is not null'
AllICDCodeDemo <- sqlQuery(ch,AllICDCodequery)##SQL Query function literally queries the table


###T1 Part 1.b.ii. Getting Average and Standard Deviation for Age
mean(AllICDCodeDemo$ageVarName)
sd(AllICDCodeDemo$ageVarName)


######Table 1: Age Mean and Standard Deviation########
####T1 part c: Everyone who had MainDisease####
##T1 part 1.c.i.Bringing in the correct dataset for the cohort
AllMainDiseasequery<-'SELECT distinct IDVar, ageVarName FROM schema.tablename'
AllMainDiseaseDemo <- sqlQuery(ch,AllMainDiseaseQuery)#SQL Query function literally queries the table


###T1 Part 1.c.ii. Getting Average and Standard Deviation for Age
mean(AllMainDiseaseDemo$ageVarName)
sd(AllMainDiseaseDemo$ageVarName)


###ICDCode Chart Reviewed###
###T1 Part 1.d.i. Importing the dataset
AllICDCodeCRquery<-'SELECT distinct IDVar, ageVarName FROM schema.tablename'
AllICDCodeCRDemo <- sqlQuery(ch,AllICDCodeCRQuery)#SQL Query function literally queries the table


##Part 1.d.ii. Removing NA values
AllICDCodeCRDemo2<-na.omit(AllICDCodeCRDemo)


###T1 Part 1.d.iii. Getting Average and Standard Deviation for Age
mean(AllICDCodeCRDemo2$ageVarName)
sd(AllICDCodeCRDemo2$ageVarName)



#####All Persons with Comorbs Features
###T1 Part 1.e.i. Getting Average and Standard Deviation for Age
AllComorbsquery<-'SELECT distinct IDVar, ageVarName FROM schema.tablename'
AllComorbsDemo <- sqlQuery(ch,AllComorbsquery)#SQL Query function literally queries the table


##Part 1.e.ii. Removing NA values
AllComorbsDemo2<-na.omit(AllComorbsDemo)


###T1 Part 1.e.iii. Getting Average and Standard Deviation for Age
mean(AllComorbsDemo2$ageVarName) 
sd(AllComorbsDemo2$ageVarName)



#####All Persons with Comorbs Features who were chart reviewed
###T1 Part 1.e.i. Getting Average and Standard Deviation for Age
AllComorbsCRquery<-'SELECT distinct IDVar, ageVarName FROM schema.tablename'
AllComorbsCRDemo <- sqlQuery(ch,AllComorbsCRquery)##SQL Query function literally queries the table


##Part 1.e.ii. Removing NA values
AllComorbsCRDemo2<-na61.32(.omit(AllComorbsCRDemo)


###T1 Part 1.e.iii. Getting Average and Standard Deviation for Age
mean(AllComorbsCRDemo2$ageVarName)
sd(AllComorbsCRDemo2$ageVarName)




########################Histograms and Figures##########################


######Histogram 1: Number of ICDCode Codes Assigned Per Month for those with OtherDisease, run before 3/30/23
#Method 1:
####Step 1: Bring In the Data Table
ICDCodePerMonthQuery<-'select * from schema.tablename
ICDCodePerMonth<-sqlQuery(ch,ICDCodePerMonthQuery)



##Step 2a. Creating Dates by reformatting:
ICDCodePerMonth2<-ICDCodePerMonth
ICDCodePerMonth2$DayMonYr_fmt<-gsub("_","-", ICDCodePerMonth$DayMonYr)
ICDCodePerMonth2$DayMonYr_fmt<-gsub("010","10", ICDCodePerMonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
ICDCodePerMonth2$DayMonYr_fmt<-gsub("011","11", ICDCodePerMonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
ICDCodePerMonth2$DayMonYr_fmt<-gsub("012","12", ICDCodePerMonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
ICDCodePerMonth2$DayMonYr_fmt2<- as.Date(ICDCodePerMonth2$DayMonYr_fmt, format='%d-%m-%Y')                              

##Step 2.b.sorting by the new var
#one method of sorting:
ICDCodePerMonth2<- ICDCodePerMonth2[order(ICDCodePerMonth2$DayMonYr_fmt2)]

####Step 2c: Option 1: Creating the histogram With Period 1 and 2 Info

ICDCodeCodes_Month_bypd<-
  ggplot(data=ICDCodePerMonth2,aes(x=DayMonYr_fmt2, fill=as.factor(TimePeriod)))+ 
  geom_bar()+
  labs(x="Month and Year ICDCode Assigned", y="Frequency of ICDCode Assignment",
       title="     Count of First ICDCode Assigment Dates Among
             MainDisease Positive Patients Per Month")+
  scale_fill_manual(values = c("#3333FF", "#CCCC33"),name="MainDisease Diagnosis Period", labels=c("Before DateCutoff", "After DateCutoff"))#this code adds the Legend labels and changes the bar colors
  scale_x_continuous(breaks=seq(min(ICDCodePerMonth2$DayMonYr_fmt2), max((ICDCodePerMonth2$DayMonYr_fmt2), by=23)))#this code ensures one tick mark per LocationType



######Histogram 1: Number of ICDCode Codes Assigned Per Month for those with OtherDisease
##Step 1: Importing the dataset
ICDCodePerMonthQuery<-'select * from schema.tablename'
ICDCodePerMonth<-sqlQuery(ch,ICDCodePerMonthQuery)



##Step 2a. Creating Dates by reformatting date:
ICDCodePerMonth2<-ICDCodePerMonth
ICDCodePerMonth2$DayMonYr_fmt<-gsub("_","-", ICDCodePerMonth$DayMonYr)
ICDCodePerMonth2$DayMonYr_fmt<-gsub("010","10", ICDCodePerMonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
ICDCodePerMonth2$DayMonYr_fmt<-gsub("011","11", ICDCodePerMonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
ICDCodePerMonth2$DayMonYr_fmt<-gsub("012","12", ICDCodePerMonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
ICDCodePerMonth2$DayMonYr_fmt2<- as.Date(ICDCodePerMonth2$DayMonYr_fmt, format='%d-%m-%Y')                              



##Step 2.a.ii. reformatting the Assign_month var to act as a label
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("10-", "Oct-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("11-", "Nov-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("12-", "Dec-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("1-", "Jan-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("2-", "Feb-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("3-", "Mar-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("4-", "Apr-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("5-", "May-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("6-", "Jun-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("7-", "Jul-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("8-", "Aug-", ICDCodePerMonth2$FP_AsgnMonth2)
ICDCodePerMonth2$FP_AsgnMonth2<-gsub("9-", "Sep-", ICDCodePerMonth2$FP_AsgnMonth2)


##Step 2.b.sorting by the new var
ICDCodePerMonth2<- ICDCodePerMonth2 %>% arrange(DayMonYr_fmt2)



####Step 2c: Option 1: Creating the histogram With Period 1 and 2 Info

ICDCodeCodes_Month_bypd<-
  ggplot(data=ICDCodePerMonth2,aes(x=DayMonYr_fmt2, fill=as.factor(TimePeriod)))+ 
  geom_bar()+
  labs(x="Month and Year ICDCode Assigned", y="Frequency of First ICDCode Code Assignment",
       title="                    Count of First ICDCode Assigment Dates Among
                           MainDisease Positive Patients Per Month")+
  scale_x_continuous(breaks=ICDCodePerMonth2$DayMonYr_fmt2, labels=ICDCodePerMonth2$Asgn_Month2)+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))+## This rotates the x axis labels to be vertical
  scale_fill_manual(values = c("#3333FF", "#CCCC33"),name="MainDisease Diagnosis Period", labels=c("Before DateCutoff", "After DateCutoff"))
#this code adds the Legend labels and changes the bar colors


######Figure 2: The number of days from MainDisease First Positive to ICDCode Assignment in Period 1 and Period 2
###step 1: Importing the dataset
TimetoICDCodeQuery<-'select * from schema.tablename where DaystoICDCode>0'
TimetoICDCode<-sqlQuery(ch,TimetoICDCodeQuery)

####Getting the Median Days for FirstPositive in P1 and P2, 4/25/23
aggregate(TimetoICDCode$DaystoICDCode, list(TimetoICDCode$TimePeriod), median)

###Step 2: Creating a box and whisker Plot with 
ggplot()+geom_boxplot(data=TimetoICDCode, mapping=aes(x=DaystoICDCode, color=TimePeriod))+
  labs(x="Days from First Positive to ICDCode Assignment", y="MainDisease Diagnosis Before or After ICDCode Introduced",
       title="Differences in Days from First MainDisease Diagnosis Date and First ICDCode Date"
       #between patients Diagnosed with MainDisease Before and After ICDCode was Introduced"
  )
##AD: saved



#####Graph 3: Prevalence of ICDCode Code Divided by the Prevalence of MainDisease Cases Per Month
##Step 1: Bringing in the dataset from SQL
PrevQuery<-'select * from schema.tablename'
prevmonth<-sqlQuery(ch,PrevQuery)


######G3 Method 2: Prevalence as the cumulative totals
####Step 2: Creating the Date variable
#This is to make sure cumulative frequency is calculated correctly, and so the graph tick marks will be in the correct order

##Creating prevmonth2 without the ICDCodevsMainDisease and ComorbsvsMainDisease (for readability reasons)
prevmonth2<-prevmonth[c('MonYr','MainDiseaseCount_MY', 'ICDCodeCount_MY','ComorbsCount_MY', 'DayMonYr')]

##Step 1b. Sorting: R creates the line plot in the order that the points were received
#creating a var replacing _ with - and converting it to date format
prevmonth2$DayMonYr_fmt<-gsub("_","-", prevmonth2$DayMonYr) ##replacing underscore with -
prevmonth2$DayMonYr_fmt<-gsub("010","10", prevmonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
prevmonth2$DayMonYr_fmt<-gsub("011","11", prevmonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
prevmonth2$DayMonYr_fmt<-gsub("012","12", prevmonth2$DayMonYr_fmt)##getting rid of the extra 0 from 10, 11 and 12
prevmonth2$DayMonYr_fmt2<- as.Date(prevmonth2$DayMonYr_fmt, format='%d-%m-%Y')#Putting it into date format                              

##Step 1.b.sorting by the new var
prevmonth2<- prevmonth2[order(prevmonth2$DayMonYr_fmt2)]

##A different method of sorting 
prevmonth2<- prevmonth2 %>% arrange(DayMonYr_fmt2)


####Step 2: Calculating the cumulative totals per month
##Step 2a. Cleaning up Prevalence Values
prevmonth2$ICDCodeCount_MY[is.na(prevmonth2$ICDCodeCount_MY)]<-00.0001 #AD: this replaces N/A with 0 so the cumulative value can be calculated

##step 2b. calculating the cumulative counts
prevmonth2$MainDiseasePrev_cumul<-cumsum(prevmonth2$MainDiseaseCount_MY)
prevmonth2$ICDCodePrev_cumul<-cumsum(prevmonth2$ICDCodeCount_MY)
prevmonth2$ComorbsPrev_cumul<-cumsum(prevmonth2$ComorbsCount_MY)

##Step 2c. Creating the prevalence ratio variables
prevmonth2$ICDCodeprevCR<-((prevmonth2$ICDCodePrev_cumul/prevmonth2$MainDiseasePrev_cumul)*100)#ICDCode cumulative prevalence, added *100 
prevmonth2$ComorbsprevCR<-((prevmonth2$ComorbsPrev_cumul/prevmonth2$MainDiseasePrev_cumul)*100) #Comorbs cumulative prevalence,  added *100 




#####Step 3: Creating the plot
#step 2.c. The code for the line plot
F3Method2<-ggplot()+geom_point(data=prevmonth2, mapping=aes(x=DayMonYr_fmt2, y=ICDCodeprevCR, color='ICDCode Percent'), shape=15)+
  geom_point(data=prevmonth2, mapping=aes(x=DayMonYr_fmt2, y=ComorbsprevCR,  color='Comorbs Percent'), shape=19)+
  
  labs(x="Month and Year", y="Percent",##AD 3/20/23: Changed Prevalence Ratio to % 
       title="Percentages of Total MainDisease Patients Who Ever Received ICDCode or had a Comorb
                              Between Time 1 and Time 2"
F3Method2




###Additional Graph to QC Check cumulative Prevalence Counts of Comorbs and MainDisease, to show why Comorbs percent declined at times
ggplot()+geom_point(data=prevmonth2, mapping=aes(x=DayMonYr_fmt2, y=MainDiseasePrev_cumul, color='Ever had MainDisease'), shape=15)+
  geom_point(data=prevmonth2, mapping=aes(x=DayMonYr_fmt2, y=ComorbsPrev_cumul,  color='Ever had Comorbs'), shape=19)+
  geom_point(data=prevmonth2, mapping=aes(x=DayMonYr_fmt2, y=ICDCodeprevCR,  color='Ever had ICDCode'), shape=6)+
  labs(x="Month and Year", y="Number of Patients", #AD Changed Prevalence Ratio to % 
       title="Cumulative Totals of Persons who Ever Had MainDisease, Comorbs or ICDCode
                              Between Time1 and Time2")
####AD: Saved As 



######Figure 4: Bar plot of the number of First ICDCode Instances per LocationType (count of persons who got their first ICDCode code in each LocationType)
##Step 1: Bringing in the dataset from SQL
LocationTypeQuery<-'select * from schema.tablename'
ICDCodeBYLocationType<-sqlQuery(ch,LocationTypeQuery)


head((ICDCodeBYLocationType))


##Step 2: Option 1: Creating a bar chart of ICDCode patients by Region (Masked LocationType Number)
PatientsbyRegion<-ggplot(data=ICDCodeBYLocationType)+ 
  geom_bar(mapping=aes(x=LocationTypeMask, fill='RED'))+
  labs(x="Region", y="Number of Persons",
       title="Figure 4 Number of Persons Who Received ICDCode By Region")+
  scale_x_continuous(breaks=seq(min(ICDCodeBYLocationType$LocationTypeMask), max(ICDCodeBYLocationType$LocationTypeMask, by=23)))+ #this code ensures one tick mark per LocationType
  theme(legend.position="none")#This code removes the Legend




##Step 2: Option 2: For reference, creating a bar chart of ICDCode patients by Ractual LocationType
PatientsbyLocationType<-ggplot(data=ICDCodeBYLocationType)+ 
  geom_bar(mapping=aes(x=LocationType, fill='RED'))+
  labs(x="LocationType", y="Number of Persons",
       title="Figure 4 Number of Persons Who Received ICDCode By Actual LocationType Number")+
  scale_x_continuous(breaks=seq(min(ICDCodeBYLocationType$LocationTypeMask), max(ICDCodeBYLocationType$LocationTypeMask, by=23)))+##this code ensures one tick mark per LocationType
  theme(legend.position="none")##This code removes the Legend







######Figure 5: Bar plot of the number of total ICDCode Assignment Dates per LocationType
##Step 1: Bringing in the dataset from SQL
LocationTypeDtsQuery<-'select * from schema.tablename'
ICDCodeDatesBYLocationType<-sqlQuery(ch,LocationTypeDtsQuery)

##Step 2: Creating a bar chart of ICDCode Dates by LocationType
DatesbyLocationType<-ggplot(data=ICDCodeDatesBYLocationType)+ 
  geom_bar(mapping=aes(x=LocationTypeMask, fill='RED'))+
  labs(x="Region", y="Number of Persons",
       title="Figure 4 Number of Times ICDCode Assigned By Region")+
  scale_x_continuous(breaks=seq(min(ICDCodeBYLocationType$LocationTypeMask), max(ICDCodeBYLocationType$LocationTypeMask, by=23)))+##this code ensures one tick mark per LocationType
  theme(legend.position="none")##This code removes the Legend



#######Graph 6  #########

###This graph is to how the number of people who were diagnosed with MainDisease in a given month from Time1 to Time2 
###who went on to get the ICDCode code between Time 1 and Time 2 who had their first ICDCode date after their MainDisease first Positive
###Cohort used in the graph: persons who had MainDisease FirstPositive before Time1 and had their first ICDCode code between Time 1 and Time 2


##Step 1: Importing the dataset
Graph6Query<-'select * from schema.tablename'
Graph6<-sqlQuery(ch,Graph6Query)



##Step 2a: Sorting the dataset
#A method of sorting 
Graph6_v2 <- Graph6 %>% arrange(FP_YrMonday_fmt)

##Step 2b. Checking that it sorted correctly
Graph6_v2 %>% distinct(FP_YrMonday_fmt, FP_MonYr_Label)



###Step 3: Creating the graph 6

ICDCodeCasesperMainDiseaseTimeMon<-
  ggplot(data=Graph6_v2)+ 
  geom_bar(mapping=aes(x=FP_YrMonday_fmt))+
  labs(x="First MainDisease Positive Month", y="Number of Patients Later Assigned ICDCode",
       title="Count of Persons Assigned ICDCode between Time 1 and Time 2
                              by MainDisease Diagnosis Month")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))+##AD: This rotates the x axis labels to be vertical
  scale_fill_manual(values = c("#3333FF"))






###############     Graph 7: Dividing the number of new ICDCode Cases by the total PossiblePredictor cases in the 12 months before that month ##### 
####E.g. Divide the total new ICDCode cases in Month10 by the total PossiblePredictor cases from Time 1 to Time 2
    ####then multiply by 100,000#######

###Graph 7 Step 1: Bringing in the cohorts
###Graph 7 Step 1a. Bringing in the cohort of Total MainDisease Cases Per Month
  #In the section called Graph 7
Graph7MainDiseaseQuery<-'select * from schema.tablename'
Graph7MainDisease<-sqlQuery(ch,Graph7MainDiseaseQuery)



###Graph 7 Step 2: Calculating the cumulative prevalence of MainDisease cases in
#       for the 12 months leading up to each Diagnosis month



###Graph 7 Step 3: Dividing the number of new ICDCode cases per month by the cumulative total PossiblePredictor cases in the previous 12 months
##Graph 7 Step 3a: Creating a new dataset to avoid overwriting the original:
Graph7MainDisease2 <- Graph7MainDisease

##Graph 7 Step 3b: Calculating the new var ICDCode_dvdby_MainDisease 
Graph7MainDisease2 <- Graph7MainDisease2 %>% mutate(ICDCode_dvdby_MainDisease = ICDCodePerMonth/PossiblePredictorCumulTotal)


###Graph 7 Step 4: Multiplying the number from step 3 by 100k
Graph7MainDisease2$ICDCodedvdbyMainDisease_x10k <- Graph7MainDisease2$ICDCode_dvdby_PossiblePredictor*10000



###Graph 7 Step 5: Sorting the data by Month
Graph7MainDisease2<- Graph7MainDisease2 %>% arrange(MonYr)




###Graph 7 Step 6: Creating the Graph
VAGRAPH7 <- ggplot(data=Graph7MainDisease2, aes(x=MonYr, y=ICDCodedvdbyMainDisease_x10k))+ 
  geom_col(fill="#0033CC")+
  scale_x_discrete(labels=Graph7MainDisease2$MonYrLabel)+
  scale_y_continuous(limits=c(0, 150), breaks = c(0,25,50,75, 100, 125, 150) )+
  labs(x="First ICDCode ICD Code Diagnosis Month", 
       y="Frequency of Patients Assigned the ICDCode ICD-10 Code  
            per 10,000 Incident MainDisease Cases",
       title="Number of New Patients Given the ICDCode ICD Code per 10,000 New MainDisease Cases 
          in the Previous 12 months at Location, Not Including Month of ICDCodeTime")+
  theme(text=element_text(family="serif", face="bold"))













