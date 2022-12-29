# Clear the Memory
rm(list =ls())
# Copy Paste This Command in your R-File. Make Sure You copy this statement as it is
dir1<-getwd()
# Copy Paste This Command in your R-File. Make Sure You copy this statement as it is
setwd(dir1)

# your info (NetID_LastName_FirstName);
name1 = "SXB220123_Bhujade_Siddhant";name1

# Create output file name using name1.
csvfile <- paste(name1,"_HW3.csv",sep="");csvfile

# Instruct to send all the output of your calculation  to a csv file 
sink(csvfile)

# Use cat function to write your name
cat("NAME",  sep = ","   ,  "Siddhant Bhujade", "\n")

# Use cat function to write your netid
cat("NETID", sep=",", "SXB220123","\n")

# Use this cat function to print the Problem 1 in your output
cat("Problem 1","\n")

#read excel
library(readxl)
fff <- read_excel("HW3-6359-F22.xlsx",sheet ="t-test")

#Calculate the Mean of single population
ans1 = mean(fff$Weight)
cat("The Mean of the sample is  ",  sep = ","   , ans1 , "\n")

#Calculate the Std. Deviation
ans2 = sd(fff$Weight)
cat("Standard Deviation is ",  sep = ","   ,  ans2 , "\n")

#Calculate the Sx-Bar
n <- length(fff$Weight)
ans3 = ans2/sqrt(n)
cat("Sx-Bar is ",  sep = ","   ,  ans3 , "\n")

#Upper cut-off point
t <- qt(0.0125,n-1,lower.tail = FALSE)
ans4 = 150 + t*ans3
cat("Upper cut-off point is  ",  sep = ","   , ans4 , "\n")

#Lower cut-off point
ans5 = 150 - t*ans3
cat("Lower cut-off point is  ",  sep = ","   , ans5, "\n")

#P-value
tscore = (ans1-150)/ans3
ans6 = 2*pt(tscore,n-1,lower.tail = FALSE)
cat("P-Value is  ",  sep = ","   , ans6, "\n")

#Decision. Make Sure You Print ANY ONE of the the statement depending on your Result. Don't make Changes to the given statement.
cat("Decision",  sep=",", "We Fail to reject Null Hypothesis", "\n") 

#x = t.test(fff$Weight,alternative = "two.sided",mu =150,conf.level = 0.975);x


# Use this cat function to print the Problem 2 in your output
cat("Problem 2","\n")

#Problem 2:  ANOVA single factor
#You have selected a random sample of Seniors in 4 different cities to see if there is any difference 
#in the proportion of their assets invested in Stocks.  Run an ANOVA test with a significance level of 5%

#read excel
aaa = read_excel("HW3-6359-F22.xlsx",sheet ="ANOVA")
grpMean = aggregate(aaa$Stocks,list(aaa$City),FUN = mean)

#Calculate The Mean of Dallas city 
mean_dallas =grpMean$x[2]
cat("The Mean of Dallas city",sep = ",",mean_dallas,"\n")

#Calculate The Mean of Pittsburgh city 
mean_Pittsburgh =grpMean$x[3]
cat("The Mean of Pittsburgh city ",sep = ",",mean_Pittsburgh,"\n")

#Calculate The Mean of Boston city 
mean_Boston =grpMean$x[1]
cat("The Mean of Boston city ",sep = ",",mean_Boston,"\n")

#Calculate The Mean of Seattle city 
mean_Seattle =grpMean$x[4]
cat("The Mean of Seattle city ",sep = ",",mean_Seattle,"\n")

#P-Value
anovaAns = aov(aaa$Stocks~aaa$City)
anovaSum = summary(anovaAns)
pValue = 0.0244
cat("P-Value is",sep = ",",pValue,"\n")

#Decision. Make Sure You Print ANY ONE of the the statement depending on your Result. Don't make Changes to the given statement.
cat("Decision",  sep="," ,  "We reject the Null Hypothesis", "\n")

# Use this cat function to print the Problem 3 in your output
cat("Problem 3","\n")

#Problem 3:  Log-Transformation
#The first column consists of Years and the Amount of Radiation measured in units of becquerel (Bq) emitted by the Hypothetical Element. Perform Log Transformation with base e

#read excel
sss = read_excel("HW3-6359-F22.xlsx",sheet ="Log")

#Calculate the Skewness Before Transformation
library(moments)
SkewBefore = skewness(sss$Radiation)
cat("The Skewness Before Log Transformation  ",sep = ",",SkewBefore,"\n")

# Calculate the Skewness After log Transformation with base e
LogTransE = log(sss$Radiation)
skewAfterLogE = skewness(LogTransE)
cat("The Skewness After Log Transformation with base e  ",sep = ",",skewAfterLogE,"\n")

# Calculate the Skewness After log Transformation with base 10
LogTrans10 = log10(sss$Radiation)
skewAfterLog10 = skewness(LogTrans10)
cat("The Skewness After Log Transformation with base 10  ",sep = ",",skewAfterLog10,"\n")

# Compare the skewness of both log Transformations is it the same? Answer 'Yes' or 'No'  Only.
cat("Similar", sep = ",","Yes"  , "\n")

# Stop writing to the CSV file.  
sink()

# Partition the graph area into 4 parts (2 rows and 2 columns). This will print 
#  all 4 graphs on one screen. 
par(mfrow=c(2,2))

# Plot the Histogram and QQplot Graph comparing Before and After log Transformation with base e. 
histBefore <- hist(sss$Radiation,main="Before Log Transformation")
histAfter <- hist(LogTransE,main="After Log Transformation")
qqnorm(sss$Radiation,main="Before Log Transformation")
qqnorm(LogTransE,main="After Log Transformation")
