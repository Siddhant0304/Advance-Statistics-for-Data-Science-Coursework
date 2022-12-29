#  Clear the environment
rm (list = ls())
  
# your info (NetID_LastName_FirstName); write to Console
name1 <- "SXB220123_Bhujade_Siddhant";name1

#  Assign dir1 to the folder on your computer where this excel file (HW1) is.
dir1 <- "C:/Users/siddh/OneDrive - The University of Texas at Dallas/Adv_Stats/HW/HW1"

#  set the working directory to dir1
setwd(dir1)
# Copy and paste this command into your R file. Keep it as a comment.
# setwd("C:\Users\saura\Box\TA-Saurabh\22F\6359\Homework\Homework 1\Sec-003")

#  load readxl library
library(readxl)

# read this excel file  (sheet = Pioneer). Do not include the file path here, only the file name
table<-read_excel("HW1-6359.xlsx", sheet="Pioneer")

# rename the 2nd column to Quantity
names(table)[2]<-"Quantity"

# Create a new vector Total which is Quantity x Price 
Total<- c(table$Quantity*table$Price);Total

# Create a new vector Commission which is Total x 0.10
Commission <- c(Total * 0.10);Commission

# add the new vectors to the excel file.  This will create two new columns.
table<-data.frame(table,Total,Commission)

# Create output file name using name1.  All your output will go to this file.  
csvfile <- paste(name1,"_HW1.csv",sep=""); csvfile

# send the output to the csv file you just created
sink(csvfile)

# Use the cat function to write your name (First  Last).  This must be Row 1 of your CSV file.  
cat("NAME",  sep = ","   ,  "Siddhant Bhujade", "\n")

# Use cat function to write your netid.  This must be Row 2 of csv file.
cat("NETID" ,  sep = ","   , "SXB220123", "\n")

# write the length of the 1st column.  This must be Row 3 of csv file.
len1 = length(table$Part.No)
cat("LENGTH" , sep = "," , len1, "\n")

# Like above, calculate and print the following values along with the labels (as 
#  shown above for length) to the cvs file  (in the order given below)

# Average price
avg = mean(table$Price)
cat("AVERAGE" , sep = "," , avg, "\n")

#Minimum price
Min = min(table$Price)
cat("MINIMUM" , sep ="," , Min, "\n")

#Maximum price
Max = max(table$Price)
cat("MAXIMUM" , sep ="," , Max, "\n")

#Total quantity
Total_Qty = sum(table$Quantity)
cat("TOTAL QUANTITY" , sep ="," ,Total_Qty, "\n")

#Total commission
Total_Com = sum(table$Commission)
cat("TOTAL COMISSION" , sep ="," ,Total_Com, "\n")

#Average commission
Avg_Com = mean(table$Commission)
cat("AVERAGE COMISSION" , sep ="," ,Avg_Com, "\n")

#std deviation of the commission (treat as sample)
Var = var(table$Commission)
Var_Sample = Var *(len1-1)/len1
Std_Com = sqrt(Var_Sample)
cat("STD DEV COMISSION" , sep ="," ,Std_Com, "\n")

# create a divider line.  This must be Row 11 of csv file. Copy and Paste this Command 
cat("--------------------------------", "\n")

# write the updated table (the whole table) with fields seperated by a comma
# This must be Row 12 of csv file.
write.table(table, sep= "," , row.names=FALSE)

#PART B
# create a divider line. This must be Row 50 of csv file. Copy and Paste this Command
cat("--------------------------------", "\n")

# This must be on Row 51 of the CSV file
cat("Part B", "\n")

# read the sheet(Inc_Exp_Data)
table2 <- read_excel("HW1-6359.xlsx", sheet="Inc_Exp_Data")

# What is the Mean Expense of a Household?
mean_exp = mean(table2$Mthly_HH_Expense)
cat("Mean Expense of a household", sep=",", mean_exp,"\n")

# What is the Median Household Expense?
median_exp = median(table2$Mthly_HH_Expense)
cat("Median Household Expense", sep=",", median_exp,"\n")

# How many households have 2 or more earning members?
count <- length(table2$No_of_Earning_Members[table2$No_of_Earning_Members>1])
cat("Households have 2 or more earning members", sep=",", count,"\n")

# Calculate the frequency of each Highest _Qualified_Member i.e. (How many  Graduate, Under-Graduate, Post-Graduate, etc)
freq_table <- table(table2$Highest_Qualified_Member)

# Write the table into csv file which you just created
write.table(freq_table, sep= "," , row.names=FALSE)

# Calculate Standard Deviation for first 1st columns.
std = sd(table2$Mthly_HH_Income)
cat("Standard Deviation for first columns.", sep = ",",std,"\n")


# Stop writing to the CSV file. 
sink()
