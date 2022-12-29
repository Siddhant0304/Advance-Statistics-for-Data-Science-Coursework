
#  Clear the environment
rm (list = ls())

# your info (NetID_LastName_FirstName); write to Console
name1 <- "SXB220123_Bhujade_Siddhant";name1

#  Assign dir1 to the folder on your computer where this excel file (HW) is. 
dir1 <- "C:/Users/siddh/OneDrive - The University of Texas at Dallas/Adv_Stats/HW/HW_Bonus"

#  set the working directory to dir1
setwd(dir1)

# Copy and paste this setwd command comment into your R file. This is TA's directory
# where your R files will be stored for grading.  
# setwd(""C:/Users/saura/Box/22F/6359/Homework/Bonus_HW"")

#  load readxl library
library(readxl)

# read this excel file  (sheet = Data). Do not include the file path here, only the file name, 
# i.e. ""Bonus_HW.xlsx"" in this case.  Do not change the file name.  

Table1 <- read_excel("Bonus_HW.xlsx",sheet = "Data")

# Get the length of table and store it in variable named "len"
len <- length(Table1$A1)

# Put Any number between(112-999) in seed( ).  This will lock your random numbers
set.seed(112)

# Create a new vector A3 containing random numbers between 80 to 100 of   size len
A3 <- runif(len,min = 80,max = 100)

# Round to 0 decimal
Round_A3 <- round(A3,digits =0)

# Add newly created vector A3  to the existing table as the third column and name it A3.(Note: When you add a new vector to a table, you have to name the column at the same time.)
Table1 <- data.frame(Table1,Round_A3)
names(Table1)[names(Table1)=="Round_A3"]<-"A3"

# Create a vector which has totals of each row
Total <- c(Table1$A1 + Table1$A2 + Table1$A3)

# Create a dataframe which has the 4 columns A1, A2, A3, and Total1) and name the  dataframe as t2
t2 <- data.frame(Table1,Total)

# Get the column names of table t2
names(t2)

# Rename (1st 2nd and 3rd) columns to "Test1" "Test2" "Test3"
names(t2)[1]<- "Test1"
names(t2)[2]<- "Test2"
names(t2)[3]<- "Test3"

# Create a Grade vector with all F's and add it as 4th column
Grade <- rep(c("F"),times = len)
t2 <- data.frame(t2,Grade)

# Who has a total of 250 or more?
#Everyone who scored 250 or more gets a D
for (i in 1:nrow(t2)){
  if(t2$Total[i] >= 250){
    t2$Grade[i] <- "D"
  }
}
t2


# Assign other Grades as given Below
#>=280 is A
#>= 270 is B
#>= 260 is C
for (i in 1:nrow(t2)){
  if(t2$Total[i] >= 280){
    t2$Grade[i] <- "A"
  }
  if(t2$Total[i] >= 270 & t2$Total[i] < 280){
    t2$Grade[i] <- "B"
  }
  if(t2$Total[i] >= 260 & t2$Total[i] < 270){
    t2$Grade[i] <- "C"
  }
}
t2


# Create output file name using variable name1 which was created earlier. All your output should go to this file now.
csvfile <- paste(name1,"_Bonus_HW.csv",sep=""); csvfile
#write.csv(t2,file = csvfile)

# send the output to the csv file you just created
sink(csvfile)

# Use cat function to write your name (First  Last).  This will be Row 1 of csv file.  
#(This Step is an Important step)
cat("NAME",  sep = ","   ,  "Siddhant Bhujade", "\n")

# Use cat function to write your netid.  This will be Row 2 of csv file.
#(This Step is an Important step)
cat("NETID" ,  sep = ","   , "SXB220123", "\n")

# create a divider line. This must be on Row 3 of csv file. Copy and Paste this command
cat("--------------------------------", "\n")

# write the updated table (the whole table) with fields seperated by a comma
write.table(t2, sep= "," , row.names=FALSE)

# Stop writing to the CSV file.  
sink()
