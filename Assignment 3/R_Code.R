#  Clear the environment
rm (list = ls())

# Get Current Working Directory and store it in dir1 variable 
dir1 <- "C:/Users/siddh/OneDrive - The University of Texas at Dallas/Adv_Stats/HW/HW2"

# Set your working Directory by using the Following Command
# setwd("C:\Users\saura\Box\TA-Saurabh\22F\6359\Homework\Homework 1\Sec-003")
setwd(dir1)

# your info (NetID_LastName_FirstName); write to Console
name1 <- "SXB220123_Bhujade_Siddhant";name1

# Load Moments library  (do not install any package in any assignments)
library(moments)

# Create output file name using name1.
csvfile <- paste(name1,"_HW2.csv",sep=""); csvfile

# Instruct to send all the output of your calculation  to a csv file 
sink(csvfile)

# Use cat function to write your name
cat("NAME",  sep = ","   ,  "Siddhant Bhujade", "\n")

# Use cat function to write your netid
cat("NETID", sep=",", "SXB220123","\n")

# Use this cat function to print the Part A in your output
cat("Part A","\n")

# a) None of the LED bulbs are defective?
ans1 = dbinom(10,10,0.95)
cat("None of the LED bulbs are defective?",sep=",",ans1,"\n")

# b) Exactly one of the LED bulbs is defective?
ans2 = dbinom(1,10,0.05)
cat("Exactly one of the LED bulbs is defective?",sep=",",ans2,"\n")

# c) Two or fewer of the LED bulbs are defective?
ans3 <- pbinom(2,10,0.05)
cat("Two or fewer of the LED bulbs are defective?",sep=",",ans3,"\n")

# d) Three or more of the LED bulbs are defective
ans4 <- 1-ans3
cat("Three or more of the LED bulbs are defective",sep=",",ans4,"\n")

# Use this cat function to print the Part B in your output
cat("Part B","\n")

# a) Probability that the agent sells some policies is
ans5 <- 1- dpois(0,3)
cat("Probability that the agent sells some policies is",sep=",",ans5,"\n")

# b. Agent sells 2 or more but less than 5 policies
ans6 <- sum(dpois(2:4,3))
cat("Agent sells 2 or more but less than 5 policies",sep=",",ans6,"\n")

# Use this cat function to print the Part C in your output
cat("Part C","\n")

# Assume  Mean = 80, Sigma = 13
# Find P(X ≥  92)
ans7 <- pnorm(92,80,13,FALSE)
cat("P(X ≥  92)",sep=",",ans7,"\n")

# Find P(72 ≤ X ≤ 95)
ans8 = pnorm(95,80,13,TRUE)-pnorm(72,80,13,TRUE)
cat("P(72 ≤ X ≤ 95)",sep=",",ans8,"\n")

# Find the cut-off for top 10%.
ans9 = qnorm(0.90,80,13,TRUE)
cat("Find the cut-off for top 10%",sep=",",ans9,"\n")

# Find the cutoff for bottom 12%.
ans10 = qnorm(0.12,80,13,TRUE)
cat(" Find the cutoff for bottom 12%",sep=",",ans10,"\n")

# t-distribution
# Assume  Mean = 80,  std dev = 13, sample size = 23
# Find the cut-off for top 10%.
ans11 = qt(p=0.9,df=22,lower.tail = TRUE)*13 +80
cat("Find the cut-off for top 10%.",sep=",",ans11,"\n")

# Find the cutoff for bottom 12%.
ans12 = qt(p=0.12,df=22,lower.tail = TRUE)*13 +80
cat("Find the cutoff for bottom 12%..",sep=",",ans12,"\n")

# Generate a normal distribution dataset with mean = 83, sigma = 27, n  = 200
# Assign it to a vector, say, nm1.  Assume nm1 is a population
nm1<-rnorm(200,83,27)
nm1_int = as.integer(nm1)

# For nm1, find the following  and print the values and label. 
#Mean
Mean = mean(nm1_int)
cat("Mean" , sep="," , Mean , "\n")

#Population Std Dev (R gives sample std dev.  Convert it into population std dev)
sd_population = sqrt(199/200)*sd(nm1_int)
cat("Standard Deviation" , sep="," , sd_population, "\n")

#Skewness
Skewness = skewness(nm1_int)
cat("Skewness" , sep="," , Skewness, "\n")

# Numbers outside µ ± 2σ will be treated as outliers.  Determine the upper and lower
# cut-off numbers.  Also the number of outliers.  Label everything.
# What is the upper cut-off number?
Upper = Mean + (2*sd_population)
cat("Upper Cut-off Number" , sep="," , Upper, "\n")

# What is the lower cutoff number? 
Lower = Mean - (2*sd_population)
cat("Lower Cut-off Number" , sep="," , Lower, "\n")

# How many numbers are more than the upper cutoff number?  Use length function.
no_greater_than_cutoff = length(subset(nm1_int,nm1_int> Upper))
cat("Numbers above upper cut-off" , sep="," , no_greater_than_cutoff, "\n")

# How many numbers are below the lower cutoff number?  Use length function. 
no_lower_than_cutoff = length(subset(nm1_int,nm1_int< Lower))
cat("Numbers below lower cut-off" , sep="," , no_lower_than_cutoff, "\n")

# create a divider line
cat("_________________________", "\n")

# Write summary statistics of nm1
summary(nm1_int)

# create a divider line
cat("_________________________", "\n")

# write the vector nm1 in one column
cat(nm1_int, sep="\n")

# Stop writing to the CSV file.  
sink()

# Partition the graph area into 4 parts (2 rows and 2 columns). This will print 
par(mfrow=c(2,2))

#  all 4 graphs below on one screen. 
# density plot of nm1
d1<-density(nm1_int);  plot(d1)

# boxplot of nm1
b1 <- boxplot(nm1_int)

# Histogram of nm1
h1 <- hist(nm1)

# qqplot of nm1
qqnorm(nm1)


