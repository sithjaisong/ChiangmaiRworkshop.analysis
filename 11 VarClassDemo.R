######################################################################################################
# title         : Creating Classes of Variables;
# purpose       : R script for Creating Classes or Categories of Variables;
# prepared by   : Nancy P. Castilla;
# last update   : in IRRI, Los Baños, June 2012;

# outputs       : data sorted according to levels of selected variable; histogram
#               : minimum, average, maximum values and count of selected variable
#               : frequency distribution, classes coded according to frequency table

inputfile <- "FreqSurveyDemo.csv" #specify input file
outtext  <- "ClassesDemo.doc" #specify name of sink file
outcsv1 <- "SortedData.csv" #specify file name of variables sorted based on values of selected variable
outcsv2 <- "VarCoded.csv" #specify file name of variables with additional column for classes 
outputgraph <-"VarHist.png" #specify file name of histogram for selected variable

#Specify variable
Variable = "Yield"

#Specify values
histminval <- 1.5 # minimum value of histogram x axis
histmaxval <- 10 # maximum value of histogram x axis
histinterval <- 0.5 # increment value of histogram x axis

breaknum = 6   #specify number of break points
break1 <- 0    #specify value of 1st break point
break2 <- 2.6  #specify value of 2nd break point
break3 <- 3.5 #specify value of 3rd break point
break4 <- 5.75 #specify value of 4th break point, put "#" before line if not applicable
break5 <- 8   #specify value of 5th break point, put "#" before line if not applicable
break6 <- 10   #specify value of 6th break point, put "#" before line if not applicable

#Specify codes
code1 <- "Class_1" #specify code for 1st class interval 
code2 <- "Class_2" #specify code for 2nd class interval 
code3 <- "Class_3" #specify code for 3rd class interval, put "#" before line if not applicable
code4 <- "Class_4" #specify code for 4th class interval, put "#" before line if not applicable 
code5 <- "Class_5" #specify code for 5th class interval, put "#" before line if not applicable 

# Set the working directory of the R process
setwd("E:/ChiangMai Workshop/RScriptsAndData")
######################################################################################################

#Send numerical results to an MS Word file               
sink(file=outtext) 

# Read the csv file in table format and create a data frame named "Data" 
Data<-read.table(inputfile, sep=",", header=T)
options (width=100)

Y.col <- match(Variable, names(Data)) 
#Data$Y <-Data[,Y.col]
Data$Y <-Data[,Variable]

# Sort data in ascending order according to values of selected variable
Varsorted<-Data[order(Data$Y),]
DropY <-names(Varsorted) %in% c("Y")
SortedNew <- Varsorted[!DropY]

# Output to .csv file, all variables sorted according to ascending order of selected variable
write.table(SortedNew, file=outcsv1, sep=",",row.names=F)
cat("\n", format("***************** CREATING CATEGORIES OR CLASSES *****************"), "\n\n", sep = "")
title1 <- paste(" ",'VARIABLE =',Variable)
print(title1, quote = F)
cat('\n',format(" ", justify="left"))

# Show minimum, average and maximum values and count of selected variable 
cat('\n',format("MINIMUM VALUE = "))
min(Data$Y)
cat('\n',format("AVERAGE VALUE = "))
mean (Data$Y)
cat('\n',format("MAXIMUM VALUE = "))
max(Data$Y)
cat('\n',format("TOTAL NO. OF OBSERVATIONS (COUNT) = "))
length(Data$Y)

# Create histogram
bins=seq(histminval,histmaxval,by=histinterval)
bins
png(file=outputgraph)
quality=100
hist(Data$Y, breaks=bins, col="khaki1", border="brown", 
     xlab="Values", col.lab = "blue",  ylab="Frequency", 
     main='Frequency Distribution', col.main= "red")

# Create Stem-and-Leaf plot
cat('\n',format("STEM-AND-LEAF PLOT"))
stem(Data$Y) 

# Create frequency table
cat('\n',format("Frequency Table"), "\n\n", sep = "")
if (breaknum == 3)
{print(VarFreq3<- table(cut((Data$Y), b=c(break1, break2, break3))))}
if (breaknum == 4)
{print(VarFreq4<- table(cut((Data$Y), b=c(break1, break2, break3, break4))))}
if (breaknum == 5)
{print(VarFreq5<- table(cut((Data$Y), b=c(break1, break2, break3, break4, break5))))}
 if (breaknum == 6)
{print(VarFreq6<- table(cut((Data$Y), b=c(break1,break2,break3,break4,break5,break6))))}

# Indicate cumulative frequency
cat('\n',format("Cumulative Frequency"),"\n\n", sep = "")
if (breaknum == 3) {print(cumsum(VarFreq3))}
if (breaknum == 4) {print(cumsum(VarFreq4))}
if (breaknum == 5) {print(cumsum(VarFreq5))}
if (breaknum == 6) {print(cumsum(VarFreq6))}

# Code selected variable based on results of frequency table
# Create new variable named "Class"
if (breaknum == 3)
 {Data$Class [Data$Y <= break3] <- code2
 Data$Class [Data$Y <= break2] <- code1}
if (breaknum == 4)
{Data$Class [Data$Y <= break4] <- code3
Data$Class [Data$Y <= break3] <- code2
Data$Class [Data$Y <= break2] <- code1}
if (breaknum == 5)
{Data$Class [Data$Y <= break5] <- code4
Data$Class [Data$Y <= break4] <- code3
Data$Class [Data$Y <= break3] <- code2
Data$Class [Data$Y <= break2] <- code1}
if (breaknum == 6)
{Data$Class [Data$Y <= break6] <- code5
Data$Class [Data$Y <= break5] <- code4
Data$Class [Data$Y <= break4] <- code3
Data$Class [Data$Y <= break3] <- code2
Data$Class [Data$Y <= break2] <- code1}
cat('\n',format(" ", justify="left"))

# Include in output text file selected variable and classes 
DataVar <-cbind(Data$Y, Data$Class)
colnames(DataVar) <- c( paste(" ", Variable), "Class")
cat('\n',format(" ", justify="left"))
print(DataVar, quote=F)

# Output to .csv file; file contains all variables & new column: Y coded based on frequency table
DropYClass <-names(Data) %in% c(Variable, "Class")
DataNew <- Data[!DropYClass]
DataVarClass<-cbind(DataNew, DataVar)
write.table(DataVarClass, file=outcsv2, quote=F, row.names=F, sep=",",)

dev.off()
sink()
