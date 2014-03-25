############################################################################################
# title         : Introduction to Simple Correspondence Analysis;
# purpose       : R script for Simple Correspondence Analysis;
#               : For Review Workshop and;
#               : Statistical Analysis of Crop Health Data in Thailand; 
# prepared by   : Nancy P. Castilla;
# last update   : in IRRI, Los Ba?os, June 2012;



inputfile    <- "CASurveyDemo.csv" # filename of data to be used
outnumeric   <- "CADemoOut.txt" # filename where numeric results will be saved
outgraph     <- "CADemoGraph.png" # filename where graphical display will be saved

rowVar <- c("CLUSIP") # specify row variable(s)
columnVar <- c("CLUSPS", "Yclass", "SITE") # specify column variable(s)

suppcols <- 10:13 # specify number(s) of supplementary column(s) based table named SurveyTab  
maptype <- "symmetric" # specify type of map, check CA package manual for other options

# Set the working directory of the R process
setwd("~/Documents/RScriptsandData")
############################################################################################


# Send numerical results to an MS Word file               
sink(file=outnumeric) 


# Main Title of the Output
cat('\n',format("****CORRESPONDENCE ANALYSIS, NUMERIC RESULTS****"))
cat('\n',format(" ", justify="left"))
cat('\n',format(" ", justify="left"))
# Read the csv file in table format and create a data frame named "Survey" 
Survey<-read.table(inputfile, sep=",", header=T)
options (width=100)

# Name of contingency table to be analyzed
cat('\n',format("Contingency Table to be Analyzed"))
cat('\n',format(" ", justify="left"))
cat('\n',format(" ", justify="left"))
ConTabAll <- NULL

for (i in (1:length(columnVar))) 
  {
  # Create a contingency table
  ConTab <- table(Survey[c(rowVar,columnVar[i])])
  ConTabAll <- cbind(ConTabAll, ConTab)
  }
print(SurveyTable <- ConTabAll)

# Load library CA               
library(ca)
cat('\n',format("<<< SIMPLE CORRESPONDENCE ANALYSIS OUTPUT >>>"))
cat('\n',format(" ", justify="left"))
# Perform simple correspondence analysis (CA) of the data in SurveyTab
caSurvey<-ca(SurveyTable)

# Perform simple CA with columns 10 to 13 as supplementary
caSurvey<-ca(SurveyTable, supcol=suppcols)
cat('\n',format("NAMES OF ENTRIES"))
cat('\n',format(" ", justify="left"))

# Show the list of all entries
names(caSurvey)
# Specifies the minimum number of significant digits 
options(digits=3)

# Print the numerical results of CA
cat('\n',format("-------------------------GENERAL OUTPUT-------------------------"))
cat('\n',format(" ", justify="left"))
print(caSurvey)

cat('\n',format("------------------------DETAILED OUTPUT-------------------------"))
cat('\n',format(" ", justify="left"))
# Print the detailed numerical results  
summary(caSurvey)

# Save output graph to png file
png(file = outgraph)
quality = 100

# Print symmetric map (default), check manual for other options with different symbols for profiles
plot(caSurvey, map=maptype, pch = c(16, 17, 22, 15))

# Create a three-dimensional display of the CA
plot3d(caSurvey)

# Specify the name and color of main title and labels and colors of axes of the graph
title(main="CORRESPONDENCE ANALYSIS", col.main="red", xlab="Axis 1", ylab="Axis 2", col.lab="blue")

#Return output to the terminal.
dev.off()
sink ()
