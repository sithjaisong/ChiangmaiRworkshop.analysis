# ----------------------------------------------------------------------------------
# title         : Introduction to Chi-Square Test of Independence;
# purpose       : R script for Chi-square Test of Independence;
# pprepared by  : Nancy P. Castilla 
# modified by   : Alaine A. Gulles of IRRI 06.21.2012
# ----------------------------------------------------------------------------------
# USER INPUT SPECIFICATION:

# filename of the dataset to be used
  inputfile    <- "ChiSurveyDemo.csv"

# filename where the output will be saved
  outtables    <- "ChiGenOut.txt"

# name of the row and column variables
  rowVar    <- "CLUSIP"
  columnVar <- c("CLUSPS", "Yclass", "SITE")

# Set the working directory of the R process
#setwd("E:/ChiangMai Workshop/RScriptsAndData")
 
#####################################################################################
# Start of the Analysis:

# Send numerical results to an MS Word/text file               
  sink(file = outtables) 

# Read the csv file in table format and create a data frame named "Survey" 
  SurveyData <- read.table(inputfile, sep=",", header=T)
  options (width = 100)



# Main Title of the Output
  cat("\n", format("****************** CHI-SQUARE TEST ******************"), "\n\n", sep = "")

for (i in (1:length(columnVar))) {
	# Create a contingency table
	  ConTab <- table(SurveyData[c(rowVar,columnVar[i])])
	# ConTabAll <- cbind(ConTabAll, ConTab) 

	# Print the table of observed frequency
	  cat("\n", format(paste(rowVar, "X", columnVar[i], "CONTINGENCY TABLE, OBSERVED FREQUENCY")), "\n\n", sep = "")
	  print(ConTab)

	# Perform Chi-square test
	# Turn off Yate's correction for continuity using the option correct=F
	  chisqResult <- chisq.test(ConTab, correct=F)

	# Print the table of expected frequency
	# Round values of expected frequencies up to 3 decimal places (default = 0).
	  cat("\n\n", format(paste(rowVar, "X", columnVar[i], "TABLE, EXPECTED FREQUENCY")), "\n\n", sep = "")
	  print(round(chisqResult$expected, digits = 3))

	# Print the table of residuals
	  cat("\n\n", format(paste(rowVar, "X", columnVar[i], "TABLE, RESIDUALS")), "\n\n", sep = "")
	  print(round(chisqResult$residuals, digits = 3))

	# Print result of chi-square test statistics
	  print(chisqResult)
	  cat("-------------------------------------------------------------\n\n", sep = "")
}

sink()
 
