# ----------------------------------------------------------------------------------
# CHIANG MAI WORKSHOP
# TITLE: Randomized Complete Block Design for Single Factor
# Programmed by: Ms. Violeta I. Bartolome 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (18June2012)
# Program Description: 
#     R script for Randomized Complete Block Design for Single Factor
#     Input file is a .csv file 
#    	Perform pairwise mean comparison
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.datafile <- "RCB.csv"

# Output filename specification:
  output.graphfile <- "RCBGraphs.pdf"
  output.textfile <- "RCBOutput.txt"

# input variables:
  trmt <- "Trt"
  rep <- "rep"
  respvar <- c("Yield")
  alpha = 0.05
  
# Specify the working director of the R process
setwd("~/Documents/RScriptsandData")

# ----------------------------------------------------------------------------------
# Start of the Analysis:

# Load the packages needed
  library(agricolae)
  library(gregmisc)
  library(reshape)

# Read the CSV file and create a data frame named "RCB.data"
  RCB.data <- read.csv(input.datafile)

# Convert the trmt and rep as factor
  RCB.data[,trmt] <- factor(RCB.data[,trmt])
  RCB.data[,rep]  <- factor(RCB.data[,rep])

# Determine the treatment levels
  ntrmt <- nlevels(RCB.data[,trmt])
  if (ntrmt <= 5) ptest <- "LSD" else ptest <- "HSD"

# Determine the number of replicates
  nrep <- nlevels(RCB.data[,rep])

# Determine the number of response variables to be analyzed
  nrespvar <- length(respvar)

# Save the output to a file
  options(width = 100)
  pdf(file = output.graphfile, paper = "a4")
  sink(file = output.textfile)

# Main title of the Output
  options (width = 100)
  cat(format("****************** ANALYSIS OF VARIANCE ******************"), "\n\n", sep = "")

for (i in (1:nrespvar)) {
	# Display the boxplot
	  boxplot(RCB.data[,respvar[i]], 
		    main = paste("Boxplot of", respvar[i]),
		    ylab = respvar[i])

	# Perform Analysis of Variance
	  model <- aov(formula(paste(respvar[i], "~", rep, "+", trmt)), data = RCB.data)
	  title1 <- paste("Response Variable = ", respvar[i], sep = "")
  	  cat(title1, "\n\n")
	  print(summary(model))
   	  plot(model$fitted.values, model$residual, 
		 main = paste("Scatterplot of the fitted model vs residual of ", respvar[i], sep = ""), 
		 xlab = "Fitted Values", ylab = "Residuals")

	# Perform pairwise mean comparison
	  df <- df.residual(model)
	  MSError <- deviance(model)/df
	  if (ptest == "LSD") { capture.output(comparison <- HSD.test(RCB.data[,respvar[i]], RCB.data[,trmt], df, MSError, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison <- HSD.test(RCB.data[,respvar[i]], RCB.data[,trmt], df, MSError, alpha = alpha, group = TRUE))  }
		  
	  comparison <- comparison[,1:3]
	  comparison <- comparison[order(comparison$trt),]
	  comparison[3] <- trim(comparison[3])
	  comparison <- rename(comparison, c(means = "Trt-mean", M = " "))
	  cat("\n\nResult of Pairwise Mean Comparison using", ptest,"test\n\n")
	  print(comparison, row.names = FALSE)
	  cat("\n--------------------------------------------------------------------\n\n", sep = "")
}

# Terminate output diversion and graphical diversion
  sink()
  dev.off()