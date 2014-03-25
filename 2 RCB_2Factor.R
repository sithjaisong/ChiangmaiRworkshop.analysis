# ----------------------------------------------------------------------------------
# CHIANG MAI WORKSHOP
# TITLE: Randomized Complete Block Design for Several Factors
# Programmed by: Ms. Violeta I. Bartolome 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (21June2012)
# Program Description: 
#     R script for Randomized Complete Block Design for Several Factors
#     Input file is a .csv file 
#    	Perform pairwise mean comparison
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.datafile <- "Fact2.csv"

# Output filename specification:
  output.graphfile <- "RCB2FactorGraph.pdf"
  output.textfile <- "RCB2FactorOutput.txt"

# name of the variable to be used:
  factor1 <- "F1"
  factor2 <- "F2"
  rep <- "rep"
  respvar <- c("Yield") # use c("Yield", "Plant height") for multiple response
  alpha = 0.05
  
# Specify the working director of the R process
  setwd("E:/ChiangMai Workshop/RScriptsAndData")

# ----------------------------------------------------------------------------------
# Start of the Analysis:

# Load the packages needed
  library(agricolae)
  library(gregmisc)
  library(reshape)

# Read the CSV file and create a data frame named "RCB.data"
  RCB.data <- read.csv(input.datafile)

# Convert the factor1, factor2 and rep as factor
  RCB.data[,factor1] <- factor(RCB.data[,factor1])
  RCB.data[,factor2] <- factor(RCB.data[,factor2])
  RCB.data[,rep]  <- factor(RCB.data[,rep])

# Determine the number of levels and levels of each factor
  a <- nlevels(RCB.data[, factor1])
  b <- nlevels(RCB.data[, factor2])
  F1Levels <- levels(RCB.data[, factor1])
  F2Levels <- levels(RCB.data[, factor2])

# Determine the number of replicates
  r <- nlevels(RCB.data[,rep])

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
	  model <- aov(formula(paste(respvar[i], "~", rep, "+", factor1, "*", factor2)), data = RCB.data)
	  title1 <- paste("Trait = ", respvar[i], sep = "")
  	  cat(title1, "\n\n")
	  print(summary(model))
   	  plot(model$fitted.values, model$residual, 
		 main = paste("Scatterplot of the fitted model vs residual of ", respvar[i], sep = ""), 
		 xlab = "Fitted Values", ylab = "Residuals")

	# Perform pairwise mean comparison
	  df <- df.residual(model)
	  MSError <- deviance(model)/df

	# Comparison of the main effect of factor1
	  if (a <= 5) {  
		testF1 <- "LSD"
		capture.output(comparison1 <- LSD.test(RCB.data[,respvar[i]], RCB.data[,factor1], df, MSError, alpha = alpha, group = TRUE))
	  } else {
		testF1 <- "HSD"
		capture.output(comparison1 <- HSD.test(RCB.data[,respvar[i]], RCB.data[,factor1], df, MSError, alpha = alpha, group = TRUE))
	  }
	  
	  comparison1 <- comparison1[,1:3]
	  comparison1 <- comparison1[order(comparison1$trt),]
	  comparison1[3] <- trim(comparison1[3])
	  comparison1 <- rename(comparison1, c(means = paste(factor1, "-mean", sep = ""), M = " "))
	  cat("\n\nComparing", factor1,"means using", testF1,"test\n\n")
	  print(comparison1, row.names = FALSE)

	# Comparison of factor1 for each level of factor2
	  comparison2 <- matrix(0, a, 3*b)
	  dim(comparison2) <- c(a, 3*b)
	  comparison2 <- as.data.frame(comparison2)
	  for (j in (1:b)) {
		k = j + 2*(j - 1)
		if (testF1 == "LSD") {
		      capture.output(comparison <- LSD.test(RCB.data[,respvar[i]][RCB.data[,factor2] == F2Levels[[j]]], 
									  RCB.data[,factor1][RCB.data[,factor2] == F2Levels[[j]]], 
									  df, MSError, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(RCB.data[,respvar[i]][RCB.data[,factor2] == F2Levels[[j]]], 
									  RCB.data[,factor1][RCB.data[,factor2] == F2Levels[[j]]], 
									  df, MSError, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison2[k:(k+2)] <- comparison[1:3]
		colnames(comparison2)[k] <- factor1
		colnames(comparison2)[k+1] <- F2Levels[j]
		colnames(comparison2)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:b)) {
		k = j + 2*(j - 1)
		comparison2[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", factor1, "at each level of", factor2," using",testF1,"test\n\n")
	  print(comparison2, row.names = FALSE)

	# Comparison of the main effect of factor2
	  if (b <= 5) {
		testF2 <- "LSD"  
		capture.output(comparison3 <- LSD.test(RCB.data[,respvar[i]], RCB.data[,factor2], df, MSError, alpha = alpha, group = TRUE))
	  } else {
		testF2 <- "HSD"  
		capture.output(comparison3 <- HSD.test(RCB.data[,respvar[i]], RCB.data[,factor2], df, MSError, alpha = alpha, group = TRUE))
	  }
	  
	  comparison3 <- comparison3[,1:3]
	  comparison3 <- comparison3[order(comparison3$trt),]
	  comparison3[3] <- trim(comparison3[3])
	  comparison3 <- rename(comparison3, c(means = paste(factor2,"-mean", sep = ""), M = " "))
	  cat("\n\nComparing", factor2,"means using", testF2,"test\n\n")
	  print(comparison3, row.names = FALSE)

	# Comparison of factor2 for each level of factor1
	  comparison4 <- matrix(0, b, 3*a)
	  dim(comparison4) <- c(b, 3*a)
	  comparison4 <- as.data.frame(comparison4)
	  for (j in (1:a)) {
		k = j + 2*(j - 1)
		if (testF2 == "LSD") {
		      capture.output(comparison <- LSD.test(RCB.data[,respvar[i]][RCB.data[,factor1] == F1Levels[[j]]], 
									  RCB.data[,factor2][RCB.data[,factor1] == F1Levels[[j]]], 
									  df, MSError, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(RCB.data[,respvar[i]][RCB.data[,factor1] == F1Levels[[j]]], 
									  RCB.data[,factor2][RCB.data[,factor1] == F1Levels[[j]]], 
									  df, MSError, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison4[k:(k+2)] <- comparison[1:3]
		colnames(comparison4)[k] <- factor2
		colnames(comparison4)[k+1] <- F1Levels[j]
		colnames(comparison4)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:a)) {
		k = j + 2*(j - 1)
		comparison4[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", factor2," at each level of ", factor1," using",testF2,"test\n\n")
	  print(comparison4, row.names = FALSE)
	  cat("\n--------------------------------------------------------------------\n\n", sep = "")
}

# Terminate output diversion and graphical diversion
  sink()
  dev.off()

