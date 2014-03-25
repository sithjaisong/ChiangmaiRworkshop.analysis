# ----------------------------------------------------------------------------------
# CHIANG MAI WORKSHOP
# TITLE: Split Plot Design
# Programmed by: Ms. Violeta I. Bartolome 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (21June2012)
# Program Description: 
#     R script for Split plot design in RCB Design
#     Input file is a .csv file 
#    	Perform pairwise mean comparison
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.datafile <- "stat1.csv"

# Output filename specification:
  output.graphfile <- "NstatGraph.pdf"
  output.textfile <- "NstatOutput.txt"

# name of the variable to be used:
  MP <- "stage"
  SP <- "soil"
  rep <- "location"
  respvar <- c("Nitrogen") # use c("Yield", "PHeight") for multiple response
  alpha = 0.05
  
# Specify the working director of the R process
setwd("~/Documents/RScriptsandData")

# ----------------------------------------------------------------------------------
# Start of the Analysis:

# Load the packages needed
  library(agricolae)
  library(reshape)
  library(gregmisc)

# Read the CSV file 
  Split.data <- read.csv(input.datafile)

# Convert the MP, SP and rep as factor
  Split.data[,MP] <- factor(Split.data[,MP])
  Split.data[,SP] <- factor(Split.data[,SP])
  Split.data[,rep]  <- factor(Split.data[,rep])

# Determine the number of levels and levels of each factor
  a <- nlevels(Split.data[, MP])
  b <- nlevels(Split.data[, SP])
  MPLevels <- levels(Split.data[, MP])
  SPLevels <- levels(Split.data[, SP])
  if (a <= 5) testMP <- "LSD" else testMP <- "HSD"
  if (b <= 5) testSP <- "LSD" else testSP <- "HSD"

# Determine the number of replicates
  r <- nlevels(Split.data[,rep])

# Determine the number of response variables to be analyzed
  nrespvar <- length(respvar)

# Save the output to a file
  options(width = 100)
  pdf(file = output.graphfile, paper = "a4")
  sink(file = output.textfile)

# Main title of the Output
  options (width = 100)
  cat(format("****************** ANALYSIS OF VARIANCE ******************"), "\n", sep = "")
  cat(format("**************** SPLIT PLOT DESIGN IN RCB ****************"), "\n\n", sep = "")

for (i in (1:nrespvar)) {
	# Display the boxplot
	  boxplot(Split.data[,respvar[i]], 
		    main = paste("Boxplot of", respvar[i]),
		    ylab = respvar[i])

	# Perform Analysis of Variance
	  model <- suppressWarnings(aov(formula(paste(respvar[i], "~", rep, "+", MP, "*", SP, "+ Error((",rep,":",MP,")/", MP,")")), data = Split.data))
	  title1 <- paste("Trait = ", respvar[i], sep = "")
  	  cat(title1, "\n\n")
	  print(summary(model))

	  model2 <- suppressWarnings(aov(formula(paste(respvar[i], "~", rep, "+", MP, "*", SP, "+ ",rep,":",MP)), data = Split.data))
   	  plot(model2$fitted.values, model2$residual, 
		 main = paste("Scatterplot of the fitted model vs residual of ", respvar[i], sep = ""), 
		 xlab = "Fitted Values", ylab = "Residuals")

	# Perform pairwise mean comparison
	  dfa <- df.residual(model[[2]])
	  MSEa <- deviance(model[[2]])/dfa
	  dfb <- df.residual(model[[3]])
	  MSEb <- deviance(model[[3]])/dfb
        dfab <- (MSEa + (b-1) * MSEb)^2/(MSEa^2/dfa +((b - 1) * MSEb)^2/dfb)
        MSEab <- (MSEa + (b-1) * MSEb)/b

	# Comparison of the main effect of MP
	  if (testMP == "LSD") { capture.output(comparison1 <- LSD.test(Split.data[,respvar[i]], Split.data[,MP], dfa, MSEa, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison1 <- HSD.test(Split.data[,respvar[i]], Split.data[,MP], dfa, MSEa, alpha = alpha, group = TRUE)) }
	  
	  comparison1 <- comparison1[,1:3]
	  comparison1 <- comparison1[order(comparison1$trt),]
	  comparison1[3] <- trim(comparison1[3])
	  comparison1 <- rename(comparison1, c(means = paste(MP, "-mean", sep = ""), M = " "))
	  cat("\n\nComparing", MP,"means using", testMP,"test\n\n")
	  print(comparison1, row.names = FALSE)

	# Comparison of the main effect of SP
	  if (testSP == "LSD") { capture.output(comparison3 <- LSD.test(Split.data[,respvar[i]], Split.data[,SP], dfb, MSEb, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison3 <- HSD.test(Split.data[,respvar[i]], Split.data[,SP], dfb, MSEb, alpha = alpha, group = TRUE)) }
	  
	  comparison3 <- comparison3[,1:3]
	  comparison3 <- comparison3[order(comparison3$trt),]
	  comparison3[3] <- trim(comparison3[3])
	  comparison3 <- rename(comparison3, c(means = paste(SP,"-mean", sep = ""), M = " "))
	  cat("\n\nComparing", SP,"means using", testSP,"test\n\n")
	  print(comparison3, row.names = FALSE)

	# Comparison of MP for each level of SP
	  comparison2 <- matrix(0, a, 3*b)
	  dim(comparison2) <- c(a, 3*b)
	  comparison2 <- as.data.frame(comparison2)
	  for (j in (1:b)) {
		k = j + 2*(j - 1)
		if (testMP == "LSD") {
		      capture.output(comparison <- LSD.test(Split.data[,respvar[i]][Split.data[,SP] == SPLevels[[j]]], 
									  Split.data[,MP][Split.data[,SP] == SPLevels[[j]]], 
									  dfab, MSEab, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(Split.data[,respvar[i]][Split.data[,SP] == SPLevels[[j]]], 
									  Split.data[,MP][Split.data[,SP] == SPLevels[[j]]], 
									  dfab, MSEab, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison2[k:(k+2)] <- comparison[1:3]
		colnames(comparison2)[k] <- MP
		colnames(comparison2)[k+1] <- SPLevels[j]
		colnames(comparison2)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:b)) {
		k = j + 2*(j - 1)
		comparison2[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", MP, "at each level of", SP," using",testMP,"test\n\n")
	  print(comparison2, row.names = FALSE)

	# Comparison of SP for each level of MP
	  comparison4 <- matrix(0, b, 3*a)
	  dim(comparison4) <- c(b, 3*a)
	  comparison4 <- as.data.frame(comparison4)
	  for (j in (1:a)) {
		k = j + 2*(j - 1)
		if (testSP == "LSD") {
		      capture.output(comparison <- LSD.test(Split.data[,respvar[i]][Split.data[,MP] == MPLevels[[j]]], 
									  Split.data[,SP][Split.data[,MP] == MPLevels[[j]]], 
									  dfb, MSEb, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(Split.data[,respvar[i]][Split.data[,MP] == MPLevels[[j]]], 
									  Split.data[,SP][Split.data[,MP] == MPLevels[[j]]], 
									  dfb, MSEb, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison4[k:(k+2)] <- comparison[1:3]
		colnames(comparison4)[k] <- SP
		colnames(comparison4)[k+1] <- MPLevels[j]
		colnames(comparison4)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:a)) {
		k = j + 2*(j - 1)
		comparison4[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", SP," at each level of ", MP," using",testSP,"test\n\n")
	  print(comparison4, row.names = FALSE)
	  cat("\n--------------------------------------------------------------------\n\n", sep = "")
}

# Terminate output diversion and graphical diversion
  sink()
  dev.off()

