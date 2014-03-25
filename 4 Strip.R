# ----------------------------------------------------------------------------------
# CHIANG MAI WORKSHOP
# TITLE: Strip Plot Design
# Programmed by: Ms. Violeta I. Bartolome 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (21June2012)
# Program Description: 
#     R script for Strip Plot Design
#     Input file is a .csv file 
#    	Perform pairwise mean comparison
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.datafile <- "Strip.csv"

# Output filename specification:
  output.graphfile <- "StripGraph.pdf"
  output.textfile <- "StripOutput.txt"

# name of the variable to be used:
  HF <- "HF"
  VF <- "VF"
  rep <- "rep"
  respvar <- c("Yield") # use c("Yield", "PHeight") for multiple response
  alpha = 0.05
  
# Specify the working director of the R process
  setwd("E:/ChiangMai Workshop/RScriptsAndData")

# ----------------------------------------------------------------------------------
# Start of the Analysis:

# Load the packages needed
  library(agricolae)
  library(reshape)
  library(gregmisc)

# Read the CSV file 
  Strip.data <- read.csv(input.datafile)

# Convert the HF, VF and rep as factor
  Strip.data[,HF] <- factor(Strip.data[,HF])
  Strip.data[,VF] <- factor(Strip.data[,VF])
  Strip.data[,rep] <- factor(Strip.data[,rep])

# Determine the number of levels and levels of each factor
  a <- nlevels(Strip.data[, HF])
  b <- nlevels(Strip.data[, VF])
  HFLevels <- levels(Strip.data[, HF])
  VFLevels <- levels(Strip.data[, VF])
  if (a <= 5) testHF <- "LSD" else testHF <- "HSD"
  if (b <= 5) testVF <- "LSD" else testVF <- "HSD"

# Determine the number of replicates
  r <- nlevels(Strip.data[,rep])

# Determine the number of response variables to be analyzed
  nrespvar <- length(respvar)

# Save the output to a file
  options(width = 100)
  pdf(file = output.graphfile, paper = "a4")
  sink(file = output.textfile)

# Main title of the Output
  options (width = 100)
  cat(format("****************** ANALYSIS OF VARIANCE ******************"), "\n", sep = "")
  cat(format("******************** STRIP PLOT DESIGN *******************"), "\n\n", sep = "")

for (i in (1:nrespvar)) {
	# Display the boxplot
	  boxplot(Strip.data[,respvar[i]], 
		    main = paste("Boxplot of", respvar[i]),
		    ylab = respvar[i])

	# Perform Analysis of Variance
	  model <- suppressWarnings(aov(formula(paste(respvar[i], " ~ ", rep, "+", HF, "*", VF, "+ Error((",rep,":",HF,"+",rep,":",VF,")/(",HF,"+",VF,"))", sep = "")), data = Strip.data))
	  title1 <- paste("Trait = ", respvar[i], sep = "")
  	  cat(title1, "\n\n")
	  print(summary(model))

	  model2 <- suppressWarnings(aov(formula(paste(respvar[i], "~", rep, "+", HF, "*", VF, "+",rep,":",HF, "+", rep, ":", VF, sep = "")), data = Strip.data))
   	  plot(model2$fitted.values, model2$residual, 
		 main = paste("Scatterplot of the fitted model vs residual of ", respvar[i], sep = ""), 
		 xlab = "Fitted Values", ylab = "Residuals")

	# Perform pairwise mean comparison
	  dfa <- df.residual(model[[2]])
	  dfb <- df.residual(model[[3]])
	  dfc <- df.residual(model[[4]])
	  MSEa <- deviance(model[[2]])/dfa
	  MSEb <- deviance(model[[3]])/dfb
	  MSEc <- deviance(model[[4]])/dfc

        dfac <- (MSEa + (b-1) * MSEc)^2/(MSEa^2/dfa +((b - 1) * MSEc)^2/dfc)
        dfbc <- (MSEb + (a-1) * MSEc)^2/(MSEb^2/dfb +((a - 1) * MSEc)^2/dfc)
        MSEac <- (MSEa + (b-1) * MSEc)/b
        MSEbc <- (MSEb + (a-1) * MSEc)/a

	# Comparison of the main effect of HF
	  if (testHF == "LSD") {  capture.output(comparison1 <- LSD.test(Strip.data[,respvar[i]], Strip.data[,HF], dfa, MSEa, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison1 <- HSD.test(Strip.data[,respvar[i]], Strip.data[,HF], dfa, MSEa, alpha = alpha, group = TRUE))  }
	  
	  comparison1 <- comparison1[,1:3]
	  comparison1 <- comparison1[order(comparison1$trt),]
	  comparison1[3] <- trim(comparison1[3])
	  comparison1 <- rename(comparison1, c(means = paste(HF, "-mean", sep = ""), M = " "))
	  cat("\n\nComparing", HF,"means using", testHF,"test\n\n")
	  print(comparison1, row.names = FALSE)

	# Comparison of the main effect of VF
	  if (testVF == "LSD") { capture.output(comparison3 <- LSD.test(Strip.data[,respvar[i]], Strip.data[,VF], dfb, MSEb, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison3 <- HSD.test(Strip.data[,respvar[i]], Strip.data[,VF], dfb, MSEb, alpha = alpha, group = TRUE)) }
	  
	  comparison3 <- comparison3[,1:3]
	  comparison3 <- comparison3[order(comparison3$trt),]
	  comparison3[3] <- trim(comparison3[3])
	  comparison3 <- rename(comparison3, c(means = paste(VF,"-mean", sep = ""), M = " "))
	  cat("\n\nComparing", VF,"means using", testVF,"test\n\n")
	  print(comparison3, row.names = FALSE)

	# Comparison of HF for each level of VF
	  comparison2 <- matrix(0, a, 3*b)
	  dim(comparison2) <- c(a, 3*b)
	  comparison2 <- as.data.frame(comparison2)
	  for (j in (1:b)) {
		k = j + 2*(j - 1)
		if (testHF == "LSD") {
		      capture.output(comparison <- LSD.test(Strip.data[,respvar[i]][Strip.data[,VF] == VFLevels[[j]]], 
									  Strip.data[,HF][Strip.data[,VF] == VFLevels[[j]]], 
									  dfac, MSEac, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(Strip.data[,respvar[i]][Strip.data[,VF] == VFLevels[[j]]], 
									  Strip.data[,HF][Strip.data[,VF] == VFLevels[[j]]], 
									  dfac, MSEac, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison2[k:(k+2)] <- comparison[1:3]
		colnames(comparison2)[k] <- HF
		colnames(comparison2)[k+1] <- VFLevels[j]
		colnames(comparison2)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:b)) {
		k = j + 2*(j - 1)
		comparison2[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", HF, "at each level of", VF," using",testHF,"test\n\n")
	  print(comparison2, row.names = FALSE)

	# Comparison of VF for each level of HF
	  comparison4 <- matrix(0, b, 3*a)
	  dim(comparison4) <- c(b, 3*a)
	  comparison4 <- as.data.frame(comparison4)
	  for (j in (1:a)) {
		k = j + 2*(j - 1)
		if (testVF == "LSD") {
		      capture.output(comparison <- LSD.test(Strip.data[,respvar[i]][Strip.data[,HF] == HFLevels[[j]]], 
									  Strip.data[,VF][Strip.data[,HF] == HFLevels[[j]]], 
									  dfbc, MSEbc, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(Strip.data[,respvar[i]][Strip.data[,HF] == HFLevels[[j]]], 
									  Strip.data[,VF][Strip.data[,HF] == HFLevels[[j]]], 
									  dfbc, MSEbc, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison4[k:(k+2)] <- comparison[1:3]
		colnames(comparison4)[k] <- VF
		colnames(comparison4)[k+1] <- HFLevels[j]
		colnames(comparison4)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:a)) {
		k = j + 2*(j - 1)
		comparison4[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", VF," at each level of ", HF," using",testVF,"test\n\n")
	  print(comparison4, row.names = FALSE)
	  cat("\n--------------------------------------------------------------------\n\n", sep = "")
}

# Terminate output diversion and graphical diversion
  sink()
  dev.off()

