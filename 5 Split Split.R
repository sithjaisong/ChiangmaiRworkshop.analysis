# ----------------------------------------------------------------------------------
# CHIANG MAI WORKSHOP
# TITLE: Split-Split Plot Design
# Programmed by: Ms. Violeta I. Bartolome 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (21June2012)
# Program Description: 
#     R script for Split-Split plot design in RCB Design
#     Input file is a .csv file 
#    	Perform pairwise mean comparison
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.datafile <- "ssplot.csv"

# Output filename specification:
  output.graphfile <- "SSplitGraph.pdf"
  output.textfile <- "SSplitOutput.txt"

# name of the variable to be used:
  MP <- "MP"
  SP <- "SP"
  SSP <- "SSP"
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
  SSplit.data <- read.csv(input.datafile)

# Convert the MP, SP and rep as factor
  SSplit.data[,MP] <- factor(SSplit.data[,MP])
  SSplit.data[,SP] <- factor(SSplit.data[,SP])
  SSplit.data[,SSP] <- factor(SSplit.data[,SSP])
  SSplit.data[,rep]  <- factor(SSplit.data[,rep])

# Determine the number of levels and levels of each factor
  a <- nlevels(SSplit.data[, MP])
  b <- nlevels(SSplit.data[, SP])
  c <- nlevels(SSplit.data[, SSP])
  MPLevels <- levels(SSplit.data[, MP])
  SPLevels <- levels(SSplit.data[, SP])
  SSPLevels <- levels(SSplit.data[, SSP])
  if (a <= 5) testMP <- "LSD" else testMP <- "HSD"
  if (b <= 5) testSP <- "LSD" else testSP <- "HSD"
  if (c <= 5) testSSP <- "LSD" else testSSP <- "HSD"

# Determine the number of replicates
  r <- nlevels(SSplit.data[,rep])

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
	  boxplot(SSplit.data[,respvar[i]], 
		    main = paste("Boxplot of", respvar[i]),
		    ylab = respvar[i])

	# Perform Analysis of Variance
	  model <- suppressWarnings(aov(formula(paste(respvar[i], "~", rep, " + ", MP, "*", SP, " * ",SSP,"+ Error((",rep,":",MP,"+",rep,":",MP,":",SP,")/(", MP,"+",SP,"))", sep = "")), data = SSplit.data))
	  title1 <- paste("Trait = ", respvar[i], sep = "")
  	  cat(title1, "\n\n")
	  print(summary(model))

	  model2 <- suppressWarnings(aov(formula(paste(respvar[i], "~", rep, " + ", MP, "*", SP, "*",SSP," + ",rep,":",MP, "+",rep,":",MP,":",SP, sep = "")), data = SSplit.data))
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

        dfab <- (MSEa + (b-1) * MSEb)^2/(MSEa^2/dfa +((b - 1) * MSEb)^2/dfb)
        dfac <- (MSEa + (c-1) * MSEc)^2/(MSEa^2/dfa +((c - 1) * MSEc)^2/dfc)
        dfbc <- (MSEb + (c-1) * MSEc)^2/(MSEb^2/dfb +((c - 1) * MSEc)^2/dfc)

        MSEab <- (MSEa + (b-1) * MSEb)/b
        MSEac <- (MSEa + (c-1) * MSEc)/c
        MSEbc <- (MSEb + (c-1) * MSEc)/c

        dfabc <- (MSEa + (b-1) * MSEb + b*(c-1)*MSEc)^2/
		     (MSEa^2/dfa +((b - 1) * MSEb)^2/dfb + (b*(c-1)*MSEc)^2/dfc)
	  MSEabc <- (MSEa + (b-1) * MSEb + b*(c-1)*MSEc)/(b*c)

	# Comparison of the main effect of MP
	  if (testMP == "LSD") { capture.output(comparison1 <- LSD.test(SSplit.data[,respvar[i]], SSplit.data[,MP], dfa, MSEa, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison1 <- HSD.test(SSplit.data[,respvar[i]], SSplit.data[,MP], dfa, MSEa, alpha = alpha, group = TRUE)) }
	  
	  comparison1 <- comparison1[,1:3]
	  comparison1 <- comparison1[order(comparison1$trt),]
	  comparison1[3] <- trim(comparison1[3])
	  comparison1 <- rename(comparison1, c(means = paste(MP, "-mean", sep = ""), M = " "))
	  cat("\n\nComparing", MP,"means using", testMP,"test\n\n")
	  print(comparison1, row.names = FALSE)

	# Comparison of the main effect of SP
	  if (testSP == "LSD") { capture.output(comparison3 <- LSD.test(SSplit.data[,respvar[i]], SSplit.data[,SP], dfb, MSEb, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison3 <- HSD.test(SSplit.data[,respvar[i]], SSplit.data[,SP], dfb, MSEb, alpha = alpha, group = TRUE)) }
	  
	  comparison3 <- comparison3[,1:3]
	  comparison3 <- comparison3[order(comparison3$trt),]
	  comparison3[3] <- trim(comparison3[3])
	  comparison3 <- rename(comparison3, c(means = paste(SP,"-mean", sep = ""), M = " "))
	  cat("\n\nComparing", SP,"means using", testSP,"test\n\n")
	  print(comparison3, row.names = FALSE)

	# Comparison of the main effect of SSP
	  if (testSSP == "LSD") { capture.output(comparison5 <- LSD.test(SSplit.data[,respvar[i]], SSplit.data[,SSP], dfc, MSEc, alpha = alpha, group = TRUE))
	  } else { capture.output(comparison5 <- HSD.test(SSplit.data[,respvar[i]], SSplit.data[,SSP], dfc, MSEc, alpha = alpha, group = TRUE)) }
	  
	  comparison5 <- comparison5[,1:3]
	  comparison5 <- comparison5[order(comparison5$trt),]
	  comparison5[3] <- trim(comparison5[3])
	  comparison5 <- rename(comparison5, c(means = paste(SSP,"-mean", sep = ""), M = " "))
	  cat("\n\nComparing", SSP,"means using", testSSP,"test\n\n")
	  print(comparison5, row.names = FALSE)

	# Comparison of MP for each level of SP
	  comparison2 <- matrix(0, a, 3*b)
	  dim(comparison2) <- c(a, 3*b)
	  comparison2 <- as.data.frame(comparison2)
	  for (j in (1:b)) {
		k = j + 2*(j - 1)
		if (testMP == "LSD") {
		      capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,SP] == SPLevels[[j]]], 
									  SSplit.data[,MP][SSplit.data[,SP] == SPLevels[[j]]], 
									  dfab, MSEab, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,SP] == SPLevels[[j]]], 
									  SSplit.data[,MP][SSplit.data[,SP] == SPLevels[[j]]], 
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

	# Comparison of MP for each level of SSP
	  comparison6 <- matrix(0, a, 3*c)
	  dim(comparison6) <- c(a, 3*c)
	  comparison6 <- as.data.frame(comparison6)
	  for (j in (1:c)) {
		k = j + 2*(j - 1)
		if (testMP == "LSD") {
		      capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  SSplit.data[,MP][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  dfac, MSEac, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  SSplit.data[,MP][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  dfac, MSEac, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison6[k:(k+2)] <- comparison[1:3]
		colnames(comparison6)[k] <- MP
		colnames(comparison6)[k+1] <- SSPLevels[j]
		colnames(comparison6)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:c)) {
		k = j + 2*(j - 1)
		comparison6[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", MP, "at each level of", SSP," using",testSSP,"test\n\n")
	  print(comparison6, row.names = FALSE)


	# Comparison of SP for each level of MP
	  comparison4 <- matrix(0, b, 3*a)
	  dim(comparison4) <- c(b, 3*a)
	  comparison4 <- as.data.frame(comparison4)
	  for (j in (1:a)) {
		k = j + 2*(j - 1)
		if (testSP == "LSD") {
		      capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,MP] == MPLevels[[j]]], 
									  SSplit.data[,SP][SSplit.data[,MP] == MPLevels[[j]]], 
									  dfb, MSEb, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,MP] == MPLevels[[j]]], 
									  SSplit.data[,SP][SSplit.data[,MP] == MPLevels[[j]]], 
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

	# Comparison of SP for each level of SSP
	  comparison7 <- matrix(0, b, 3*c)
	  dim(comparison7) <- c(b, 3*c)
	  comparison7 <- as.data.frame(comparison7)
	  for (j in (1:c)) {
		k = j + 2*(j - 1)
		if (testSP == "LSD") {
		      capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  SSplit.data[,SP][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  dfbc, MSEbc, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  SSplit.data[,SP][SSplit.data[,SSP] == SSPLevels[[j]]], 
									  dfbc, MSEbc, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison7[k:(k+2)] <- comparison[1:3]
		colnames(comparison7)[k] <- SP
		colnames(comparison7)[k+1] <- SSPLevels[j]
		colnames(comparison7)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:c)) {
		k = j + 2*(j - 1)
		comparison7[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", SP," at each level of ", SSP," using",testSP,"test\n\n")
	  print(comparison7, row.names = FALSE)

	# Comparison of SSP for each level of MP
	  comparison8 <- matrix(0, c, 3*a)
	  dim(comparison8) <- c(c, 3*a)
	  comparison8 <- as.data.frame(comparison8)
	  for (j in (1:a)) {
		k = j + 2*(j - 1)
		if (testSSP == "LSD") {
		      capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,MP] == MPLevels[[j]]], 
									  SSplit.data[,SSP][SSplit.data[,MP] == MPLevels[[j]]], 
									  dfc, MSEc, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,MP] == MPLevels[[j]]], 
									  SSplit.data[,SSP][SSplit.data[,MP] == MPLevels[[j]]], 
									  dfc, MSEc, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison8[k:(k+2)] <- comparison[1:3]
		colnames(comparison8)[k] <- SP
		colnames(comparison8)[k+1] <- MPLevels[j]
		colnames(comparison8)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:a)) {
		k = j + 2*(j - 1)
		comparison8[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", SSP," at each level of ", MP," using",testSSP,"test\n\n")
	  print(comparison8, row.names = FALSE)

	# Comparison of SSP for each level of SP
	  comparison9 <- matrix(0, c, 3*b)
	  dim(comparison9) <- c(c, 3*b)
	  comparison9 <- as.data.frame(comparison9)
	  for (j in (1:b)) {
		k = j + 2*(j - 1)
		if (testSSP == "LSD") {
		      capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,SP] == SPLevels[[j]]], 
									  SSplit.data[,SSP][SSplit.data[,SP] == SPLevels[[j]]], 
									  dfc, MSEc, alpha = alpha, group = TRUE))

		} else {
		      capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,SP] == SPLevels[[j]]], 
									  SSplit.data[,SSP][SSplit.data[,SP] == SPLevels[[j]]], 
									  dfc, MSEc, alpha = alpha, group = TRUE))
		}
	      comparison <- comparison[order(comparison$trt),]
		comparison9[k:(k+2)] <- comparison[1:3]
		colnames(comparison9)[k] <- SP
		colnames(comparison9)[k+1] <- SPLevels[j]
		colnames(comparison9)[k+2] <- " "
	  }
	  l = 0
	  for (j in (2:b)) {
		k = j + 2*(j - 1)
		comparison9[k - l] <- NULL
		l <- l + 1
	  }
	  cat("\n\nComparing", SSP," at each level of ", SP," using",testSSP,"test\n\n")
	  print(comparison9, row.names = FALSE)

	# Comparison of MP for each level of SP and SSP
	  cat("\n\nComparing", MP," at each level of ", SP," and ", SSP," using",testMP,"test\n\n")
        for (z in (1:c)) {
		  comparison10 <- matrix(0, a, 3*b)
		  dim(comparison10) <- c(a, 3*b)
		  comparison10 <- as.data.frame(comparison10)
		  for (j in (1:b)) {
			k = j + 2*(j - 1)
			if (testMP == "LSD") {
			      suppressWarnings(capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,SP] == SPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]],
										  SSplit.data[,MP][SSplit.data[,SP] == SPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]],
										  alpha = alpha, group = TRUE, dfabc, MSEabc)))

			} else {
			      suppressWarnings(capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,SP] == SPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]], 
										  SSplit.data[,MP][SSplit.data[,SP] == SPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]], 
										  dfabc, MSEabc, alpha = alpha, group = TRUE)))
			}
		      comparison <- comparison[order(comparison$trt),]
			comparison10[k:(k+2)] <- comparison[1:3]
			colnames(comparison10)[k] <- MP
			colnames(comparison10)[k+1] <- SPLevels[j]
			colnames(comparison10)[k+2] <- " "
	  	}
	  	l = 0
		  for (j in (2:b)) {
			k = j + 2*(j - 1)
			comparison10[k - l] <- NULL
			l <- l + 1
		  }
		  cat("\n\n",SSP,"=", SSPLevels[z],"\n\n")
		  print(comparison10, row.names = FALSE)
	  }
	  
	# Comparison of MP for each level of SP and SSP
	  cat("\n\nComparing", SP," at each level of ", MP," and ", SSP," using",testSP,"test\n\n")
        for (z in (1:c)) {
		  comparison11 <- matrix(0, b, 3*a)
		  dim(comparison11) <- c(b, 3*a)
		  comparison11 <- as.data.frame(comparison11)
		  for (j in (1:a)) {
			k = j + 2*(j - 1)
			if (testSP == "LSD") {
			      suppressWarnings(capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,MP] == MPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]], 
										  SSplit.data[,SP][SSplit.data[,MP] == MPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]], 
										  dfbc, MSEbc, alpha = alpha, group = TRUE)))

			} else {
			      suppressWarnings(capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,SP] == SPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]], 
										  SSplit.data[,MP][SSplit.data[,SP] == SPLevels[[j]] & SSplit.data[,SSP] == SSPLevels[[z]]], 
										  dfbc, MSEbc, alpha = alpha, group = TRUE)))
			}
		      comparison <- comparison[order(comparison$trt),]
			comparison11[k:(k+2)] <- comparison[1:3]
			colnames(comparison11)[k] <- SP
			colnames(comparison11)[k+1] <- MPLevels[j]
			colnames(comparison11)[k+2] <- " "
	  	}
	  	l = 0
		  for (j in (2:a)) {
			k = j + 2*(j - 1)
			comparison11[k - l] <- NULL
			l <- l + 1
		  }
		  cat("\n\n",SSP,"=", SSPLevels[z],"\n\n")
		  print(comparison11, row.names = FALSE)
	  }
	# Comparison of SSP for each level of MP and SP
	  cat("\n\nComparing", SSP," at each level of ", MP," and ", SP," using",testSSP,"test\n\n")
	  for (z in 1:b) {
		comparison12 <- matrix(0,c,3*a)
		dim(comparison12) <- c(c,3*a)
   		comparison12 <- as.data.frame(comparison12)
		for(j in 1:a) {    
		      k = j + 2 * (j - 1)
			if (testSSP == "LSD") {
			      suppressWarnings(capture.output(comparison <- LSD.test(SSplit.data[,respvar[i]][SSplit.data[,MP] == MPLevels[[j]] & SSplit.data[,SP] == SPLevels[[z]]],
							    	 			             SSplit.data[,SSP][SSplit.data[,MP] == MPLevels[[j]] & SSplit.data[,SP] == SPLevels[[z]]],
						     						       alpha = alpha, dfc,MSEc)))
			} else {
			      suppressWarnings(capture.output(comparison <- HSD.test(SSplit.data[,respvar[i]][SSplit.data[,MP] == MPLevels[[j]] & SSplit.data[,SP] == SPLevels[[z]]],
											             SSplit.data[,SSP][SSplit.data[,MP] == MPLevels[[j]] & SSplit.data[,SP] == SPLevels[[z]]],
						     							 alpha = alpha, dfc,MSEc)))
			}

      		comparison <- comparison[order(comparison$trt),]
		      comparison12[k:(k+2)] <- comparison[1:3] 
		      colnames(comparison12)[k] <- "trt"		
		      colnames(comparison12)[k+1] <- MPLevels[[j]]		
		      colnames(comparison12)[k+2] <- " "
      	}
		l = 0
	  	for(j in 2:a) {
      		k = j + 2*(j-1)
      		comparison12[k-l] <- NULL 
      		l = l + 1     
     		}
		  cat("\n\n",SP,"=", SPLevels[z],"\n\n")
		  print(comparison12, row.names = FALSE)
  	  }

	  
	  cat("\n--------------------------------------------------------------------\n\n", sep = "")
}

# Terminate output diversion and graphical diversion
  #sink()
  #dev.off()

