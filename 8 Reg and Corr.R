# ----------------------------------------------------------------------------------
# CHIANG MIA WORKSHOP
# TITLE: Correlation and Regression Analysis
# Programmed by: Ms. Leilani A. Nora 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (21June2012)
# Program Description: 
#     R script for Correlation and Regression Analysis
#     Input file is a .csv file 
#    	Perform pearson correlation analysis
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.corrfile <- "corr.csv"
  input.regfile <- "DASHeight.csv"

# Output filename specification:
  output.graphfile <- "CorrAndRegGraph.pdf"
  output.textfile <- "CorrAndRegOutput.txt"

# name of the variable to be used for regression analysis:
  corrVar <- c("GY14", "N", "P", "K")
  X <- "DAS"      
  Y <- "Height"
  
# Specify the working director of the R process
  setwd("E:/ChiangMai Workshop/RScriptsAndData")

# ----------------------------------------------------------------------------------
# Start of the Analysis:

# required library / packages
  library(agricolae)

# Read the CSV file for to be used for the analysis
  Corr.data <- read.csv(input.corrfile)
  Reg.data <- read.csv(input.regfile)

# Save the output to a file
  options(width = 100)
  pdf(file = output.graphfile, paper = "a4")
  sink(file = output.textfile)

# Main title of the Correlation Output
  options (width = 100)
  cat(format("****************** CORRELATION ANALYSIS ******************"), "\n\n", sep = "")

# Perform the correlation analysis
  corrResult <- correlation(Corr.data[,corrVar])

# Print the correlation coefficient and the p-value
  cat("Pearson's Correlation Coefficient\n\n")
  print(round(corrResult$correlation, digits = 2))
  cat("\n\np-value:\n\n")
  print(round(corrResult$pvalue, digits = 4))
    

# Main title of the Regression Output
  cat(format("****************** SIMPLE LINEAR REGRESSION ******************"), "\n\n", sep = "")
  
# Perform Regression Analysis: Seedling rate and GY means
  RegModel <- lm(formula(paste(Y,"~",X)), data = Reg.data)
  print(anova(RegModel))
  cat("\n\n")
  print(summary(RegModel))
  cat("\n\n")
  sink() 
    
# Scatterplot and Fitted line
  plot(Reg.data[,X], Reg.data[,Y], main="Scatterplot with Fitted Line", 
       xlab = paste(X), ylab = paste("Mean of",Y), col="Red")
  abline(RegModel, col = "blue", lty = 3, lwd = 2)
  if (RegModel$coef[[2]] < 0) {
  	mtext(paste(Y,".pred = ", RegModel$coef[[1]]," - ",round(abs(RegModel$coef[[2]]),4), " x ",X," with r = ", round(sqrt(summary(RegModel)$r.squared),2), sep = ""),side=3, cex=0.7)      
  } else {
  	mtext(paste(Y,".pred = ", RegModel$coef[[1]]," + ", round(RegModel$coef[[2]],4), " x ",X, " with r = ", round(sqrt(summary(RegModel)$r.squared),2), sep = ""),side=3, cex=0.7)  
  }

# Residual Plot
  plot(RegModel$fitted.values,RegModel$residual, 
       main = "Residual Plot", xlab = "Fitted", ylab = "Residuals", col = "red")		
  abline(h = 0, col = "blue", lty = 3, lwd = 2)

dev.off()
	
	
