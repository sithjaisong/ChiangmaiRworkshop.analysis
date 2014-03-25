# ----------------------------------------------------------------------------------
# CHIANG MAI WORKSHOP
# TITLE: Cluster Analysis for different types of data
# Programmed by: Ms. Leilani A. Nora 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (22June2012)
# Program Description: 
#     R script for Cluster Analysis for different types of data
#     Input file is a .csv file 
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.datafile <- "dummy_survey.csv"

# Output filename specification:
  output.datafile <- "ClusterMixedData.csv"
  output.graphfile <- "ClusterMixedGraph.pdf"
  output.textfile <- "ClusterMixedOutput.txt"

# Other input speficiations:
  ROWNAME <- "FIELD_NO"       	# column name for row names
  STANDARDIZE <- TRUE      		# FALSE if no standardization, TRUE if with standardization
  START.PS <- "N"				# first Production Situation variable name to be used
  END.PS <- "VAR" 			# last Production Situation variable name to be used
  BINARY.var <- "CEM"			# binary data
  CLUSTER.method <- "average"	  	# clustering method to be used
  CLUSTER.NO <- 4				# number of clusters
  BORDERCOLOR <- c("red", "blue", "green", "purple")
  displayMember <- TRUE			# use TRUE if members of the cluster will be displayed, otherwise use FALSE
  
# Specify the working director of the R process
 # setwd("E:/ChiangMai Workshop/RScriptsAndData")

# ----------------------------------------------------------------------------------
# Start of the Analysis:

# required library / packages
  library(doBy)
  library(cluster)

# Read data 
  Cluster.data <- read.csv(input.datafile, header = T, row.names = ROWNAME) 

# Save the output to a file
  options(width = 100)
  pdf( file = output.graphfile, paper = "a4r")
  sink(file = output.textfile)

# Selection of Injury profile data 
  start.col.PS <- match(START.PS, names(Cluster.data))
  end.col.PS <- match(END.PS, names(Cluster.data))
  PS.data <- Cluster.data[(start.col.PS):(end.col.PS)]
  print(str(PS.data))

# Main title of the Correlation Output
  options (width = 100)
  cat(format("****************** CLUSTER ANALYSIS ******************"), "\n\n", sep = "")

# Distance matrix
  dist.PS <- suppressWarnings(daisy(PS.data, type = list(symm = BINARY.var)))

# Cluster analysis
  CLUSTERResult <- hclust(dist.PS, method = CLUSTER.method)

# Dendrogram
  dendro <- as.dendrogram(CLUSTERResult)
  plot(dendro, center = T, nodePar = list(lab.cex = 0.6, lab.col = "black", pch = NA), 
       main = "Dendogram for Production Situation")

# draw rectangles
  rect.hclust(CLUSTERResult, k = CLUSTER.NO, border = BORDERCOLOR)

# number of members in each cluster
  member <- cutree(CLUSTERResult, k = CLUSTER.NO)
  cat("\n\nNumber of members in each clusters\n")
  print(table(member))

# display the members of each cluster
  if (displayMember) {
	tempData <- data.frame(member)
	for (i in (1:CLUSTER.NO)) {
 		cat("\n\nMember of Cluster =",i,"\n")
		temp <- rownames(subset(tempData, member == i))
		index <- 1
		for (j in (1:ceiling(length(temp)/15))) {
			if(index+14 > length(temp)) { cat(temp[index:length(temp)], "\n")
			} else { cat(temp[index:(index+14)], "\n") }
			index <- index + 15
		}
	}
  }

# cophenitic correlation
  rcluster <- cophenetic(CLUSTERResult)
  cat("\n\nCophenetic correlation coefficient =", cor(dist.PS, rcluster))
  cat("\n\n")

# summary statistics for each cluster 
  all <- cbind(PS.data, member)
  all$number <- rownames(all)
  rownames(all) <- NULL
  cat("\n",format("Mean for each Cluster", justify="left"), "\n")
  print(summaryBy(.~member, data = all, FUN = c(mean)), digits = 3)
  cat("\n\n",format("Standard Deviation for each Cluster", justify="left"), "\n")
  print(summaryBy(.~member, data = all, FUN = c(sd)), digits = 3)
  cat("\n\n",format("Minimum for each Cluster", justify="left"), "\n")
  print(summaryBy(.~member, data = all, FUN = c(min)), digits = 3)
  cat("\n\n",format("Maximum observation for each Cluster", justify="left"), "\n")
  print(summaryBy(.~member, data = all, FUN = c(max)), digits = 3)

# output to .csv file
  write.table(all, file = output.datafile, row.names = F, sep = ",")

# Termination of graphical device and output diversion
  sink()
  dev.off()
