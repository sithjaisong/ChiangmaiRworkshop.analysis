# ----------------------------------------------------------------------------------
# CHIANG MAI WORKSHOP
# TITLE: Cluster Analysis for ratio and interval data
# Programmed by: Ms. Leilani A. Nora 
#                International Rice Research Institute (IRRI)
# Modified by: Alaine A. Gulles of IRRI (22June2012)
# Program Description: 
#     R script for Cluster Analysis for ratio and interval data
#     Input file is a .csv file 
# ----------------------------------------------------------------------------------

# Input filename specification:
  input.datafile <- "dummy_survey.csv"

# Output filename specification:
  output.datafile <- "ClusterIntervalData.csv"
  output.graphfile <- "ClusterIntervalGraph.pdf"
  output.textfile <- "ClusterIntervalOutput.txt"

# Other input speficiations:
  ROWNAME <- "FIELD_NO"       	# column name for row names
  STANDARDIZE <- TRUE      		# FALSE if no standardization, TRUE if with standardization
  START.IP <- "DH"			# first Injury Profile variable name to be used
  END.IP <- "WB" 				# last Injury Profile variable name to be used
  DISTANCE.method <- "manhattan"
  CLUSTER.method <- "ward"
  CLUSTER.NO <- 4
  BORDERCOLOR <- c("red", "blue", "green", "purple")
  displayMember <- TRUE
  
# Specify the working director of the R process
  setwd("E:/ChiangMai Workshop/RScriptsAndData")

# ----------------------------------------------------------------------------------
# Start of the Analysis:

# required library / packages
  library(doBy)
  library(cluster)

# Read data 
  Cluster.data <- read.csv(input.datafile, header = T, row.names = ROWNAME) 
  str(Cluster.data)

# Save the output to a file
  options(width = 100)
  pdf(file = output.graphfile, paper = "a4r")
  sink(file = output.textfile)

# Selection of Injury profile data #####
  start.col.IP <- match(START.IP, names(Cluster.data))
  end.col.IP <- match(END.IP, names(Cluster.data))
  IP.data <- Cluster.data[(start.col.IP):(end.col.IP)]
  print(str(IP.data))

# Main title of the Correlation Output
  options (width = 100)
  cat(format("****************** CLUSTER ANALYSIS ******************"), "\n\n", sep = "")

# Standardize data
  if (STANDARDIZE) { 
     stdClusterData <- scale(IP.data)
  }  else stdClusterData <- IP.data

# Distance matrix
  dist <- dist(stdClusterData, method = DISTANCE.method)

# Cluster analysis
  CLUSTERResult <- hclust(dist, method = CLUSTER.method)

# Dendrogram
  dendro <- as.dendrogram(CLUSTERResult)
  plot(dendro, center = T, nodePar = list(lab.cex = 0.5, lab.col = "blue", pch = NA))

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
  cat("\nCophenetic correlation coefficient = ")
  print(cor(dist,rcluster))
  cat("\n\n")

# summary statistics for each cluster 
  all <- cbind(IP.data, member)
  all$number <- rownames(all)
  rownames(all) <- NULL
  cat('\n',format("Summary statistics for each Cluster", justify="left"))
  cat('\n',format(" ", justify="left")) 
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
  dev.off()
  sink()
