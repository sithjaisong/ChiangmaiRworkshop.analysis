###############################################################
# input file is a .csv  file                                  #
# Use the following variable names                            #
#     Trt for Treatment                                       #
#     Rep for block factor                                    #
#     Year                                                    #
#     Season                                                  #
# Use HSD.test if number of levels of a factor is 6 or more   #
# Programmed by: VBartolome                                   #
# Date: January 2012                                          #
###############################################################
inputfile <- "dummy_field.csv"
Traits <- c("WB","RT","GY")

OutputGraph <-"RCB_graphs.pdf"
output <- "RCB_Year_Season.txt"
Tlevels <- c("DSR","FPR","HYB","WST")
alpha <- .05

#########################################################
library(agricolae)
library(gregmisc)
library(reshape)

data <- read.table(inputfile, sep=",", header=TRUE)
data$Trt <- factor(data$Trt, levels=Tlevels)
data$Rep <- factor(data$Rep)
data$Season <- factor(data$Season)
data$Year <- factor(data$Year)

t <- length(levels(data$Trt))
r <- length(levels(data$Rep))
no.Traits <- length(Traits)
s <- length(levels(data$Season))
y <- length(levels(data$Year))

pdf(file = OutputGraph, paper = "a4"); 
sink(file = output) 

for (trait in 1:no.Traits)  {
    for (year in 1:y)        {
        for (season in 1:s)    {   
   
   title1 <- paste('Trait=',Traits[[trait]],
                              'Year=', levels(data$Year)[[year]],
                              'Season=', levels(data$Season)[[season]])
   cat('\n',format(title1, width=90, justify="left"))

   data$Y <- data[(data$Year==levels(data$Year)[[year]]& 
                  data$Season==levels(data$Season)[[season]]),Traits[[trait]]]
   
   boxplot(data$Y)
   title(title1,cex.main = 1,  col.main= "blue")

   model <- aov(Y ~ Rep + Trt, data=data)
   print(summary(model))
   plot(model$fitted.values,model$residual)
   title(title1,cex.main = 1,  col.main= "blue")
   
   df <- df.residual(model)
   MSerror <- deviance(model)/df
   sink("temp.txt")
   comparison <- LSD.test(data$Y,data$Trt,df,MSerror, alpha=alpha, group=TRUE)
   comparison <- comparison[,1:3]
   comparison <- comparison[order(comparison$trt),]
   comparison[3] <- trim(comparison[3])
   comparison <- rename(comparison,c(means="Trt-mean", M=" "))
   sink()
   print(comparison)
   
}  }  }

dev.off()
sink()