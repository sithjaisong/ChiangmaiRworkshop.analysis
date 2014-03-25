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

OutputGraph <-"Combine_graphs.pdf"
output <- "Combine_Year_Season.txt"
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

Ylevels <- levels(data$Year)
Slevels <- levels(data$Season)

t <- length(levels(data$Trt))
r <- length(levels(data$Rep))
no.Traits <- length(Traits)
s <- length(levels(data$Season))
y <- length(levels(data$Year))

pdf(file = OutputGraph, paper = "a4"); 
sink(file = output) 

for (trait in 1:no.Traits)  {
    
   title1 <- paste('Trait=',Traits[[trait]])
   cat('\n',format(title1, justify="left"))
   cat('\n',format(" ", justify="left")) 

   data$Y <- data[,Traits[[trait]]]
   
   model <- aov(Y ~ Year + Season + Year:Season + Rep:Year:Season
              + Trt + Trt:Year + Trt:Season + Trt:Year:Season, data=data)
   out <- anova(model)
   plot(model$fitted.values,model$residual)
   title(title1,cex.main = 1,  col.main= "blue")
   
#### ANOVA table

Ydf <- out[rownames(out)=="Year", "Df"]
Sdf <- out[rownames(out)=="Season", "Df"]
YSdf <- out[rownames(out)=="Year:Season", "Df"]
RYSdf <- out[rownames(out)=="Year:Season:Rep", "Df"]
Tdf <- out[rownames(out)=="Trt", "Df"]
TYdf <- out[rownames(out)=="Year:Trt", "Df"]
TSdf <- out[rownames(out)=="Season:Trt", "Df"]
TYSdf <- out[rownames(out)=="Year:Season:Trt", "Df"]
Edf <- out[rownames(out)=="Residuals", "Df"]

YSS <- out[rownames(out)=="Year", "Sum Sq"]
SSS <- out[rownames(out)=="Season", "Sum Sq"]
YSSS <- out[rownames(out)=="Year:Season", "Sum Sq"]
RYSSS <- out[rownames(out)=="Year:Season:Rep", "Sum Sq"]
TSS <- out[rownames(out)=="Trt", "Sum Sq"]
TYSS <- out[rownames(out)=="Year:Trt", "Sum Sq"]
TSSS <- out[rownames(out)=="Season:Trt", "Sum Sq"]
TYSSS <- out[rownames(out)=="Year:Season:Trt", "Sum Sq"]
ESS <- out[rownames(out)=="Residuals", "Sum Sq"]

YMS <- out[rownames(out)=="Year", "Mean Sq"]
SMS <- out[rownames(out)=="Season", "Mean Sq"]
YSMS <- out[rownames(out)=="Year:Season", "Mean Sq"]
RYSMS <- out[rownames(out)=="Year:Season:Rep", "Mean Sq"]
TMS <- out[rownames(out)=="Trt", "Mean Sq"]
TYMS <- out[rownames(out)=="Year:Trt", "Mean Sq"]
TSMS <- out[rownames(out)=="Season:Trt", "Mean Sq"]
TYSMS <- out[rownames(out)=="Year:Season:Trt", "Mean Sq"]
EMS <- out[rownames(out)=="Residuals", "Mean Sq"]

YF <- YMS/RYSMS
SF <- SMS/YSMS
YSF <- YSMS/RYSMS
RYSF <- NA
TF <- TMS/TYMS
TYF <- TYMS/EMS
TSF <- TSMS/TYSMS
TYSF <- TYSMS/EMS
EF <- NA

YProb <- 1-round(pf(YF,Ydf,RYSdf),6)
SProb <- 1-round(pf(SF,Sdf,YSdf),6)
YSProb <- 1-round(pf(YSF,YSdf,RYSdf),6)
RYSProb <- NA
TProb <- 1-round(pf(TF,Tdf,TYdf),6)
TYProb <- 1-round(pf(TYF,TYdf,Edf),6)
TSProb <- 1-round(pf(TSF,TSdf,TYSdf),6)
TYSProb <- 1-round(pf(TYSF,YSdf,Edf),6)
EProb <- NA

SV <- format(c("Year", "Season", "YearxSeason", "Rep(YxS)", "Trt",
       "TrtxY", "TrtxS", "TrtxYxS", "Error"), justify="left")

DF <- format(rbind(Ydf,Sdf,YSdf,RYSdf,Tdf,TYdf,TSdf,TYSdf,Edf), justify="right")
SS <- format(round(rbind(YSS,SSS,YSSS,RYSSS,TSS,TYSS,TSSS,TYSSS,ESS),6),justify="right")
MS <- format(round(rbind(YMS,SMS,YSMS,RYSMS,TMS,TYMS,TSMS,TYSMS,EMS),6), justify="right")
Fvalue <- rbind(YF,SF,YSF,RYSF,TF,TYF,TSF,TYSF,EF)
Prob <-rbind(YProb,SProb,YSProb,RYSProb,TProb,TYProb,TSProb,TYSProb,EProb)

AOV <- as.table(cbind(SV, DF, SS, MS, Fvalue, Prob))
colnames(AOV) <- c("SV", "df", "SS", "MS", "F", "Prob")
rownames(AOV) <- NULL

title2 <- 'Analysis of Variance'
cat('\n',format(title2, justify="left"))
cat('\n',format(" ", justify="left")) 
print(AOV, quotes=F)

#### comparison of main effect of Trt  #######

   sink(file="temp.txt")
   comparison1 <- LSD.test(data$Y,data$Trt,TYdf,TYMS,alpha=alpha,group=TRUE)
   comparison1 <- comparison1[,1:3]
   comparison1 <- comparison1[order(comparison1$trt),]
   comparison1[3] <- trim(comparison1[3])
   sink()
   title3 <- 'Comparison of Main effect of Trt'
   cat('\n',format(title3, justify="left"))
   cat('\n',format(" ", justify="left"))

   comparison1 <- rename(comparison1,c(means="Trt-mean", M=" "))
   print(comparison1)

#### comparison of Trt at each Year (averaged over seasons) #####

   sink(file="temp.txt")
   comparison2 <- matrix(0,t,3*y)
   dim(comparison2) <- c(t,3*y)
   comparison2 <- as.data.frame(comparison2)
   for(i in 1:y) {    
      j=i+2*(i-1)
      comparison <- LSD.test(data$Y[data$Year==Ylevels[[i]]],
          data$Trt[data$Year==Ylevels[[i]]],alpha=alpha,Edf,EMS)
      comparison <- comparison[order(comparison$trt),]
      comparison2[j:(j+2)] <- comparison[1:3] 
      colnames(comparison2)[j] <- "trt"
      colnames(comparison2)[j+1] <- Ylevels[[i]]
      colnames(comparison2)[j+2] <- " "
      }
  k=0
  for(i in 2:y) {
      j=i+2*(i-1)
      comparison2[j-k] <- NULL 
      k=k+1     
     }
  sink()
   title4 <- 'Comparison of Trt for each year'
   cat('\n',format(title4, justify="left"))
   cat('\n',format(" ", justify="left")) 

  print(comparison2)

#### comparison of Trt for each season (averaged over years) #####

   sink(file="temp.txt")
   comparison3 <- matrix(0,t,3*s)
   dim(comparison3) <- c(t,3*s)
   comparison3 <- as.data.frame(comparison3)
   for(i in 1:s) {    
      j=i+2*(i-1)
      comparison <- LSD.test(data$Y[data$Season==Slevels[[i]]],
          data$Trt[data$Season==Slevels[[i]]],alpha=alpha,TYSdf,TYSMS)
      comparison <- comparison[order(comparison$trt),]
      comparison3[j:(j+2)] <- comparison[1:3] 
      colnames(comparison3)[j] <- "trt"
      colnames(comparison3)[j+1] <- Slevels[[i]]
      colnames(comparison3)[j+2] <- " "
      }
  k=0
  for(i in 2:s) {
      j=i+2*(i-1)
      comparison3[j-k] <- NULL 
      k=k+1     
     }
  sink()
   title5 <- 'Comparison of Trt for each season'
   cat('\n',format(title5, justify="left"))
   cat('\n',format(" ", justify="left")) 
   print(comparison3)

### comparison of Trt for each year and season #####
   title6 <- 'Comparison of Trt for each year and season'
   cat('\n',format(title6, justify="left"))
   cat('\n',format(" ", justify="left")) 
   for (q in 1:s) {
   title7 <- paste('Season=',levels(data$Season)[[q]])
   cat('\n',format(title7, width=90, justify="left"))
   cat('\n',format(" ", justify="left")) 
   sink(file="temp.txt")
   comparison4 <- matrix(0,t,3*y)
   dim(comparison4) <- c(t,3*y)
   comparison4 <- as.data.frame(comparison4)
   for(i in 1:y) {    
      j=i+2*(i-1)
      comparison <- LSD.test(data$Y[data$Year==Ylevels[[i]] & data$Season==Slevels[[q]]],
          data$Trt[data$Year==Ylevels[[i]] & data$Season==Slevels[[q]]],alpha=alpha,Edf,EMS)
      comparison <- comparison[order(comparison$trt),]
      comparison4[j:(j+2)] <- comparison[1:3] 
      colnames(comparison4)[j] <- "trt"
      colnames(comparison4)[j+1] <- Ylevels[[i]]
      colnames(comparison4)[j+2] <- " "
      }
  k=0
  for(i in 2:y) {
      j=i+2*(i-1)
      comparison4[j-k] <- NULL 
      k=k+1     
     }
  sink()   
print(comparison4)
  }
}  

dev.off()
sink()