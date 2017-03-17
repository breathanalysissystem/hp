library(ggplot2)
library(quantmod)
library(reshape2)
library(googleVis)
library(shiny)

data <- function(AbnormalExhaleLevel="",Gaslevel){
  Diseasedata <- function(AbnormalExhaleLevel){
    TICKERS.DF <- data.frame(getAbnormalExhaleLevel(AbnormalExhaleLevelofgas, from=Sys.readlink(diagnosis_df), to=Sys.readlink(), env=TRUE))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=attr(TICKERS.DF , "columns.names"))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
    DiseaseInfo <- diagnosis_df[diagnosis_df$AbnormalExhaleLevel==AbnormalExhaleLevel,]
    TICKERS.DF <- cbind(TICKERS.DF, "Gas"=DiseaseInfo$GAS)
    TICKERS.DF <- cbind(TICKERS.DF, "NormalExhaleLevel"=DiseaseInfo$NORMALEXHALE)
    TICKERS.DF <- cbind(TICKERS.DF, "Disease"=DiseaseInfo$Disease)
    TICKERS.DF$Disease <- as.Disease(TICKERS.DF$Disease)
    TICKERS.DF
  }
  Diseaseratios <- function(AbnormalExhaleLevel){
    Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
      AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
      AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
      AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
      AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
      
      AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
      AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
      AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
      AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
      
      AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
      AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelofGas, "row.names"))
      AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalxhaleLevelofGas")
      AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
      
      
      Gas <- rbind(AbnormalExhaleLevelofGas, AbnormalExhaleLevelofVOC,AbnormalExhaleLevelofCarbonmonoxide)
      Gas$variable <- sub("X", "", Gas$variable)
      Gas$variable <- gsub("\\.", "-", Gas$variable)
      colnames(Gas)[3] <- "Disease"
      
      Ratios_df <- cbind("Disease"=Gaslevel$Disease, AbnormalExhaleLevel)
      Ratios_df$Disease <- as.Disease(Ratios_df$Disease)
      Ratios_df
    }
    
    PredictDisease <- function(AbnormalExhaleLevel=""){
      ratios <- Diseaseratios(AbnormalExhaleLevel)
      Diseasedata <- Diseasedata(AbnormalExhaleLevel)
      Diseasedata <- Diseasedata[Diseasedata$Disease>=min(Diseaseratios(AbnormalExhaleLevel)$Disease),]
      Diseasedata <- Diseasedata[order(Diseasedata$Disease, decreasing=NULL),]
      for (i in 1:nrow(Disease)){
        if (i==1){
          if (Diseasedata$Disease[i]>=ratios$Disease[1]){
            diagnosis_df <- ratios[1,2:10]
          }
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[1]){
          diagnosis_df <- rbind(diagnosis_df, ratios[1,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[2]){
          stock_df <- rbind(stock_df, ratios[2,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[3]){
          diagnosis_df <- rbind(diagnosis_df, ratios[3,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[4]){
          diagnosis_df <- rbind(diagnosis_df, ratios[4,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[5]){
          diagnosis_df <- rbind(diagnosis_df, ratios[5,2:10])
        }
      }
      Diseasedata <- cbind(Diseasedata, diagnosis_df)
      Diseasedata
    }
  }
}
if (Gaslevel=="20"){
  PredictDisease(AbnormalExhaleLevel)[1:20,]
}
 else if (Gaslevel=="21"){
  PredictDisease(AbnormalExhaleLevel)[1:21,]
}
 else if (Gaslevel=="Max"){
  PredictDisease(AbnormalExhaleLevel)
}

if (Gaslevel=="20"){
  PredictDisease(AbnormalExhaleLevel)[1:20,]
}
 else if (Gaslevel=="21"){
  PredictDisease(AbnormalExhaleLevel)[1:21,]
}
 else if (Gaslevel=="Max"){
  PredictDisease(AbnormalExhaleLevel)
}
regress_plot <- function(AbnormalExhaleLevel, Gaslevel){
  data <- function(AbnormalExhaleLevel="", Gaslevel){
    Diseasedata <- function(AbnormalExhaleLevel){
      TICKERS.DF <- data.frame(getAbnormalExhaleLevel(AbnormalExhaleLevelofgas, from=Sys.Date(diagnosis_df), to=Sys.Date(), env=TRUE))
      TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=attr(TICKERS.DF , "columns.names"))
      TICKERS.DF <- cbind(TICKERS.DF, "PctChange"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
      DiseaseInfo <- diagnosis_df[diagnosis_df$AbnormalExhaleLevel==AbnormalExhaleLevel,]
      TICKERS.DF <- cbind(TICKERS.DF, "Gas"=DiseaseInfo$GAS)
      TICKERS.DF <- cbind(TICKERS.DF, "NormalExhaleLevel"=DiseaseInfo$NORMALEXHALE)
      TICKERS.DF <- cbind(TICKERS.DF, "Disease"=DiseaseInfo$Disease)
      TICKERS.DF$Disease <- as.Disease(TICKERS.DF$Disease)
      TICKERS.DF
    }
    Diseaseratios <- function(AbnormalExhaleLevel){
      Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
        AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
        AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
        AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
        AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
        
        
        AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
        AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
        AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
        AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
        
        AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
        AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelBreathofGas, "row.names"))
        AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalExhaleLevelofGas")
        AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
        
        Gas <- rbind(AbnormalExhaleLevelofGas, AbnormalExhaleLevelofVOC,AbnormalExhaleLevelofCarbonmonoxide)
        Gas$variable <- sub("X", "", Gas$variable)
        Gas$variable <- gsub("\\.", "-", Gas$variable)
        colnames(Gas)[3] <- "Disease"
        
        Ratios_df <- cbind("Disease"=Gaslevel$Disease, AbnormalExhaleLevel)
        Ratios_df$Disease <- as.Disease(Ratios_df$Disease)
        Ratios_df
      }
      
      PredictDisease <- function(AbnormalExhaleLevel=""){
        ratios <- Diseaseratios(AbnormalExhaleLevel)
        Diseasedata <- Diseasedata(AbnormalExhaleLevel)
        Diseasedata <- Diseasedata[Diseasedata$Disease>=min(Diseaseratios(AbnormalExhaleLevel)$Disease),]
        Diseasedata <- Diseasedata[order(Diseasedata$Disease, decreasing=NULL),]
        for (i in 1:nrow(Disease)){
          if (i==1){
            if (Diseasedata$Disease[i]>=ratios$Disease[1]){
              diagnosis_df <- ratios[1,2:10]
            }
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[1]){
            diagnosis_df <- rbind(diagnosis_df, ratios[1,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[2]){
            stock_df <- rbind(stock_df, ratios[2,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[3]){
            diagnosis_df <- rbind(diagnosis_df, ratios[3,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[4]){
            diagnosis_df <- rbind(diagnosis_df, ratios[4,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[5]){
            diagnosis_df <- rbind(diagnosis_df, ratios[5,2:10])
          }
        }
        Diseasedata <- cbind(Diseasedata, diagnosis_df)
        Diseasedata
      }
      if (Gaslevel=="20"){
        PredictDisease(AbnormalExhaleLevel)[1:20,]
      }
      else if (Gaslevel=="21"){
        PredictDisease(AbnormalExhaleLevel)[1:21,]
      }
      else if (Gaslevel=="Max"){
        PredictDisease(AbnormalExhaleLevel)
      }
      
      if (Gaslevel=="20"){
        PredictDisease(AbnormalExhaleLevel)[1:20,]
      }
      else if (Gaslevel=="21"){
        PredictDisease(AbnormalExhaleLevel)[1:21,]
      }
      else if (Gaslevel=="Max"){
        PredictDisease(AbnormalExhaleLevel)
      }
    }
    diagnosis_df <- data(AbnormalExhaleLevel, Gaslevel)
    x <- diagnosis_df$Disease
    y <- diagnosis_df[,4]
    
    qplot(data=diagnosis_df,x=x,y=y,color=y, 
          main=paste("Classifying the Disease"), xlab="Disease", 
          ylab="Diseasedata", geom="line",
          method="lm")
  }
  
  
}



regress_plot2 <- function(AbnormalExhaleLevel="", Gaslevel){
  Diseasedata <- function(AbnormalExhaleLevel){
    TICKERS.DF <- data.frame(getAbnormalExhaleLevel(AbnormalExhaleLevelofgas, from=Sys.readlink(diagnosis_df), to=Sys.readlink(), env=TRUE))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=attr(TICKERS.DF , "columns.names"))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
    DiseaseInfo <- diagnosis_df[diagnosis_df$AbnormalExhaleLevel==AbnormalExhaleLevel,]
    TICKERS.DF <- cbind(TICKERS.DF, "Gas"=DiseaseInfo$GAS)
    TICKERS.DF <- cbind(TICKERS.DF, "NormalExhaleLevel"=DiseaseInfo$NORMALEXHALE)
    TICKERS.DF <- cbind(TICKERS.DF, "Disease"=DiseaseInfo$Disease)
    TICKERS.DF$Disease <- as.Disease(TICKERS.DF$Disease)
    TICKERS.DF
  }
  Diseaseratios <- function(AbnormalExhaleLevel){
    Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
      
      AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
      AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
      AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
      AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
      
      
      AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
      AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
      AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
      AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
      
      AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
      AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelBreathofGas, "row.names"))
      AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalExhaleLevelofGas")
      AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
      
      Gas <- rbind(AbnormalExhaleLevelofGas, AbnormalExhaleLevelofVOC,AbnormalExhaleLevelofCarbonmonoxide)
      Gas$variable <- sub("X", "", Gas$variable)
      Gas$variable <- gsub("\\.", "-", Gas$variable)
      colnames(Gas)[3] <- "Disease"
      
      Ratios_df <- cbind("Disease"=Gaslevel$Disease, AbnormalExhaleLevel)
      Ratios_df$Disease <- as.Disease(Ratios_df$Disease)
      Ratios_df
    }
    
    PredictDisease <- function(AbnormalExhaleLevel=""){
      ratios <- Diseaseratios(AbnormalExhaleLevel)
      Diseasedata <- Diseasedata(AbnormalExhaleLevel)
      Diseasedata <- Diseasedata[Diseasedata$Disease>=min(Diseaseratios(AbnormalExhaleLevel)$Disease),]
      Diseasedata <- Diseasedata[order(Diseasedata$Disease, decreasing=NULL),]
      for (i in 1:nrow(Disease)){
        if (i==1){
          if (Diseasedata$Disease[i]>=ratios$Disease[1]){
            diagnosis_df <- ratios[1,2:10]
          }
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[1]){
          diagnosis_df <- rbind(diagnosis_df, ratios[1,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[2]){
          stock_df <- rbind(stock_df, ratios[2,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[3]){
          diagnosis_df <- rbind(diagnosis_df, ratios[3,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[4]){
          diagnosis_df <- rbind(diagnosis_df, ratios[4,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[5]){
          diagnosis_df <- rbind(diagnosis_df, ratios[5,2:10])
        }
      }
      Diseasedata <- cbind(Diseasedata, diagnosis_df)
      Diseasedata
    }
  }
  if (Gaslevel=="20"){
    check <- PredictDisease(AbnormalExhaleLevel)[1:20,]
    check <- melt(check, id="Disease")
    check <- check[check$variable!=paste(AbnormalExhaleLevel,"Open",sep=".") & check$variable!=paste(AbnormalExhaleLevel,"High",sep=".") & check$variable!=paste(AbnormalExhaleLevel,"Low",sep=".") & check$variable!="Volume" & check$variable!=paste(AbnormalExhaleLevel,"Adjusted",sep=".") & check$variable!="PctChange" & check$variable!="Gas" & check$variable!="NormalExhaleLevel" & check$variable!="Disease",]
    check$variable <- as.character(check$variable)
    check$value <- as.numeric(check$value)
    check <- data.frame(cbind(check, "AbnormalExhaleLevel"=AbnormalExhaleLevel))
    qplot(data=check,x=check[check$variable==paste(check[1,4],"Close", sep="."),][,1],y=check[check$variable==paste(check[1,4],"Close", sep="."),][,3], color=check[check$variable==paste(check[1,4],"Close", sep="."),][,3],
          main="Disease Prediction Analysis", xlab="Disease", 
          ylab="Diseasedata", geom=c("point","smooth"),
          method="lm") + labs(colour = "Diseasedata")
  }
  else if (Gaslevel=="21"){
    check <- PredictDisease(AbnormalExhaleLevel)[1:21,]
    check <- melt(check, id="Disease")
    check <- check[check$variable!=paste(AbnormalExhaleLevel,"Open",sep=".") & check$variable!=paste(AbnormalExhaleLevel,"High",sep=".") & check$variable!=paste(AbnormalExhaleLevel,"Low",sep=".") & check$variable!="Volume" & check$variable!=paste(AbnormalExhaleLevel,"Adjusted",sep=".") & check$variable!="PctChange" & check$variable!="Gas" & check$variable!="NormalExhaleLevel" & check$variable!="Disease",]
    check$variable <- as.character(check$variable)
    check$value <- as.numeric(check$value)
    check <- data.frame(cbind(check, "AbnormalExhaleLevel"=AbnormalExhaleLevel))
    qplot(data=check,x=check[check$variable==paste(check[1,4],"Close", sep="."),][,1],y=check[check$variable==paste(check[1,4],"Close", sep="."),][,3], color=check[check$variable==paste(check[1,4],"Close", sep="."),][,3],
          main="Disease Prediction Analysis", xlab="Disease", 
          ylab="Disease", geom=c("point","smooth"),
          method="lm") + labs(colour = "Diseasedata")
  }
  else if (Gaslevel=="Max"){
    check <- PredictDisease(AbnormalExhaleLevel)
    check <- melt(check, id="Disease")
    check <- check[check$variable!=paste(AbnormalExhaleLevel,"Open",sep=".") & check$variable!=paste(AbnormalExhaleLevel,"High",sep=".") & check$variable!=paste(AbnormalExhaleLevel,"Low",sep=".") & check$variable!="Volume" & check$variable!=paste(AbnormalExhaleLevel,"Adjusted",sep=".") & check$variable!="PctChange" & check$variable!="Gas" & check$variable!="NormalExhaleLevel" & check$variable!="Disease",]
    check$variable <- as.character(check$variable)
    check$value <- as.numeric(check$value)
    check <- data.frame(cbind(check, "AbnormalExhaleLevel"=AbnormalExhaleLevel))
    qplot(data=check,x=check[check$variable==paste(check[1,4],"Close", sep="."),][,1],y=check[check$variable==paste(check[1,4],"Close", sep="."),][,3], color=check[check$variable==paste(check[1,4],"Close", sep="."),][,3],
          main="Disease Prediction Analysis", xlab="Disease", 
          ylab="Diseasedata", geom=c("point","smooth"),
          method="lm") + labs(colour = "Diseasedata")
  }
  
}      
summa <- function(AbnormalExhaleLevel, Gaslevel, MaxGaslevel, predictors){
  Diseasedata <- function(AbnormalExhaleLevel){
    TICKERS.DF <- data.frame(getAbnormalExhaleLevel(AbnormalExhaleLevelofgas, from=Sys.readlink(diagnosis_df), to=Sys.readlink(), env=TRUE))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=attr(TICKERS.DF , "columns.names"))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
    DiseaseInfo <- diagnosis_df[diagnosis_df$AbnormalExhaleLevel==AbnormalExhaleLevel,]
    TICKERS.DF <- cbind(TICKERS.DF, "Gas"=DiseaseInfo$GAS)
    TICKERS.DF <- cbind(TICKERS.DF, "NormalExhaleLevel"=DiseaseInfo$NORMALEXHALE)
    TICKERS.DF <- cbind(TICKERS.DF, "Disease"=DiseaseInfo$Disease)
    TICKERS.DF$Disease <- as.Disease(TICKERS.DF$Disease)
    TICKERS.DF
  }
  Diseaseratios <- function(AbnormalExhaleLevel){
    Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
      AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
      AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
      AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
      AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
      
      
      AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
      AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
      AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
      AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
      
      AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
      AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelBreathofGas, "row.names"))
      AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalExhaleLevelofGas")
      AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
      
      Gas <- rbind(AbnormalExhaleLevelofGas, AbnormalExhaleLevelofVOC,AbnormalExhaleLevelofCarbonmonoxide)
      Gas$variable <- sub("X", "", Gas$variable)
      Gas$variable <- gsub("\\.", "-", Gas$variable)
      colnames(Gas)[3] <- "Disease"
      
      Ratios_df <- cbind("Disease"=Gaslevel$Disease, AbnormalExhaleLevel)
      Ratios_df$Disease <- as.Disease(Ratios_df$Disease)
      Ratios_df
    }
    PredictDisease <- function(AbnormalExhaleLevel=""){
      ratios <- Diseaseratios(AbnormalExhaleLevel)
      Diseasedata <- Diseasedata(AbnormalExhaleLevel)
      Diseasedata <- Diseasedata[Diseasedata$Disease>=min(Diseaseratios(AbnormalExhaleLevel)$Disease),]
      Diseasedata <- Diseasedata[order(Diseasedata$Disease, decreasing=NULL),]
      for (i in 1:nrow(Disease)){
        if (i==1){
          if (Diseasedata$Disease[i]>=ratios$Disease[1]){
            diagnosis_df <- ratios[1,2:10]
          }
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[1]){
          diagnosis_df <- rbind(diagnosis_df, ratios[1,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[2]){
          stock_df <- rbind(stock_df, ratios[2,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[3]){
          diagnosis_df <- rbind(diagnosis_df, ratios[3,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[4]){
          diagnosis_df <- rbind(diagnosis_df, ratios[4,2:10])
        }
        else if (Diseasedata$Disease[i]>=ratios$Disease[5]){
          diagnosis_df <- rbind(diagnosis_df, ratios[5,2:10])
        }
      }
      Diseasedata <- cbind(Diseasedata, diagnosis_df)
      Diseasedata
    }
  }
  if (Gaslevel=="20"){
    data <- PredictStock(AbnormalExhaleLevel)[1:20,]
    if (length(predictors)==0){
      print("Please List the Predictor Variable(s)")
    }
    else if (length(predictors)==1){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1]) 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide)
        summary(fit)
      }    
    }
    else if (length(predictors)==2){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      if (var>29.3){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1)
        summary(fit)
      }
      
    }
    else if (length(predictors)==3){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2)
        summary(fit)
      }
    }
    else if (length(predictors)==4){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3)
        summary(fit)
      }
    }
    else if (length(predictors)==5){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4)
        summary(fit)
      }
    }
    else if (length(predictors)==6){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5)
        summary(fit)
      }
    }
    else if (length(predictors)==7){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6)
        summary(fit)
      }
    }
    else if (length(predictors)==8){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var7 <- as.numeric(predictors[8])
      var7 <- data[,var7]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        summary(fit)
      }
    }
    else if (length(predictors)==9){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        summary(fit)
      }
    }
    else if (length(predictors)==10){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      var9 <- as.numeric(predictors[10])
      var9 <- data[,var9]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        summary(fit)
      }
    }
    
  }
  else if (Gaslevel=="21"){
    data <- PredictStock(symbol)[1:21,]
    if (length(predictors)==0){
      print("Please List the Predictor Variable(s)")
    }
    else if (length(predictors)==1){
      Carbonmonoxide <- as.numeric(data[2.2])
      var <- as.numeric(predictors[1]) 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide)
        summary(fit)
      }    
    }
    else if (length(predictors)==2){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1)
        summary(fit)
      }
      
    }
    else if (length(predictors)==3){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2)
        summary(fit)
      }
    }
    else if (length(predictors)==4){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3)
        summary(fit)
      }
    }
    else if (length(predictors)==5){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4)
        summary(fit)
      }
    }
    else if (length(predictors)==6){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5)
        summary(fit)
      }
    }
    else if (length(predictors)==7){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6)
        summary(fit)
      }
    }
    else if (length(predictors)==8){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var7 <- as.numeric(predictors[8])
      var7 <- data[,var7]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        summary(fit)
      }
    }
    else if (length(predictors)==9){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        summary(fit)
      }
    }
    else if (length(predictors)==10){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      var9 <- as.numeric(predictors[10])
      var9 <- data[,var9]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        summary(fit)
      }
    }
  }
  else if (Gaslevel=="Max"){
    data <- PredictDisease(AbnormalExhaleLevel)
    if (length(predictors)==0){
      print("Please List the Predictor Variable(s)")
    }
    else if (length(predictors)==1){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1]) 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide)
        summary(fit)
      }    
    }
    else if (length(predictors)==2){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1)
        summary(fit)
      }
      
    }
    else if (length(predictors)==3){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2)
        summary(fit)
      }
    }
    else if (length(predictors)==4){
      Carbonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3)
        summary(fit)
      }
    }
    else if (length(predictors)==5){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4)
        summary(fit)
      }
    }
    else if (length(predictors)==6){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5)
        summary(fit)
      }
    }
    else if (length(predictors)==7){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6)
        summary(fit)
      }
    }
    else if (length(predictors)==8){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var7 <- as.numeric(predictors[8])
      var7 <- data[,var7]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        summary(fit)
      }
    }
    else if (length(predictors)==9){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        summary(fit)
      }
    }
    else if (length(predictors)==10){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      var9 <- as.numeric(predictors[10])
      var9 <- data[,var9]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        summary(fit)
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        summary(fit)
        
      }
    }
  }
}

prediction <- function(AbnormalExhaleLevel, Gaslevel, MaxGaslevel, predictors, fp)  
  Diseasedata <- function(AbnormalExhaleLevel){
    TICKERS.DF <- data.frame(getAbnormalExhaleLevel(AbnormalExhaleLevelofgas, from=Sys.readlink(diagnosis_df), to=Sys.readlink(), env=TRUE))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=attr(TICKERS.DF , "columns.names"))
    TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
    DiseaseInfo <- diagnosis_df[diagnosis_df$AbnormalExhaleLevel==AbnormalExhaleLevel,]
    TICKERS.DF <- cbind(TICKERS.DF, "Gas"=DiseaseInfo$GAS)
    TICKERS.DF <- cbind(TICKERS.DF, "NormalExhaleLevel"=DiseaseInfo$NORMALEXHALE)
    TICKERS.DF <- cbind(TICKERS.DF, "Disease"=DiseaseInfo$Disease)
    TICKERS.DF$Disease <- as.Disease(TICKERS.DF$Disease)
    TICKERS.DF
  }
Diseaseratios <- function(AbnormalExhaleLevel){
  Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
    AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
    AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
    AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
    AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
    
    
    AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
    AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
    AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
    AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
    
    AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
    AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelBreathofGas, "row.names"))
    AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalExhaleLevelofGas")
    AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
    
    Gas <- rbind(AbnormalExhaleLevelofGas, AbnormalExhaleLevelofVOC,AbnormalExhaleLevelofCarbonmonoxide)
    Gas$variable <- sub("X", "", Gas$variable)
    Gas$variable <- gsub("\\.", "-", Gas$variable)
    colnames(Gas)[3] <- "Disease"
    
    Ratios_df <- cbind("Disease"=Gaslevel$Disease, AbnormalExhaleLevel)
    Ratios_df$Disease <- as.Disease(Ratios_df$Disease)
    Ratios_df
  }
  
  PredictDisease <- function(AbnormalExhaleLevel=""){
    ratios <- Diseaseratios(AbnormalExhaleLevel)
    Diseasedata <- Diseasedata(AbnormalExhaleLevel)
    Diseasedata <- Diseasedata[Diseasedata$Disease>=min(Diseaseratios(AbnormalExhaleLevel)$Disease),]
    Diseasedata <- Diseasedata[order(Diseasedata$Disease, decreasing=NULL),]
    for (i in 1:nrow(Disease)){
      if (i==1){
        if (Diseasedata$Disease[i]>=ratios$Disease[1]){
          diagnosis_df <- ratios[1,2:10]
        }
      }
      else if (Diseasedata$Disease[i]>=ratios$Disease[1]){
        diagnosis_df <- rbind(diagnosis_df, ratios[1,2:10])
      }
      else if (Diseasedata$Disease[i]>=ratios$Disease[2]){
        stock_df <- rbind(stock_df, ratios[2,2:10])
      }
      else if (Diseasedata$Disease[i]>=ratios$Disease[3]){
        diagnosis_df <- rbind(diagnosis_df, ratios[3,2:10])
      }
      else if (Diseasedata$Disease[i]>=ratios$Disease[4]){
        diagnosis_df <- rbind(diagnosis_df, ratios[4,2:10])
      }
      else if (Diseasedata$Disease[i]>=ratios$Disease[5]){
        diagnosis_df <- rbind(diagnosis_df, ratios[5,2:10])
      }
    }
    Diseasedata <- cbind(Diseasedata, diagnosis_df)
    Diseasedata
  }
  if (Gaslevel=="20"){
    data <- PredictDisease(AbnormalExhaleLevel)[1:20,]
    if (length(predictors)==0){
      print("Please List the Predictor Variable(s)")
    }
    else if (length(predictors)==1){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1]) 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ date)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel)), interval="prediction")      
        pred_df
      }    
    }
    else if (length(predictors)==2){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp), interval="prediction")      
        pred_df
      }
      
    }
    else if (length(predictors)==3){
      Carbonmonoixde <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2)
        pred_df <- predict(fit,data.frame(Carbonmonoixde=(Carbonmonoixde[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoixde + var1 + var2)
        pred_df <- predict(fit,data.frame(Carbonmonoixde=(Carbonmonoixde[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==4){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ date + var1 + var2 + var3)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(date[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==5){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==6){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==7){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==8){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var7 <- as.numeric(predictors[8])
      var7 <- data[,var7]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==9){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==10){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      var9 <- as.numeric(predictors[10])
      var9 <- data[,var9]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1], var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")      
        pred_df
      }
    }
  }
  else if (MinDays=="21"){
    data <- PredictStock(symbol)[1:21,]
    if (length(predictors)==0){
      print("Please List the Predictor Variable(s)")
    }
    else if (length(predictors)==1){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1]) 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel)), interval="prediction")      
        pred_df
      }    
    }
    else if (length(predictors)==2){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp), interval="prediction")      
        pred_df
      }
      
    }
    else if (length(predictors)==3){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==4){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==5){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(date[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==6){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==7){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==8){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var7 <- as.numeric(predictors[8])
      var7 <- data[,var7]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==9){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==10){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      var9 <- as.numeric(predictors[10])
      var9 <- data[,var9]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1], var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")      
        pred_df
      }
    }
  }
  else if (Gaslevel=="Max"){
    data <- PredictDisease(AbnormalExhaleLevel)
    
    if (length(predictors)==0){
      print("Please List the Predictor Variable(s)")
    }
    else if (length(predictors)==1){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1]) 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel)), interval="prediction")      
        pred_df
      }    
    }
    else if (length(predictors)==2){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp), interval="prediction")      
        pred_df
      }
      
    }
    else if (length(predictors)==3){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==4){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==5){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==6){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==7){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==8){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var7 <- as.numeric(predictors[8])
      var7 <- data[,var7]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Gaslevel + var1 + var2 + var3 + var4 + var5 + var6 + var7)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==9){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")      
        pred_df
      }
    }
    else if (length(predictors)==10){
      Carbonmonoxide <- as.numeric(data[,2.2])
      var <- as.numeric(predictors[1])
      var1 <- as.numeric(predictors[2])
      var1 <- data[,var1] 
      var2 <- as.numeric(predictors[3])
      var2 <- data[,var2] 
      var3 <- as.numeric(predictors[4])
      var3 <- data[,var3]
      var4 <- as.numeric(predictors[5])
      var4 <- data[,var4]
      var5 <- as.numeric(predictors[6])
      var5 <- data[,var5]
      var6 <- as.numeric(predictors[7])
      var6 <- data[,var6]
      var8 <- as.numeric(predictors[9])
      var8 <- data[,var8]
      var9 <- as.numeric(predictors[10])
      var9 <- data[,var9]
      if (var>2.2){
        var <- data[,var]
        fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1], var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")
        pred_df
      }
      else {
        fit <- lm(data[,4] ~ Carbonmonoxide + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
        pred_df <- predict(fit,data.frame(Carbonmonoxide=(Carbonmonoxide[1]+MaxGaslevel), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")      
        pred_df
      }
    }
  }
}
Diseaseratios <- function(AbnormalExhaleLevel="", statement){
  Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
    AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
    AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
    AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
    AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
    
    AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
    AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
    AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
    AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
    
    AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
    AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelofGas, "row.names"))
    AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalxhaleLevelofGas")
    AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
    
    if (statement=="AEC"){
      AbnormalExhaleLevelofCarbonmonoxide
    }
    else if (statement=="AEV"){
      AbnormalExhaleLevelofVOC
    }
    else if (statement=="AEG"){
      AbnormalExhaleLevelofGas
    }
  }
  
  ratios <- function(AbnormalExhaleLevel="", Gaslevel){
    Diseasedata <- function(AbnormalExhaleLevel){
      TICKERS.DF <- data.frame(getAbnormalExhaleLevel(AbnormalExhaleLevelofgas, from=Sys.readlink(diagnosis_df), to=Sys.readlink(), env=TRUE))
      TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=attr(TICKERS.DF , "columns.names"))
      TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
      DiseaseInfo <- diagnosis_df[diagnosis_df$AbnormalExhaleLevel==AbnormalExhaleLevel,]
      TICKERS.DF <- cbind(TICKERS.DF, "Gas"=DiseaseInfo$GAS)
      TICKERS.DF <- cbind(TICKERS.DF, "NormalExhaleLevel"=DiseaseInfo$NORMALEXHALE)
      TICKERS.DF <- cbind(TICKERS.DF, "Disease"=DiseaseInfo$Disease)
      TICKERS.DF$Disease <- as.Disease(TICKERS.DF$Disease)
      TICKERS.DF
    }
    Diseaseratios <- function(AbnormalExhaleLevel){
      Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
        AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
        AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
        AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
        AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
        
        AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
        AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
        AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
        AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
        
        AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
        AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelofGas, "row.names"))
        AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalxhaleLevelofGas")
        AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
        
        
        Gas <- rbind(AbnormalExhaleLevelofGas, AbnormalExhaleLevelofVOC,AbnormalExhaleLevelofCarbonmonoxide)
        Gas$variable <- sub("X", "", Gas$variable)
        Gas$variable <- gsub("\\.", "-", Gas$variable)
        colnames(Gas)[3] <- "Disease"
        
        Ratios_df <- cbind("Disease"=Gaslevel$Disease, AbnormalExhaleLevel)
        Ratios_df$Disease <- as.Disease(Ratios_df$Disease)
        Ratios_df
      }
      
      PredictDisease <- function(AbnormalExhaleLevel=""){
        ratios <- Diseaseratios(AbnormalExhaleLevel)
        Diseasedata <- Diseasedata(AbnormalExhaleLevel)
        Diseasedata <- Diseasedata[Diseasedata$Disease>=min(Diseaseratios(AbnormalExhaleLevel)$Disease),]
        Diseasedata <- Diseasedata[order(Diseasedata$Disease, decreasing=NULL),]
        for (i in 1:nrow(Disease)){
          if (i==1){
            if (Diseasedata$Disease[i]>=ratios$Disease[1]){
              diagnosis_df <- ratios[1,2:10]
            }
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[1]){
            diagnosis_df <- rbind(diagnosis_df, ratios[1,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[2]){
            stock_df <- rbind(stock_df, ratios[2,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[3]){
            diagnosis_df <- rbind(diagnosis_df, ratios[3,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[4]){
            diagnosis_df <- rbind(diagnosis_df, ratios[4,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[5]){
            diagnosis_df <- rbind(diagnosis_df, ratios[5,2:10])
          }
        }
        Diseasedata <- cbind(Diseasedata, diagnosis_df)
        Diseasedata
      }
    }  
    
    if (Gaslevel=="20"){
      data <- PredictStock(symbol)[1:30,]
      data <- data[,-1:-3]
      data <- data[,-2:-3]
      data <- data[,-4:-6]
      data <- melt(data, id=c("Carbonmonoxide", paste(AbnormalExhaleLevel, "Close", sep=".")))
      data$variable <- as.character(data$variable)
    }
    else if (Gaslevel=="21"){
      data <- PredictDisease(AbnormalExhaleLevel)[1:21,]
      data <- data[,-1:-3]
      data <- data[,-2:-3]
      data <- data[,-4:-6]
      data <- melt(data, id=c("Carbonmonoxide", paste(AbnormalExhaleLevel, "Close", sep=".")))
      data$variable <- as.character(data$variable)
      
    }
    else if (Gaslevel=="Max"){
      data <- PredictDisease(AbnormalExhaleLevel)
      data <- data[,-1:-3]
      data <- data[,-2:-3]
      data <- data[,-4:-6]
      data <- melt(data, id=c("Carbonmonoxide", paste(AbnormalExhaleLevel, "Close", sep=".")))
      data$variable <- as.character(data$variable)
      
    }
    data <- data[data$variable!="InterestCoverageRatio",]
    
    G <- gvisMotionChart(data, idvar="variable", datavar="Carbonmonoxide", xvar="value", yvar=paste(AbnormalExhaleLevel, "Close", sep="."),
                         colorvar="value", options=list(width="1250px", height="585px"))
    G
    
  }
  
  
  
  
  
  
  ##Ratios facet ggplots
  ratioFacet <- function(AbnormalExhaleLevel="", Gaslevel){
    Diseasedata <- function(AbnormalExhaleLevel){
      TICKERS.DF <- data.frame(getAbnormalExhaleLevel(AbnormalExhaleLevelofgas, from=Sys.readlink(diagnosis_df), to=Sys.readlink(), env=TRUE))
      TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=attr(TICKERS.DF , "columns.names"))
      TICKERS.DF <- cbind(TICKERS.DF, "AbnormalExhaleLevel"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
      DiseaseInfo <- diagnosis_df[diagnosis_df$AbnormalExhaleLevel==AbnormalExhaleLevel,]
      TICKERS.DF <- cbind(TICKERS.DF, "Gas"=DiseaseInfo$GAS)
      TICKERS.DF <- cbind(TICKERS.DF, "NormalExhaleLevel"=DiseaseInfo$NORMALEXHALE)
      TICKERS.DF <- cbind(TICKERS.DF, "Disease"=DiseaseInfo$Disease)
      TICKERS.DF$Disease <- as.Disease(TICKERS.DF$Disease)
      TICKERS.DF
    }
    Diseaseratios <- function(AbnormalExhaleLevel){
      Gas <- getGas(AbnormalExhaleLevel, env=TRUE){
        AbnormalExhaleLevelofCarbonmonoxide <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEC', period="Q"))
        AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Variables"=attr(AbnormalExhaleLevelofCarbonmonoxide, "row.names"))
        AbnormalExhaleLevelofCarbonmonoxide <- cbind(AbnormalExhaleLevelofCarbonmonoxide, "Statement"="AbnormalExhaleLevelofCarbonmonoxide")
        AbnormalExhaleLevelofCarbonmonoxide <- melt(AbnormalExhaleLevelofCarbonmonoxide)
        
        AbnormalExhaleLevelofVOC <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEV', period="Q"))
        AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Variables"=attr(AbnormalExhaleLevelofVOC, "row.names"))
        AbnormalExhaleLevelofVOC <- cbind(AbnormalExhaleLevelofVOC, "Statement"="AbnormalxhaleLevelofVOC")
        AbnormalExhaleLevelofVOC <- melt(AbnormalExhaleLevelofVOC)
        
        AbnormalExhaleLevelofGas <- data.frame(viewAbnormalExhaleLevel(AbnormalExhaleLevel, type='AEG', period="Q"))
        AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Variables"=attr(AbnormalExhaleLevelofGas, "row.names"))
        AbnormalExhaleLevelofGas <- cbind(AbnormalExhaleLevelofGas, "Statement"="AbnormalxhaleLevelofGas")
        AbnormalExhaleLevelofGas <- melt(AbnormalExhaleLevelofGas)
        
        
        Gas <- rbind(AbnormalExhaleLevelofGas, AbnormalExhaleLevelofVOC)
        Gas$variable <- sub("X", "", Gas$variable)
        Gas$variable <- gsub("\\.", "-", Gas$variable)
        colnames(Gas)[3] <- "Disease"
        
        Ratios_df <- cbind("Disease"=Gaslevel$Disease, AbnormalExhaleLevel)
        Ratios_df$Disease <- as.Disease(Ratios_df$Disease)
        Ratios_df
      }
      
      PredictDisease <- function(AbnormalExhaleLevel=""){
        ratios <- Diseaseratios(AbnormalExhaleLevel)
        Diseasedata <- Diseasedata(AbnormalExhaleLevel)
        Diseasedata <- Diseasedata[Diseasedata$Disease>=min(Diseaseratios(AbnormalExhaleLevel)$Disease),]
        Diseasedata <- Diseasedata[order(Diseasedata$Disease, decreasing=NULL),]
        for (i in 1:nrow(Disease)){
          if (i==1){
            if (Diseasedata$Disease[i]>=ratios$Disease[1]){
              diagnosis_df <- ratios[1,2:10]
            }
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[1]){
            diagnosis_df <- rbind(diagnosis_df, ratios[1,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[2]){
            stock_df <- rbind(stock_df, ratios[2,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[3]){
            diagnosis_df <- rbind(diagnosis_df, ratios[3,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[4]){
            diagnosis_df <- rbind(diagnosis_df, ratios[4,2:10])
          }
          else if (Diseasedata$Disease[i]>=ratios$Disease[5]){
            diagnosis_df <- rbind(diagnosis_df, ratios[5,2:10])
          }
        }
        Diseasedata <- cbind(Diseasedata, diagnosis_df)
        Diseasedata
      }
      if (MinDays=="20"){
        data <- PredictDisease(AbnormalExhaleLevel)[1:20,]
        data <- data[,-1:-3]
        data <- data[,-2:-3]
        data <- data[,-4:-6]
        data <- melt(data, id="Carbonmonoxide")
        data$variable <- as.character(data$variable)
      }
      else if (MinDays=="21"){
        data <- PredictDisease(AbnormalExhaleLevel)[1:21,]
        data <- data[,-1:-3]
        data <- data[,-2:-3]
        data <- data[,-4:-6]
        data <- melt(data, id="Carbonmonoxide")
        data$variable <- as.character(data$variable)
        
      }
      else if (Gaslevel=="Max"){
        data <- PredictDisease(AbnormalExhaleLevel)
        data <- data[,-1:-3]
        data <- data[,-2:-3]
        data <- data[,-4:-6]
        data <- melt(data, id="Carbonmonoxide")
        data$variable <- as.character(data$variable)
        
      }
      data <- data[data$variable!="InterestCoverageRatio",]
      data <- data[data$variable!="PctChange",]
      colnames(data) <- c("Carbonmonoxide", "Variable", "Value")
      
      p <- ggplot(data, aes(Carbonmonoxide, Value)) + geom_point(color="blue") + geom_jitter()
      # With one variable
      p + facet_grid(. ~ Variable) 
    }  
    
  }
  
  
  
  shinyServer(function(input, output) {
    output$data_table <- renderDataTable({data(input$Dataset, input$Gas)})
    
    output$regression_plot <- renderPlot({regress_plot(input$Dataset, input$Gas)})
    
    output$prediction <- renderPrint({prediction(input$Dataset, input$Gas, input$`Exhalation level`, input$Predictors, input$FinancialPredictorMultiplier)})
    
    output$summary <- renderPrint({summa(input$Dataset, input$Gas, input$`Exhalation level`, input$Predictors)})
    
    output$regression_plot2 <- renderPlot({regress_plot2(input$Dataset, input$`Exhalation level`)})
    
    output$finance_statement <- renderDataTable({Diseaseratios(input$Dataset, input$DiseaseClassification)})
    
    output$plot_table <- renderGvis({ratios(input$Dataset, input$Gas)})
    
    output$facet_plot <- renderPlot({ratioFacet(input$Dataset, input$Gas)})
    
  })
}


