convertEXP <- function(storm){
  
  storm[,c(26)] <- sapply(storm[,c(26)],as.character) 
  
  storm$PROPDMGEXP[storm$PROPDMGEXP=="-"] <- 0
  storm$PROPDMGEXP[storm$PROPDMGEXP=="?"] <- 0
  storm$PROPDMGEXP[storm$PROPDMGEXP=="+"] <- 0
  storm$PROPDMGEXP[storm$PROPDMGEXP==""]  <- 0
  storm$PROPDMGEXP[storm$PROPDMGEXP=="h"] <- 2
  storm$PROPDMGEXP[storm$PROPDMGEXP=="H"] <- 2
  storm$PROPDMGEXP[storm$PROPDMGEXP=="K"] <- 3
  storm$PROPDMGEXP[storm$PROPDMGEXP=="m"] <- 6
  storm$PROPDMGEXP[storm$PROPDMGEXP=="M"] <- 6
  storm$PROPDMGEXP[storm$PROPDMGEXP=="B"] <- 9
  
  storm[,c(28)] <- sapply(storm[,c(28)],as.character) 
  storm$CROPDMGEXP[storm$CROPDMGEXP=="?"] <- 0
  storm$CROPDMGEXP[storm$CROPDMGEXP==""]  <- 0
  storm$CROPDMGEXP[storm$CROPDMGEXP=="k"] <- 3
  storm$CROPDMGEXP[storm$CROPDMGEXP=="K"] <- 3
  storm$CROPDMGEXP[storm$CROPDMGEXP=="m"] <- 6
  storm$CROPDMGEXP[storm$CROPDMGEXP=="M"] <- 6
  storm$CROPDMGEXP[storm$CROPDMGEXP=="B"] <- 9 
  
  return(storm)
}