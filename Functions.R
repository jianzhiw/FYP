# To create result vector
createResultsVector <- function (MktName){
  results <- rep(0,8)
  nam <- c("Mkt", "LongPL", "ShortPL", "L Win %", "L Trades", 
           "S Win %", "S Trades", "Misc")
  names(results) <- nam
  results["Mkt"] <- MktName
  return(results)
}

calcStats <- function(x){
  results <- 1:2
  v <- na.omit(x)
  
  # Win %
  wins <- length(v[v>0])
  losses <- length(v[v<0])
  results[1] <- round(wins/(wins+losses)*100)
  
  # Num Trades
  results[2] <- length(v)
  
  return ( results )
}


# Naive Long System
NaiveLongSystem <- function (Mkt, MktName){
  results <- createResultsVector(MktName)
  Mkt$Long <- Mkt$Close - Mkt$Open
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))

  Stats <- calcStats(Mkt$Long)
  results[4:5] <- Stats
  
  return(results)
}


# Naive Reverse System
NaiveReversePrev <- function (Mkt, MktName){
  results <- createResultsVector(MktName)
  
  Mkt$pl <- Mkt$Close - Mkt$Open
  Mkt$prevPL <- c(NA, Mkt$pl[-length(Mkt$pl)])
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$prevPL<0, Mkt$Close-Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE ))
  # Trade Short
  Mkt$Short <- ifelse(Mkt$prevPL>0, Mkt$Open-Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm= TRUE))
  # calculate Long results
  Stats <- calcStats(Mkt$Long)
  results[4:5] <- Stats
  # calculate Short results
  Stats <- calcStats(Mkt$Short)
  results[6:7] <- Stats
  
  return(results)
}


# SMA
SMA_sys <- function (Mkt, sma, MktName){
  results <- createResultsVector(MktName)
  
  sma.value <- SMA(Mkt["Open"], sma) #create sma vector
  Mkt <- cbind(Mkt, sma.value) #add sma vector as new col
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$Open > Mkt$sma.value, Mkt$Close - Mkt$Open, NA)
  results ["LongPL"] <- round(sum(Mkt$Long, na.rm=T))
  # Trade Short
  Mkt$Short <- ifelse(Mkt$Open < Mkt$sma.value, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=T))
  #calculate Long results
  results[4:5] <- calcStats(Mkt$Long)
  #calculate Short results
  results[6:7] <- calcStats(Mkt$Short)
  results[8] <- paste("SMA", sma)
  
  return(results)
}


# MACD
MACD_sys <- function(Mkt, MktName){
  
  results <- createResultsVector(MktName)
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$macd>Mkt$signal, Mkt$Close-Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  # Trade Short
  Mkt$Short <- ifelse(Mkt$macd<Mkt$signal, Mkt$Open-Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #calculate Long results
  results[4:5] <- calcStats(Mkt$Long)
  #calculate Short results
  results[6:7] <- calcStats(Mkt$Short)
  
  return(results)
}


# aroon
aroon_sys <- function(Mkt, MktName){
  results <- createResultsVector(MktName)
  # Trade Long
  Mkt$Long <- ifelse(Mkt$aroonUp >= 70, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  # Trade Short
  Mkt$Short <- ifelse(Mkt$aroonDn >= 70, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #calculate Long results
  results[4:5] <- calcStats(Mkt$Long)
  #calculate Short results
  results[6:7] <- calcStats(Mkt$Short)
  
  return(results)
}


# SAR
sar_sys <- function(Mkt, MktName){
  results <- createResultsVector(MktName)
  Mkt$prevsar <- c(NA, Mkt$sar[-length(Mkt$sar)])
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$Open > Mkt$prevsar, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm = TRUE))
  # Trade Short
  Mkt$Short <- ifelse(Mkt$Open < Mkt$prevsar, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm = TRUE))
  
  #calculate Long results
  results[4:5] <- calcStats(Mkt$Long)
  
  #calculate Short results
  results[6:7] <- calcStats(Mkt$Short)
  
  return(results)
}


# Daily Breakout System
breakout_sys <- function(Mkt, MktName){
  results <- createResultsVector(MktName)
  
  Mkt$prevHigh <- c(NA, Mkt$High[ - length(Mkt$High)])
  Mkt$prevLow <- c(NA, Mkt$Low[ - length(Mkt$Low)])
  
  # Break out high
  Mkt$Long <- ifelse(Mkt$High > Mkt$prevHigh, Mkt$Close - Mkt$prevHigh, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm = TRUE))
  # Break out low
  Mkt$Short <- ifelse(Mkt$Low < Mkt$prevLow, Mkt$prevLow - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm = TRUE))
  
  Stats <- calcStats(Mkt$Long)
  results[4:5] <- Stats
  Stats <- calcStats(Mkt$Short)
  results[6:7] <- Stats
  
  return(results)
}


# System 1 Model
ts_1 <- function (Mkt, MktName ){
  results <- createResultsVector(MktName)
  
  Mkt$p_c <- c(NA, Mkt$Close[ - length(Mkt$Close)]) # prev close
  Mkt$p_p <- c(NA, Mkt$p[ - length(Mkt$p)]) # prev pred
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$p_p > Mkt$p_c, Mkt$Close - Mkt $Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  # Trade Short
  Mkt$Short <- ifelse(Mkt$p_p < Mkt$p_c, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  
  Stats <- calcStats2(Mkt$Long)
  results[4:5] <- Stats
  
  Stats <- calcStats2(Mkt$Short)
  results[6:7] <- Stats
  
  return(results)
}


# System 2 Model
ts_2 <- function (Mkt, MktName){
  results <- createResultsVector(MktName)
  
  Mkt$p_p <- c(NA, Mkt$p[ - length(Mkt$p)]) # prev prediction
  Mkt$p_p2 <- c(NA, Mkt$p_p[ - length(Mkt$p_p)]) # two days ago prediction
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$p_p > Mkt$p_p2, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  # Trade Short
  Mkt$Short <- ifelse(Mkt$p_p < Mkt$p_p2, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  
  Stats <- calcStats2(Mkt$Long)
  results[4:5] <- Stats
  
  Stats <- calcStats2(Mkt$Short)
  results[6:7] <- Stats
  
  return(results)
}