######################
## UTILITY FUNCTIONS #
######################

## function that computes the difference between two subsequent values
compute.difference <- function(values) {
  dif <- vector(mode = "numeric", length = length(values))
  dif[1] <- NA
  for(i in 1:(length(values)-1)) {
    dif[i+1] <- abs(values[i+1] - values[i])
  }
  dif
}

## function that provides summary statistics about clusters
summary.stats <- function(feature.set, clusters, cl.num) {
  sum.stats <- aggregate(x = feature.set, 
                         by = list(clusters), 
                         FUN = function(x) { 
                           m <- mean(x, na.rm = T)
                           sd <- sqrt(var(x, na.rm = T))
                           paste(round(m, digits = 2), " (", 
                                 round(sd, digits = 2), ")", sep = "")
                         })
  sum.stat.df <- data.frame(cluster = sum.stats[,1], 
                            freq = as.vector(table(clusters)),
                            sum.stats[,-1])
  
  sum.stats.transpose <- t( as.matrix(sum.stat.df) )
  sum.stats.transpose <- as.data.frame(sum.stats.transpose)
  attributes <- rownames(sum.stats.transpose)
  sum.stats.transpose <- as.data.frame( cbind(attributes, sum.stats.transpose) )
  colnames(sum.stats.transpose) <- c( "attributes", rep("Mean (SD)", cl.num) )
  rownames(sum.stats.transpose) <- NULL
  sum.stats.transpose
}

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}
