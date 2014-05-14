corr <- function(directory, threshold = 0) {
        
        ## 'directory' is a character vector of length 1 indicating the location 
        ## of the CSV files
        
        ## 'threshold' is a nmeric vector of length 1 indicating 
        ## the number of completely observed observations (on all variables)
        ## required to compute the correlation between nitrate and sulfate
        ## the default is 0
        
        ## Return a numeric vector of correlations
        
        complete.counts <- complete(directory)        
        counts.above <- complete.counts[,"nobs"] > threshold
        file.ids <- complete.counts[counts.above,"id"]
        
        result <- vector(mode="numeric",length=length(file.ids))
        
        count<-0
        
        for (id in file.ids) {
                monitor.obs <- read.csv(paste(directory, "/", sprintf("%03s", as.character(id)), ".csv", sep=""))
                count <- count+1
                result[count] <- cor(monitor.obs["nitrate"], monitor.obs["sulfate"], use="complete.obs")
        }
        
        result

}
