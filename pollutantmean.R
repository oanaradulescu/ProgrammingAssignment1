
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating 
    ## the location of the CSV file
    
    ## 'pollutant' is a character vector of length 1 indicating 
    ## the name of the pollutant for which we will calculate the mean;
    ## either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list 
    ## in the 'id' vector (ignoring NA values)
    
    monitordata<-NULL
    
    for (i in id) {
        df<-read.csv(paste(directory,"/",sprintf("%03s",i),".csv",sep=""))
        monitordata<-rbind(monitordata,df)
    }
    
    mean(monitordata[,pollutant], na.rm=T)
}
