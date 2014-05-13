complete <- function(directory, id = 1:332) {

    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    
    monitordata<-NULL
    
    for (i in id) {
        df<-read.csv(paste(directory,"/",sprintf("%03s",i),".csv",sep=""))
        monitordata<-rbind(monitordata,df)
    }
    
    good.bv<-complete.cases(monitordata)
    good.df<-monitordata[good.bv,]
    cnts<-tapply(good.df[,4],as.factor(good.df[,4]),length,simplify=F)
    
    cnts.df<-as.data.frame(cnts)
    
    cnts.df
}
