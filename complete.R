complete <- function(directory, id = 1:332) {
    
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
