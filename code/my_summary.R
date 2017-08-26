my_summary<-function(data, file="summary.csv",app=FALSE){
  data<-as.data.frame(lapply(data,function(x) {
    x<-as.factor(x)
    levels(x)<-droplevels(x)
    }))
  cols<-ncol(data)
  
  write.table("summary",file=file,col.names=FALSE,sep=",",append=app)
  # write out summary for each column
  for (i in 1:cols){
    a<-data[,i]
    variable_name<-names(data)[i]
  
    #vertical factors
    detail<-table(a)
    prop<-prop.table(detail)
    the_table<-as.data.frame(cbind(detail,round(prop*100,2)))
    rownames(the_table)<-as.data.frame(detail)[,1]
    colnames(the_table)<-c(paste(variable_name,"N",sep=",",collapse=NULL),"%")
    write.table(t(c(variable_name,"N","%")),file=file,append=TRUE,col.names = FALSE,row.names=FALSE,sep=",")
    
    write.table(the_table,file=file,append=TRUE,col.names = FALSE,sep=",")
  }
}