read_data<-function(file=file){
  
data<-read.table(file=file, header=TRUE,sep=",")
data$chosen<-0

#Drug type
data$Drug_Type<-""
for (x in c("Low", "High")){
  data$chosen<-0
  for (i in grep(paste0(x,"*"),names(data), ignore.case=TRUE)){
    data$chosen <- data$chosen + is.na(data[,i])
  }
  data$Drug_Type[data$chosen==2]<-x
}

#Insurance Type
data$Insurance_Type<-""
for (x in c("Indemnity", "traditional", "uninsured")){
  data$chosen<-0
  for (i in grep(paste0("*",x),names(data), ignore.case=TRUE)){
    data$chosen <- data$chosen + is.na(data[,i])
  }
  data$Insurance_Type[data$chosen==1]<-x
}

# check drug
#data[1:10,c("Drug_Type",names(data)[grep("*_value*",names(data), ignore.case=TRUE)])]

# check insurance
#data[1:100,c("Insurance_Type",names(data)[grep("*_value*",names(data), ignore.case=TRUE)])]


data$Bucarin_consumption<-NA
for (i in grep("*_value*",names(data), ignore.case=TRUE)){
  # check if already defined
  data$chosen<- !is.na(data[,i])
  if (sum(!is.na(data$Bucarin_consumption[data$chosen]))>0){
    print("error trying to change something already defined")
    print(data[!is.na(data$Bucarin_consumption[data$chosen]),grep("*_value*",names(data), ignore.case=TRUE)])
  }
  else{
    # update result
    data$Bucarin_consumption[data$chosen]<-data[data$chosen,i]
  }
}

# check response
#data[1:10,c("Bucarin_consumption",names(data)[grep("*_value*",names(data), ignore.case=TRUE)])]

data$chosen<-NULL
return(data)
}
