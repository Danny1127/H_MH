draw_CI<-function(lfit,data=data){
  # takes in the model and data, gives prediction and se for each group
  data$treatment<-as.factor(apply(data[,c("Drug_Type","Insurance_Type")],1,function(x){
    paste0(ifelse(x[1]==1,"H",ifelse(x[1]==0,"L","NA")),
           ifelse(x[2]==0,"N",ifelse(x[2]==1,"T",
                                     ifelse(x[2]==2,"I","NA"))),collapse="_")
  }))
  result_list<-by(data=data,data$treatment,function(x){
    prediction<-predict.glm(lfit,newdata=x,type="response")
    pred_mean<-mean(prediction,na.rm=T)
    re_pred<-ifelse(prediction>0.5,1,0)
    re_mean<-mean(re_pred,na.rm=T)
    ### variance of mean is var(sum X/N) = var(X)/N^2 
    se<-sqrt(var(prediction,na.rm=T))/sum(!is.na(prediction))
    ori_se<-sqrt(var(x$Bucarin_consumption,na.rm=T))/sum(!is.na(x$Bucarin_consumption))
    re_se<-sqrt(var(re_pred,na.rm=T))/sum(!is.na(re_pred))
    
    # x_tp <- cbind(x, predict(mylogit, newdata = x, type = "link",
    #                                     se = TRUE))
    # x_tp <- within(x_tp, {
    #   PredictedProb <- plogis(fit)
    #   LL <- plogis(fit - (1.96 * se.fit))
    #   UL <- plogis(fit + (1.96 * se.fit))
    # })
    # 
    
    result<-list(pred_mean,se,re_mean,re_se,ori_se)
    return(result)
  })
  return(result_list)
}