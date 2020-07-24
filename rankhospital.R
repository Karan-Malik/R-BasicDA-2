

rankhospital<-function(state,outcome,num='best'){
  
  df1<-read.csv('outcome-of-care-measures.csv')
  
  df<-df1[,c(2,7,11,17,23)]
  names(df)<-c('Name','State','heart.attack','heart.failure','pneumonia')
  
  if (state %in% df$State==FALSE) stop('invalid state')
  if (outcome %in% names(df)[c(3,4,5)] ==FALSE ) stop('invalid outcome')
  
  df<-data.frame(df[df['State']==state & df[outcome] !='Not Available',])
  df[outcome]<-sapply(df[outcome],as.numeric)
  
  df<-df[order(df$Name),]
  df<-df[order(df[outcome]),]
  
  if (num=='best') print(df[1,1])
  else if (num=='worst') print(df[length(df$Name),1])
  else print(df[num,1])
  
  
}