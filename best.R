
df1<-read.csv('outcome-of-care-measures.csv')
head(df1)
str(df1)

#Histogram of 30 day death rates from Heart Attacks
hist(as.numeric(df1[,11]))


# To give the best hospital in a state for a particular outcome
best<-function(state,outcome){
  
  temp<-data.frame(table(df1$State))
  flag<-0
  for (i in temp[,1]){
    if (i==state) flag<-1
  }
  if (flag==0) stop('invalid state')
  
  if (outcome=='heart attack') out_col<-11
  else if (outcome=='heart failure') out_col<-17
  else if (outcome=='pneumonia') out_col<-23
  else stop('invalid outcome')
  
  print(tapply(df1[,out_col],df1$State,function(x) min(as.numeric(x),na.rm=TRUE)))  
  
  x<-split(df1[,c(2,7,out_col)],df1$State)
  print((x[[state]])
}








