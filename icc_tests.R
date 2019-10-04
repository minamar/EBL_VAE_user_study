#We examine the intra-class correlation (ICC) to determine if multi-level modeling is the correct choice for our analysis. 
#The ICC measures the degree of clustering in our data and answers the question, “How much does my Level 2 predict the total variance of my study?”
#If your ICC is greater than 0, you have a multi-level study.

ICC.Model<-function(Model.Name) {
  tau.Null<-as.numeric(lapply(summary(Model.Name)$varcor, diag))
  sigma.Null <- as.numeric(attr(summary(Model.Name)$varcor, "sc")^2)
  ICC.Null <- tau.Null/(tau.Null+sigma.Null)
  return(ICC.Null)
}

ICC.Model(Null)