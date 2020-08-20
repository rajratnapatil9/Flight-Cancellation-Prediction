source("TRAINED_MODEL.R")
source("new_input_sample.R")


score <- function(new_input_sample)
{
  #using qda since that gave maximum AUC on training set
  pred.qda <- (predict(qda_fits, new_input_sample)$posterior)[,'1']
  return(pred.qda)
}
label <- function(new_input_sample)
{
  Probs<-score(new_input_sample)
  #choosing thrshold 0.85 after conducting threshold vs accuracy analysis on training dataset
  threshold<-0.85
  test_size <- nrow(new_input_sample)
  lables=rep(0,test_size)
  lables[Probs>threshold]=1
  return(lables)
}
score(new_input_sample)
label(new_input_sample)


