flight<-read.csv("flight.csv")
#Removing NAs and 2 rows where distance is 0 and negative
flight<-flight[-c(1193,2966,504,315,3064,674,3746),]
smp_size <- floor(0.99 * nrow(flight))
train_ind <- sample(seq_len(nrow(flight)), size = smp_size)
train <- flight[train_ind, ]
qda_fits=qda(Canceled~.-ArrDelay-SchedElapsedTime-UniqueCarrier, data = train)