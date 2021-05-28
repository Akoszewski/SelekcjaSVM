source("functions.r")

DATA_SET <- read.csv("Datasets/Peptidome1/Peptidome1.txt", sep="\t", header=FALSE)
DATA_SET <- as.data.frame(t(as.matrix(DATA_SET)))
CRITERIA <- DATA_SET[c(1),-c(1:2)]
DATA <- DATA_SET[-c(1), -c(1)]

#x <- data.matrix(DATA[,-c(1)])
x <- apply(as.matrix(DATA[,-c(1)]), 2, as.numeric)
# x <- DATA[,-c(1)]
y <- as.integer(data.matrix(DATA[,c(1)]))
y[c(1:5)]=3

#to trzeba poprawic rzeby wybiera³o losowo jeszcze na razie dzia³ajmy na ca³ym x
#n_train  <-  round(0.8*nrow(x))
#x_train  <-  x[c(0:n_train),]
#x_test   <-  x[-c(0:n_train),]
#y_train  <-  y[c(0:n_train)]
#y_test   <-  y[-c(0:n_train)]

#RANK = SVMRFE(x, y, CRITERIA)






#FScoreSelection(x_train, y_train, CRITERIA)
# print(fscores)
