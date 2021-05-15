library(e1071)
DATA <- read.csv("D:/Studia/Mgr/Semestr_3/UMB/PROJEKT/SelekcjaSVM/Datasets/Peptidome1/Peptidome1.txt", sep="\t", header=FALSE)
DATA <- as.data.frame(t(as.matrix(DATA)))
CRITERIA <- DATA[c(1),-c(1:2)]
DATA <- DATA[-c(1), -c(1)]

x <- data.matrix(DATA[,-c(1)])
y <- as.integer(data.matrix(DATA[,c(1)]))

y[y==2]=-1

n_train  <-  round(0.8*nrow(x))
x_train  <-  x[c(0:n_train),]
x_test   <-  x[-c(0:n_train),]
y_train  <-  y[c(0:n_train)]
y_test   <-  y[-c(0:n_train)]

p <- length(CRITERIA)

RANK <- data.frame(c(1:(length(CRITERIA)-1)))


while(p>=1){
  model <- svm(x_train, y_train, kernel = "linear", scale=FALSE)
  rank_criteria <- model[["decision.values"]]^2
  min_index <- which.min(rank_criteria)

  RANK[p,1] <- min_index
  RANK[p,2] <- CRITERIA[min_index]

  
  x_train <- x_train[,-c(min_index)]
  p <- p-1
}
