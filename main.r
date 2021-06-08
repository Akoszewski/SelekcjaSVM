# library(Rdimtools)
# library(sqldf)
source("functions.r")
DATA_SET <- read.csv("Datasets/Peptidome1/Peptidome1.txt", sep="\t", header=FALSE)
DATA_SET <- as.data.frame(t(as.matrix(DATA_SET)))
CRITERIA <- DATA_SET[c(1),-c(1:2)]
DATA <- DATA_SET[-c(1), -c(1)]
DATA <- DATA[sample(nrow(DATA)),] # shuffle data


#x <- data.matrix(DATA[,-c(1)])
x <- apply(as.matrix(DATA[,-c(1)]), 2, as.numeric)
# x <- DATA[,-c(1)]
y <- as.integer(data.matrix(DATA[,c(1)]))
#y[c(1:5)]=3



#norm_minmax = norm_minmax(x)


#to trzeba poprawic rzeby wybiera�o losowo jeszcze na razie dzia�ajmy na ca�ym x
n_train  <-  round(0.6*nrow(x))
x_train  <-  x[c(0:n_train),]
x_test   <-  x[-c(0:n_train),]
y_train  <-  y[c(0:n_train)]
y_test   <-  y[-c(0:n_train)]

# RANK_return = SVM_RFE(x, y, CRITERIA)
# print(RANK_return)

fscores = FScoreSelection(x, y, CRITERIA)
# print(fscores)

model <- svm(x_train, y_train, kernel = "linear", scale=FALSE)
# plot.svm(x_train, y_train, formula, fill = TRUE, grid = 50, slice = list(),symbolPalette = palette(), svSymbol = "x", dataSymbol = "o", ...)
pred <- predict(model, x_test)
df <- data.frame(pred, y_test)
# print(df)

#sqldf("select * from df where Amount in (select min(Amount) from df)")

bestFeatures <- fscores[1:100,]
print(bestFeatures)
DATA_F <- DATA[bestFeatures$Criteria]
print(DATA_F)

#x <- data.matrix(DATA[,-c(1)])
x <- apply(as.matrix(DATA_F[,-c(1)]), 2, as.numeric)
# x <- DATA[,-c(1)]
# y <- as.integer(data.matrix(DATA_F[,c(1)]))
#y[c(1:5)]=3

n_train  <-  round(0.6*nrow(x))
x_train  <-  x[c(0:n_train),]
x_test   <-  x[-c(0:n_train),]
# y_train  <-  y[c(0:n_train)]
# y_test   <-  y[-c(0:n_train)]
# print(y_test)

model <- svm(x_train, y_train, kernel = "linear", scale=FALSE)
# plot.svm(x_train, y_train, formula, fill = TRUE, grid = 50, slice = list(),symbolPalette = palette(), svSymbol = "x", dataSymbol = "o", ...)
pred <- predict(model, x_test)
df <- data.frame(pred, y_test)
print(df)

# # y[y == 2] <- 1
# fscores2 <- do.fscore(
#   x,
#   y,
#   # ndim = 2,
#   # preprocess = c("null", "center", "scale", "cscale", "decorrelate", "whiten")
# )

# print(fscores2)

