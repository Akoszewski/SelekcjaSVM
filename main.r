rm(list = ls())

# library(Rdimtools)
library("ggplot2");
source("functions.r")
DATA_SET <- read.csv("Datasets/Peptidome1/Peptidome1.txt", sep="\t", header=FALSE)
#DATA_SET <- read.csv("Datasets/Leukemia/Leukemia_500.txt", sep="\t", header=FALSE)
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

featuresSvm <- SVM_RFE(x, y, CRITERIA)
c_parameter = GridSearchC(x, y, min_value = 0, max_value = 1000, number = 10);
featuresSvmOptimized <- SVM_RFE(x, y, CRITERIA, c_parameter)
# print(RANK_return)

#accuracySVM <- GetAccForBestFeatures(x, y, RANK_return$Criteria, 10)
criteria_calculated = read.csv("criteria.txt")
fscores = FScoreSelection(x, y, CRITERIA)
#fscores_shuffled <- fscores[sample(nrow(fscores)),]

# space wynosi domyslnie 1 a w przypadku pominiecia max funkcja przechodzi do konca wektora
accuraciesFscore <- GetAllAccuracies(fscores, space = 10, max = 300)
accuraciesSVM <- GetAllAccuracies(featuresSvm, space = 10, max = 300)
accuraciesSVMOptimized <- GetAllAccuracies(featuresSvmOptimized, space = 10, max = 300)
accuraciesSVMBestOptimized <- GetAllAccuracies(criteria_calculated, space = 10, max = 300)



wykres = ggplot() + 
   geom_line(data = accuraciesFscore, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies fscore")) +
   geom_line(data = accuraciesSVMOptimized, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies SVM Optimized")) +
   geom_line(data = accuraciesSVM, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies SVM")) +
   geom_line(data = accuraciesSVMBestOptimized, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies SVM best optimized")) +
   xlab('Number of features') +
   ylab('Accuracy') + 
   labs(title="Accuracy according to no of features")

print(wykres)
# # y[y == 2] <- 1
# fscores2 <- do.fscore(
#   x,
#   y,
#   # ndim = 2,
#   # preprocess = c("null", "center", "scale", "cscale", "decorrelate", "whiten")
# )


# print(fscores2)

