rm(list = ls())

# library(Rdimtools)
library("ggplot2");
source("functions.r")
# DATA_SET <- read.csv("Datasets/Peptidome1/Peptidome1.txt", sep="\t", header=FALSE)
DATA_SET <- read.csv("Datasets/Leukemia/Leukemia_500.txt", sep="\t", header=FALSE)
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

RANK_return <- SVM_RFE(x, y, CRITERIA)
# print(RANK_return)

#accuracySVM <- GetAccForBestFeatures(x, y, RANK_return$Criteria, 10)

fscores = FScoreSelection(x, y, CRITERIA)
fscores_shuffled <- fscores[sample(nrow(fscores)),]

# space wynosi domyslnie 1 a w przypadku pominiecia max funkcja przechodzi do konca wektora
accuraciesFscore <- GetAllAccuracies(fscores, space = 10, max = 300)
accuraciesRandom <- GetAllAccuracies(fscores_shuffled, space = 10, max = 300)
accuraciesSVM <- GetAllAccuracies(RANK_return, space = 10, max = 300)


wykres = ggplot() + 
   geom_line(data = accuraciesFscore, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies fscore")) +
   geom_line(data = accuraciesRandom, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies random")) +
   geom_line(data = accuraciesSVM, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies SVM")) +
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

