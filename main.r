# library(Rdimtools)
library("ggplot2");
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

#RANK_return = SVM_RFE(x, y, CRITERIA)
# print(RANK_return)

#accuracySVM <- GetAccForBestFeatures(x, y, RANK_return$Criteria, 10)

fscores = FScoreSelection(x, y, CRITERIA)

print('For best features:')
accuracy <- GetAccForBestFeatures(x, y, fscores$Criteria, 10)
accuracy <- GetAccForBestFeatures(x, y, fscores$Criteria, 100)
accuracy <- GetAccForBestFeatures(x, y, fscores$Criteria, 500)
accuracy <- GetAccForBestFeatures(x, y, fscores$Criteria, nrow(fscores))
print('')
print('For random features:')
fscores_shuffled <- fscores[sample(nrow(fscores)),]
accuracy <- GetAccForBestFeatures(x, y, fscores_shuffled$Criteria, 10)
accuracy <- GetAccForBestFeatures(x, y, fscores_shuffled$Criteria, 100)
accuracy <- GetAccForBestFeatures(x, y, fscores_shuffled$Criteria, 500)
accuracy <- GetAccForBestFeatures(x, y, fscores_shuffled$Criteria, nrow(fscores_shuffled))

numsOfScores <- c()
accuracies <- c()
for (numOfScores in 2:nrow(fscores)) {
    accuracy <- GetAccForBestFeatures(x, y, fscores$Criteria, numOfScores)
    numsOfScores <- c(numsOfScores, numOfScores)
    accuracies <- c(accuracies, accuracy)
}

accuraciesDf<- data.frame(numsOfScores, accuracies)
colnames(accuraciesDf) <- c('NumOfScores', 'Accuracy')

wykres = ggplot() + 
   geom_line(data = accuraciesDf, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies fscore")) +
   #geom_line(data = accuracies_conf, aes(x = LabelledDataSize, y = Accuracy, colour = "Accuracies random")) +
   xlab('Number of features') +
   ylab('Accuracy') + 
   labs(title="Fscore accuracy according to no of features")


# # y[y == 2] <- 1
# fscores2 <- do.fscore(
#   x,
#   y,
#   # ndim = 2,
#   # preprocess = c("null", "center", "scale", "cscale", "decorrelate", "whiten")
# )


# print(fscores2)

