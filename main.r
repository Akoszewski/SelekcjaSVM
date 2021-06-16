rm(list = ls())

library("ggplot2");
source("functions.r")

# zbior danych z dwiema klasami
# DATA_SET <- read.csv("Datasets/Peptidome1/Peptidome1.txt", sep="\t", header=FALSE)

# zbior z wieloma klasami
# DATA_SET <- read.csv("Datasets/Leukemia/Leukemia.txt", sep="\t", header=FALSE)
DATA_SET <- read.csv("Datasets/Leukemia/Leukemia_500.txt", sep="\t", header=FALSE)

DATA_SET <- as.data.frame(t(as.matrix(DATA_SET)))
CRITERIA <- DATA_SET[c(1),-c(1:2)]
DATA <- DATA_SET[-c(1), -c(1)]
DATA <- DATA[sample(nrow(DATA)),] # miesza dane losowo

x <- apply(as.matrix(DATA[,-c(1)]), 2, as.numeric)
y <- as.integer(data.matrix(DATA[,c(1)]))

featuresSvm <- SVM_RFE(x, y, CRITERIA)
c_parameter = GridSearchC(x, y, min_value = 0, max_value = 1000, number = 10);
featuresSvmOptimized <- SVM_RFE(x, y, CRITERIA, c_parameter)

fscores = FScoreSelection(x, y, CRITERIA)

# space wynosi domyslnie 1 a w przypadku pominiecia max funkcja przechodzi do konca wektora
accuraciesFscore <- GetAllAccuracies(fscores, space = 10, max = 300)
accuraciesSVM <- GetAllAccuracies(featuresSvm, space = 10, max = 300)
accuraciesSVMOptimized <- GetAllAccuracies(featuresSvmOptimized, space = 10, max = 300)

wykres = ggplot() + 
   geom_line(data = accuraciesFscore, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies fscore")) +
   geom_line(data = accuraciesSVMOptimized, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies SVM Optimized")) +
   geom_line(data = accuraciesSVM, aes(x = NumOfScores, y = Accuracy, colour = "Accuracies SVM")) +
   xlab('Number of features') +
   ylab('Accuracy') + 
   labs(title="Accuracy according to no of features")

print(wykres)
print("")
print("")

print("Wybrane wartości dokładności dla algorytmu SVMRFE z domyslnym parametrem kosztu:")
accuracySvm10 <- GetAccForBestFeatures(x, y, featuresSvm$Criteria, 10)
accuracySvm20 <- GetAccForBestFeatures(x, y, featuresSvm$Criteria, 20)
accuracySvm50 <- GetAccForBestFeatures(x, y, featuresSvm$Criteria, 50)
accuracySvm100 <- GetAccForBestFeatures(x, y, featuresSvm$Criteria, 100)
accuracySvm200 <- GetAccForBestFeatures(x, y, featuresSvm$Criteria, 200)
accuracySvm500 <- GetAccForBestFeatures(x, y, featuresSvm$Criteria, 500)

print("Wybrane wartości dokładności dla algorytmu SVMRFE z parametrem kosztu dobranym za pomocą Grid Search:")
accuracySvmOptimized10 <- GetAccForBestFeatures(x, y, featuresSvmOptimized$Criteria, 10)
accuracySvmOptimized20 <- GetAccForBestFeatures(x, y, featuresSvmOptimized$Criteria, 20)
accuracySvmOptimized50 <- GetAccForBestFeatures(x, y, featuresSvmOptimized$Criteria, 50)
accuracySvmOptimized100 <- GetAccForBestFeatures(x, y, featuresSvmOptimized$Criteria, 100)
accuracySvmOptimized200 <- GetAccForBestFeatures(x, y, featuresSvmOptimized$Criteria, 200)
accuracySvmOptimized500 <- GetAccForBestFeatures(x, y, featuresSvmOptimized$Criteria, 500)

print("Wybrane wartości dokładności dla cech otrzymanych z pomocą fscore:")
accuracyFscore10 <- GetAccForBestFeatures(x, y, fscores$Criteria, 10)
accuracyFscore20 <- GetAccForBestFeatures(x, y, fscores$Criteria, 20)
accuracyFscore50 <- GetAccForBestFeatures(x, y, fscores$Criteria, 50)
accuracyFscore100 <- GetAccForBestFeatures(x, y, fscores$Criteria, 100)
accuracyFscore200 <- GetAccForBestFeatures(x, y, fscores$Criteria, 200)
accuracyFscore500 <- GetAccForBestFeatures(x, y, fscores$Criteria, 500)

