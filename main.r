# library(Rdimtools)
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

RANK_return = SVM_RFE(x, y, CRITERIA)
# print(RANK_return)

fscores = FScoreSelection(x, y, CRITERIA)

print('For best features:')
accuracy <- GetAccForBestFeatures(x, y, fscores, 10)
accuracy <- GetAccForBestFeatures(x, y, fscores, 100)
accuracy <- GetAccForBestFeatures(x, y, fscores, 500)
accuracy <- GetAccForBestFeatures(x, y, fscores, nrow(fscores))
print('')
print('For random features:')
fscores <- fscores[sample(nrow(fscores)),]
accuracy <- GetAccForBestFeatures(x, y, fscores, 10)
accuracy <- GetAccForBestFeatures(x, y, fscores, 100)
accuracy <- GetAccForBestFeatures(x, y, fscores, 500)
accuracy <- GetAccForBestFeatures(x, y, fscores, nrow(fscores))

# # y[y == 2] <- 1
# fscores2 <- do.fscore(
#   x,
#   y,
#   # ndim = 2,
#   # preprocess = c("null", "center", "scale", "cscale", "decorrelate", "whiten")
# )

# print(fscores2)

