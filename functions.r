library(e1071)

SVM_RFE <- function(x, y, CRITERIA) {
    Rank <- data.frame(c(1:(length(CRITERIA))))
    p <- ncol(CRITERIA)
        x_svm = x
    CRITERIA_SVM = CRITERIA
    y_svm = y
    while (p >= 2) {
        rank_criteria = matrix(0, 1, ncol(CRITERIA_SVM))
        for (i in seq(y[which.max(y)])){ 
            y_svm[y==i] = 1
            y_svm[y!=i] = -1
            model <- svm(x_svm, y_svm, kernel = "linear", scale=FALSE)
            beta = drop(t(model$coefs)%*%x_svm[model$index,])
            rank_criteria = rank_criteria+beta^2
        }
        rank_criteria_average = rank_criteria/y[which.max(y)]
        min_index <- which.min(rank_criteria_average)
        Rank[p, 1] <- CRITERIA_SVM[min_index]
        CRITERIA_SVM = CRITERIA_SVM[,-c(min_index)]
        x_svm <- x_svm[,-c(min_index)]
        p <- p - 1
    }
    Rank[1, 1] <- CRITERIA_SVM[1]
    return (Rank)
}

FScoreSelection <- function(x, y, CRITERIA) {
    fscores <- c()
    bestFscores <- c()
    bestIndices <- c()
    # RANK <- data.frame(c(1:(length(CRITERIA))))
    for (col in 1:ncol(x)) {
        x1 = x[which(y == 1), col]
        x0 = x[which(y == 2), col]
        fscore = abs(mean(x0) - mean(x1))/sqrt(var(x0) + var(x1))
        fscores <- append(fscores, fscore)
    }
    df <- data.frame(fscores, t(CRITERIA))
    df <- df[order(-fscores),]
    colnames(df) <- c('Fscores', 'Criteria')
    return (df)
}

GetPrediction <- function(x, y, splitProportion = 0.6) {
    n_train  <-  round(splitProportion * nrow(x))
    x_train  <-  x[c(0:n_train),]
    x_test   <-  x[-c(0:n_train),]
    y_train  <-  y[c(0:n_train)]
    y_test   <-  y[-c(0:n_train)]
    model <- svm(x_train, y_train, kernel = "linear", scale=FALSE)
    # plot.svm(x_train, y_train, formula, fill = TRUE, grid = 50, slice = list(),symbolPalette = palette(), svSymbol = "x", dataSymbol = "o", ...)
    pred <- predict(model, x_test)
    df <- data.frame(pred, y_test)
    colnames(df) <- c('Predicted', 'Actual')
    return (df)
}

GetAccuracy <- function(prediction) {
    correct_vals <- 0
    for (row in 1:nrow(prediction)) {
        if (round(prediction[row, 'Predicted']) == prediction[row, 'Actual']) {
            correct_vals <- correct_vals + 1
        }
    }
    return (correct_vals/nrow(prediction))
}

GetAccForBestFeatures <- function(x, y, fscores, noOfFeatures) {
    bestFeatures <- fscores[1:noOfFeatures,]
    DATA_F <- DATA[bestFeatures$Criteria]

    x <- apply(as.matrix(DATA_F[,-c(1)]), 2, as.numeric)

    prediction <- GetPrediction(x, y)
    accuracy <- GetAccuracy(prediction)
    # print(prediction)
    print(paste("Accuracy (for", noOfFeatures, "features):", gsub(" ", "", paste(accuracy * 100, "%"))))
    return (accuracy)
}
