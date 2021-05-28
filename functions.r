library(e1071)

SVMRFE <- function(x, y, CRITERIA) {
    
    RANK <- data.frame(c(1:(length(CRITERIA))))
    p <- ncol(CRITERIA)

    
    x_svm = x
    CRITERIA_SVM = CRITERIA
    while (p >= 2) {
        model <- svm(x_svm, y, kernel = "linear", scale=FALSE)
        beta = drop(t(model$coefs)%*%x_svm[model$index,])
        beta0 = model$rho
        
        rank_criteria <- beta^2
        min_index <- which.min(rank_criteria)
        
        RANK[p, 1] <- CRITERIA_SVM[min_index]
        CRITERIA_SVM = CRITERIA_SVM[,-c(min_index)]
        x_svm <- x_svm[,-c(min_index)]
        p <- p - 1
        
    }

    RANK[1, 1] <- CRITERIA_SVM[1]
    return (RANK)
}

FScoreSelection <- function(x, y, CRITERIA) {
    fscores <- c()
    bestFscores <- c()
    bestIndices <- c()
    # RANK <- data.frame(c(1:(length(CRITERIA))))
    for(col in 1:ncol(x)) {
        x1 = x[which(y == 1), col]
        x0 = x[which(y == -1), col]
        fscore = abs(mean(x0) - mean(x1))/sqrt(var(x0) + var(x1))
        fscores <- append(fscores, fscore)

        # if (fscore > 2.0) {         # potem to bedzie lepiej rozwiazane
        #     bestFscores <- c(bestFscores, fscore)
        #     bestIndices <- c(bestIndices, col)
        # }
        # else {

        # }
    }
    df <- data.frame(fscores, t(CRITERIA))
    df <- df[order(-fscores),]
    return (df)
}
