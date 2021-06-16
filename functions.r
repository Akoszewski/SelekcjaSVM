library(e1071)

SVM_RFE <- function(x, y, CRITERIA) {
    # Rank <- data.frame(c(1:(length(CRITERIA))))
    rank <- c()
    p <- ncol(CRITERIA)
    x_svm = x
    CRITERIA_SVM = CRITERIA
    y_svm = y
    while (p >= 2) {
        rank_criteria = matrix(0, 1, ncol(CRITERIA_SVM))
        if (p %% 50 == 0) {
            print(paste("SVMRFE in progress p =", p))
        }
        for (i in seq(y[which.max(y)])){ 
            y_svm[y==i] = 1
            y_svm[y!=i] = -1
            model <- svm(x_svm, y_svm, kernel = "linear", scale=FALSE)
            beta = drop(t(model$coefs)%*%x_svm[model$index,])
            rank_criteria = rank_criteria+beta^2
        }
        rank_criteria_average = rank_criteria/y[which.max(y)]
        min_index <- which.min(rank_criteria_average)
        # Rank[p, 1] <- t(CRITERIA_SVM[min_index])
        rank <- c(t(CRITERIA_SVM[min_index]), rank)
        #rank <- c(rank, t(CRITERIA_SVM[min_index]))
        CRITERIA_SVM = CRITERIA_SVM[,-c(min_index)]
        x_svm <- x_svm[,-c(min_index)]
        p <- p - 1
    }
    
    rank <- c(as.character(CRITERIA_SVM[1]), rank)
    
    df <- data.frame(rank)
    colnames(df) <- c('Criteria')
    return (df)
}

FScoreSelection <- function(x, y, CRITERIA) {
    fscores <- c()
    bestFscores <- c()
    bestIndices <- c()
    # RANK <- data.frame(c(1:(length(CRITERIA))))
    for (col in 1:ncol(x)) {
        classes = unique(y)
        xj = c()
        fscore = 0
        for (class in classes) {
            nj <- sum(y == class) # number of element of value class in vector y
            fscore = fscore + nj * (mean(x[which(y == class)]) - mean(x[,col]))^2 / var(x[,col])^2
        }
        fscores <- append(fscores, fscore)
    }
    df <- data.frame(fscores, t(CRITERIA))
    df <- df[order(-fscores),]
    colnames(df) <- c('Fscores', 'Criteria')
    return (df)
}

GetAccForBestFeatures <- function(x, y, bestFeatures, noOfFeatures) {
    bestFeatures <- bestFeatures[1:noOfFeatures]
    DATA_F <- DATA[bestFeatures]
    x <- apply(as.matrix(DATA_F[,-c(1)]), 2, as.numeric)

    x_svm = x
    y_svm = y
    
    min_value = 0 
    max_value = 1000
    cros_number = length(y)
    number = 10
    
    accuracySum <- 0
    for (i in seq(y[which.max(y)])){ 
        y_svm[y==i] = 1
        y_svm[y!=i] = -1
        
        tot_accuracy_vec <- c()
        
        for(j in seq(number-1)){
            c_parameter = min_value+(max_value-min_value)/number*(j)
            model <- svm(x_svm, y_svm, 
                         kernel = "linear", scale=FALSE, class.weights = "inverse",
                         type = "C-classification", cost = c_parameter, cross = cros_number)
            tot_accuracy_vec = c(tot_accuracy_vec, model["tot.accuracy"])
        }
        
        
        max_accuracy_index <- which.max(tot_accuracy_vec)
        c_parameter_optima = min_value+(max_value-min_value)/number*max_accuracy_index
        tot_accuracy_vec <- c()
        
        for(k in seq(number)){
            c_parameter = c_parameter_optima+(max_value-min_value)/10/number*(k-number/2)
            model <- svm(x_svm, y_svm, 
                         kernel = "linear", scale=FALSE, class.weights = "inverse",
                         type = "C-classification", cost = c_parameter, cross = cros_number)
            tot_accuracy_vec = c(tot_accuracy_vec, model["tot.accuracy"], c_parameter)
        }
        
        max_accuracy_index <- which.max(tot_accuracy_vec)
        c_parameter_optima = c_parameter_optima+(max_value-min_value)/10/number*(max_accuracy_index-number/2)
        tot_accuracy_vec <- c()
        
        for(m in seq(number)){
            c_parameter = c_parameter_optima + (max_value-min_value)/100/number*(m-number/2)
            model <- svm(x_svm, y_svm, 
                         kernel = "linear", scale=FALSE, class.weights = "inverse",
                         type = "C-classification", cost = c_parameter, cross = cros_number)
            tot_accuracy_vec = c(tot_accuracy_vec, model["tot.accuracy"], c_parameter)
        }
        
        max_accuracy_index <- which.max(tot_accuracy_vec)
        c_parameter_optima = c_parameter_optima + (max_value-min_value)/100/number*(max_accuracy_index-number/2)
        
        model <- svm(x_svm, y_svm, 
                     kernel = "linear", scale = FALSE, class.weights = "inverse",
                     type = "C-classification", cost = c_parameter_optima, cross = cros_number)
        
        accuracySum <- accuracySum + model$tot.accuracy
    }
    accuracy <- accuracySum/y[which.max(y)]
    
    print(paste("Accuracy (for", noOfFeatures, "features):",
                gsub(" ", "", paste(accuracy, "%"))))
    return (accuracy)
}

GetAllAccuracies <- function(features, space = 1, max = 0) {
    numsOfScores <- c()
    accuracies <- c()
    if (max == 0) {
        max = nrow(features)
    }
    for (numOfScores in seq(2, max, by = space)) {
        accuracy <- GetAccForBestFeatures(x, y, features$Criteria, numOfScores)
        numsOfScores <- c(numsOfScores, numOfScores)
        accuracies <- c(accuracies, accuracy)
    }
    df <- data.frame(numsOfScores, accuracies)
    colnames(df) <- c('NumOfScores', 'Accuracy')
    return (df)
}
