#import the package
# library(randomForest)
library(e1071)

data <- read.csv("./Datasets/Peptidome1/Peptidome1.txt", sep="\t", header=FALSE)
data <- as.data.frame(t(as.matrix(data)))
data <- data[-c(1), -c(1)]
nrow(data)
ncol(data)
length(data)

# data <- subset(data, select = -c(enrollee_id))

# # Set random seed to make results reproducible:
# set.seed(17)
# # Calculate the size of each of the data sets:
# data_set_size <- floor(nrow(data)/2)
# # Generate a random sample of "data_set_size" indexes
# indexes <- sample(1:nrow(data), size = data_set_size)
# # Assign the data to the correct sets
# training <- data[indexes,]
# validation1 <- data[-indexes,]

# print(training)

# # Perform training:
# rf_classifier = randomForest(target ~ ., data=training, ntree=100, mtry=10, importance=TRUE)

# dat = data.frame(x, y = as.factor(y))

# svmfit = svm(V2 ~ .,data, kernel = "linear", cost = 10, scale = FALSE)

indx <- sapply(data, is.factor)
data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))

x <- subset(data, select = -V2)
y <- data$V2
model <- svm(x, y)
print(model)

# test with train data
pred <- predict(model, x)
# (same as:)
# pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])