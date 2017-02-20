library(neuralnet)
library(readr)
leaves <- read_csv("D:/Uni/4/pr/leaves.csv")

convert_names <- function(x) {
  x <- factor(x, levels = c(1,2,3), labels = c("Acer", "Quercus","Alnus"))
}

leaves$Type <- convert_names(leaves$Type)
ind <- sample(2, nrow(leaves), replace = TRUE, prob=c(0.7, 0.3))
trainset = leaves[ind == 1,]
testset = leaves[ind == 2,]

trainset$Acer = trainset$Type == "Acer"
trainset$Quercus = trainset$Type == "Quercus"
trainset$Alnus = trainset$Type == "Alnus"

#network = neuralnet(Acer + Quercus + Alnus ~ AreaR + Eccentricity + Extent + AxisRat + Solidity, 
#                    trainset, rep = 10, hidden= 6, threshold = 0.011,
#                    stepmax = 1e+07, lifesign = "minimum",
#                    lifesign.step = 1000,
#                    learningrate.factor = list(minus = 0.5, plus = 1.2))

nn = neuralnet(Acer + Quercus + Alnus ~ AreaR + Eccentricity + Extent + AxisRat + Solidity, 
                    trainset, rep = 5, hidden= 6, threshold = 0.011,
                    stepmax = 1e+07, lifesign = "minimum",
                    lifesign.step = 1000,
                    learningrate.factor = list(minus = 0.5, plus = 1.2))

nn$result.matrix
head(nn$generalized.weights[[1]])
plot(nn)
