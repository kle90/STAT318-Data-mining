#Question 1
#a)

library(caret)
dim(iris)
summary(iris)

# there are 150 rows and 5 columns/variables. The variables are Sepal.length, Sepal.Width, Petal.Length, Petal.Width and Species.

#b)
#  Group by species
par(mfrow=c(2,2))
boxplot(Sepal.Length~Species, data = iris)
boxplot(Sepal.Width~Species, data = iris)
boxplot(Petal.Length~Species, data = iris)
boxplot(Petal.Width~Species, data = iris)

#c)
summary(iris)
# The data is not unbalacned as there are 3 classes in species (setosa, versicolor, virginica) with all equal ratios 50:50:50.

#d)

x <- iris[, 1:4]
y <- iris$Species
caret::featurePlot(x, 
                   y, 
                   plot="density", 
                   scales = list(x = list(relation="free"), 
                                 y = list(relation="free")), 
                   adjust = 1.5, 
                   pch = "o", 
                   layout = c(4, 1), 
                   auto.key = list(columns = 3))

# From the density plot for petal length and petal width, we can see that the density of setosa flower does not overlap with the densities of the versicolor and virginica iris flowers.
# Because there is no overlap these might be better predictors for flower species. 

# In the Sepal.Length and Sepal.Width plot all three densities (setosa, versicolor and virginica) have overlap which may mean they are not as significant for predictions. 
#There is a a lot more overlap between all three species for the sepal length and sepal width features, so these variables are not as significant in predictions.

The density plot for the petal length and petal width of setosa flowers does not overlap the densities of the versicolor and virginica iris flowers,
so these might be better predictors for flower species. There is a a lot more overlap between all three species for the sepal length and sepal width features, so these variables are not as significant in predictions.



#e)
nfolds <- 10
traincontrol = trainControl(method = "cv",
                            number = nfolds)
LDA <- train(Species ~ ., 
             data = iris, 
             method = "lda", 
             trControl = traincontrol)

KNN <- train(Species ~ ., 
             data = iris, 
             method = "knn", 
             tuneGrid   = expand.grid(k = c(1,2,3,4,5,6,7,8,9,10)),
             trControl = traincontrol, 
             metric="Accuracy")

results <- resamples(list(lda=LDA, knn=KNN))
summary(results)
dotplot(results)

The LDA appears to be slightly more accurate than the KNN model, and has a smaller 95% confidence interval as well. In this case, we would choose the LDA model for better accuracy and since it is a simpler model.

Question 2



library(tree)
library(gbm)
library(randomForest)

setwd("H:/Documents/STAT318/Assignments/Assignment3")
training = read.csv("carseatsTrain.csv")
testing = read.csv("carseatsTest.csv")

trainining_carseattree = tree(Sales~., data=training)
summary(trainining_carseattree)



plot(trainining_carseattree)
text(trainining_carseattree,
     cex=0.5)
prunedTree <- cv.tree(trainining_carseattree)
prunedTree

tree_prune <- prune.tree(prunedTree)
plot(tree_prune)
text(tree_prune,
     cex=0.7)
