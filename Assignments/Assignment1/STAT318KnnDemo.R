## STAT318/462 kNN regression function

kNN <- function(k,x.train,y.train,x.pred) {
        # 
        ## This is kNN regression function for problems with
        ## 1 predictor
        #
        ## INPUTS
        #
        # k       = number of observations in nieghbourhood 
        # x.train = vector of training predictor values
        # y.train = vector of training response values
        # x.pred  = vector of predictor inputs with unknown
        #           response values 
        #
        ## OUTPUT
        #
        # y.pred  = predicted response values for x.pred
        
        ## Initialize:
        n.pred <- length(x.pred);		y.pred <- numeric(n.pred)
        
        ## Main Loop
        for (i in 1:n.pred){
                d <- abs(x.train - x.pred[i])
                dstar = d[order(d)[k]]
                y.pred[i] <- mean(y.train[d <= dstar])		
        }
        ## Return the vector of predictions
        invisible(y.pred)
}

set.seed(48792793)

training <- read.csv("AutoTrain.csv")
training

testing <- read.csv("AutoTest.csv")
testing

x.train <- training$horsepower
x.train
y.train <- training$mpg
y.train

x.pred <- testing$mpg
x.pred

#?seq
#x.pred <- seq(0, 0, length=1000)
#x.pred

test <- kNN(2, x.train, y.train, x.pred)
test

plot(x.train,
     y.train,
     cex=1,
     col="red",
     ylim=c(10, 50),
     xlim=c(0, 250)
)

k = 10
kNN.vals = kNN(k,
               x.train,
               y.train,
               x.pred)
points(x.pred,
       kNN.vals,
       pch=15,
       col="blue",
       cex=0.6)


