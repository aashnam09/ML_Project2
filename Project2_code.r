install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",repos="http://download.mosek.com/R/8", configure.vars="PKG_MOSEKHOME=C:/Program Files/Mosek/8/tools/platform/win64x86 PKG_MOSEKLIB=mosek64")
library(Rmosek)
require(Rmosek)
library(caret)

AssignmentResults <- function()
{
#======================================== Experiment 1 =========================================
blog.set <- read.csv('C:/Users/aashn/OneDrive/Desktop/Assign1/BlogFeedback/blogData_train.csv',header=FALSE, sep=",")
set.seed(123)

train.set <- sample(seq_len(nrow(blog.set)), size = (floor(0.70*nrow(blog.set))))
blog.train.set <- blog.set[train.set, ]
blog.train.set

blog.test.set <- blog.set[-train.set, ]
blog.test.set

#========================================== Creating Feature matrix =================================
require(Rmosek)
blog.train.xMatrix <- data.matrix(blog.train.set[,51:60])
blog.train.yMatrix <- data.matrix(blog.train.set[,"V281"])

# Empty list
qo.model <- list()
qo.model$sense <- "min"

# c variable in quadratic problem
cvar <- crossprod(blog.train.xMatrix,blog.train.yMatrix)
 
# coefficients
qo.model$c <- as.vector(cvar)
	
blog.x2matrix <- crossprod(blog.train.xMatrix, blog.train.xMatrix)
blog.x2matrix[upper.tri(blog.x2matrix)] <- 0

# Index of non-zero elements
blog.index <- which(blog.x2matrix != 0, arr.ind=TRUE)

# Q matrix - i : row indexes, j : column indexes, v : values
qo.model$qobj <- list(i = blog.index[,1], j = blog.index[,2], v = blog.x2matrix[blog.index])

# Constraint Matrix A
qo.model$A <- Matrix( rep(0, 10), nrow = 1, byrow=TRUE, sparse=TRUE )

# Constraint bounds
qo.model$bc <- rbind(blc=-Inf, buc= Inf) 

# Parameter bounds
qo.model$bx <- rbind(blx=rep(-Inf,10), bux = rep(Inf,10))

# Mosek solver
r <- mosek(qo.model)
	
print("Mean square error for Rmosek implementation of training data")
RmosekResults(blog.train.set[, 51:60], blog.train.set[, "V281"])

print("Mean square error for Rmosek implementation of test data")
RmosekResults(blog.test.set[, 51:60], blog.test.set[, "V281"])
	
LinearModelResults(blog.train.set, blog.test.set)
}

#=========================================Linear Model Implementation===========================================================
LinearModelResults <- function(trainsetData, testsetData) 
{

blog.data <- trainsetData[, c(51:60, 281)]
blog.lm <- lm(V281~., data=blog.data)
print("Mean square error for Linear Model of training data")
print(mean(blog.lm$residuals^2))
	
blog.predict <- predict(blog.lm, testsetData[, c(51:60, 281)], se.fit=TRUE)
		
mseLM = mean((blog.predict$fit - testsetData[,281])^2)
print("Mean square error for Linear Model of Test data")
print(mseLM)

}

#=============================================Rmosek Implementation===========================================================
RmosekResults <- function(blogX, blogY)
{

yHat = blogX[,1]*-0.20473016 + blogX[,2]*-0.28727579 + blogX[,3]*0.02768148 + blogX[,4]*0.21594995 + blogX[,5]*0.00000000 + blogX[,6]*2.30850597 + blogX[,7]*-3.28659353 + blogX[,8]*-1.41793199 + blogX[,9]*-1.78338969
print(yHat)
	
error <- (blogY - yHat)*(blogY - yHat)
	
mseRM <- mean(error)
print(mseRM)

}
