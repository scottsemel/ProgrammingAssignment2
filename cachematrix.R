A = matrix(c(2, 4, 3, 1, 5, 7, 6, 5, 4),nrow=3,ncol=3) 

# > solve(A)
#           [,1]       [,2]       [,3]
# [1,] -0.3191489  0.8085106 -0.5319149
# [2,] -0.0212766 -0.2127660  0.2978723
# [3,]  0.2765957 -0.2340426  0.1276596
#
# Just to make sure its giving the right inverse check a similar function
# install.packages('pracma') 
# library(pracma)
# This is pseudoinverse(A) which also works on non square matrices.
#
# pinv(A)
#           [,1]       [,2]       [,3]
# [1,] -0.3191489  0.8085106 -0.5319149
# [2,] -0.0212766 -0.2127660  0.2978723
# [3,]  0.2765957 -0.2340426  0.1276596
#

## This function turns a matrix into a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set just saves the value and clears m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get is a function with no arguments and just returns x
        get <- function() x
        ## setinverse takes the inverse and saves it in i
        setinverse <- function(inverse) i <<- inverse
        ## getinverse has no arguments and just returns i
        getinverse <- function() i
        ## 'list' just coerces them into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function uses the previous list of functions to save 
## the inverse of a matrix in a cache so it does not need to 
## be calculated if it was already calculated previously.
##
## This function returns a matrix that is the inverse of 'x' if 
## 'x' is a square matrix.

cacheSolve <- function(x, ...) {
        
        ## puts the inverse into i
        i <- x$getinverse()
        ## check to see if i is not empty
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## put the matrix into data
        data <- x$get()
        ## find the inverse
        i <- solve(data, ...)
        ## save the inverse
        x$setinverse(i)
        ## output the inverse
        i
}

# test example
mat = makeCacheMatrix(A)
cacheSolve(mat)


