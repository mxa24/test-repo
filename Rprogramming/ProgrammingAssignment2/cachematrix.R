## The two functions makeCacheMatrixx and cacheSolve take a square matrix, generate the
## inverse and then store the inverse in the cache so that it can be looked up again
## thus reducing the compute time needed to recall the inverse of the matrix.


## The first function, makeCacheMatrix creates a special "matrix", object that can 
## cache its inverse which is really a list containing a function to:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse
## 4. get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function (y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix <<- solve
        getinverse <- function() matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The next function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        matrix <- x$getinverse()
        if(!is.null(matrix)) {
              message("getting cached data")
              return(matrix)
        }
        data <- x$get()
        matrix <- solve(data, ...)
        x$setinverse(matrix)
        matrix
}

## To use the above functions with an example square matrix:
## source("cachematrix.R")
## max <- matrix(data = c(3,1,8,5), nrow = 2, ncol = 2)
## max2 <- makeCacheMatrix(max)
## cacheSolve(max2)

## Note:
## cacheSolve(max2) should generate the same output as solve(max2)
## the second time that cacheSolve(max2) is called, it should print the
## message "getting cached data".
