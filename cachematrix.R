## The following code is for the programming assignment 2 of Coursera course
## R programming.
## The details can be found here:
## https://class.coursera.org/rprog-004/human_grading/view/courses/972139/assessments/3
## 

## This function takes matrix x as input and caches x into the memory

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        matrix(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes matrix x as input and returns its inverse.
## The input is a matrix and the output is also a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
