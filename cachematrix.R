## This file contains makeCacheMatrix and cacheSolve, meant to cache
## the inverse of a matrix.


## makeCacheMatrix takes an argument x which is an inputted matrix and 
## retrieves it through the get object. It then sets an inverse in 
## setsolve and gets it in getsolve.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve is designed to set an object s based off of the solve that
## was obtained previously into the getsolve object. It then checks
## to see if s is null, if it is not null it will print a message that it 
## is retrieving the previously cached data and then will return that 
## previously cached data. If it is null it will 
## set an object data and then solve.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

## Matrix used from Alan E. Berger's community forum post

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
aMatrix <- makeCacheMatrix(m1)
cacheSolve(aMatrix)
