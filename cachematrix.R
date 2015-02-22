## Following functions cache's the inverse of a matrix and avoids recalculation when called for at a later time

## makeCacheMatrix creates the inverse of a matrix and cache's it(saves it) in memory

## makeCacheMatrix takes as its argument a matrix supplied by the user
makeCacheMatrix <- function(x = matrix()) {
     ## creates a new vector m and assigns the value null to the vector
     m <- NULL
     ## set is assigned the function which takes as its argument the value y, 
     set <- function(y) {
          ##where y searches for the variable x in the local environment and its parent environments 
          ##by lexical scoping
          x <<- y
          ##it binds the value NULL to the vector m in the parent environments as well since 
          ##new value for m will be computed and stored in the function closure for each value of x supplied
          m <<- NULL
     }
     ## gets the value of x in get
     get <- function() x
     ## setsolve stores the result of solving the inverse of matrix in m
     setsolve <- function(solve) m <<- solve
     ## getsolve gets and stores the value of m
     getsolve <- function() m
     ## creates a list of values to be stored/cached in the environment of the function
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## cacheSolve checks if the inverse of a previously studied matrix has been computed and returns it from memory
## cacheSolve takes as its argument a function --- makeCacheMatrix
cacheSolve <- function(x, ...) {
     ## it gets the value of m from the list cached as x$getsolve from makeCacheMatrix
     m <- x$getsolve()
     ##next it tests if the value of m is NULL which would be the case when cacheSolve is run for the first time
     ## and it skips the if loop
     if(!is.null(m)) {
          ## checks for any cached values of m in the environment of the given x value and returns the value 
          ## of m and the message
          message("getting cached data")
          return(m)
     }
     ## assigns the value of x$get to data
     data <- x$get()
     ## solves for the inverse of the matrix and assigns it to m
     m <- solve(data, ...)
     ## stores the value of m in the cache for use in future operations
     x$setsolve(m)
     ## returns the value of m - solution for the inverse of the matrix
     m
}
