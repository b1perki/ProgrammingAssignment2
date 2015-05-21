## These two functions are used to demonstrate the ability to cache the answer to
## a numeric function so that it doesn't have to be computed over and over again.

## makeCacheMatrix - creates a special matrix object that can then be used in the 
## follow-on cacheSolve function to get the inverse of the matrix either 
## from a cached location or by running the solve function and then caching the 
## answer for later use.  This function takes a matrix(x) as its input
## or at least an object that can be coerced into a matrix.


makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function (y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invx <<- solve
        getinv <- function() invx
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  This function takes a special matrix object created in the previous function
##  and gets the inverse of it - first checking to see if the inverse was 
##  previously cached - if so it pulls the answer from cache - if not it uses
##  the solve function to get the inverse and then stores in it cache for later
##  use.  It uses getinv and setinv functions from the special matrix object 
##  created in the function above to save and retrieve the inverse so it doesn't 
##  have to be computed each time. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                invx <- x$getinv()
                if (!is.null(invx)) {
                        message("getting cached data")
                        return (invx)
                }
                data <- x$get()
                invx <- solve(data)
                x$setinv(invx)
                invx
}
