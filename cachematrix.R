## Put comments here that give an overall description of what your
## functions do

## function containing 4 sub-functions to set/get the content of a matrix x  
## and get/set the inverse of matrix x
## using functions get/set and getsolve/setsolve resp.
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) m <<- solve
        
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## function to ensure that the solve() function is called only once on a 
## matrix; if the result of solve() has not been cached already.
##  
## if we have got a valid m already, return its value and exit function
## otherwise, get its data and calculate inverse of m using solve
## and assign calculated value to cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()        

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
      
        data <- x$get()
        m <- solve(data, ...)

        x$setsolve(m)
        m        
}
