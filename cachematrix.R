## this file contains two functions that work together to invert a matrix. It  
## retrieves a cached version of the solution if the solution has already been  
## calculated in order to prevent performance issues with larger matrices

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

        ## sets solution to NULL every time makeCacheMatrix is called
        s <- NULL
        
        
## Code from this point down is not executed when running makeCacheMatrix
## This area defines three sub-functions to be called by the 
## cacheSolve function
        
        
        ## Sub-function for cacheSolve to retrieve original matrix for calcs
        get <- function() x
        
        ## Sub-function for cacheSolve to store the first computation of the
        ## matrix inversion solution in a separate environment
        setinverse <- function(solution) s <<- solution
        
        ## Sub-function for cacheSolve to retrieve cached solution on
        ## subsequest runs
        getinverse <- function() s
        
        ## List that stores the subfunction definitions for passing to cacheSolve
        list(get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}

## cacheSolve: This function computes the inverse of the matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Checks if there is a stored solution
        s <- x$getinverse()
        
        ## If the stored solution variable is not NULL, return the cached solution
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## If there is no cached solution, retrieve the original matrix by calling  
        ## the get() subfunction from makeCacheMatrix
        data <- x$get()
        
        ## Calculate, cache and return the solution    
        s <- solve(data, ...)
        x$setinverse(s)
        s
        
}
