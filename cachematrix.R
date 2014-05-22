## Layne Mills, May 2014
## R programming, Assignment 2
## May, 2014

## makeCacheMatrix function
## Layne Mills, May 2014
## creates a list of functions for a matrix, storing and retrieving an inverse if it has been calculated
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    #internal Set function
    set <- function( y )
    {
        x <<- y
        inv <<- NULL
    }
    
    #internal Get function
    get <- function() x
    
    # internal setInverse function
    setInverse <- function( solve ) inv <<- solve
    
    # internal getInverse function
    getInverse <- function() inv
    
    #return this list that contains all these functions...usec by cacheSolve
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## cacheSolve function
## Layne Mills, May 2014
## Takes a matrix, of "type" cacheMatrix (really a list structure created by makeCacheMatrix)
## and determines whether cached version of calcuation already exists. If it does, it returns that,
## otherwise uses the functionality built into the cacheMatrix list to pull the value, then calculates
## an inverse, and caches the result, and returns it.
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    ## if it isn't null, been calculated before
    if ( !is.null( inv ) )
    {
        message( "Getting cached data" )
        return( inv )
    }
    
    ## ok, not found in structure, so grab the data from the list, and 
    ## calculate the inverse, and store it
    data <- x$get()
    inv <- solve( data, ...)
    x$setInverse( inv )
    
    # return the inverse
    inv
}
