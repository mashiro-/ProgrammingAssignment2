## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is based on the one showed on the assessment page
## first it accept a matrix as input and initializes the value "inv" to be NULL
## the function returns a list of object (functions) and is able to cache the inverse
## of the matrix "x"
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                ## <<- is used to override the value of a variable when is in another
                ## environment
                x <<- y 
                ## once the function is called it initialize the inv value to NULL
                ## clearing the cached value
                inv <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve)  inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This is the function that calculates the inverse of the matrix x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        ## If the inverse value is already stored in the cache, it doesn't perform the
        ## calculation and retrieve the value stored in the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
## data is the local variable where the matrix is passed from the function "get()" in
## the makeCacheMatrix function
        data <- x$get()
        inv <- solve(data, ...)
## call the setinverse() function and store the "inv" in the cache
        x$setinverse(inv)
        inv
}
