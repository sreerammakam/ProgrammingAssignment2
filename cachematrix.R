## makeCacheMatrix is a function that stores a special "matrix" value. It should be used with a variable to save that value.
## example a <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2)) creates a 2x2 matrix by row
## This function also is a list of 4 other functions set,get,setinverse,getinverse
## set is to change the value of the matrix to another value and sets the value of i to NULL
## get shows the latest value in the matrix
## setinverse is a assignment function which needs a value "inverse" to be passed and is assigned to i
## getinverse returns the value of the variable i which is set by the setinverse else NULL

makeCacheMatrix <- function(x = matrix()) {
        i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) i  <<- inverse
        getinverse  <- function() i
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        i  <- x$getinverse()
##Checks if the value of i is not null, if not null then it comes out and returns i
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
## If the value of i is null then it proceeds to calculate the inverse
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinverse(i)
        i
}