## The first function "makeCacheMatrix" accepts an invertible matrix and returns
## a vector which is a list of functions that can be performed. Four functions 
## can be performed - set, get, setinverse, and getinverse.
##The second function "cacheSolve" caches the inverse matrix. It first checks if
## an inverse matrix exists. If so, it gets the matrix from the cache. 
## if not it calculates the inverse and caches it.


## The function "makeCacheMatrix" accepts an invertible matrix, and returns a 
## list of functions - set, get, setinverse, getinverse. The function 'set' 
## assigns the input matrix to a variable 'x' in cache. The function 'get' 
## retrieves the matrix stored in the cache. Function 'setinverse' calculates
## inverse of the matrix and stores it in cachce. Function 'getinverse' 
## retrieves the inverted matrix from the cache.
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function 'cacheSolve' accepts the list of functions returned from the 
## function above. The output of this function is the inverted matrix,
## It uses the 'get' function from the input list of functions
## to retrieve the inverse matrix if it exists. If inverse matrix does not exist, 
## it is calculated and stored in the cache using the 'set' function from the 
## list of functions.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

