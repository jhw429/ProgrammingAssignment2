## Matrix inversion is often a costly computation. Therefore, it can be better to cache its inverse.
## The following functions solve this problem.

## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse. 
## It consists of setting and getting the value of the matrix (set/get), and then set and get the inverse value (setinverse/getinverse)

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function, cacheSolve, computes the inverse of the matrix that was created by makeCache matrix. 
## In the if-loop, the function checks if the inverse has already been created.
## If the inverse exists, it gets the result (get). If not, the inverse will be computed (setinverse).

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
