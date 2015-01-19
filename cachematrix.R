## The following two functions are able to cache the inverse of a matrix 
## so that when we need it again, it can be looked up in the cache rather
## than recomputed, if the contents of a matrix are not changing.

## The first function, makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() m
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The second function, cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then it gets the inverse from the cache and
## skips the computation.

cacheSolve <- function(m, ...) {
    inv <- m$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- m$get()
    inv <- solve(matrix, ...)
    m$setinv(inv)
    inv
}
