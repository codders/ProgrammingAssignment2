## These functions create a Matrix with a "cached", or lazily
## evaluated solution. For some reason this is written as two
## functions at module scope, instead of encapsulating one
## inside the other. I suspect the lecturers for this course
## are not software engineers.

## This function creates the so-called 'CacheMatrix'.
## It returns a list of functions that allow you to get
## and set the value of the matrix, and to violate the 
## encapsulation of the "caching" by directly manipulating
## the value stored in the cache.
makeCacheMatrix <- function(storedMatrix = matrix()) {
  cachedInverse <- NULL
  set <- function(newMatrix) {
    storedMatrix <<- newMatrix
    cachedInverse <<- NULL
  }
  get <- function() storedMatrix
  ## This setInverse function should really be internal to makeCacheMatrix
  setInverse <- function(inverse) cachedInverse <<- inverse
  ## This getInverse function should be where the inverse is lazily calculated
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the result of a makeCacheMatrix and calculates
## the inverse of the matrix (unless it has already been calculated).
## This really ought to be defined inside the lexical scope of makeCacheMatrix,
## but it isn't because simpletons.
cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  storedMatrix <- cacheMatrix$get()
  inverse <- solve(storedMatrix)
  cacheMatrix$setInverse(inverse)
  inverse
}

## This is how that function should have looked
betterCacheMatrix <- function(storedMatrix = matrix()) {
  cachedInverse <- NULL
  set <- function(newMatrix) {
    storedMatrix <<- newMatrix
    cachedInverse <<- NULL
  }
  get <- function() storedMatrix
  getInverse <- function() {
    if (is.null(cachedInverse)) {
      cachedInverse <<- solve(storedMatrix)
    } else {
      message("Returning cached inverse")
    }
    return(cachedInverse)
  } 
  list(set = set, get = get,
       getInverse = getInverse)
}

