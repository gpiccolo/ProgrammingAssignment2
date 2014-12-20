## These functions provide a caching mechanism to store the inverse of a matrix provided. 
## For this exercise, it is assumed that the matrix is always invertible

## This function store the original matrix and the inverse of that.
## Provides some getter and setter to manipulate the given matrix and
## his inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #inverse of matrix
  set <- function(y) {
    #set the matrix in the object
    x <<- y
    i <<- NULL #inital state
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function check if an inverse of given matrix was previously calculated and 
## stored in cache. If value was stored in cache, the function return it insteard recalculate
## the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
          #the inverse is already un cache, no caching needed
          message("get inverse from cache")
          return(i)
        }
        #else compute the inverse of matrix
        matrix <- x$get()
        i <- solve(matrix)
        x$setInverse(i) #cache result
        i
}
