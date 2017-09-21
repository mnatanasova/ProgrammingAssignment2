## This function creates a special "matrix".
## 

makeCacheMatrix <- function(x = matrix()) {
                   inv <- NULL
                   set <- function(y) {
                          x <<- y
                          inv <<- NULL                               ## set the value of the matrix
                   }
                   get <- function() x                               ## get the value ot the matrix
                   setinv <- function(inversion) inv <<- inversion   ## set the inverse of the special "matrix"
                   getinv <- function() inv                          ## get the inverse of the special "matrix"
                   list( set = set, get = get,
                         setinv = setinv,
                         getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with makeCacherMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
              inv <- x$getinv()
              if(!is.null(inv)){
                       message("getting cached data")
                       return(inv)
              }
              data <- x$get()
              inv <- solve(data, ...)  ## Computing the inverse of a square matrix was done with the solve function
              x$setinv(inv)
              inv       ## Return a matrix that is the inverse of 'x'
}
