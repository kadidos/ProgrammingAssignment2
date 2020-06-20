## assignment2 , Kadidos
## this function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

        k <- NULL
       set <- function(y) {
          x <<- y
          k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse
  getinverse <- function() k
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
       i <- x$getinverse()
       if (!is.null(k)) {
          message("getting cached data")
          return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}
