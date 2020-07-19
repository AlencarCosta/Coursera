## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
## matriz is the portuguese word for matrix
makeCacheMatrix <- function( matriz = matrix() ) {
      
      ## Initializing
      k <- NULL
      
      ## setting a matrix
      set <- function( matrix ) {
            matriz <<- matrix
            k <<- NULL
      }
      
      ## getting the matrix
      get <- function() {
            matriz
      }
      
      ## Setting the inverse of the matrix
      setInverse <- function(inverse) {
            k <<- inverse
      }
      
      ## Getting the inverse of the matrix
      getInverse <- function() {
            k
      }
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      ##dados is the portuguese word for data
      matriz <- x$getInverse()
      
      if( !is.null(matriz) ) {
            message("getting cached data")
            return(matriz)
      }
      
      ## Get the matrix from our object
      dados <- x$get()
      
      ## Calculate the inverse using matrix multiplication
      matriz <- solve(dados) %*% dados
      
      ##inverse to the object
      x$setInverse(matriz)
      matriz
}