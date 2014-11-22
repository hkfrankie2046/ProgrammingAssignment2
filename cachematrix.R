## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##Defining the functions makeCacheMatrix
makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  
## 1. Set the value of the matrix  
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }

## 2. Get the value of the matrix
  get <- function() m

## 3. Set the value of the inverse
  setInverse <- function(inverse) i <<- inverse

## 4. Get the value of the inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
## 1. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }

## 2. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}