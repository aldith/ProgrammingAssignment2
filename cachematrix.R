
# makeCacheMatrix a special "matrix" object that can cache its inverse.
## It works in combination with cacheSolve to get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Create a place for the cached value
  m <- NULL
  
  ## Create the matrix
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  ## Get the value of the matrix
  get <- function() x 
  ## Invert the matrix and implement the cached value
  setinverse <- function(inverse) m <<- inverse 
  ## Get the new matrix
  getinverse <- function() m  
  ## Return the function
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse) 
}

## cacheSolve return a matrix that is the inverse of 'x'
## calculated in makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Define a place for the matrix
  m <- x$getinverse()
  
  ## return the inverted matrix, if the inverse exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## create an inverted matrix, if the inverse doesn't exist
  else {
    data <- x$get()
    m <- solve(data)
    x$setinverse(m) 
    m
  }
}

