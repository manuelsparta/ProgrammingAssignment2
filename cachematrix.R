## This function save a matrix and cache its inverse the
# first time an inversion is requested

makeCacheMatrix <- function(x = matrix()) {
  # the inverse matrix is initialized to NULL 
  minv <- NULL
  
  # the set function save the matrix and delete any
  # previously saved inverse
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  # the get function simply return the original matrix
  get <- function() x
  
  # the inverse matrix is computed and saved
  setinv <- function(solve) minv <<- solve
  
  # retrive the inverse of the original matrix
  getinv <- function() minv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function check if the invertion of a matrix was computed
# and return the cached value if it is available, 
# if not, the inverse matrix is computed and saved for 
# future references

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # retrive the current status of the inverse matrix
  minv <- x$getinv()
  # if minv is different fomr NULL, the inverse matrix has 
  # already been computed and the cache data can be returned
  # the function stop here
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  
  # there was not cached inverse matrix, this has to be computed
  # by retriving the original data and solving it
  message("cached data is empty, computing from scratch")
  data <- x$get()
  minv <- solve(data, ...)
  
  # before returning the inverse matrix, it is saved 
  # for future reference 
  x$setinv(minv)
  minv
}

