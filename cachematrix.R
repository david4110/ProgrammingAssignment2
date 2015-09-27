## These two function blocks provide increased processing efficiency when
## doing inverse matrix computations. The first block reads the matrix and
## also stores and retrieves computed inverse values. The second block
## performs the actual matrix inversion, but first checks to determine if the
## inverse value is already stored from first function call processes.  

## This block outputs a list from functions reading matrix and caching inverse values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    # initializes matrix
  set <- function(y) {        
    x <<- y
    inv <<- NULL
    ## set provides way to change matrix variables in the larger function
     }
  get <- function() x
    ## stores matrix values
  setinverse <- function(inverse) inv <<- inverse
    ## takes in an arguement and saves it out to larger function
  getinverse <- function() inv
    ## returns the value from setinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
        ## saves the four function output in a list
  }



## This block checks if inverse cached, if not computes and sends to store inverse

cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
     ## pulls cached value of matrix inverse
  if(!is.null(inv)) {
    message("getting cached data")
      ## if cached value exists, retrives value and ends cachesolve function call
    return(inv)
    }
  data <- x$get()
    ## if no cache value exists, reads in matrix
  inv <- solve(data, ...)
    ## return a matrix that is the inverse of 'x'
  x$setinverse(inv)
    ## caches inverse matrix value
  inv
    ## returns inverse value
  
   }
