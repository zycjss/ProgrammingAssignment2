## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #give value to i
  set <- function(y) {
    x <<- y   
    #after setting y, x=y, i=null
    i <<- NULL
  }
  get <- function() x
  #give value of x to get
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  #copy i to getinverse()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #cache appears
        data <- x$get()
        #give the x-numeric to data
        i <- solve(data, ...)
        #calculate inverse
        x$setinverse(i)
        #cache i
        i
        #return i
}
