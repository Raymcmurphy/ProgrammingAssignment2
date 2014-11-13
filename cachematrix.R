## Two functions calculate the inverse matrix for a given one and store the result in cache

## The first function takes matrix and store it and its inverse matrix in cache
## We can create and store new matrix by x$setM(matrix()) than defining of
## inverse matrix ahould be implemented again using second function that store
## inverse matrix in environment of the first function

makeCacheMatrix <- function(x = matrix()) {
  IM<-NULL
  setM<-function(y) {
    x<<-y
    IM<<-NULL
  }
  getM<-function() x
  setIM<-function(cacheIM) IM<<-cacheIM
  getIM<-function() IM
  list (setM=setM, getM=getM, setIM=setIM, getIM=getIM)
}


## The second function at the first step takes the value of the inverse matrix from
## the first function and check if it equals 'NULL' or not. If it is not equal 'NUll' so
## it was calculted previously and stored in the cache of the first function.
## In that case the second function return the cached inverse matrix. in the case if the 
## value of inverse matrix from the first function equals 'NULL', so the 
## inverse matrix was not calculated and the second calculate it and store in cache of
## the first function.

cacheSolve <- function(x, ...) {
  IM <- x$getIM()
  if(!is.null(IM)) {
    message("getting cached inverse matrix")
    return (IM)
  }
  data<-x$getM()
  IM<-solve(data)
  x$setIM(IM)
  IM
  ## Return a matrix that is the inverse of 'x'
}
