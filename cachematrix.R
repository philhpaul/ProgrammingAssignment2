## cache a matrix inversion operation
## 

## create a list to set and get value of a matrix and to set and get value of its inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL  # m is mat_invrs, reset to NULL on calling
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<-function(){x} #retn value of O mat
  setinverse<-function(inverse) {m<<-inverse} # store as sup_assigned on first access
  getinverse<-function(){m} #retn cached mat to cachematrix() on subsequent access
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #list internal funcs
}


## compute matrix inverse and set in cache if not already done
## else recover inverse from cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {   #if inverse already, returned as cached 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  # if m NULL, compute inverse
  x$setinverse(m)        # store computed value
  m                      # return result
}
