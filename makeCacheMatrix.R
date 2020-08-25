makeCacheMatrix <- function(x = matrix()){#intialising function
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL  #set function for matrix x
      }
      get <- function() {x}# get funtion
      setInverse <- function(inverse) {inv <<- inverse}# inverse funtionf or matrix x
      getInverse <- function() {inv}#get inverse function
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}#creating the list

cacheSolve <- function(x, ...){#gets cache data
      inv <- x$getInverse()
      if(!is.null(inv)){#checking where inverse is null
            message("getting cached data")
            return(inv)#returns inverse value
      }
      mat <- x$get()
      inv <- solve(mat, ...) #calculate inverse value
      x$setInverse(inv)
      inv         #returns inverse value
      
}
