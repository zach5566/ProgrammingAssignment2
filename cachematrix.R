## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that caches the matrix inverse

makeCacheMatrix <- function(x = matrix()) { ##make default mode matrix
    i <- NULL ##initialize matrix inverse as NULL
    set <- function(y) { ##define set function to assign
      x <<- y ##matrix value
      i <<- NULL ##if new matrix, reset i to NULL
    }
    get <- function()x ##gives value of matrix argument
    setinverse <- function(inverse)i<<- inverse ##assigns value of i
    getinverse <- function() { ##finds i for matrix x
      inver <- ginv(x)
      inver%*%x
    }
    list(set = set, get = get,  ##for $ operator
         setinverse = setinverse,
         getinverse = getinverse)
  }


## Write a short comment describing this function
## This function gives the inverse of the special matrix object created by the first function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() 
  if(!is.null(i)) { ##if i's not NULL, print message and i
    message ("getting cached data")
    return(i)
  }
  mat <- x$get() ##create object that contains x
  i <- solve(mat,...) ##get inverse of that object
  x$setinverse(i) ##find i and print it
  i 
}
