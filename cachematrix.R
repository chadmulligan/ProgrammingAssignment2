## A set of two functions that calculate and cache the inverse of a matrix. 

## makeCacheMatrix creates a matrix that can cache its inverse (xinv is assigned in a higher environment thanks to "<<-"), 
## so xinv is accessible from other environments (said: functions, more specifically here, cacheSolve()).
## It comprehends a set 4 functions that sets, gets the matrix provided as an argument, and that sets and gets its inverse.
 

makeCacheMatrix <- function(x = matrix()) {
              xinv <- NULL
              
              #if you set a new matrix here with set(), the inverse "xinv" will be resetted. 
              #if xinv was already storing a value, now it is set to NULL (xinv <<- NULL)
              #thus, when cacheSolve will call x$getinv() for the first time on the new matrix
              #it will return NULL as the inverse has not yet been computed. 
              set <- function(y) {
                  x <<- y
                  xinv <<- NULL
              }
              
              #returnsthe original matrix
              get <- function() x
              
              #forces storing the argument matrixinv as the inverse of the matrix xinv
              setinv <- function(matrixinv) xinv <<- matrixinv
              
              #returns the matrix stored as the inverse in xinv
              getinv <- function() xinv
              
              list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}


## cacheSolve will return the stored inverse matrix xinv set by makeCacheMatrix. 
## if such a value does not exist, il will calculate the inverse of matrix x, and store the value in xinv. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              #Testing whether the inverse has already been calculated
              xinv <- x$getinv()
              
              #if so, getinv() <> NULL, and it returns xinv
              if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
              }
              
              #if not, calculate and return the inverse thanks to solve(), and store the output in xinv.
              data <- x$get()
              xinv <- solve(data, ...)
              x$setinv(xinv)
              xinv
}


#Testing with an invertible matrix
ma<-matrix(c(1, 1, -1, 2), 2, 2, byrow=T)
macache <- makeCacheMatrix(ma)
cacheSolve(macache)

#the inverse has already been calculated, calling cacheSolve() should return "getting cached data". 
cacheSolve(macache)

#let's set a new invertible matrix within macache
macache$set(matrix(c(2,3, 2, 2), 2, 2, byrow=T))

#let's see whether we have an inverse for this one
macache$getinv()

#nope. Line 15 resetted it. Let's call cacheSolve. 
cacheSolve(macache)

#again. See? "getting cached data". 
cacheSolve(macache)

#Have a nice day. 
