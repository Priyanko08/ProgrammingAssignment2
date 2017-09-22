## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching the Inverse of a Matrix:
## Without computing repeatedly it is better to cache the inverse of a matrix as its a complicated calculation 
## Through the below functions we have created a pair of objects which stores a matrix and cache its inverse 


makeCacheMatrix <- function(x = matrix()) {
    myinverse <- NULL
        set <- function(y) {
                x <<- y
                myinverse <<- NULL
        }
        get <- function() x
        settinginverseInverse <- function(inverse) myinverse <<- inverse
        getInverse <- function() myinverse
        list(set = set,
             get = get,
             settinginverseInverse = settinginverseInverse,
             getInverse = getInverse)
}



## Write a short comment describing this function
## The function calculates the inverse of "matrix" used through matrix makeCacheMatrix above.
## If we already calculate the inverse without changing the matrix then it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		   myinverse <- x$getInverse()
        if (!is.null(myinverse)) {
                message("getting cached data")
                return(myinverse)
        }
        mat <- x$get()
        myinverse <- solve(mat, ...)
        x$settinginverseInverse(myinverse)
        myinverse
}
##########################################################################################################################################

Solutions:

sol1 <- makeCacheMatrix(matrix(10:13, 2, 2))
sol1$get()

Output:
    
    > sol1$get()
[,1] [,2]
[1,]   10   12
[2,]   11   13



cacheSolve(sol1)

Output:
    
> cacheSolve(sol1)
      [,1] [,2]
[1,] -6.5    6
[2,]  5.5   -5    



sol1$getInverse()

> sol1$getInverse()
     [,1] [,2]
[1,] -6.5    6
[2,]  5.5   -5


sol1$set(matrix(c(20, 30, 10, 40), 2, 2))
sol1$get()

> sol1$get()
      [,1] [,2]
[1,]   20   10
[2,]   30   40


sol1$getInverse()
sol1$getInverse()
NULL


cacheSolve(sol1)
> cacheSolve(sol1)
       [,1]  [,2]
[1,]  0.08 -0.02
[2,] -0.06  0.04


sol1$getInverse()

> sol1$getInverse()
      [,1]  [,2]
[1,]  0.08 -0.02
[2,] -0.06  0.04





