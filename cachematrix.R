##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
                {
                  inverse<-NULL
                  set<-function(y)
                        {
                          x<<-y
                          inverse<<-NULL
                        }
                  get<- function() x
                  setinverse<-function(solve) inverse<<-solve(x)
                  getinverse<-function() inverse
                  list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
                }


## This function checks if there is a cached inverse for a matrix. 
## If there is, it prints the cached results, if not, it calculates it and caches the results

cacheSolve <- function(x, ...) 
              {
                inverse<-x$getinverse()
                if(!is.null(inverse))
                {
                  message("getting cached data")
                  return(inverse)
                }
                data<-x$get()
                inverse<-solve(data,...)
                x$setinverse(inverse)
                inverse
}

## test the code
x<-matrix(data=rnorm(10),2,2)
x
solve(x)
makeCacheMatrix(x)
cacheSolve(makeCacheMatrix(x))
