## The combination of the below two functions are used to
## calculate the inverse of a matrix if not available in the
## cache. If it is available in the cache it is retreived from
## there

## This function contains a series of functions used to either 
## store the inverse matrix or return the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        getD<-function() x
        setInverse<-function(inv) i <<- inv
        getInverse<-function()i
        list(getD=getD,getInverse=getInverse,setInverse=setInverse)
}


## This function looks for the inverse of a matrix in the cache and
## if not availabe calculates it

cacheSolve <- function(x, ...) {
         i<-x$getInverse()
        if (!is.null(i)){
                message("Invoked Cache")
                return(i)
        }
        data<-x$getD()
        if (nrow(data)==ncol(data))
        {i<-solve(data)
        
        }else
        {
                message("Matrix not Square")
        }
        x$setInverse(i)
        i
}
