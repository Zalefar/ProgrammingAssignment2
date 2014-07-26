## Creates and saves the inverse matrix of a user supplied matrix. Caches
## last evaluated inverse matrix. Can prevent the neccesity of reevaluating 
## the same matrix for its inverse repeatedly. 
## Assumes invertability of passed matrices.
 


## Creates matrix and special list of functions whose purpose is 
## to find and set inverse values for supplied matrices,
## Pass a vector and desired number of row and columns.
## standard matrix arguments. (c(),row#, column#)
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL 
    
    set <- function(y){ ## Set new matrices whose inverses we wish to find
        x <<- y
        inv <<- Null
    }
    
    get <- function() ## Gets and prints the current matrix to double check it is the correct one
        x
    
    setInv <- function(inverse) ## Sets the inverse matrix for the current matrix
        inv <<- inverse
    
    getInv <- function() ## returns the inverse matrix to make sure its present
        inv
    
    ## Allows us to call and set makeMatrix functions by 'x$(__func_name__)' 
    list(set = set, 
          get = get,
          setInv = setInv,
          getInv = getInv) 
}


## function finds inverse if not already set for supplied matrix and 
## then moves to cahce the inverse value. 
## uses solve() and lets 'b' be taken as identity matrix
cacheSolve <- function(x, ...) {
       inv <- x$getInv()    ## retrieves inverse value
       
       if(!is.null(inv)){   ## checks to see if inverse matrix already exists
           message("retrieving cached inverse matrix")
           inv ## if inverse matrix already exist, returns it
       }
       
       matrix <- x$get() ## if inverse matrix does not exit find current matrix
       inv <- solve(matrix,...) ## finds the inverse matrix of the current matrix
       x$setInv(inv)    ## sets the inverse value for that matrix
       inv              ## returns the inverse matrix
       
}
