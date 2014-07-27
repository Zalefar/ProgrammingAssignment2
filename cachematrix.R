## Creates and saves the inverse matrix of a user supplied matrix. Caches
## last evaluated inverse matrix. Prevents the neccesity of re-evaluating 
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
        inv <<- NULL
    }
    
    get <- function() ## Returns the current matrix to double check it is the correct one
        x
    
    setInv <- function(inverse) ## Sets the inverse matrix for the current matrix
        inv <<- inverse
    
    getInv <- function() ## Returns the inverse matrix to make sure its present
        inv
    
    ## Allows us to call and set makeCacheMatrix functions by using extractor '$' 
    list(set = set, get = get, 
          setInv = setInv,
          getInv = getInv) 
}


## function finds inverse if not already set for supplied matrix and 
## then moves to cahce the inverse matrix. Otherwrise caculates inverse matrix.
## uses solve() and lets 'b' be taken as identity matrix
cacheSolve <- function(x, ...) {
       inv <- x$getInv()  ## retrieves inverse value
       
       if(!is.null(inv)){ ## checks to see if inverse matrix already exists and returns it
           message("retrieving cached inverse matrix")
           inv 
       }
       
       matrix <- x$get()        ## If inverse matrix does not exist find current
       inv <- solve(matrix,...) ## matrix, solve, cache and return it.
       x$setInv(inv)    
       inv              
       
}
