## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#################################################
# Function makeCachMatrix created to cache
# matrix and matrix inverse
# x - is first parameter that take matrix
#################################################

makeCacheMatrix <- function(x = matrix()) {
        
        inv_mtrx <- NULL
        mtrx <- x
        
        # Below functions to get & set matrix
        
        set <- function(y_mtrx = matrix()) {
                mtrx <<- y_mtrx
                inv_mtrx <<- NULL
        }
        
        get <- function() mtrx
        
        
        # Below functions to get & set matrix inverse
                
        get_inv_mtrx <- function() inv_mtrx
        
        set_inv_mtrx <- function(x) inv_mtrx <<- x
        
        # Return list of functions
                
        list(set = set, get = get,get_inv_mtrx = get_inv_mtrx,
             set_inv_mtrx = set_inv_mtrx)
        
}


## Write a short comment describing this function
############################################################################
# Functions cacheSolve calculates Inverse for matrix if x & 
# mk_cache_mtrx_list are not identical
# returned from makeCacheMatrix.
# x -                  is first parameter of type matrix that can be compared 
#                      with matrix cached in mk_cache_mtrx_list parameter
#                      (2nd param) 
# mk_cache_mtrx_list - is second parameter and is list returned by calling
#                      makeCacheMatrix function.
###########################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!exists("x")) stop("INPUT VALIDATION ERROR:Input matrix object does not defined")
        
        m <- mk_cache_mtrx_list$get()
        
        if(!is.null(mk_cache_mtrx_list$get_inv_mtrx()) & identical(m,x)) {
                message("getting cached data")
                return(mk_cache_mtrx_list$get_inv_mtrx())
        }
        
        #if Matrix is not identical set matrix.
        mk_cache_mtrx_list$set(x)
        
        #if inv_mtrx does not exist
        loc_inv_mtrx  <- solve(x, LINPACK = FALSE)
        
        #set inverse of matrix to cache
        mk_cache_mtrx_list$set_inv_mtrx(loc_inv_mtrx)
        
        loc_inv_mtrx
        
}
