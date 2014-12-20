## makeCacheMatrix and CacheSove work as a pair of functions to cache the 
## inverse of a square matrix and return the inverse from the cache.

## makeCacheMatrix creates a special "matrix" object x for each new matrix  
## encountered and caches the inverse m for each. The special "matrix" object 
## contains a list of instructions for each function (set, get, setsolution
## and getsolution) it encounters.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # initiate object m in containing environment to accept the 
                   # inverse of the special "matrix" 
        
        ## set could be called by cacheSolve to superassign a new original
        ## matrix to x; making it accessable outside of the local environment,
        ## and m would be reset back to NULL, ready for a new matrix.
        
        set <- function(original=matrix()) {
                x <<- original  # superassigns the new original matrix value to 'x' 
                m <<- NULL      # superassigns NULL value to 'm' 
        }
        
        get <- function() x     # gets the original matrix x from parent environment 
        setsolution <- function(solution) m <<- solution    # superassign the solution to m
        getsolution <- function() m    # return the solution m to cacheSolve
        
        ## list of instructions for caching of the special "matrix" for each of 
        ## the functions contained in makeCacheMatrix
        list(set = set, get = get,    
             setsolution = setsolution,
             getsolution = getsolution)
}        

## cacheSolve calls on makeCacheMatrix to return what is stored in m; if null,
## cacheSolve determines the inverse and sends this to makeCacheMatrix before
## returning the inverse. If m is not null, it returns the cached value of the
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolution()  #function calls makeCacheMatrix and retrieves m
        
        ## if m is not null, return a message and then return m which terminates
        ## cacheSolve()
        if (!is.null(m)) {
                message("getting cached data") 
                return (m)
        }
        ## if the value of m is NULL then... 
        matrix <- x$get()  # get value of x from makeCacheMatrix & temporarily 
                           # and locally store it in an object called matrix
        m <- solve(matrix, ...)  # calculate inverse and assign it locally to m 
        x$setsolution(m)  # the value of m is sent to makeCacheMatrix to be cached
        m       ## return the value of m
}