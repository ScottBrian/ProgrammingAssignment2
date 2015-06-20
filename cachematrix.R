###############################################################################
# 
#  This file contains two functions: 
#
#  1) makeCacheMatrix creates the cache matrix object and the functions used
#  for the cache matrix, and returns the object as a function list. See 
#  details below in the description for the makecacheMatrix function.
#
#  2) cacheSolve returns the inverse of a matrix. See details below in the 
#  description for the cacheSolve function.     
#
#  The matrix cache holds two matrices: 
#     1) an invertible matrix (placed into the cache by the user when 
#        makeCacheMatrix is called, and/or by a setMatrix call)
#     2) the inverse of the invertible matrix (placed into the cache by
#        function cacheSolve)
#
#  The idea behind the cached matrix is to improve efficiency by retaining a
#  copy of the inverse matrix once its been solved. The first invocation 
#  of cacheSolve will incur the full cost of doing the solve and returning
#  the result. Subsequent invocations, however, will be able to skip the
#  solve step and simply return the retained copy. 
#
#  Example sequence of invocations:
#
#      myCMat <- makeCacheMatrix()     # instantiate cache object and 
#                                      # return list functions
#      myInvertibleMatrix <- matrix(1:4, nrow = 2, ncol = 2) # create matrix
#      
#      myCMat$setMatrix(myInvertibleMatrix) # place matrix in the cache
# 
#      myInverseMatrix <- cacheSolve(myCMat) # first call will
#                                      # solve the cached matrix to create 
#                                      # the inverse, save the inverse, and 
#                                      # then return it   
#      myInverseMatrix <- cacheSolve(myCMat) # second call will
#                                      # determine that the inverse is 
#                                      # already solved and will simply 
#                                      # return it, thus avoiding the
#                                      # overhead of solving again
#
###############################################################################

###############################################################################
#
# function name: makeCacheMatrix
#
# description: creates the cache object and functions for an 
#              invertible matrix and its inverse  
#              
# input: x - specifies an invertible matrix that is to be placed into
#            the cache.   
#            
#                    
# output: list of functions: 
#            1) setMatrix(inMatrix) - copy invertible matrix to the cache
#                               input: an invertible matrix
#                               output: none 
#            2) getMatrix()   - retrive the cached invertible matrix
#                               input: none
#                               output: cached invertible matrix
#            3) setInverse(inInvMatrix) - copy the inverse matrix to the cache
#                               input: inverse (solved) matrix
#                               output: none  
#            4) getInverse()  - retrieve the inverse cached matrix 
#                               (will return the cached inverse if 
#                                previously saved with setInverse, or NULL
#                                if not yet saved)
#                               input: none
#                               output: inverse matrix or NULL 
#
# notes: 1) setMatrix can be used by mainline while getMatrix, SetInverse, and
#           getInverse are intended as internal functions to be used by the 
#           cacheSolve function 
#        2) the makecacheMatrix functions do not perform validity checks
#           for the invertible or inverse matrices - it simply provides
#           a cache for such matrices to be stored and retrieved. It is the 
#           responsibility of the user to ensure the validity of the matrices.  
#            
#
# example invocations: 
#     myMat1 <- matrix(1:4, nrow=2, ncol=2)
#     myCMat1 <- makeCacheMatrix(myMat1)
#
#     myMat2 <- matrix(c(1,2,3,4,5,4,3,2,1), nrow=3, ncol=3)
#     myCMat2 <- makeCacheMatrix()
#     myCMat2$setMatrix(myMat2)
#                
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
             
       cacheInverseMatrix <- NULL      # init inverse matrix to NULL
      
       # return matrix cache with list of functions 
       list(     
            # create setMatrix function
            setMatrix = function(inMatrix) {# input is invertible matrix
               x <<- inMatrix               # copy input matrix into cache
               cacheInverseMatrix <<- NULL  # clear residual inverse matrix
                                            # (this will also ensure that 
                                            # cacheSolve will perform a 
                                            # new solve) 
            }
       
           ,# create getMatrix function
            getMatrix = function() x  # return cached matrix
       
           ,# create setInverse function
            setInverse = function(inInvMatrix) {
              cacheInverseMatrix <<- inInvMatrix # copy inverse to cache 
            }         
       
           ,# create getInverse function
            getInverse = function() cacheInverseMatrix # return cached inverse
           )
}
## End of makeCacheMatrix #####################################################

###############################################################################
#
# function name: cacheSolve
#
# description: Returns the inverse of a matrix. This is done efficiently
#              as follows: 
#              Processing on the first call: 
#                1) solve the cached invertible matrix to produce the
#                   inverse matrix
#                2) save the inverse matrix in the cache
#                3) return the inverse matrix
#              Processing on subsequent calls: 
#                1) return the inverse matrix previously saved in the cache   
#
# input: x - cache object returned by makeCacheMatrix
#                    
# output: solved (inverse) matrix  
#            
# notes: 1) It is the callers responsibility to ensure that an invertible 
#           matrix was previously stored into the cache when makeCacheMatrix
#           was called or by having called the setMatrix function. Failure to
#           do so will lead to unpredicatble results. Currently, this function
#           does not perform any validity checks to ensure a matrix was
#           previously stored or that it is invertible. 
#     
# example invocations: 
#     myMat <- matrix(1:4, nrow=2, ncol=2)
#     myCMat <- makeCacheMatrix(myMat)
#     myInverseMatrix <- cacheSolve(myCMat)
#                
###############################################################################
cacheSolve <- function(x, ...) {              # input is function list 
        
       theInverseMatrix <- x$getInverse()     # get the cached inverse or NULL
       
       if(is.null(theInverseMatrix)) {        # if not yet solved
               # get invertible matrix, solve it, then store the inverse
               theInverseMatrix <- solve(x$getMatrix()) # produce inverse 
               x$setInverse(theInverseMatrix) # copy inverse into cache
       }
       
       theInverseMatrix                       # return the inverse
}
## End of cacheSolve ##########################################################
