
# makeCacheMatrix() and cacheSolve()

# sample results included in this file after the functions to aid marking review

## File contains a pair of functions to be used to for matrix inversion
###     matrix inverse results are stored for efficiency, to avoid re-calculation
###     the inverse is calcuated and stored for the first call on each matrix
###     subsequent calls of the same matrix inform the user that cached data is retrieved
###     functions work as a pair to check if the underlying data in the matrix has changed


## makeCacheMatrix input is a matrix for which the inverse will be calculated by cacheSolve()
###     input matrix x is assumed to be square and invertible
###     output is list of functions and data to be used as input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) 
{
    # Return list required by cacheSolve() to calculate and store matrix inverse
        
        ##  create matrix of the same dimensions to hold the inverse results 
        
        xrow <- nrow(x)
        xcol <- ncol(x)
        inv <- matrix(nrow = xrow, ncol = xcol)
        
        
        ## sets up functions and data for cachesolve to use 
        ###     making y available outside the Global Environment
        ###     enables checking if matrix data has changed
        
        set <- function(y = matrix()) 
        {
                x <<- y  
                inv <- matrix(nrow = xrow, ncol = xcol) 
        }

        
        ## create functions and data which will be used by the output list
        ###     key is storing the setinverse data outside the Global Environment
        ###     so cacheSolve() can query if the inverse has been previously calculated or not
        
        get <- function() x ## stores the original input data
        setinverse <- function(inv_ext) inv <<- inv_ext  
        getinverse <- function() inv
        
        
        ## store list to be used as input to cachesolve() and return
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes as input list data created by makeCacheMatrix
###     Checking if the inverse has been previously calculated, it returns the stored inverse
###     User is notified that the results have been retrieved from cache
###     If the inverse for the input data stored in $get has not been previously calculated
###     the inverse is calculated and stored; also initialising the existance of the inverse by 
###     setinverse data held outside the Global Environment so cache is used in  subsequent runs 

cacheSolve <- function(x, ...) 
{
    # Return a matrix that is the inverse of 'x'
        

        ## retrieves inverse from input list created by makeCacheMatrix
        
        inv <- x$getinverse()
        
        ##  loop to check if the inverse has been previously calculated 
        ###     check if inverse is blank; sum of matrix with all NAs is NA
        
                na_chk <- !is.na(sum(inv))  ## TRUE for inverse matrix is not blank

                if(na_chk == TRUE) 
                {    
        ###     inverse exists so message console retrieving data 
                        
                        message("getting cached data") 
                }
                
        ## ELSE clause, so inverse has not been previously calculated
        ###     retrieve base data from input list
                
                        data <- x$get()
                
         ## calculate inverse on data from $get value
         ###      update setinverse values for retrieval on future calls for same data
                        inv <- solve(data, ...)
                        x$setinverse(inv)
                
         ## return the matrix inverse after cache message or first calculation
                        
        inv
}

# TEST DATA 

#       **  BELOW shows matrix being calculated and cached correctly
#          Also, error condition where the underlying data is changed **

## mt5 *new matrix*
###      [,1] [,2]
### [1,]    3    4
### [2,]    4    2

##  test_mt5 <- makeCacheMatrix(mt5)

##> str(test_mt5) *output of makeCacheMatrix*
###List of 4
###     $ set       :function (y = matrix())  
###             ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 11 12 15 5 12 5 11 15
###             .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000196cdad0> 
###     $ get       :function ()  
###             ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 18 12 18 23 12 23 18 18
###             .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000196cdad0> 
###     $ setinverse:function (inv_ext)  
###             ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 19 19 19 51 19 51 19 19
###             .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000196cdad0> 
###     $ getinverse:function ()  
###             ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 20 19 20 32 19 32 20 20
###             .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000196cdad0> 

## cacheSolve(test_mt5) *first pass, calculates inverse*
###         [,1] [,2]
###   [1,]    1 -0.5
###   [2,]   -2  1.5

## cacheSolve(test_mt5) *second pass, retrieves cached inverse*
###   getting cached data
###         [,1] [,2]
###   [1,]    1 -0.5
###   [2,]   -2  1.5

## cacheSolve(test_mt3)  *also works for 3 x 3 matrix*
###   getting cached data
###         [,1] [,2] [,3]
###    [1,]  -24   18    5
###    [2,]   20  -15   -4
###    [3,]   -5    4    1

##  mt5 ** CHANGE DATA in mt5 **
###        [,1] [,2]
###   [1,]    2    4
###   [2,]   -3    1

##  test_mt5 <- makeCacheMatrix(mt5)
##  cacheSolve(test_mt5)             ** identifies data has changed and calculates new inverse
###        [,1]       [,2]
###   [1,] 0.07142857 -0.2857143
###   [2,] 0.21428571  0.1428571