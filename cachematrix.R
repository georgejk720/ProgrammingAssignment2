#make a matrix that will cache and calculate the inverse of the matrix
makeCacheMatrix <- function(m = numeric()) {
        #set the inverse matrix to null
        i <- NULL
        #cache variables m and i, the matrix and inverse matrix
        set <- function(x) {
                m <<- x
                i <<- NULL
        }
        #return the cached matrix
        get <- function() m
        #set the inverse matrix
        setSolve <- function(solve) i <<- solve
        #calculate the inverse matrix, if it has not already been calculated
        getSolve <- function() i
        #return the function objects, as a list of length 4
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

#return the inverse of the matrix, either from cache or through calculating it
cacheSolve <- function(m, ...) {
        i <- m$getSolve()
        #return i from cache if it exists
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #get matrix and assign it to "data"
        data <- m$get()
        #calculate inverse and assign it to i
        i <- solve(data, ...)
        #set inverse of m to be i
        m$setSolve(i)
        #display i
        i
}

#can test with the following

#source ("Assignment2.R")

#m1<-matrix(data=c(2,2,3,2),nrow=2,ncol=2)
#m2<-makeCacheMatrix(m1)
#cacheSolve(m2)
#Expected output-
#[,1] [,2]
#[1,]   -1  1.5
#[2,]    1 -1.0
#cacheSolve(m2)
#Expected output-
#        getting cached data
#[,1] [,2]
#[1,]   -1  1.5
#[2,]    1 -1.0

# m <- makeCacheMatrix(matrix(c(2,4,3,1,5,7,7,4,2,6,5,9,2,4,8,9),nrow=4,ncol=4))
# cacheSolve(m)
# [,1]        [,2]       [,3]        [,4]
# [1,] -1.06842105  0.58421053  0.3894737 -0.36842105
# [2,]  0.70526316 -0.25263158 -0.1684211  0.10526316
# [3,]  0.05789474  0.12105263 -0.2526316  0.15789474
# [4,] -0.25263158 -0.07368421  0.2842105 -0.05263158
# cacheSolve(m)
# getting cached data
# [,1]        [,2]       [,3]        [,4]
# [1,] -1.06842105  0.58421053  0.3894737 -0.36842105
# [2,]  0.70526316 -0.25263158 -0.1684211  0.10526316
# [3,]  0.05789474  0.12105263 -0.2526316  0.15789474
# [4,] -0.25263158 -0.07368421  0.2842105 -0.05263158
# m$get() %*% m$getSolve()
# [,1]         [,2]          [,3]          [,4]
# [1,]  1.000000e+00 1.387779e-16 -1.110223e-16 -2.775558e-17
# [2,] -2.220446e-16 1.000000e+00  0.000000e+00 -5.551115e-17
# [3,] -4.440892e-16 0.000000e+00  1.000000e+00 -3.330669e-16
# [4,]  0.000000e+00 2.220446e-16  0.000000e+00  1.000000e+00