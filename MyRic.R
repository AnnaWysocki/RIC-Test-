RIC <- function(Data, numCols, numRows, r, numReps) {
	#Data: Matrix
	#numCols: number of variables
	#numRows: Sample size 
	#r: Vector of numReps unique values between 1 and numRows (if numReps > numRows, than just the vector 1:numRows)
	#numReps: number of repitions (or Sample Size if Sample Size is greater than desired number of repititions) 
	#lambda_opt: just the value to be returned, doesn't actually need to be an argument, always passed in as 0
	lambda_min <- Inf
	for(i in 1:numReps) { #for every repetition
		tmp_r <- r[i]				#tmp_r is given value for that repetition
		lambda_max <- 0.0
		for(j in 1:numCols) {		#for every column
			#use k values 1:nuCols, but skip (j+1)
			kVals <- 1:numCols
			kVals <- kVals[!kVals %in% (j+1)] 
			for(k in kVals) {			
				if(tmp_r < numRows) { #need this in case tmp_r == numRows, then it would try to do (numRows + 1 : numRows)
					tmp <- sum(Data[(1+tmp_r):numRows, j] * Data[1:(numRows - tmp_r), k])
				}
				tmp <- tmp + sum(Data[1:tmp_r, j] * Data[(numRows - tmp_r + 1):numRows, k])	
				tmp <- abs(tmp)
				if(tmp > lambda_max){ #set to lambda_max if this is our biggest lambda so far
					lambda_max <- tmp
				}
			}
		}		
		#now that we've maximized lambda, if this is our smallest maximized lambda so far, keep it
		if(lambda_max < lambda_min) { 
			lambda_min <- lambda_max
		}
	}
	lambda_min
}

# upTo <- function(x, y) {
# 	#take two numbers x,y y and return c(x:y) only if x <= y
# 	if(x <= y) {
# 		return(c(x:y))
# 	} else {
# 		return(c())
# 	}
# }

RIC <- function(Data, numCols, numRows, r, numReps) {
	#Data: Matrix
	#numCols: number of variables
	#numRows: Sample size
	#r: Vector of 1:numReps
	#numReps: number of repitions (or Sample Size if Sample Size is greater than desired number of repititions)
	#lambda_opt: just the value to be returned, doesn't actually need to be an argument, always passed in as 0
	lambda_min <- Inf
	for(i in 1:numReps) { #for every repetition
		tmp_r <- r[i]				#tmp_r is given value for that repetition
		lambda_max <- 0.0
		for(j in 1:numCols) {		#for every column
			#for the k values
			kVals <- 1:numCols
			kVals <- kVals[!kVals %in% (j+1)]
			for(k in kVals) {
				tmp <- 0.0
				for(m in upTo(1,numRows - tmp_r)) {			#for every row not one of the last tmp_rth rows
					tmp <- tmp + Data[m + tmp_r, j] * Data[m, k]
				}
				for(m in upTo(numRows-tmp_r + 1,numRows)) {	#for every row not one of the first tmp_rth rows
					tmp <- tmp + Data[m - (numRows - tmp_r), j] * Data[m, k]
				}
				tmp <- abs(tmp)
				if(tmp > lambda_max){ #set to lambda_max if this is our biggest lambda so far
					lambda_max <- tmp
				}
			}
		}
		#now that we've maximized lambda, if this is our smallest maximized lambda so far, keep it
		if(lambda_max < lambda_min) {
			lambda_min <- lambda_max
		}
	}
	lambda_min
}