# Commented code for me to understand! (part by part)
# Considering memory complexity testing part only (included recorded.times and related code since main loop runs through it)
# Omitted Plot results functionality

CompEst = function(
d,                            # data.frame on which the algorithm is to be tested. (can be a vector or a matrix as well)

f,                            # User-defined function that runs the algorithm, taking d as first argument. No return value is needed.

random.sampling = FALSE,      # If TRUE, a random sample is taken at each step; if FALSE the first N observations are taken at each step.
          # Choosing a random sampling is relevant whith the use of replicates to help the discrimination power for complexity functions. 
          
max.time = 30,                # Maximum time in seconds allowed for each step of the analysis. 
     # The function will stop once this time limit has been reached. Default is 30 seconds. There is no such limitation regarding memory.
 
start.size = NULL,            # The size in rows of the first sample to run the algorithm. Default is `floor(log2(nrow(d)))`. 
                              # If strata is not NULL, we recommend to enter a multiple of the number of categories.
 
replicates = 2,               # The number of replicated runs of the algorithm for a specific sample size. Default = 2
 
strata = NULL,                # (stratified sampling) a string, the name of the categorical column of d that must be used for stratified sampling. A fixed proportion of the categories will be sampled, always keeping at least one observation per category.

power.factor = 2,             # The common ratio of the geometric progression of the sample sizes. 
                              # Default is 2, and will make sample sizes double every step. Decimal numbers are allowed.
                              
alpha.value=0.005)            # The alpha risk of the test whether the model is significantly different from a constant relation. 
                              # Default is 0.005. (just like we use in Anova tests, at 5%/1% level of significance)
{

is_myOS_windows <- Sys.info()["sysname"] %in% c("windows", "Windows", "WINDOWS") # check if OS is windows by looking at sysname  
      # attribute from Sys.info function for the string vector of possible 'Windows' combinations -> Boolean (TRUE for windows)
          
      # memory.size() function only works in systems having Windows OS, hence the restriction.             

size <- NULL    # variable to store size
NlogN_X <- NULL # seperate variable to denote NlogN complexity (works a bit differently)
model <- NULL   # variable to store model
dataset_name   <- deparse(substitute(d)) # take the dataset name into a string and keep it for reference inside the function.
algorithm_name <- deparse(substitute(f)) # take the algorithm name into a string and keep it for reference inside the function.
                                         # can use 'as.character(substitute(d))' and 'as.character(substitute(f))' as well.
          
  if ((!is.null(strata)) & (!is.data.frame(d)))
    stop("Stratified sampling is only compatible with input data in a data frame.")
  # i.e. if Strata exists, it needs to be in a data frame. otherwise stop
          
  N <- NROW(d) # let N be number of rows in our data frame (or vector or matrix)
          
  if (is.null(start.size))
    start.size <- floor(log2(NROW(d)))
  # change value of first sample size (in rows) to log(N) if null (default is null)   
          
  if (!is.null(strata)) # if strata exists (not null)
  {
    if (!(strata %in% names(d)))  # here %in% searches for strata (the string of our required attribute/column to sample) 
                                  # inside our data frame's (d) column names
      stop(paste0("No column named *", strata, "* found in the data. Stopping the process.")) # if not found, stop process
    else 
      start.size = max(start.size, NROW(unique(d[strata]))) # else take the maximum number of unique rows from our strata column
                                                            # as the initial size (in no. of rows) to run the sampling
  }        
        
  # with start size fixed above, we multiply it with 25 power-factor based increasing sizes
  # which are replicated (numbers would be large for a large power factor)        
  default.sample.sizes <- start.size * rep(power.factor^(0:25), each = replicates) # (replicated x2 for default value of replicates = 2)
  # to obtain our default vector (for sample sizes)  
  # Note the values range from a small number to a very big number hence we need to restrict it, like as done below with 2 limitations:        
          
  sample.sizes         <- floor(append(default.sample.sizes[default.sample.sizes >= start.size & default.sample.sizes < N], 
                          rep(N, each = replicates))) 
                       # restrictions: minimum value be set at start.size, and maximum inferior to the size of data. The ‘append’ 
                       # function ensures that we add to the vector a final size that would be exactly the size of the original data 
                       # (with possible replicates), because the (n-1) size would be inferior to that. 
          
                       # Take the default sample sizes in between start size (inclusive) and number of rows in our data frame 
                       # (of course, start size rows will be less than N) and append them to their replicates. (x2 for default)
                       # Then take the floor of the number (of rows) --> possibly a fraction/decimal -> rounded to integer.
          
                       # Replicates functionality : 
                       # For example: if N=c(1:4), rep(N, each=replicates) -> 1 1 2 2 3 3 4 4  (for default value of replicates = 2)
                       # Append functionality : Following syntax 'append(x, values)' 
                       # -> Append x (vector containing values to be appended) to values (vector to be appended to/against)                
          
   recorded.times       <- 0 # initialize recorded times as 0
          
   recorded.mems        <- 0 # initialize recorded memory as 0
          
  i                    <- 1 # loop variable, initialize as 1

    if (length(unique(sample.sizes)) < 3) # ask sir, (line 90-92 in CompEst) - should be >2 as per stop message below
      stop("Current configuration does not allow to compute more than 2 sample sizes and model fitting will fail.
      Stopping the process. Please increase max.time or decrease start.size.")

     while((recorded.times[i] < max.time/power.factor/replicates) & (i < length(sample.sizes))) # main loop
     # Loop until recorded times is less than (maximum time * replicates)/power.factor
     # and index(i) is less than length of sample sizes for an iteration.
     # Here '&' returns a vector after doing element-by-element comparisons. (different from &&)
    
     { # begin loop
          if (is.null(strata))
            sampled.data     <- rhead(d, sample.sizes[i]) # implement rhead: (Not CompEst function code -> seperated by '---' )
#-------------------------------------------------------------------------------------------------------------------------------------------               
                                rhead = function(data, rows=7, is.random=TRUE) # our data frame is passed as 'data' inside this function.
                                                                               # rows -> number of rows to be sampled.
                                { # begin rhead 
  if (rows>NROW(data))
  {
    message(paste0("Sample size is greater than the data: returning the initial data", ifelse(is.random, " after shuffling", "")))
  # if number of rows in sample size is greater than the number of rows in data frame then make it equal to number of rows in data frame.
  # also use ifelse vector clause to append suffix 'after shuffling' if its random, else dont append anything to statement. ("")
    rows = NROW(data)
  }
                       
  if(is.null(dim(data))) # for vector 
  {  if (is.random==TRUE)
      return(data[base::sample(NROW(data), rows)])
   # return sampled vector via sample (from base R) function where we take elements of sampling as the number of rows in our 
   # vector and the total size of the sample as the rows (default 7) which is a parameter of this function.
   # additional parameter for sample function : 'replace' is FALSE (by default) if not mentioned, hence no dupes will be there.
    else 
      return(data[1:rows]) # else return vector composing of 1 to number of rows passed to function (default 7)
  }
     
  # same part for matrix or data frame                                        
  if (is.random==TRUE)
    return(data[base::sample(NROW(data), rows), ])
   # return sampled data frame via sample (from base R) function where we take elements eligible for sampling as the number of rows 
   # in our data frame and the total size of the sample as the rows (default 7) which is a parameter of this function.
   # additional parameter for sample function : 'replace' is FALSE (by default) if not mentioned, hence no dupes of rows will occur.
  else 
    return(data[1:rows, ]) # else return data frame composing of 1 to number of rows passed to function (default 7)
                                } # end rhead 
#-------------------------------------------------------------------------------------------------------------------------------------------                    
                                  # back to CompEst()
          else 
            sampled.data  <- do.call("rbind", by(d, d[strata], GroupedSampleFracAtLeastOneSample, prop = sample.sizes[i]/N))
               
          # do.call makes the call to a function which is rbind here which binds our data frame d by the sample data frame returned
          # by GroupedSampleFracAtLeastOneSample function, with parameters d[strata] passed as d_subset, prop (being proportion) 
          # passed as (samples sizes or selective rows from total rows (per iteration) / total number of rows) as far as I understand
               
          # implement GroupedSampleFracAtLeastOneSample:  
#-------------------------------------------------------------------------------------------------------------------------------------------
GroupedSampleFracAtLeastOneSample = function(d_subset, prop, is.random=TRUE) # will return sampled data
{
  if (prop > 1)
    prop <- min(1, prop) # set proportion to 1. (its a fraction, cannot be greater than 1)
  
  nb.to.sample = ifelse(prop * nrow(d_subset) < 1, 1, prop * nrow(d_subset))
  # same as : if(prop * nrow(d_subset) < 1) nb.to.sample=1  
  #           else                          nb.to.sample=prop*nrow(d_subset))
  if (is.random==TRUE) #
    return(d_subset %>% sample_n(nb.to.sample)) # selecting random rows to sample from our data frame by using sample_n function (from dplyr)
  else 
    return(d_subset %>% head(nb.to.sample)) # selecting first few rows to sample from our data frame
}
#-------------------------------------------------------------------------------------------------------------------------------------------     if (is_myOS_windows)
         # back to CompEst()
               
    if (is_myOS_windows) # time + memory           
    {
      gc(); gc();      
      memory.before    <- memory.size() # take current memory usage using memory.size (function only available for Windows) 
              
      recorded.times   <- append(recorded.times, system.time(f(sampled.data))[3])
                       # add the recorded times from the timings returned by system.time function for our sampled data (say x) 
                       # which uses our function/algorithm (say f) -> f(x) into [3] of recorded.times variable 
              
      memory.after     <- memory.size() # take memory usage after our algorithm based function executes (then take difference)
      gc(); gc();
      recorded.mems    <- append(recorded.mems, memory.after - memory.before) # append difference to memory record
      i                <- i+1 # increment loop variable 
    } 
    else # only time
    {
      gc(); gc(); # call twice to garbage collection assure memory is released (or to avoid memory leaks) 
      # memory.before    <- memory.size() 
      # Above line commented because memory.size() function doesn't apply to operating systems other than windows.
              
      recorded.times   <- append(recorded.times, system.time(f(sampled.data))[3])
              
      # memory.after     <- memory.size()
      gc(); gc();
      # recorded.mems    <- append(recorded.mems, memory.after - memory.before)
      # Above line commented because memory.size() function doesn't apply to operating systems other than windows.
      i                <- i+1 # increment loop variable 
    }
               
   } # end loop (which starts at line 100)
          
     recorded.times = tail(recorded.times, -1) 
     # get all the recorded times (-1 from reverse = all obs. in increasing order) in recorded.times
     # I tried this > tail(c(1:5),-1)
     # and got this : [1] 2 3 4 5
     # hence the same would apply here    
          
  if (is_myOS_windows) 
     recorded.mems  = tail(recorded.mems,  -1)
     # Likewise, get all the recorded memory (again, for Windows OS only) in recorded.mems 

  if (length(recorded.times) %in% c(1, 2)) 
    stop("The allowed max.time value is too small to run the algorithm on more than 2 sample sizes. 
    Unable to proceed with cross-validation or model fitting. Stopping the process. Please increase max.time or decrease start.size.")
           
  # MEMORY RESULTS
          
  if (is_myOS_windows)
  {
    temp  <- CompEstBenchmark(data.frame('size'   = head(sample.sizes, length(recorded.times)), # assign size as the sample sizes uptil the no. of recorded times
                                         'time'   = recorded.times, # assign recorded time observations as time 
                                         "memory" = recorded.mems) # likewise for memory
                                          %>%                      # ask sir
                                          mutate(NlogN_X = size*log(size)), use="memory")
          
    # CompEstBenchmark implementation:  
#----------------------------------------------------------------------------------------------------------------------------------------            
   CompEstBenchmark = function(to.model, use="memory") # benchmark based on parameters passed model and recorded.mems 
  {
    # if(use=="memory") -> considering only the memory part below:
             
    constant    <- glm(memory~1,          data=to.model); to.model['constant'] = fitted(constant)
    # define variables representing complexity classes with a formula based on a generalized linear model/glm (extension of linear regression models)
    # with formula of type (time/memory ~ size), where size can be constant (1), quadratic (squared or ^2) and so on.
    # The '~' should be thought of as saying "is distributed as" or "is dependent on" the RHS (seen in regression functions)
    # Or when specifying a model, '~' means (LHS) 'as a function of' (RHS).
    # extract fitted values from objects (returned by modeling functions) by using 'fitted' function
             
    linear      <- glm(memory~size,       data=to.model); to.model['linear'] = fitted(linear)
    quadratic   <- glm(memory~I(size^2),  data=to.model); to.model['quadratic'] = fitted(quadratic)
    # Note: The I() function acts to convert the argument inside I(...) to as we expect it to be or work like.
    # Here if we just use (size^2) like I tried, it doesn't evaluate to the square of our size as we expect.
    # which is primarily because in function formula we need to explicitly inhibit the interpretation of operators 
    # such as "+", "-", "*" and "^" as formula operators, so they are used as arithmetical operators. ("^" is used here inside)
    
    # Likewise, for the rest: 
    cubic       <- glm(memory~I(size^3),  data=to.model); to.model['cubic'] = fitted(cubic)
    squareroot  <- glm(memory~sqrt(size), data=to.model); to.model['squareroot'] = fitted(squareroot)
    log         <- glm(memory~log(size),  data=to.model); to.model['log'] = fitted(log)
    NlogN       <- glm(memory~NlogN_X,    data=to.model); to.model['NlogN'] = fitted(NlogN)
    # Note: NlogN is seperately considered from 'NlogN_x' variable
    
    # create a list with strings (denoting respective complexity classes) assigned their fitted values:
    model.list <- list('constant'   = constant,
                       'linear'     = linear,
                       'quadratic'  = quadratic,
                       'cubic'      = cubic,
                       'squareroot' = squareroot,
                       'log'        = log,
                       'NlogN'      = NlogN)
    return(list(model.list, to.model)) # return list (goes to temp variable)
  }    
#----------------------------------------------------------------------------------------------------------------------------------------            
    
    # back to CompEst()  
            
    model.list  <- temp[[1]] # list of those 7 models (extracting 1st parameter of temp)
            
    to.model    <- temp[[2]] # The same data that was sent to temp as to.model itself (extracting 2nd parameter of temp)
            
    benchmark   <- lapply(model.list, function(x) cv.glm(to.model, x)$delta[2]) # apply cross validation on the list of glms, extracting the second delta attribute (adjusted delta error)
    # calculates cross validation error of prediction for each of the models in model.list (7 complexity classes) and store in 'benchmark' variable
                          
    best.model  <- names(which.min(benchmark))
    # find the 'best' fitted model in terms of having the lowest/minimal error, i.e. minimum(benchmark)
                          
    if (best.model=="constant") message("Best model CONSTANT for memory can be caused by not choosing a sufficiently high max.time value")
    # Kind of an exception as constant complexity prediction can be caused by other factors as mentioned above. (even when trend is actually not constant)
                          
    full.memory <- CompEstPred(model.list, benchmark, N, use="memory")
    # Pass the full list of models (7), the vector of (LOO) errors (benchmark) and no. of rows to CompEstPred function 
    # and store result in 'full.memory' variable.                     
    
    # CompEstPred implementation:
#----------------------------------------------------------------------------------------------------------------------------------------                          

  CompEstPred = function(model.list, benchmark, N, use="time")
  {
            # seperate case for NlogN:
  if (names(which.min(benchmark))=="NlogN") 
    estimation <- predict(model.list[[which.min(benchmark)]], newdata = data.frame('NlogN_X' = N*log(N)))
            
   else     # 
    estimation <- predict(model.list[[which.min(benchmark)]], newdata = data.frame('size' = N))
            
  return(ifelse(use=="time", as.character(seconds_to_period(round(estimation, 2))), paste0(round(estimation), " Mb")))
  # if use=="time" then return time (estimation), rounded upto 2 decimal places,
  # else (for memory), return memory used (estimation) followed by Mb (memory usage is calculated in megabytes)
}
                          
#----------------------------------------------------------------------------------------------------------------------------------------    
    
    # back to CompEst()                      
                          
    signif.test <- tail(anova(model.list[[which.min(benchmark)]], test="F")$Pr, 1)
    # apply significance test via analysis of variance..ask sir
                          
    uncertain   <- is.na(signif.test) | signif.test > alpha.value
    # if na values occur or test is insignifant / fails significance test, then mark as uncertain)
                          
    if (uncertain) 
       message("warning: best MEMORY model not significantly different from a constant relationship. Increase max.time or replicates.")
    # Display message notifying user regarding the above aspect if test fails (or uncertain=TRUE)
     
    # create the output list for memory:                      
    output.memory = list('best.model' = toupper(best.model), # best model in caps
                         'memory.usage.on.full.dataset' = full.memory, 
                         'system.memory.limit' = paste0(memory.limit(), " Mb"),
                         'p.value.model.significance' = signif.test)
  
  # return list of sample sizes and time + memory complexity results                        
  return(list("sample.sizes" = sample.sizes,
              "TIME COMPLEXITY RESULTS" = output.time,
              "MEMORY COMPLEXITY RESULTS" = output.memory))
}        
