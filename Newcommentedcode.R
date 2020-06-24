ComplexityClassifier = function(
bench.bool = FALSE,           # boolean indicating whether to use benchmarking (otherwise sampling is done on user given data frame) or not 
data.df,                      # data.frame on which the user provided algorithm/function will run on.  
func,                         # user-defined function that runs the algorithm, taking data.df as an argument if sampling is to be done (otherwise not required for benchmarking)
time.limit = 30,              # maximum time in seconds allowed for each step of the analysis. 
start.size = NULL,            # The size in rows of the first sample to run the algorithm. Default is set to `floor(log2(nrow(d)))`. 
replicates = 2,               # The number of replicated runs of the algorithm for a specific sample size. Default is set to 2
strata = NULL,                # a string, the name of the categorical column of our data frame data.df that must be used for stratified sampling.                             
alpha.value = 0.01,           # the alpha value of the F-test to demark whether the model is significantly different from predicted complexity.          
plot.bool = TRUE)             # boolean signifying if plot is required to showcase the complexity trend visually. Default is set to true        
{          
  size <- NULL    # variable to store size
  model <- NULL   # variable to store model
  # # take the name of the dataset and algorithm for reference inside the function (used while plotting) Am using deparse to evaluated the substituted string, but one could use as.character as well.
  dataset_name   <- deparse(substitute(data.df))
  algorithm_name <- deparse(substitute(f))       
                                               
   
  # unit-test added null check for strata column and an expected class of "data.frame" for our input data.df        
  if ((!is.null(strata)) & (!is.data.frame(data.df)))
    stop("Stratified sampling is only compatible with input data in a data frame.")
       
  # let N be number of rows in our data frame       
  N <- NROW(data.df) 
          
  # change value of first sample size (in rows) to log(N) if null (default is null)        
  if (is.null(start.size))
    start.size <- floor(log2(NROW(data.df)))   
          
  # unit-test added null check for strata column       
  if (!is.null(strata)) # if strata exists (not null)
  {
    if (!(strata %in% names(data.df)))  # here %in% searches for strata (the string of our required attribute/column to sample) inside our data frame's column names
      stop(paste0("No column named *", strata, "* found in the data. Stopping the process.")) # if not found, stop process
    else 
      start.size = max(start.size, NROW(unique(data.df[strata]))) # else take the maximum number of unique rows from our strata column as the initial size (in no. of rows) to run the sampling
  }        
        
  # with start size fixed above, we multiply it with a hardcoded value based on increasing sizes which are then replicated to provide a larger input for classification      
  default.sample.sizes <- start.size * rep(2^(0:25), each = replicates) 
  # Note the values range from a small number to a very big number hence we need to restrict it, as done below with the limitation:        
          
  sample.sizes         <- floor(append(default.sample.sizes[default.sample.sizes >= start.size & default.sample.sizes < N], 
                          rep(N, each = replicates))) 
                       # restrictions: start.size should be in between defaults and size of data. The ‘append’ function ensures that we add to the vector a final size that would be exactly the size of the original data (with possible replicates), because the (n-1) size would be inferior to that. 

                       # Replicates functionality: 
                       # For example: if N=c(1:4), rep(N, each=replicates) -> 1 1 2 2 3 3 4 4  (for default value of replicates = 2)
                       # Append functionality : Following syntax 'append(x, values)' 
                       # -> Append x (vector containing values to be appended) to values (vector to be appended to/against)  
          
   # initialize required variables with appropriate initial values:      
   recorded.times       <- 0 
   recorded.mems        <- 0 
   i                    <- 1 

     while((recorded.times[i] < time.limit/replicates) & (i < length(sample.sizes))) 
     # Loop until the recorded sampling times are less than the time limit is to replicate ratio (half for default), and uptil the total sample sizes allocated per iteration:
     {    
          # do the sampling (using sample from base R) if strata is not provided or null (or stratified sampling is not an option):
          if (is.null(strata))
            sampled.data     <- (data[base::sample(NROW(data), rows),])
          # else perform stratified sampling (using sample_n) on required column, with an appropriate proportion and collect the combined data frame by an rbind():
          else 
            sampled.data  <- do.call("rbind", by(data.df, data.df[strata], d_subset %>% sample_n(nb.to.sample), prop = sample.sizes[i]/N))       
      
      # in order to take time amd memory data, the time and memory.size() function is called before and after  
      memory.before    <- memory.size() # take current memory usage using memory.size (function only available for Windows) 
      recorded.times   <- append(recorded.times, system.time(f(sampled.data))[3])
                       # add the recorded times from the timings returned by system.time function for our sampled data (say x) 
                       # which uses our function/algorithm (say f) -> f(x) into [3] of recorded.times variable 
      memory.after     <- memory.size() # take memory usage after our algorithm based function executes (then take difference)
      recorded.mems    <- append(recorded.mems, memory.after - memory.before) # append difference to memory record
      i                <- i+1 # increment loop variable          
    }
               
     # obtain all the recorded time and memory
     recorded.times = tail(recorded.times, -1)  
     recorded.mems  = tail(recorded.mems,  -1)
    # create a data frame assigning recorded time and memory observations as 'time' and 'memory' columns respectively for the glms to classify:
    benchmark.data <- data.frame('size' = head(sample.sizes, length(recorded.times)), 'time' = recorded.times, 'memory' = recorded.mems))
   } #else part for benchmarked data       
    
    constant    <- glm(memory~1,          data=benchmark.data); to.model['constant'] = fitted(constant)
    # define variables representing complexity classes with a formula based on a generalized linear model/glm (extension of linear regression models)
    # with formula of type (time/memory ~ size), where size can be constant (1), quadratic (squared or ^2) and so on.
    # The '~' should be thought of as saying "is distributed as" or "is dependent on" the RHS (seen in regression functions)
    # Or when specifying a model, '~' means (LHS) 'as a function of' (RHS).
    # extract fitted values from objects (returned by modeling functions) by using 'fitted' function
    squareroot  <- glm(memory~sqrt(size), data=benchmark.data); to.model['squareroot'] = fitted(squareroot)
    log         <- glm(memory~log(size),  data=benchmark.data); to.model['log'] = fitted(log)
    linear      <- glm(memory~size,       data=benchmark.data); to.model['linear'] = fitted(linear)
    quadratic   <- glm(memory~I(size^2),  data=benchmark.data); to.model['quadratic'] = fitted(quadratic)
    cubic       <- glm(memory~I(size^3),  data=benchmark.data); to.model['cubic'] = fitted(cubic)
    # Note: The I() function acts to convert the argument inside I(...) to as we expect it to be or work like.
    # Here if we just use (size^2) like I tried, it doesn't evaluate to the square of our size as we expect.
    # which is primarily because in function formula we need to explicitly inhibit the interpretation of operators 
    # such as "+", "-", "*" and "^" as formula operators, so they are used as arithmetical operators. ("^" is used here inside) 

    # create a list with strings (denoting respective complexity classes) assigned their fitted values:
    model.list <- list('constant' = constant, 'squareroot' = squareroot, 'log' = log, 'linear' = linear, 'quadratic'  = quadratic, 'cubic' = cubic)

    # calculate adjusted cross validation error of prediction for each of the aforementioned models:                    
    cross.validated.errors <- lapply(model.list, function(x) cv.glm(to.model, x)$delta[2]) # apply cross validation on the list of glms, extracting the second delta attribute (adjusted delta error)
          
    # find the 'best' fitted model in terms of having the lowest/minimal error with respect to a complexity class:                               
    best.model  <- names(which.min(cross.validated.errors))                 
                                           
    # Use a significance testing via analysis of variance with an F-test:                       
    signif.test <- tail(anova(model.list[[which.min(benchmark)]], test="F")$Pr, 1)
             
    # Give the user a warning for inaccuracy in prediction if significance test fails (defaults set at 1% alpha)                  
    uncertain   <- is.na(signif.test) | signif.test > alpha.value               
    if (uncertain) 
       message("Warning : Actual/Expected complexity class might differ from given complexity class, as per F-test failure.")
     
    # plot
}        
