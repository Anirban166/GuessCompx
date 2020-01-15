# Commented code for me to understand! (part by part)
# Considering memory complexity testing part only
# Omitted Plot results functionality

# A few keynotes based on my understanding: 

# Involves 'Stratified Sampling': 
# A stratified random sample divides the population into smaller groups, or strata, based on shared characteristics. 
# strata: Stratified random sampling involves dividing the entire population into homogeneous groups called strata.

# Recorded values of time and memory stored in recorded.times and recorded.mems respectively.

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

size <- NULL    #
NlogN_X <- NULL # ask sir
model <- NULL   # 
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
          
  if (!is.null(strata)) 
  {
    if (!(strata %in% names(d)))  # here %in% searches for strata (the string of our required attribute/column to sample) 
                                  # inside our data frame (d) column names
      stop(paste0("No column named *", strata, "* found in the data. Stopping the process.")) # if not found, stop process
    else 
      start.size = max(start.size, NROW(unique(d[strata]))) # else take the maximum number of unique rows from our strata column
                                                            # as the initial size (in no. of rows) to run the sampling
  }        
          
  default.sample.sizes <- start.size * rep(power.factor^(0:25), each = replicates)
  # with start size fixed above, we multiply it with replicates of our  
  # power factor (common ratio of the GP our sample sizes follow/vary in) and mark them as default sample sizes.
          
  sample.sizes         <- floor(append(default.sample.sizes[default.sample.sizes >= start.size & default.sample.sizes < N], 
                          rep(N, each = replicates))) # ask sir to verify understanding (line 85)
          
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

    if (length(unique(sample.sizes)) < 3) # ask sir, (line 90-92) - should be >2 as per stop message below
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
      return(data[1:rows]) # else return data frame composing of 
  }
     
  # same part for matrix or data frame I guess                                        
  if (is.random==TRUE)
    return(data[base::sample(NROW(data), rows), ])
   # return sampled data frame via sample (from base R) function where we take elements eligible for sampling as the number of rows 
   # in our data frame and the total size of the sample as the rows (default 7) which is a parameter of this function.
   # additional parameter for sample function : 'replace' is FALSE (by default) if not mentioned, hence no dupes of rows will occur.
  else 
    return(data[1:rows, ])
                                } # end rhead 
#-------------------------------------------------------------------------------------------------------------------------------------------                    
                                  # back to CompEst()
          else 
            sampled.data     <- do.call("rbind", by(d, d[strata], GroupedSampleFracAtLeastOneSample, prop = sample.sizes[i]/N))
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
               
    if (is_myOS_windows)           
    {
      gc(); gc();      # garbage collection - ask sir why twice
      memory.before    <- memory.size() # take current memory usage using memory.size (function only available for Windows) 
              
      recorded.times   <- append(recorded.times, system.time(f(sampled.data))[3])
              
      memory.after     <- memory.size() # take memory usage after our algorithm based function executes (then take difference)
      gc(); gc();
      recorded.mems    <- append(recorded.mems, memory.after - memory.before) # append difference to memory record
      i                <- i+1 # increment loop variable 
    } 
    else 
    {
      gc(); gc();
      # memory.before    <- memory.size() 
      # Above line commented because memory.size() function doesn't apply to operating systems other than windows.
              
      recorded.times   <- append(recorded.times, system.time(f(sampled.data))[3])
              
      # memory.after     <- memory.size()
      gc(); gc();
      # recorded.mems    <- append(recorded.mems, memory.after - memory.before)
      # Above line commented because memory.size() function doesn't apply to operating systems other than windows.
      i                <- i+1 # increment loop variable 
    }
               
   } #end loop (starts at line 100 here)
          
     recorded.times = tail(recorded.times, -1)
  if (is_myOS_windows) 
     recorded.mems  = tail(recorded.mems,  -1) 

  if (length(recorded.times) %in% c(1, 2)) 
    stop("The allowed max.time value is too small to run the algorithm on more than 2 sample sizes. 
    Unable to proceed with cross-validation or model fitting. Stopping the process. Please increase max.time or decrease start.size.")
          
  # MEMORY RESULTS

  if (is_myOS_windows)
  {
    temp        <- CompEstBenchmark(data.frame('size'   = head(sample.sizes, length(recorded.times)),
                                               'time'   = recorded.times,
                                               "memory" = recorded.mems) %>%
                                      mutate(NlogN_X = size*log(size)), use="memory")
    model.list  <- temp[[1]]
    to.model    <- temp[[2]]
    benchmark   <- lapply(model.list, function(x) cv.glm(to.model, x)$delta[2])
    best.model  <- names(which.min(benchmark))
    if (best.model=="constant") message("Best model CONSTANT for memory can be caused by not choosing a sufficiently high max.time value")
    full.memory <- CompEstPred(model.list, benchmark, N, use="memory")
    signif.test <- tail(anova(model.list[[which.min(benchmark)]], test="F")$Pr, 1)
    uncertain   <- is.na(signif.test) | signif.test > alpha.value
    if ( uncertain ) message("warning: best MEMORY model not significantly different from a constant relationship. Increase max.time or replicates.")
    output.memory = list(
      'best.model' = toupper(best.model),
      'memory.usage.on.full.dataset' = full.memory,
      'system.memory.limit' = paste0(memory.limit(), " Mb"),
      'p.value.model.significance' = signif.test
    )
    if (plot.result==TRUE) {
      custom_titles = list("Complexity Fit against MEMORY USAGE",   paste0("ALGORITHM: ", algorithm_name, "() // DATASET: ", dataset_name, " // STRATA: ", ifelse(is.null(strata), "None", strata))  )
      to.plot <- to.model %>% select(-NlogN_X) %>% melt(measure.vars=c(3, 4:10), value.name="memory", variable.name="model") %>% mutate(best.model = model==best.model)
      print(CompEstPlot(to.plot, element_title = custom_titles, use="memory"))
    }
  } else {
    output.memory = list("Warning" = "Memory analysis is only available for Windows OS")
  }

  return(list("sample.sizes" = sample.sizes,
              "TIME COMPLEXITY RESULTS" = output.time,
              "MEMORY COMPLEXITY RESULTS" = output.memory))
}        
