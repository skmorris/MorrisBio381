# ------------------------------------------------------
# Batch Processing
# 13 May 2021
# Sarah K. Morris
# ------------------------------------------------------
getwd()
list.files()
list.files("HW8/") # can use relative path names
dir.create("RandomFiles") # create directory
dir.create("RandomFiles1/MyFiles/", recursive=TRUE) # to create nested directories

# ------------------------------------------------------
# ------------------------------------------------------
# FUNCTION file_builder
# description: creates a set of random files for regression
# inputs: file_n = number of files to create 
#         file_folder = name of folder for random files
#         file_size = c(min,max) number of rows in file
#         file_NA = average number of NA values per column
# outputs: creates a set of random files
########################################################
file_builder <- function(file_n=10,
                         file_folder="RandomFiles/", # this needs to already exist
                         file_size=c(15,100), # every file will have somewhere b/n 15 & 100 rows
                         file_NA=3) { # on average there will be 3 NA files
  
  for (i in seq_len(file_n)) { # seq_len looks at # specified and makes a sequence from 1 to that #
    file_length <- sample(file_size[1]:file_size[2], size=1) # sample from 15 to 100 (but if you adjusted inputs, you wouldn't have to adjust this too
    var_x <- runif(file_length) # create random x
    var_y <- runif(file_length) # create random y
    df <- data.frame(var_x,var_y) # bind into dataframe
    bad_vals <- rpois(n=1,lambda=file_NA) # determines NA number 
    df[sample(nrow(df), size=bad_vals),1] <- NA # populates randomly selected rows w/ NA
    df[sample(nrow(df), size=bad_vals),2] <- NA # 
    
# create label for file name with padded zeros
    file_label <- paste(file_folder, 
                        "ranFile",
                        formatC(i, # takes formatting from C language
                                width=3, 
                                format="d",
                                flag="0"),
                        ".csv",sep="")
# set up data file and incorporate time stampe and minimal metadata
    write.table(cat("# Simulated random data file for batch processing", "\n",
                    "# timestamp: ",as.character(Sys.time()), 
                    "\n", "# SKM", "\n",
                    "# ----------------------------------", "\n",
                    "\n",
                    file=file_label, 
                    row.names="",
                    col.names="",
                    sep=""))
    
# now add the data frame
    write.table(x=df, 
                file=file_label, 
                sep=",", 
                row.names=FALSE, 
                append=TRUE) # b/c we've already used the write.table and we don't want to write over it
  } # end of for loop
    
} # end of file_builder
# ------------------------------------------------------

# ------------------------------------------------------
# FUNCTION reg_stats
# description: fits linear model, extract stats
# inputs: 2 column dataframe (x&y)  
# outputs: slope,p-value, and r^2 
########################################################
reg_stats <- function(d=NULL) {
             if(is.null(d)) {
               x_var <- runif(10)
               y_var <- runif(10)
               d <- data.frame(x_var,y_var)
             }
  . <- lm(data=d,d[,2]~d[,1])
  . <- summary(.)
  stats_list <- list(Slope=.$coefficients[2,1],
                     pVal=.$coefficients[2,4],
                     r2=.$r.squared)
  
  
  return(stats_list)
  
} # end of reg_stats
# ------------------------------------------------------

library(TeachingDemos)
char2seed("Flatpicking solo")

########################################################
# Global Variables
file_folder <- "RandomFiles/"
n_files <- 100
file_out <- "StatsSummary1.csv"
########################################################

# create random data sets
dir.create(file_folder)
file_builder(file_n=n_files) #col names warning is ok to ignore
file_names <- list.files(path=file_folder)

# create a data frame to hold summary file statistics
ID <- seq_along(file_names)
file_name <- file_names
slope <- rep(NA, length(file_names))
pVal <- rep(NA, length(file_names))  
r2 <- rep(NA, length(file_names))  
  
stats_out <- data.frame(ID,file_name,slope,pVal,r2)

# batch process by looping by looping through individual files

for (i in seq_along(file_names)) { # to get current # of files in the data set
     data <- read.table(file=paste(file_folder,file_names[i],sep=""),
                        sep=",",
                        header=TRUE)
     d_clean <- data[complete.cases(data),] # subset non-NA data/clean cases
     
     . <- reg_stats(d_clean) # pull out regression stats from clean file
     
     stats_out[i,3:5] <- unlist(.) # slope,pVal,r2 = 3:5 & have to unlist b/c you cant fill a dataframe with a list
}

# set up output file and incorporate time stamp and minimal metadata

write.table(cat("# Summary stats for ", 
                "batch processing of regression models",
                "\n",
                "# timestamp: ", as.character(Sys.time()),
                "\n",
                file=file_out,
                row.names="",
                col.names="",
                sep=""))

# now add the data frame
write.table(x=stats_out,
            file=file_out,
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE) # don't worry about warning message re appending col names

## can use this StatsSummary.csv as input for summary statistics, to make graphs, etc...
## can change the file_out to StatsSummary1 if you adjust the ranFiles 
## (we deleted some of the ranFiles) and then ran the code again (line 97, then line103+)

            
  
  
  
  
  
  