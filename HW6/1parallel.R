#with parallel processing -- parallel
library(parallel)
library(nycflights13)
library(tidyverse)
data(flights)

flights <- flights%>%select(dest, air_time, origin)
#' Function to sample specific dest
#' 
#' @param dest_name specific dest
#' @return a dataframe of sample
sample_dest <- function(dest_name){
  raw <- flights[flights$dest==dest_name, ]
  size <- dim(raw)[1]
  out <- raw %>% sample_n(size, replace=T)
  return(out)
}

S <- 1000
dests <- unique(flights$dest)
# we use mclapply
# First we parallel sampling single dataset

#' Function to generate one bootstrapped sample
#' 
#' @param idx index
#' @param dest_names dest names, list
sample_dest_one <- function(idx, dest_names) {
  out <- mclapply(dest_names, function(dest) sample_dest(dest))
  out.save <- do.call(rbind, out)
  saveRDS(out.save,
          file=paste0('./samples/', idx, '_sample.rds'))
  return(0)
}
# Second we parallel sampling all dataset
print(system.time(mclapply(as.list(1:S), 
                     sample_dest_one,
                     dest_names=dests)))

