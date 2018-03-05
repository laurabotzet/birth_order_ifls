library("future") # this is the package we use to flexibly scale our processing
library("listenv") # this is a helper package that allows us to assign the results to a list even if they come in at different times

## Set up access to remote login node
login <- tweak(remote, workers = "laurabotzet@opencpu.psych.bio.uni-goettingen.de") # Your username goes before the @

options(future.makeNodePSOCK.rshcmd = "ssh")
login <- makeClusterPSOCK(dryrun = F, verbose =T,
        workers = "laurabotzet@opencpu.psych.bio.uni-goettingen.de") # Your username goes before the @
plan(login) # this tells the future package that we want to use a remote server for running our future operations

# but maybe the reason we want to shift to a server is not that we want to run a simple sequential operation there.
# maybe we want to take advantage of parallelisation and run the operation in parallel on multiple cores
## Specify future topology
## login node -> { cluster nodes } -> { multiple cores }
plan(list(
  login,
  multicore,
  sequential
))
# demo("mandelbrot", package = "future", ask = FALSE)

## (a) This will be evaluated on the cluster login computer
x %<-% { # this operator binds the result of the promise/future to x
  thost <- Sys.info()[["nodename"]]
  tpid <- Sys.getpid()
  y <- listenv()
  for (task in 1:4) {
    ## (b) This will be evaluated on a different core
    y[[task]] %<-% {
      mhost <- Sys.info()[["nodename"]]
      mpid <- Sys.getpid()
      z <- listenv()
      for (jj in 1:2) {
        ## (c) this will be evaluated sequentially on the same core
        z[[jj]] %<-% data.frame(task = task,
                                top.host = thost, top.pid = tpid,
                                mid.host = mhost, mid.pid = mpid,
                                host = Sys.info()[["nodename"]],
                                pid = Sys.getpid())
      }
      Reduce(rbind, z)
    }
  }
  Reduce(rbind, y)
}

print(x)
