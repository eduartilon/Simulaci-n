
library(tcltk)
timer = 10
start = Sys.time()

while(TRUE) {
  elapsed = as.numeric(difftime(Sys.time(), start, units = 'secs'))
  remaining = timer - elapsed
  Sys.sleep(0.1)

  print(elapsed)
  if (remaining <= 0) break
 
}


