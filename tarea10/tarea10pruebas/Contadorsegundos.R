
library(tcltk)
timer = 10
pb <- tkProgressBar("Timer")
start = Sys.time()
while(TRUE) {
  elapsed = as.numeric(difftime(Sys.time(), start, units = 'secs'))
  remaining = timer - elapsed
  Sys.sleep(0.1)
  setTkProgressBar(pb, remaining/timer, label = sprintf("Time remaining: %i seconds", round(remaining)))
  print(elapsed)
  if (remaining <= 0) break
 
}
Sys.sleep(2)
close(pb)
