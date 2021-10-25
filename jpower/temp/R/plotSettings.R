

ttestPlotSettings = list(
  lens = 20,
  x.axis.n = 8,
  pow.n.levels = 10,
  curve.n = 128,
  
  maxn = 100,
  max.scale = 1.5,
  
  mind = 0,
  maxd = 2,
  
  background.alpha = .7,
  #stripe.cols = pal(pow.n.levels)[c(1,pow.n.levels)]
  stripe.cols = c("black", "black") 
)
ttestPlotSettings$pal = function(...) 
  viridis::viridis(..., alpha = ttestPlotSettings$background.alpha)

