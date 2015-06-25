
attribute.sample.size <- function(confidence, reliability){
  # calculates sample size for tolerance intervals with stated confidence and reliability combination
  # when no failures are allowed in the sample.
  
  c <- confidence / 100; 
  r <- reliability / 100;
  thresh <- log(1 - c) / log(r)
  n <- ceiling(thresh)
  cat("\n", "The minimum sample size assuming zero failures is n =", n, "for this attribute.", "\n")
}