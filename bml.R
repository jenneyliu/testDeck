inShape <- function(bml) {
      result <- "Ideal"
      if (bml <= 18.5)
      {
            result <- "Underweight"
      } else if (bml <= 25)
      {
            result <- "Ideal"
      } else if (bml <= 30)
      {
            result <- "Overweight"
      } else 
      {
            result <- "Obese"
      } 

      result <- factor(result, levels = c("Underweight", "Ideal", "Overweight", "Obese"))
      result
}


bml_plot <- function (bml, bml_one) {
    bml_levels <- sapply(bml, inShape)
    bml_cut <- cut(bml, breaks=c(0, 18.5, 25, 30, 100))
    bml_df <- data.frame(bml_cut=bml_cut, bml_levels=bml_levels )
    
    plot(bml_levels, bml, ylab="BMI")
    points(inShape(bml_one), bml_one, col="darkred", pch=16)
}





library(manipulate)
bml <- seq(1, 50, by=0.1)
manipulate(bml_plot(bml, x), x = slider(1, 50))


bml_one = 18






