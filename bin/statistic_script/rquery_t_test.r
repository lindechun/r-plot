#++++++++++++++++++++++++
# rquery.t.test
#+++++++++++++++++++++++
# Description : Performs one or two samples t-test

# x : a (non-empty) numeric vector of data values.
# y : an optional (non-empty) numeric vector of data values
# paired : if TRUE, paired t-test is performed
# graph : if TRUE, the distribution of the data is shown
  # for the inspection of normality
# ... : further arguments to be passed to the built-in t.test() R function

# 1. shapiro.test is used to check normality
# 2. F-test is performed to check equality of variances
# If the variances are different, then Welch t-test is used

rquery.t.test<-function(x, y = NULL, paired = FALSE, graph = TRUE, ...)
{
  # I. Preliminary test : normality and variance tests
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  var.equal = FALSE # by default
  
  # I.1 One sample t test
  if(is.null(y)){
    if(graph) par(mfrow=c(1,2))
    shapiro.px<-normaTest(x, graph, 
                          hist.title="X - Histogram",
                          qq.title="X - Normal Q-Q Plot")
    if(shapiro.px < 0.05)
      warning("x is not normally distributed :",
              " Shapiro-Wilk test p-value : ", shapiro.px, 
              ".\n Use a non-parametric test like Wilcoxon test.")
  }
  
  # I.2 Two samples t test
  if(!is.null(y)){
    
      # I.2.a unpaired t test
      if(!paired){
          if(graph) par(mfrow=c(2,2))
          # normality test
          shapiro.px<-normaTest(x, graph, 
                                hist.title="X - Histogram",
                                qq.title="X - Normal Q-Q Plot")
          shapiro.py<-normaTest(y, graph,
                                hist.title="Y - Histogram",
                                qq.title="Y - Normal Q-Q Plot")
          if(shapiro.px < 0.05 | shapiro.py < 0.05){
              warning("x or y is not normally distributed :",
                      " Shapiro test p-value : ", shapiro.px,
                      " (for x) and ", shapiro.py, " (for y)",
                      ".\n Use a non parametric test like Wilcoxon test.")
            }
          # Check for equality of variances
          if(var.test(x,y)$p.value >= 0.05) var.equal=TRUE
        } 
      
      # I.2.b Paired t-test
      else {
        if(graph) par(mfrow=c(1,2))
        d = x-y 
        shapiro.pd<-normaTest(d, graph, 
                              hist.title="D - Histogram",
                              qq.title="D - Normal Q-Q Plot")
        if(shapiro.pd < 0.05 )
          warning("The difference d ( = x-y) is not normally distributed :",
                  " Shapiro-Wilk test p-value : ", shapiro.pd, 
                  ".\n Use a non-parametric test like Wilcoxon test.")
      } 
      
   }
  
  # II. Student's t-test
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- t.test(x, y, paired=paired, var.equal=var.equal, ...)
  return(res)
}


#+++++++++++++++++++++++
# Helper function
#+++++++++++++++++++++++

# Performs normality test using Shapiro Wilk's method
# The histogram and Q-Q plot of the data are plotted

# x : a (non-empty) numeric vector of data values.
# graph : possible values are TRUE or FALSE. If TRUE,
  # the histogram and the Q-Q plot of the data are displayed
# hist.title : title of the histogram
# qq.title : title of the Q-Q plot
normaTest<-function(x, graph=TRUE, 
                    hist.title="Histogram", 
                    qq.title="Normal Q-Q Plot",...)
  {  
  # Significance test
  #++++++++++++++++++++++
  shapiro.p<-signif(shapiro.test(x)$p.value,1) 
  
  if(graph){
    # Plot : Visual inspection
    #++++++++++++++++
    h<-hist(x, col="lightblue", main=hist.title, 
            xlab="Data values", ...)
    m<-round(mean(x),1)
    s<-round(sd(x),1)
    mtext(paste0("Mean : ", m, "; SD : ", s),
          side=3, cex=0.8)
    # add normal curve
    xfit<-seq(min(x),max(x),length=40)
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="red", lwd=2)
    # qq plot
    qqnorm(x, pch=19, frame.plot=FALSE,main=qq.title)
    qqline(x)
    mtext(paste0("Shapiro-Wilk, p-val : ", shapiro.p),
          side=3, cex=0.8)
  }
  return(shapiro.p)
}
