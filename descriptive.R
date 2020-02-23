library(psych)
library(ggplot2)
library(ggpubr)
library(corrplot)

#+++++++++++++++++++++++++
# Computing of correlation matrix
#+++++++++++++++++++++++++
# Required package : corrplot
# x : matrix
# type: possible values are "lower" (default), "upper", "full" or "flatten";
#display lower or upper triangular of the matrix, full  or flatten matrix.
# graph : if TRUE, a correlogram or heatmap is plotted
# graphType : possible values are "correlogram" or "heatmap"
# col: colors to use for the correlogram
# ... : Further arguments to be passed to cor or cor.test function
# Result is a list including the following components :
# r : correlation matrix, p :  p-values
# sym : Symbolic number coding of the correlation matrix
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}

# Correlation
rquery.cormat(select(ratings, valence, arousal, dominance))



# Demographics
dem <- ratings[c()]

# Boxplot neg, neu, pos valence
boxplot(valence~v_cat,data=ratings, main="", xlab="Valence category", ylab="Valence scores")
# Boxplot r3=low, r4=medium, r5=high arousal
boxplot(arousal~a_cat,data=ratings, main="", xlab="arousal category", ylab="arousal scores")

# Summary statistics
str(ratings)
summary(ratings)
voi <- c("valence", "arousal", "dominance", "attention", "reaction_time")
by(ratings[voi], ratings$v_cat, summary)
by(ratings[voi], ratings$a_cat, summary)

# Histograms
ggplot(data=ratings, aes(ratings$valence)) + geom_histogram()
ggplot(data=ratings, aes(ratings$arousal)) + geom_histogram()
ggplot(data=ratings, aes(ratings$dominance)) + geom_histogram()
ggplot(data=ratings, aes(ratings$attention)) + geom_histogram()
ggplot(data=ratings, aes(ratings$reaction_time)) + geom_histogram()

# Q-Q plots
ggqqplot(ratings$valence)
ggqqplot(ratings$arousal)
ggqqplot(ratings$dominance)
ggqqplot(ratings$reaction_time)

# Normality test
shapiro.test(ratings$valence)
shapiro.test(ratings$arousal)
shapiro.test(ratings$dominance)
shapiro.test(ratings$reaction_time)

# Categorical scatter plots
ggplot(ratings, aes(x = v_cat, y = valence)) + geom_point(position = position_dodge(width = 0.4))

ggplot(ratings, aes(x = a_cat, y = arousal)) + geom_point(position = position_dodge(width = 0.4))
