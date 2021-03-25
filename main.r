## Designed to make random datasets for students to use in class assignments.

# Number of data points per dataset
n <- 40

# Upper bound for x-values. Lower bound is defaulted to zero.
r <- 40

# Create random (uniform) assortment of explanatory values.
x <- runif(n,min=0.01,max=r)

# Import list of plural nouns to be used to describe values.
nouns <- read.csv(file='nouns.csv')
print(head(nouns))

# Start a pdf file.
pdf(file="test.pdf",width=8.5,height=11,paper='special')

# Loop through the creation process. Each time through creates "two pages" that amount to a front and a back.
for (i in 1:52){
# Randomly assign an ID to this dataset	
	id <- sample(1000000000:9999999999,1)
# Randomly pick plural nouns to assign to explanatory and response values.
	pt <- sample(35:1200,1)
	xlabel <- nouns[pt,1]
	pt <- sample(35:1200,1)
	ylabel <- nouns[pt,1]
# Randomly pick which function family to use.
	funcType <- sample(2:5,1)
# Use the family function to produce a list of response values.
# Each function is randomized so that no two datasets will be identical.
### Function Types
	if (funcType == 1){
 	## Cubic
		a <- -0.00729167
		b <- 0.195833
  		y <- a*(x-10)*(x-10)*(x-10)+b*(x-10)*(x-10)
	} else if (funcType == 2){
 	## Logistic
  		mid <- runif(1,r/4,3*r/4)
		top <- runif(1,r/5,3*r)
  		c <- runif(1,-1,-.5)
  		y <- top/(1+exp(c*(x-mid)))
	} else if (funcType == 3){
 	## Quadratic
  		d <- runif(1,-.9,-.1)
		mid <- runif(1,r/3,2*r/3)
  		y <- d*(x-mid)*(x-mid)+r/2
	} else if (funcType == 4){
 	## Exponential
  		b <- runif(1,0.1,0.9)
		y <- b*exp(x/3)
	} else if (funcType == 5){
 	## Logarithmic
		a <- runif(1,1,2)
		b <- runif(1,3,8)
		c <- runif(1,5,15)
		y <- a*log(b*x)+c
	}
# Code here is to add "noise" to the response values.
# Get the range of values
	range = max(y)-min(y)
# Let the max err be about 7% 
	err = range/15
# Loop through the response values and add a random amount of noise.
	for(i in 1:length(y)){
		y[i] <- y[i]+runif(1,min=-err,max=err)
	}
# Move the response values up so that none are negative. This makes the logarithmic function easier to model.
	y <- y - min(y)+.1


##This is oudated and/or misplaced. But I'm scared to delete, just yet.
##export <- cbind(x,y)
##print(export)
##write.csv(export,'export.csv',row.names=FALSE)
##pdf(file="test.pdf",width=8.5,height=11,paper='special')

# Keeps the plot on the front page in the printable margins
	par(oma=c(2,2,2,2))
# Front page
	plot(x,y,main="Dataset for Concept Quiz #6 - Nonlinear Regressions",xlab=xlabel,ylab=ylabel)
	## g <- nls(y ~ a + ((b - a)/(1 + exp(-c * (x - d)))), start = list(a = min(y), 
    ## b = max(y), c = 1, d = round(median(x))), trace = TRUE)
	text((max(x)-min(x))/2,range/2,"Your plot might look different.")
# Back page
	plot.new()
	text(.05,.96,xlabel)
	text(.17,.96,ylabel)
	text(.1,.99,"Sample Dataset")
	for (i in 1:length(x)){
		text(.05,.95-.02*i,format(round(x[i],2),nsmall=2))
		text(.17,.95-.02*i,format(round(y[i],2),nsmall=2))
	}
	segments(.03,.945,.19,.945)
	segments(.1,.95+.02,.1,.95-.02*length(x)-.02)
	text(.5,.99,"Make a prediction with the following value:")
	text(.5,.97,"x=")
	text(.55,.97,format(round(runif(1,0.1,r),2),nsmall=2))
	text(.5,.8,"ID")
	text(.5,.77,id)
}

# Close PDF
dev.off()