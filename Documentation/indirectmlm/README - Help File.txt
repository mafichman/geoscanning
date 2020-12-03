indirect.mlm

Bootstrapping Accurate Indirect Effects in Multilevel Models

DESCRIPTION

This function can be used to bootstrap accurate indirect effects in multilevel mediation models where both paths that comprise the indirect effect are random. This is just a function for the boot() function from the package, “boot” (Canty & Ripley, 2015). The boot function requires the user to pass a function to it using the “statistic” argument. When you set the statistic argument to equal our function, indirect.mlm, then the boot function will conduct a multilevel mediation.

USAGE

boot( data, statistic=indirect.mlm, R, strata, y, x, mediator, group.id, covariates=NULL, 
      random.a=T, random.b=T, random.c=T, uncentered.x=T, between.x=F, between.m=F )

indirect.mlm.summary( boot.object )


ARGUMENTS

data		The data frame that contains all the variables for your analysis.

statistic	Must always be indirect.mlm, exactly as depicted in the Usage statement.

R		Number of bootstrap resamples you want to draw.

strata		The variable that identifies each of your Level 2 groups (e.g., participant ID).

y		A string that matches the variable name of your dependent variable.

x		A string that matches the variable name of your predictor variable.

mediator	A string that matches the variable name of your mediator variable. 
		This should always refer to an uncentered variable.	

group.id	A string that matches the variable that identifies your Level 2 groups. This string
		should be the same variable that you sent to the “strata” argument.

covariates	A string or vector of strings that contain the variable names of any covariates.
		Defaults to no covariates.

random.a	A logical value indicating whether the path from the predictor to the mediator is 
		random. Defaults to TRUE.

random.b	A logical value indicating whether the path from the mediator to the dependent 
		variable is random. Defaults to TRUE.

random.c	A logical value indicating whether the path from the predictor to the dependent 
		variable is random. Defaults to TRUE.

uncentered.x	A logical value indicating whether the predictor is uncentered. It defaults to TRUE, and
		the function group-mean centers the predictor for you on each iteration. However, if 
		the variable name provided to the “x” argument references a centered continuous variable 
		or a coded categorical variable, you will want to set this value to FALSE so the function
		will leave the predictor coded as it is in the dataset data frame.

between.x	A logical value indicating whether you want to disentangle the within-group and between-group
		effects for the predictor. This defaults to FALSE, such that only within-group effects are 
		estimated by default. If set to TRUE, then the function will follow Zhang, Zhyphur, & Preacher
		(2009) to create two variables that reflect within- and between-group effects by group-mean
		centering the predictor and calculating per-group averages for the predictor, respectively.

between.m	A logical value indicating whether you want to disentangle the within-group and between-group
		effects for the mediator. This defaults to FALSE, such that only within-group effects are 
		estimated by default. If set to TRUE, then the function will follow Zhang, Zhyphur, & Preacher
		(2009) to create two variables that reflect within- and between-group effects by group-mean
		centering the mediator and calculating per-group averages for the mediator, respectively.

boot.object	The object that is returned by the boot function. This object is sent to indirect.mlm.summary()
		to print the output.


DETAILS

The function indirect.mlm is really a helper function for the boot package. To use indirect.mlm, you must use at least four arguments of the boot function: data, statistic, R, and strata. Statistic must always equal this helper function, indirect.mlm (i.e., statistic=indirect.mlm). You can use other arguments from the boot function, too. To learn more about the boot function, type help(boot). All the other arguments listed above are specific to the indirect.mlm function.


VALUE

The returned object is from the boot class and has all the components of any boot argument. See help(boot) for a description of each of these values. However, the “t” component has bootstrapped values for 15 parameters in the following indexed order:

1	fixed slope for the (biased) total effect of the predictor on the dependent variable
2	fixed slope for the direct effect of the predictor on the dependent variable
3	fixed slope for the within-subjects effect of path a
4	fixed slope for the between-subjects effect of path a
5	fixed slope for the within-subjects effect of path b
6	fixed slope for the between-subjects effect of path b
7	covariance between the random paths, a and b, if both are random
8	unbiased estimate of the within-subjects indirect effect (i.e., mean of the products of the random 
	slopes, a and b)
9	biased estimate of the within-subjects indirect effect (i.e., products of the fixed slopes, a and b)
10	the difference between the unbiased and biased within-subjects indirect effects - should be equal to 
	the covariance between a and b
11	unbiased estimate of the between-subjects indirect effect
12	“biased” estimate of the between-subjects indirect effect. This only has the potential to be biased if
	you are only estimating between subjects effects for the predictor or mediator, but not both
13	the difference between the unbiased and biased between-subjects indirect effects — should only be non-
	zero if between-subject effects are estimated for only the predictor or only the mediator 
14	unbiased total effect
15	the difference between the biased and unbiased total effect - this should be double the covariance of a and b


EXAMPLE

# First, download the materials from http://page-gould.com/r/indirectmlm. Unzip the folder and set it to be your working directory in R. Then:

# Load Boot Package
library( boot )

# Load indirect.mlm And indirect.mlm.summary Functions
source( "indirectMLM.R" )

# Read Data
attitudes.data <- read.csv( "Multigroup Attitudes.csv" )

# Bootstrap a multilevel mediation
mediated.mlm <- boot( data=attitudes.data, statistic=indirect.mlm, R=100, strata=attitudes.data$ID, 
                      y="warmth", x="target", mediator="sympathy", group.id="ID", 
                      between.m=T, uncentered.x=F )

# Print Output
indirect.mlm.summary( mediated.mlm )
