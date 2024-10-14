# General fitting functions ----
test_that("models can be generally fit", {
	data(twins)
	scos <- cosinor(rDYX ~ hour, twins, tau = 24)
	mcos <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
	pcos <- cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")

	# Appropriate classes
	expect_s3_class(scos, "cosinor")
	expect_s3_class(mcos, "cosinor")
	expect_s3_class(pcos, "cosinor")

	# Harmonic checks
	expect_gt(length(mcos$tau), 1)
	expect_equal(max(mcos$tau) %% min(mcos$tau), 0)
	expect_warning(cosinor_features(mcos))

	# Confidence intervals
	expect_type(confint(scos), "list")
})

test_that("population cosinors can be fit", {

	# Single population cosinor
	f <- sDYX ~ hour
	data <- twins
	population = "patid"
	m1 <- cosinor(formula = f, data = data, tau = 24, population = population)
	m2 <- cosinor(formula = f, data = data, tau = c(24, 12,8), population = population)

})


test_that("kfits dataframe is appropriate for population mean", {

	df <- as.data.frame(matrix(NA,3120,3)) # data frame for data
	names(df) <- c("time","subject", "HR") # variable names
	t <- c(1:520) # time
	df[,1] <- rep(1:520,6) # six subjects
	df[,2] <- rep(1:6,520)[order(rep(1:6,520))] # time for each subject
	set.seed(1) # seed for rnd

	# generates six different signals with some noise
	for (i in 1:6){
		M <- rnorm(1, mean=70, sd=5)
		A <- rnorm(1, mean=3, sd=0.1)
		phi <- rnorm(1, mean=60, sd=10)
		e <- rnorm(c(1:520), mean=0, sd=5)
		hr.curve <- M + A*cos((2*pi/260)*t+phi)+e
		df[520*(i-1)+(1:520),3] <- hr.curve
		#print(plot(t,hr.curve))
	}

	formula <- HR ~ time
	data <- df
	tau <- 260
	population <- "subject"
	# Was erroring in the past because of a sapply leading to a matrix
	m <- cosinor(formula = formula, data = data, tau = tau, population = population)
	expect_s3_class(m, "cosinor")

})
