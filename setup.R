# Libraries for package development ====

# For exploring
library(tidyverse)
library(tidymodels)
library(devtools)
load_all()

# Test out parsnip
data(twins)
split <- initial_split(twins, prop = 3/4)
train <- training(split)
test <- testing(split)

cosinor_mod <-
	cosinor_reg(period = 24) %>%
	set_engine("card") %>%
	set_mode("regression")

cosinor_fit <-
	cosinor_mod %>%
	fit(rDYX ~ hour, data = train)

# Create new parsnip for circular
show_model_info("circular_reg")
get_fit("circular_reg")

circular_reg() %>%
	set_engine("circular") %>%
	translate()

# Data set up
data(geh)
f <- az_svg ~ lab_hba1c + cad
df <- geh[c("az_svg", "lab_hba1c", "cad")]
df$az_svg <- circular::circular(df$az_svg, units = "degrees")
mat <- model.frame(f, data = df)
x <- model.matrix(f, data = mat)
y <- mat[["az_svg"]]

# Traditional way
m_trad <-
	circular::lm.circular(y = y, x = x, type = "c-l", init = rep(0, ncol(x)), tol = 1e-1, verbose = TRUE)

# Parsnip way
m_parsnip <-
	circular_reg(pattern = "c-l", initial = rep(0, 3), tolerance = 1e-1) %>%
  set_engine("circular") %>%
  fit(f, data = df)



# Toy data set
set.seed(1234)
head(mtcars)
df <- mtcars[c("mpg", "wt", "hp")]

# Formula to eventually be used
f <- mpg ~ wt + hp

# Create the appropriate matrices. The x-matrix has an intercept
# MPG is going to be in "degrees" as a "circular" object
mat <- model.frame(f, data = mtcars)
x <- model.matrix(f, data = mat)
y <- circular::circular(mat[["mpg"]], units = "degrees")

# Making the traditional model
m_trad <- circular::lm.circular(y = y, x = x, type = "c-l", init = rep(0, ncol(x)), tol = 1e-1, verbose = TRUE)

# Setup
library(tidymodels)

# Making sure the y vector is `circular` in class
df$mpg <- circular::circular(df$mpg, units = "degrees")

# Making it the parsnip way
m_parsnip <-
	circular_reg(pattern = "c-l", initial = rep(0, 3), tolerance = 1e-1) %>%
  set_engine("circular") %>%
  fit(f, data = df)


### MINIMAL WORKING EXAMPLE

set.seed(1234)
library(circular)
library(tidyverse)
library(tidymodels)

# Start making new model
parsnip::set_new_model("circular_reg")

# Add parsnip models to another package
parsnip::set_model_mode(model = "circular_reg", mode = "regression")
parsnip::set_model_engine("circular_reg", mode = "regression", eng = "circular")
parsnip::set_dependency("circular_reg", eng = "circular", pkg = "circular")

# Arguments = type
parsnip::set_model_arg(
    model = "circular_reg",
    eng = "circular",
    parsnip = "pattern",
    original = "type",
    func = list(pkg = "circular", fun = "lm.circular"),
    has_submodel = FALSE
)

# Arguments = init
parsnip::set_model_arg(
    model = "circular_reg",
    eng = "circular",
    parsnip = "initial",
    original = "init",
    func = list(pkg = "circular", fun = "lm.circular"),
    has_submodel = FALSE
)

# Arguments = tol
parsnip::set_model_arg(
    model = "circular_reg",
    eng = "circular",
    parsnip = "tolerance",
    original = "tol",
    func = list(pkg = "circular", fun = "lm.circular"),
    has_submodel = FALSE
)

# Encoding
parsnip::set_encoding(
    model = "circular_reg",
    eng = "circular",
    mode = "regression",
    options = list(
        predictor_indicators = "traditional",
        compute_intercept = TRUE,
        remove_intercept = FALSE,
        allow_sparse_x = TRUE
    )
)

# Fit
parsnip::set_fit(
    model = "circular_reg",
    eng = "circular",
    mode = "regression",
    value = list(
        interface = "matrix",
        protect = c("x", "y"),
        func = c(pkg = "circular", fun = "lm.circular"),
        defaults = list(verbose = TRUE)
    )
)

# Prediction
parsnip::set_pred(
    model = "circular_reg",
    eng = "circular",
    mode = "regression",
    type = "numeric",
    value = list(
        pre = NULL,
        post = NULL,
        func = c(fun = "predict"),
        args = list(
            object = quote(object$fit),
            new_data = quote(new_data),
            type = "numeric"
        )
    )
)

# Official parsnip model spec
circular_reg <- function(mode = "regression", pattern = NULL, initial = NULL, tolerance = NULL) {

    # Check correct mode
    if(mode != "regression") {
        stop("`mode` should be 'regression'", call. = FALSE)
    }

    # Capture arguments
    args <- list(
        pattern = rlang::enquo(pattern),
        initial = rlang::enquo(initial),
        tolerance = rlang::enquo(tolerance)
    )

    # Model specs / slots
    parsnip::new_model_spec(
        "circular_reg",
        args = args,
        mode = mode,
        eng_args = NULL,
        method = NULL,
        engine = NULL
    )
}

### MODELING SETUP

head(mtcars)
df <- mtcars[c("mpg", "wt", "hp")]

# Formula to eventually be used
f <- mpg ~ wt + hp

# Create the appropriate matrices. The x-matrix has an intercept
# MPG is going to be in "degrees" as a "circular" object
mat <- model.frame(f, data = mtcars)
x <- model.matrix(f, data = mat)
y <- circular::circular(mat[["mpg"]], units = "degrees")

### TRADITIONAL MODEL

m_trad <- circular::lm.circular(y = y, x = x, type = "c-l", init = rep(0, ncol(x)), tol = 1e-1, verbose = TRUE)

### PARSNIP MODEL

# Making sure the y vector is `circular` in class
df$mpg <- circular::circular(df$mpg, units = "degrees")

# Making it the parsnip way
set.seed(1234)
m_parsnip <-
  circular_reg(pattern = "c-l", initial = rep(0, 3), tolerance = 1e-1) %>%
  set_engine("circular") %>%
  fit(f, data = df)

### COMPARE

print(m_trad)
print(m_parsnip)
