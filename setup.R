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

# Trying out aims
f <- az_svg ~ lab_hba1c + age + sex + bmi + cad + htn
df <- model.frame(f, data = geh)
df$az_svg <-
	circular::circular(df$az_svg, units = "degrees") %>%
	circular::conversion.circular(., units = "radians")
mat <- model.frame(f, data = df)
x <- model.matrix(f, data = mat)
y <- mat[["az_svg"]]

# Circular regression spec
circ_mod <-
	circular_reg(pattern = "c-l", tolerance = 1e-1, initial = varying()) %>%
	set_engine("circular")

b <- bullet(f, exposure = "lab_hba1c", approach = "sequential", model = circ_mod)
aims <- aim(list(circ = b))
ballistics(aims$circ)
circ <-
	aims$circ %>%
	mutate(model_spec = map2(model_spec, formulas, ~update(.x, initial = rep(0, length(all.vars(.y)))))) %>%
	ballistics()
final <- fire(circ, df)
ballistics(final)

