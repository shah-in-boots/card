# Libraries for package development ====

# For admin
library(devtools)
library(hardhat)
library(pkgdown)
library(testthat)

# For exploring
library(tidyverse)
library(tidymodels)
library(data.table)

tbl <- tibble(surv_final) %>%
	group_by(pid) %>%
	slice(1)

id <- "pid"
first <- "pd_dob"
last <- "last_contact"
event.dates <- c("atk_1st_date")
model.type <- "marginal"

s <- recur_survival_table(tbl, id, first, last, event.dates, model.type)
names(s)[1] <- "pid"
df <-
	left_join(s, tbl[c("pid", "pd_diabetes", "wvg_group")], by = "pid") %>%
	filter(pd_diabetes != 3) %>%
	mutate(dm_ecg = case_when(
		pd_diabetes == 1 & wvg_group == 1 ~ "dm1_wvg1",
		pd_diabetes == 1 & wvg_group == 2 ~ "dm1_wvg0",
		pd_diabetes == 2 & wvg_group == 1 ~ "dm0_wvg1",
		pd_diabetes == 2 & wvg_group == 2 ~ "dm0_wvg0"
	))



fit <- survfit(Surv(TSTOP, STATUS) ~ dm_ecg, data = df)
ggsurvplot(fit, data = df)
