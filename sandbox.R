# Read example data
dat <- read.csv("~/Downloads/echo.csv")


lvidd <- sapply(dat$procedure_report, extract_lvidd, USE.NAMES = FALSE)
lvef <- sapply(dat$procedure_report, extract_lvef, USE.NAMES = FALSE)
la_diam <- sapply(dat$procedure_report, extract_la_diameter, USE.NAMES = FALSE)
la_size <- sapply(dat$procedure_report, extract_la_size, USE.NAMES = FALSE)


sample_text <- "Date 01/01/2025. LA diameter is 4.5 cm. LV function is mildly to moderately reduced with an EF of 45%."

extract_la_size_quantitative(sample_text)
