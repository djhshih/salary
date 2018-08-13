library(rstan)
library(io)
library(ggplot2)
library(dplyr)
library(lubridate)

d <- qread("ut_md_anderson.csv", quote="\"");
data_date <- as.Date("2017-09-20");

d.f <- filter(d, fte > 0);

x <- with(d.f,
	list(
		I = nrow(d.f),
		G = length(levels(gender)),
		H = length(levels(ethnicity)),
		J = length(levels(job_title)),
		D = length(levels(dept_name)),
		y = log(annual_rate / fte),
		t = floor(time_length(data_date - as.Date(job_entry_date), "years")),
		g = as.numeric(gender),
		h = as.numeric(ethnicity),
		j = as.numeric(job_title),
		d = as.numeric(dept_name)
	)
);

fit <- stan(file = "salary.stan", data = x, iter=1000, chains=4);

