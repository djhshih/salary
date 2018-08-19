library(rstan)
library(io)
library(ggplot2)
library(dplyr)
library(lubridate)

set.seed(1);

rstan_options(auto_write = TRUE);
#options(mc.cores = 4);

d <- qread("houston-isd.csv", quote="\"");
data_date <- as.Date("2017-03-01");

d$race[d$race %in% c(" ", "Two or More Races")] <- NA;
d$race <- factor(d$race, levels=names(sort(table(d$race), decreasing=TRUE)));
d$race <- relevel(d$race, "White");
table(d$race)

d$gender <- relevel(d$gender, "Female");
#d$gender <- relevel(d$gender, "Male");
table(d$gender)

d.f <- filter(d, complete.cases(d));
d.f <- droplevels(d.f);

x <- with(d.f,
	list(
		I = nrow(d.f),
		G = length(levels(gender)),
		H = length(levels(race)),
		J = length(levels(job_title)),
		D = length(levels(department)),
		y = log(annual_salary / (employment_percent / 100)),
		t = floor(time_length(data_date - as.Date(latest_hire_date), "years")),
		g = as.numeric(gender),
		h = as.numeric(race),
		j = as.numeric(job_title),
		d = as.numeric(department)
	)
);
str(x)

fit <- stan(
	file = "salary.stan", data = x, iter = 1000, chains = 4,
	pars = c("theta", "phi", "mu", "nu", "beta", "sigma", "tau_mu")
);

qwrite(fit, "houston-isd.stanfit.rds");

print(fit, pars="mu");
print(fit, pars="nu");
print(fit, pars="beta")
print(fit, pars=c("theta", "phi", "sigma", "tau_mu"))


mu <- extract(fit, pars="mu")[[1]];
mu.mean <- colMeans(mu);
names(mu.mean) <- levels(d.f$job_title);

sort(exp(mu.mean))
sort(exp(mu.mean), decreasing=TRUE)


beta <- extract(fit, pars="beta")[[1]];
beta.mean <- colMeans(beta);
names(beta.mean) <- levels(d.f$dept_name);

sort(exp(beta.mean))
sort(exp(beta.mean), decreasing=TRUE)


theta <- extract(fit, pars="theta")[[1]];
summary(theta)
summary(exp(theta))
quantile(exp(theta), c(0.025, 0.975))
1 - quantile(exp(theta), c(0.025, 0.975))

