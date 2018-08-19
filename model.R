library(rstan)
library(io)
library(ggplot2)
library(dplyr)
library(lubridate)

set.seed(1);

rstan_options(auto_write = TRUE);
#options(mc.cores = 4);

d <- qread("ut_md_anderson.csv", quote="\"");
data_date <- as.Date("2017-09-20");

d$ethnicity[d$ethnicity %in% c(" ", "2+RACE")] <- NA;
d$ethnicity <- factor(d$ethnicity, levels=names(sort(table(d$ethnicity), decreasing=TRUE)));

#d$gender <- relevel(d$gender, "Female");
d$gender <- relevel(d$gender, "Male");

d.f <- filter(d, fte > 0, complete.cases(d));
d.f <- droplevels(d.f);

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

fit <- stan(
	file = "salary.stan", data = x, iter = 1000, chains = 4,
	pars = c("theta", "phi", "mu", "nu", "beta", "sigma", "tau_mu")
);

#qwrite(fit, "ut_md_anderson_ref-female.stanfit.rds");
qwrite(fit, "ut_md_anderson_ref-male.stanfit.rds");

print(fit, pars="mu");
print(fit, pars="nu");
print(fit, pars="beta")
print(fit, pars=c("theta", "phi", "sigma", "tau_mu"))

mu <- extract(fit, pars="mu")[[1]];
mu.mean <- colMeans(mu);
names(mu.mean) <- levels(d.f$job_title);

sort(exp(mu.mean))
sort(exp(mu.mean), decreasing=TRUE)

exp(mu.mean)["Professor"]
exp(mu.mean)["Associate Professor"]
exp(mu.mean)["Assistant Professor"]

exp(mu.mean)["Clinical Professor"]
exp(mu.mean)["Clinical Associate Professor"]
exp(mu.mean)["Clinical Assistant Professor"]

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

