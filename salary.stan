data {
	int<lower=1> I;  // number of individuals
	int<lower=1> G;  // number of gender groups
	int<lower=1> H;  // number of ethnic groups
	int<lower=1> J;  // number of job classes
	int<lower=1> D;  // number of departments

	real y[I];       // log annual salary
	real t[I];       // length of time at current job
	int<lower=1,upper=G> g[I];   // gender
	int<lower=1,upper=H> h[I];   // ethnicity
	int<lower=1,upper=J> j[I];   // job class
	int<lower=1,upper=D> d[I];   // department
}

parameters {
	real theta[G-1];   // gender effect (compared to female)
	real phi[H-1];     // ethnicity effect

	real beta[D];             // department-specific salary log base
	// ideally, these would have size J

	real<lower=0> nu[J];      // job-specific salary log raise

	real<lower=0> sigma;      // inter-individual variability
	real<lower=0> tau_mu;     // inter-job variability within department

	// augmented variables
	real eta_mu[J];
}

transformed parameters {
	real mu[J];               // job-specific salary log base
	// ideally, these would have size J x D, but this would create way too many parameters

	real z[I];

	for (i in 1:I) {
		int jj;
		int dd;

		jj = j[i];
		dd = d[i];

		mu[jj] = beta[dd] + tau_mu*eta_mu[jj];

		z[i] = mu[jj] + nu[jj]*t[i] + 
			(g[i] == 1 ? 0 : theta[g[i]-1]) +
			(h[i] == 1 ? 0 : phi[h[i]-1]);
	}
}

model {
	theta	~ normal(0, 1);
	phi ~ normal(0, 1);
	nu ~ normal(0, 0.1);

	eta_mu ~ normal(0, 1);

	y ~ normal(z, sigma);
}

