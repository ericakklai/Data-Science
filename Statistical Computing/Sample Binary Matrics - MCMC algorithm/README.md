# Sample Binary Matrics - MCMC algorithm

### Summary

The aim of this project is to simulate the uniform sampling of binary matrices (0-1 matrices) with fixed margins (fixed row sum and column sum) using an Markov chain Monte Carlo (MCMC) algorithm as discussed in the literature of “AN EFFICIENT MCMC ALGORITHM TO SAMPLE BINARY MATRICES WITH FIXED MARGINALS" by Norman D. Verhelst from CITO, National Institute for Educational Measurement in 2008 (Verhelst, 2008). This new algorithm, using importance sampling with Metropolis-Hasting, had been concluded to be more efficient than previous algorithms for generating the binary matrices with fixed margins because it manages to sample with a uniform stationary distribution which is simple for implementation. Such Algorithm will be applied directly into hypothesis testing for assumption of unidimensionality for Rasch Model. Specifically, samples of binary matrices with fixed marginal will be generated by implementing the effective MCMC algorithm. The samples would then be used for computing the p-value of the test-statistic for the hypothesis testing. 

On the other hand, Verhelst’s paper also compared the result of the p-value and performance of the algorithm implementation under different parameter settings, this project also try to implementation the algorithm under different parameter setting based on the mean and standard error of the p-value computed. One of the parameters that we will pay attention to is stepsize, which is the number of sample generated in order to collect one effective sample.

### Conclusion 
Based on the simulation, the resulting p-value fluctuated in the range of 0.27 to 0.33 with standard error range of 0.05 to 0.1. It is concluded that the null hypothesis of unidimensionality is failed to be rejected. Moreover, the standard error of the computed p-values has a range of 0.05 to 0.1 which decrease with increasing stepsize.
