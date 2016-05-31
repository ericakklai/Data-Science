# Hidden Markov chain model

### Challenge:
There are three sequences of data and the mission is to
guess how they are generated. 

Hint:

Each observed sequence X is simulated from a hidden Markov chain model (HMM). The corresponding hidden Z chains have state spaces of different sizes: one sequence has just 1 hidden state, another has 2 states, and the remaining sequence has 3 hidden states. That is, one of the sequences is iid from some distribution on Xt ∈ {1, 2, 3}, one sequence alternates between two distributions on X, and the last sequence switches among three different distributions on X. Which sequence is which? For each sequence, what is your best guess for the transition matrix A of the hidden chain and the emission matrix B that I used to simulate the sequence?

### Data 

 * [Sequence 1]

 * [Sequence 2]

 * [Sequence 3]

[Sequence 1]:<https://github.com/ericakklai/Data-Science/blob/edit/Statistical%20Computing/Hidden%20Markov%20chain%20model%20(HMM)/f15_hw4_seq1.txt>

[Sequence 2]:<https://github.com/ericakklai/Data-Science/blob/edit/Statistical%20Computing/Hidden%20Markov%20chain%20model%20(HMM)/f15_hw4_seq2.txt>

[Sequence 3]:<https://github.com/ericakklai/Data-Science/blob/edit/Statistical%20Computing/Hidden%20Markov%20chain%20model%20(HMM)/f15_hw4_seq3.txt>


###Discussion
R package hmm.discnp is used. Since the number of hidden states K is equal to 1, 2 or 3. The likelihood always increases when K is increased, i.e. complicated models with more parameters give higher likelihoods.

My decision should is based on whether the improvement in the
log-likelihood is worth adding extra parameters. Standard statistical procedures has been used such as likelihood ratio test (chi-square test), BIC and AIC to make the decision


### Summary of Analysis
• Use glmnet to produce Linear/ Logistic regression regularized by L1 (Lasso Regression), L2 (Ridge Regression) and Elastic Net

• Estimate the regularization coefficient that produces the minimum error

• Study and Compare the number of features selected under each of the regularization scheme