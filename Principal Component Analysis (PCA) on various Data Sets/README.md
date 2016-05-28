# Application of Principal Component Analysis on various Data Sets

• Project Aims 
This project aims to practice on usage of Principal Component Analsis (PCA) on various data sets. Various packages will be used in R for implementation (prcomp and pls1). Upon PCA, scatter plot will be used to visualize the effect of the principal components on the observations

• Data Sets used
*[Iris Data Set]
*[Wine Data Set]
*[Data on breast cancer diagnostics] (donated by Olvi Mangasarian, Nick Street, and William H. Wolberg)

[Iris Data Set]:<https://archive.ics.uci.edu/ml/machine-learning-databases/iris/>
[Wing Data Set]:<https://archive.ics.uci.edu/ml/datasets/Wine>
[Data on breast cancer diagnostics]:<http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)>

Source: [UC Irvine machine learning data repository]
[UC Irvine machine learning data repository]:<http://archive.ics.uci.edu/ml/>


• Methodology
For each dataset, I use a multinomial regression model with the lasso to predict the relevant classes. 
Besides, I used cross-validation to assess model accuracy and report the misclassification error rate. 

For each best model, the number of genes used is also recorded.
