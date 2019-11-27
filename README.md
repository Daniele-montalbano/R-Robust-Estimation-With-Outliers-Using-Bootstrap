# R-Robust-Estimation-With-Outliers-Using-Bootstrap
Robust statistics analysis aims to estimates statistics which are not conditioned by outliers, observations that are well separated from the majority of data or in some way diverted from the general trend, and are sensitive to little deviations from parametric distributions assumed for the observed dataset.

In this repository, using the statistical software R, are been analyzed robust techniques to estimate multivariate linear regression, using the Bootstrap, a simulation method where the construction of sample distribution of given statistics occurring through resampling the same observed sample.

In order to solve numerous problems concerning the approach, such as numerical instability and computational cost, Salibian-Barrera and Zamar (2002) developed the Fast and Robust Bootstrap (FRB) methodology, which applies robust estimators as S- and MM- on the bootstrap sample space; being computationally very expensive, the method was implemented by Van Aelst e Willems (2013) in the R-library FRB, where it is possible to face the multivariate linear regression problem, fitting a model and moreover estimating the regression coefficients.

The considered methods are been applied in two different cases:
1)  on suitably generated data to capture the characteristics and differences in two controlled situations:
    - FRB-GeneratedData-Case1: in Case 1 of the generated data, the regression coefficient, equal to 1.5 and statistically significant, due to the effect of 3 outliers has been distorted (estimated coefficient=0.7) while remaining statistically significant;
    - FRB-GeneratedData-Case2: in Case 2 of the generated data, the regression coefficient, equal to 0.3 and statistically non-significant, becomes statistically significant, equal to 0.834, distorted due to the effect of 3 outliers;
2)  FRB-ShortleafPineDataset: on a real “Shortleaf Pine” dataset from Atkinson e Riani (Robust Diagnostic Regression Analysis, 2000, appendix A.10). In this dataset the response variable (y) is the volume (expressed in cubic foot) of 70 Pinus Echinata trees, and the two independent variables are the trunk circumference (x1), measured in inches, and the height of each tree (x2), in feet. The aim of the analysis is "to find a formula to predict the volume (of usable wood) from the other two measures".

-------------------------------------------------------------------------------------------------------
Package 'FRB' was removed from the CRAN repository, but you can obtain formerly available versions from the archive:
https://cran.r-project.org/src/contrib/Archive/FRB/

After have stored locally the file tar.gz you can install it in R simply executing the following steps:
1)  Open R software
2)  Click "Packages" in the top menu then click "Install package(s) from local files"
3)  You will know when the package has been downloaded onto your computer when another greater-than symbol (“>”) appears
4)  Run command "library(FRB)"

Moreover, before run the r-scripts, you have also to install the package "boot", which is instead present in the CRAN repository:
"install.packages("boot")"
