![Travis build status](https://travis-ci.com/imbi-heidelberg/DescrTab2.svg?branch=master)
[![codecov](https://codecov.io/gh/imbi-heidelberg/DescrTab2/branch/master/graph/badge.svg)](https://codecov.io/gh/imbi-heidelberg/DescrTab2)
[![CRAN Version](https://www.r-pkg.org/badges/version/DescrTab2)](https://www.r-pkg.org/badges/version/DescrTab2)
[![Downloads CRAN](https://cranlogs.r-pkg.org/badges/grand-total/DescrTab2)](https://cranlogs.r-pkg.org/badges/grand-total/DescrTab2)
[![License](https://img.shields.io/cran/l/DescrTab2)](https://img.shields.io/cran/l/DescrTab2)


# DescrTab2
## Publication quality descriptive statistics tables with R

Provides functions to create descriptive statistics tables for continuous and categorical variables.
By default, summary statistics such as mean, standard deviation, quantiles, minimum and maximum for continuous variables and relative and absolute frequencies for categorical variables are calculated. DescrTab2 features a sophisticated algorithm to choose appropriate test statistics for your data and provides p-values. On top of this, confidence intervals for group differences of appropriated summary measures are automatically produces for two-group comparison.


Tables generated by DescrTab2 can be integrated in a variety of document formats, including .html, .tex and .docx documents. DescrTab2 also allows printing tables to console and saving table objects for later use.

You can install the stable version of DescrTab2 from cran by typing

```
install.packages("DescrTab2")
```
into your R console.

You can also install the development version of DescrTab2 from github by typing:
```
devtools::install_github("https://github.com/imbi-heidelberg/DescrTab2")
```


For usage instructions and detailed documentation, check out our documentation page:

https://imbi-heidelberg.github.io/DescrTab2/

Specifically this article may be useful to get aquainted with DescTab2:

https://imbi-heidelberg.github.io/DescrTab2/articles/usage_guide.html
