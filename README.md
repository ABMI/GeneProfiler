# G-CDM
OMOP-CDM Extension for genomic data

![ERD](Image/ERD.png)

# How to run

1. Install genomic package.

Using devtools, install genomic package in Rstudio.

```
install.packages("devtools")
library(devtools)
install_github("https://github.com/ABMI/G-CDM.git")
```

(If you want to install other branch, add parameter "package_test")

```
install_github("https://github.com/ABMI/G-CDM.git",ref = "package_test")
```

Username issue

```
install_github("https://github.com/ABMI/G-CDM.git",ref = "package_test", username = "ABMI"G-CDM")
```

2. Run genomic()

```
library(genomic)
genomic()
```
