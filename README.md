# GeneProfiler
Application running on [Genomic CDM (G-CDM)](https://github.com/ABMI/GeneProfiler/wiki) for exploring and analysis clinical sequencing data

# Explore Your Data
![ERD](Image/exploringData.png)

# Run Distributed Research
![ERD](Image/runDistributeResearch.png)

# How to run

1. Install genomic package.

Using devtools, install genomic package in Rstudio.

```
install.packages("devtools")
devtools::install_github("https://github.com/ABMI/G-CDM.git")
```

(If you want to install other branch, add parameter "package_test")

```
install_github("https://github.com/ABMI/G-CDM.git", ref = "package_test")
```

Username issue

```
install_github("https://github.com/ABMI/G-CDM.git", ref = "package_test", username = "ABMI"G-CDM")
```

2. Run genomic()

```
genomic::genomic()
```
