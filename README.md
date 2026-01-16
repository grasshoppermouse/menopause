
# menopause

<!-- badges: start -->
[https://doi.org/10.5281/zenodo.18149332](https://doi.org/10.5281/zenodo.18149332)
<!-- badges: end -->

Download this repo, install the dependencies, and then run:

`targets::tar_make()`

This takes about 30 minutes on my iMac M3, and should create the `menopause_paper.html` file.

There are three github packages (not on CRAN) that need to be installed first:

```
# install.packages("pak")
pak::pak("grasshoppermouse/hgEnergyGrowth")
pak::pak("grasshoppermouse/hagenutils")
pak::pak("rmcelreath/cchunts")
```

