
# menopause

<!-- badges: start -->
[https://doi.org/10.5281/zenodo.18149332](https://doi.org/10.5281/zenodo.18149332)
<!-- badges: end -->

Download this repo, and install the dependencies:

```
# install.packages("pak")
pak::local_install()
```

Then run:

`targets::tar_make()`

This takes about 90 minutes on my iMac M3, and should create the `menopause_paper.html` file. 
There is a multiprocessing setting in the `_targets.R` file (currently commented out) that could
speed things up.
