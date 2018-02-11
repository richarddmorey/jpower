# jpower

A module to compute power for various designs within [jamovi](https://www.jamovi.org)

![Screenshot of jpower](https://github.com/richarddmorey/jpower/raw/master/img/jpower1.png)

## How to install jpower in jamovi

1. Install the latest version of [jamovi](https://www.jamovi.org/download.html).

2. Install the `devtools` and `jmvtools` packages in R:

```
install.packages('devtools') # if you don't already have it
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
```

3. Make sure `jmvtools` can find jamovi from R:

```
jmvtools::check()
```

This should report a path where jamovi can be found. If it does not, see the [jamovi developer hub](https://dev.jamovi.org/tuts0101-getting-started.html) for help with `jmvtools`.

4. Download and install `jpower` in R:

```
## Create a temporary space for the package files
fname = tempfile(fileext = ".zip")
dname = dirname(fname)

## Download the jpower package  
utils::download.file(url = "https://github.com/richarddmorey/jpower/archive/master.zip",
  destfile = fname)
  
## Unzip jpower to temporary directory  
utils::unzip(fname, exdir = dname, overwrite = TRUE)

## Install jpower
jmvtools::install(pkg = paste0(dname, "/jpower-master/jpower"))

## Delete temporary files
unlink(c(fname, paste0(dname, "/jpower-master/")), recursive = TRUE)

```

5. Run jamovi and select an option from the jpower menu. Currently, only independent sample t tests are implemented.
