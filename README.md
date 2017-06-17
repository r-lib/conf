
# conf

> Persistent Package Configuration

[![Linux Build Status](https://travis-ci.org/gaborcsardi/conf.svg?branch=master)](https://travis-ci.org/gaborcsardi/conf)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/conf?svg=true)](https://ci.appveyor.com/project/gaborcsardi/conf)
[![](http://www.r-pkg.org/badges/version/conf)](http://www.r-pkg.org/pkg/conf)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/conf)](http://www.r-pkg.org/pkg/conf)

Store the configuration of your package in the user's platform dependent
config file directory. The configuration persists across R sessions, and can
also be edited manually. Configuration files are YAML files.

## Installation

```r
devtools::install_github("gaborcsardi/conf")
```

## Usage

```r
library(conf)
```

## License

MIT © RStudio Inc
