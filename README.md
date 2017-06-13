
# configstore

> Persistent Package Configuration

[![Linux Build Status](https://travis-ci.org/gaborcsardi/configstore.svg?branch=master)](https://travis-ci.org/gaborcsardi/configstore)

[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/configstore?svg=true)](https://ci.appveyor.com/project/gaborcsardi/configstore)
[![](http://www.r-pkg.org/badges/version/configstore)](http://www.r-pkg.org/pkg/configstore)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/configstore)](http://www.r-pkg.org/pkg/configstore)


Store the configuration of your package in the user's platform dependent
  config file directory. The configuration persists across R sessions, and can
  also be edited manually. Configuration files are YAML files.

## Installation

```r
devtools::install_github("gaborcsardi/configstore")
```

## Usage

```r
library(configstore)
```

## License

MIT + file LICENSE © 
