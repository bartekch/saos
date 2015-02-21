[![Build Status](https://travis-ci.org/bartekch/saos.svg)](https://travis-ci.org/bartekch/saos)

## R interface to SAOS

[SAOS](https://saos-test.icm.edu.pl/) is a repository of judgments from Polish courts of all kinds - common courts (district, regional and appellate), administrative courts, the Supreme Court of Poland, the Constitutional Tribunal and National Appeal Chamber. The [saos](http://github.com/bartekch/saos) package is an R interface to [SAOS API](https://saos-test.icm.edu.pl/api). The API does not require an API key.

Keep in mind that [SAOS](https://saos-test.icm.edu.pl/) and consequently a [saos](http://github.com/bartekch/saos) package are still under development. More functionalities will be added in the future, but some may be deprecated. 


### Installation

Right now you may install the package only from [GitHub](https://github.com/bartekch/saos) with `devtools::install_github()`:
  
```r
install.packages("devtools")
library(devtools)
install_github("bartekch/saos")
```

### Basic usage

The core function is `search_judgments()`, which enables to search repository (through API) for judgments matching given query. Query handles a variety of parameters available in API. Following example will search for the first 100 judgments (starting from the latest) with any reference to words "dobra" and "osobiste", passed by common court:

```r
library(saos)
judgments <- search_judgments(all = "dobra osobiste", 
                              courtType = "COMMON",
                              sortingField = "JUDGMENT_DATE",
                              sortingDirection = "DESC",
                              limit = 100)
class(judgments)
length(judgments)
str(judgments[[1]])
```

Search results do not include all available information about judgments. If you want to get all the details you need to use `get_judgments()` function:

```r
judgments_details <- get_judgments(judgments)
```

Afterwards you could extract specific information with `extract()`:

```r
judges <- extract(judgments_details, "judges")
str(judges)
type <- extract(judgments_details, "judgmentType")
str(type)
date <- extract(judgments_details, "judgmentDate")
str(date)
```


### Tutorial

To view the more complex tutorial see vignette:

```r
vignette("saos", "saos")
```


