# Safe html scrapping

Function `safe_html` tries to download the URL several times.

## Usage

``` r
safe_html(page, time = 60, attempts = 10)
```

## Arguments

- page:

  requested URL

- time:

  sleep interval after each failure

- attempts:

  max number of tries (if there is a problem with connection)

## Value

character vector

## Details

Function `safe_html` performes 10 (by default) attempts to download the
URL and waits 60sec (by default) after each failure

## Author

Przemyslaw Biecek

## Examples

``` r
if (FALSE) { # \dontrun{
page <- paste0('http://www.sejm.gov.pl/Sejm7.nsf/',
               'wypowiedz.xsp?posiedzenie=15&dzien=1&wyp=008')
safe_html(page)} # }
```
