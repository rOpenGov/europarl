# Loads data from configuration file

File should contains:

- dbname = "dbname"

- host = "host"

- username = "username"

- password = "password"

## Usage

``` r
read_config(
  file = system.file("config/db_config.txt", package = "europarl"),
  delim = " "
)
```

## Arguments

- delim:

  a delim parametr in read_delim

- name:

  file name or path to file

## Value

A tibble with dbname, host, username and password for database
conncetion.

## Examples

``` r
if (FALSE) { # \dontrun{
read_config()

read_config(file = "path/name.txt", delim = " ")
} # }
```
