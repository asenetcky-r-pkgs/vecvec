
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vecvec

<!-- badges: start -->

[![R-CMD-check](https://github.com/asenetcky-r-pkgs/vecvec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asenetcky-r-pkgs/vecvec/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of vecvec is to match a “primary” character vector against
another character vector “lookup”.

## Installation

`vecvec` is still in active development. Expect breaking changes.

You can install the development version of vecvec from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("asenetcky-r-pkgs/vecvec", ask = FALSE) |> suppressMessages()
```

## Usage

``` r
library(vecvec)
library(purrr)

example <-
  dplyr::lst(
    primary = create_random_names(),
    lookup = create_random_names()
  )

example |>
  purrr::map(head)
#> $primary
#> [1] "christopher"   "ELIJAH MILLER" "walker"        "MARY"         
#> [5] "Perez"         "MARTIN"       
#> 
#> $lookup
#> [1] "JAMES"          "Linda Anderson" "ELIJAH"         "thomas"        
#> [5] "Taylor"         "emma anderson"


my_vecvec <- vecvec(example$primary, example$lookup)

my_vecvec |>
  purrr::walk(dplyr::glimpse)
#> List of 2
#>  $ primary:List of 2
#>   ..$ data_index  : tibble [100 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ string_index: tibble [81 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ lookup :List of 2
#>   ..$ data_index  : tibble [100 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ string_index: tibble [79 × 2] (S3: tbl_df/tbl/data.frame)
#> List of 4
#>  $ exact_whole_string  : tibble [39 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:39] 3 4 5 6 7 11 12 17 17 17 ...
#>   ..$ lookup_id       : int [1:39] 75 89 25 45 44 42 35 24 80 96 ...
#>  $ tidy_exact_tokens   : tibble [95 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:95] 2 2 2 3 4 5 6 7 8 8 ...
#>   ..$ lookup_id       : int [1:95] 3 61 88 75 89 25 45 44 54 70 ...
#>  $ tidy_exact_substring: tibble [103 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:103] 2 2 2 3 4 5 6 7 8 8 ...
#>   ..$ lookup_id       : int [1:103] 3 61 88 75 89 25 45 44 54 70 ...
#>  $ tidy_fuzzies        : tibble [72 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:72] 2 2 3 4 5 6 7 8 8 9 ...
#>   ..$ lookup_id       : int [1:72] 3 88 75 89 25 45 44 54 36 10 ...
```

``` r
human_readable(my_vecvec)
#> # A tibble: 118 × 4
#>    original_row_id primary       lookup_id lookup_value
#>              <int> <chr>             <int> <chr>       
#>  1               2 ELIJAH MILLER         3 ELIJAH      
#>  2               2 ELIJAH MILLER        88 Miller      
#>  3               2 ELIJAH MILLER        61 ELIJAH      
#>  4               3 walker               75 WALKER      
#>  5               4 MARY                 89 Mary        
#>  6               5 Perez                25 Perez       
#>  7               6 MARTIN               45 Martin      
#>  8               7 MIA                  44 MIA         
#>  9               8 Mateo Jones          54 Mateo       
#> 10               8 Mateo Jones          36 Jones       
#> # ℹ 108 more rows
```
