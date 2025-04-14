
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vecvec

<!-- badges: start -->
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
#> [1] "WILLIAM"         "sarah"           "barbara wilson"  "ROBERT GONZALEZ"
#> [5] "allen"           "linda"          
#> 
#> $lookup
#> [1] "Luna Jones"     "Isabella Brown" "JOSEPH"         "scott"         
#> [5] "ramirez"        "clark"


my_vecvec <- vecvec(example$primary, example$lookup)

my_vecvec |>
  purrr::walk(dplyr::glimpse)
#> List of 2
#>  $ primary:List of 2
#>   ..$ data_index  : tibble [100 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ string_index: tibble [86 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ lookup :List of 2
#>   ..$ data_index  : tibble [100 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ string_index: tibble [75 × 2] (S3: tbl_df/tbl/data.frame)
#> List of 4
#>  $ exact_whole_string  : tibble [43 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:43] 2 5 5 6 9 12 19 21 21 21 ...
#>   ..$ lookup_id       : int [1:43] 95 41 98 18 68 23 79 28 38 96 ...
#>  $ tidy_exact_tokens   : tibble [131 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:131] 2 4 4 4 5 5 6 8 9 10 ...
#>   ..$ lookup_id       : int [1:131] 95 85 93 14 41 98 18 3 68 31 ...
#>  $ tidy_exact_substring: tibble [139 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:139] 1 2 4 4 4 5 5 6 8 9 ...
#>   ..$ lookup_id       : int [1:139] 92 95 85 93 14 41 98 18 3 68 ...
#>  $ tidy_fuzzies        : tibble [87 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ unique_string_id: int [1:87] 1 2 4 4 5 6 8 9 10 10 ...
#>   ..$ lookup_id       : int [1:87] 22 95 85 14 41 18 3 68 31 68 ...
```

``` r
human_readable(my_vecvec)
#> # A tibble: 159 × 4
#>    original_row_id primary         lookup_id lookup_value
#>              <int> <chr>               <int> <chr>       
#>  1               1 WILLIAM                92 Liam        
#>  2               1 WILLIAM                22 WILLIAMS    
#>  3               2 sarah                  95 sarah       
#>  4               4 ROBERT GONZALEZ        85 ROBERT      
#>  5               4 ROBERT GONZALEZ        14 GONZALEZ    
#>  6               4 ROBERT GONZALEZ        93 robert      
#>  7               5 allen                  41 allen       
#>  8               5 allen                  98 allen       
#>  9               6 linda                  18 LINDA       
#> 10               8 Joseph Moore            3 JOSEPH      
#> # ℹ 149 more rows
```
