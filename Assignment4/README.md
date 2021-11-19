Assignment 4- HPC & SQL
================
Sylvia Baeyens
due 11/19/2021

# HPC

## Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  ans = rowSums(mat)
  ans
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
 ans= t(apply(mat, 1, cumsum))
 ans
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Warning in microbenchmark::microbenchmark(fun1(dat), fun1alt(dat), unit
    ## = "relative", : less accurate nanosecond times to avoid potential integer
    ## overflows

    ## Unit: relative
    ##          expr   min       lq     mean   median       uq       max neval
    ##     fun1(dat) 30.72 29.57249 10.51067 27.88621 26.63175 0.3374159   100
    ##  fun1alt(dat)  1.00  1.00000  1.00000  1.00000  1.00000 1.0000000   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq       max neval
    ##     fun2(dat) 4.010666 3.546442 2.389378 3.476236 3.353129 0.4643742   100
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.0000000   100

In both cases, our alternative code, which contaisn no for loops, runs
much faster.

## Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   0.833   0.177   1.028

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
library(parallel)

system.time({
  cl <- makePSOCKcluster(1L)
  clusterSetRNGStream(cl, 1231)
  clusterExport(cl, c("sim_pi"), envir = environment())
  ans = unlist(parLapply(cl = cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  stopCluster(cl)
  ans
})
```

    ## [1] 3.141342

    ##    user  system elapsed 
    ##   0.002   0.002   1.244

Parallizing the code makes it run much faster.

# SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

## Question 1

How many many movies is there avaliable in each rating catagory.

``` sql
SELECT rating,
  COUNT (*) as Number_Movies
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | Number\_Movies |
|:-------|---------------:|
| G      |            180 |
| NC-17  |            210 |
| PG     |            194 |
| PG-13  |            223 |
| R      |            195 |

5 records

</div>

## Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
SELECT rating,
  AVG(replacement_cost) as AvgReplacementCost, 
  AVG(rental_rate) as AvgRentalRate
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | AvgReplacementCost | AvgRentalRate |
|:-------|-------------------:|--------------:|
| G      |           20.12333 |      2.912222 |
| NC-17  |           20.13762 |      2.970952 |
| PG     |           18.95907 |      3.051856 |
| PG-13  |           20.40256 |      3.034843 |
| R      |           20.23103 |      2.938718 |

5 records

</div>

## Question 3

Use table film\_category together with film to find the how many films
there are with each category ID

``` sql
SELECT category_id,
  COUNT (*) as Number_Films
FROM film_category
GROUP BY category_id
```

<div class="knitsql-table">

| category\_id | Number\_Films |
|:-------------|--------------:|
| 1            |            64 |
| 2            |            66 |
| 3            |            60 |
| 4            |            57 |
| 5            |            58 |
| 6            |            68 |
| 7            |            62 |
| 8            |            69 |
| 9            |            73 |
| 10           |            61 |

Displaying records 1 - 10

</div>

## Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT *
  FROM category as a
  INNER JOIN 
    (SELECT category_id, COUNT (*) as Number_Films
      FROM film_category GROUP BY category_id) as b
  ON a.category_id = b.category_id
  ORDER BY Number_Films DESC
  LIMIT 1
```

<div class="knitsql-table">

| category\_id | name   | last\_update        | category\_id | Number\_Films |
|-------------:|:-------|:--------------------|-------------:|--------------:|
|           15 | Sports | 2006-02-15 09:46:27 |           15 |            74 |

1 records

</div>

The most popular movie category is the sports category.

## Clean up

``` r
# clean up
dbDisconnect(con)
```
