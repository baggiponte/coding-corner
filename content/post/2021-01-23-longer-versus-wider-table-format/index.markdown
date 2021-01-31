---
title: 'Longer Versus Wider Table Format'
author: 'Luca Baggi'
date: '2021-01-23'
slug: []
categories: []
tags: []
ShowToc: true
---

Today's goals is to compare readability with long versus wide format of data. Plus, we will fill a date column with missing values in a panel dataset. Let's load the packages we are going to use:




```r
library(tidyverse)

# set a theme to stay for the rest of the plotting
theme_set(theme_minimal())
```

# Load the Data

We will be using COVID-19 vaccination data, which are available at the following [repo](https://github.com/italia/covid19-opendata-vaccini). I also collected and wrangled the data [here](https://github.com/orizzontipolitici/covid19-vaccine-data) for [Orizzonti Politici](https://www.orizzontipolitici.it/).

We proceed by loading the dataset via its url, so that it's updated every time we run the script.


```r
url_doses_delivered_ita <-
  "https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/consegne-vaccini-latest.csv"

read_csv(url_doses_delivered_ita, col_types = cols(area = col_factor())) %>%
  rename(
    dosi_consegnate = numero_dosi,
  ) %>%
  relocate(data_consegna, .after = 'area') -> doses

doses %>% head(10)
```

```
## # A tibble: 10 x 4
##    area  data_consegna fornitore       dosi_consegnate
##    <fct> <date>        <chr>                     <dbl>
##  1 ABR   2020-12-27    Pfizer/BioNTech             135
##  2 ABR   2020-12-30    Pfizer/BioNTech            7800
##  3 ABR   2021-01-05    Pfizer/BioNTech            3900
##  4 ABR   2021-01-07    Pfizer/BioNTech            3900
##  5 ABR   2021-01-11    Pfizer/BioNTech            3900
##  6 ABR   2021-01-12    Pfizer/BioNTech            4875
##  7 ABR   2021-01-13    Moderna                    1300
##  8 ABR   2021-01-18    Pfizer/BioNTech            3510
##  9 ABR   2021-01-20    Pfizer/BioNTech            3510
## 10 ABR   2021-01-21    Pfizer/BioNTech            2340
```

Our data has the following columns: `area` indicates the region, `data` when deliveries happened, `fornitore` is the supplier, `dosi_consegnate` how many doses have been delivered.

Now we will perform the same operations on the dataset. Namely, we are going to:

* Create the implicit missing values;
* Create a cumulative sum of doses column for each supplier;
* Create a cumulative sum of doses;
* Choose the most informative format.

# Deal with implicit missing values

The quickest way (I know of) is to create a grid with the missing values and then join it with the data.


```r
doses %>%
  tidyr::expand(
    # only works if it's the first argument
    data_consegna = full_seq(data_consegna, 1),
    area, fornitore) -> doses_grid
```

And then perform the join. In this case, we do not have to specify the argument `on` and the same dimensions of the joint item and the joined one show the join was executed successfully! (What a tongue twist.)


```r
doses %>%
  full_join(doses_grid) %>%
  # reorder rows
  arrange(area, data_consegna) %>%
  # fill NAs
  mutate(dosi_consegnate = coalesce(dosi_consegnate, 0)) %>%
  arrange(area, data_consegna, fornitore) -> doses_long
```

Let's use what we have done so far to make a visualisation:


```r
doses_long %>%
  ggplot(aes(data_consegna, dosi_consegnate)) +
  geom_col(aes(fill = fornitore)) +
  labs(
    title = 'Total Vaccine Doses Delivered',
    x = NULL,
    y = NULL
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="2100" />

Moderna has so far made only one delivery!

# Work with Long Format

Data is already in the long format (normally, we would need to use `pivot_longer()` to switch from wide to long format).

We might create a column with the cumulative total number of doses available each day in each region and set up a `shiny` app to monitor closely how things are going in each region.

# Wide Format

Now, we have to turn `doses` into the wide format. First, we need to pivot the data: that's almost half of the following code and it's much easier than expected. Then we group the data by area and create a bunch of columns.


```r
doses_long %>%
  # pivot to the wider format
  pivot_wider(
    # create new features out of the levels of the following column
    names_from = fornitore,
    # values of the new features come from the following column
    values_from = dosi_consegnate
  ) %>%
  # for simplicity, let's rename
  rename(
    dosi_pfizer = 'Pfizer/BioNTech',
    dosi_moderna = 'Moderna'
  ) %>%
  group_by(area) %>%
  mutate(
    # replce NA with zero
    dosi_pfizer = coalesce(dosi_pfizer, 0),
    dosi_moderna = coalesce(dosi_moderna, 0),
    # create columns with totals
    totale_pfizer = cumsum(dosi_pfizer),
    totale_moderna = cumsum(dosi_moderna),
    totale_dosi = totale_moderna + totale_pfizer
  ) %>%
  ungroup() -> doses_wide

doses_wide %>% head(10)
```

```
## # A tibble: 10 x 7
##    area  data_consegna dosi_moderna dosi_pfizer totale_pfizer totale_moderna
##    <fct> <date>               <dbl>       <dbl>         <dbl>          <dbl>
##  1 ABR   2020-12-27               0         135           135              0
##  2 ABR   2020-12-28               0           0           135              0
##  3 ABR   2020-12-29               0           0           135              0
##  4 ABR   2020-12-30               0        7800          7935              0
##  5 ABR   2020-12-31               0           0          7935              0
##  6 ABR   2021-01-01               0           0          7935              0
##  7 ABR   2021-01-02               0           0          7935              0
##  8 ABR   2021-01-03               0           0          7935              0
##  9 ABR   2021-01-04               0           0          7935              0
## 10 ABR   2021-01-05               0        3900         11835              0
## # … with 1 more variable: totale_dosi <dbl>
```

# Long vs Wide Format

Each of the two has its own shortcomings and advantages. Let's see some visual examples:

## Cumulative Doses Available


```r
doses_wide %>%
  ggplot(aes(
    x = data_consegna,
    y = totale_dosi,
    fill = area)) + 
  geom_area() +
  labs(
    title = 'Total Doses Available in Italy',
    x = NULL,
    y = NULL,
    fill = 'Region'
  ) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="2100" />

This can't be swiftly achieved with a long format:


```r
doses_long %>%
  group_by(area, fornitore) %>%
  mutate(totale_dosi = cumsum(dosi_consegnate)) %>%
  ggplot(aes(data_consegna, totale_dosi, fill = area)) +
  geom_area()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="2100" />

```r
doses_long %>%
  group_by(area, fornitore) %>%
  mutate(totale_dosi = cumsum(dosi_consegnate)) %>%
  ggplot(aes(data_consegna, totale_dosi, fill = area)) +
  geom_col()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-2.png" width="2100" />

```r
doses_long %>%
  group_by(area, fornitore) %>%
  mutate(totale_dosi = cumsum(dosi_consegnate)) %>%
  ggplot(aes(data_consegna, totale_dosi, fill = fornitore)) +
  geom_col()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-3.png" width="2100" />

`geom_area()` fails and `geom_col()` is not as elegant. We can use it for faceting, though!


```r
doses_long %>%
  group_by(area, fornitore) %>%
  mutate(totale_dosi = cumsum(dosi_consegnate)) %>%
  ggplot(aes(data_consegna, totale_dosi, fill = fornitore)) +
  geom_area() +
  facet_wrap(~ area, nrow = 4) +
  theme(legend.position = 'bottom') +
  labs(
    title = 'Deliveries by Region',
    x = NULL,
    y = NULL,
    fill = NULL
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="2100" />

# Vaccine Deliveries Grouped by Area

Let's use `group_by(area)` to get cumulative data for each region.


```r
doses_wide %>%
  group_by(area) %>%
  summarise(
    totale_pfizer = sum(dosi_pfizer),
    totale_moderna = sum(dosi_moderna)
  ) %>%
  mutate(
    totale_dosi = totale_pfizer + totale_moderna
  ) %>%
  add_row(
    area = 'ITA',
    totale_pfizer = sum(.$totale_pfizer),
    totale_moderna = sum(.$totale_moderna),
    totale_dosi = sum(.$totale_dosi),
  )
```

```
## # A tibble: 22 x 4
##    area  totale_pfizer totale_moderna totale_dosi
##    <chr>         <dbl>          <dbl>       <dbl>
##  1 ABR           37380           1300       38680
##  2 BAS           20775              0       20775
##  3 CAL           62680              0       62680
##  4 CAM          165495           8200      173695
##  5 EMR          213525           7400      220925
##  6 FVG           60715              0       60715
##  7 LAZ          222670           7500      230170
##  8 LIG           77540              0       77540
##  9 LOM          379530              0      379530
## 10 MAR           43880           1500       45380
## # … with 12 more rows
```

The wide format does not offer much, but can be better integrated with other data to create new features. The long format allows to make quick plotting, but adding a row with totals for the country would be definitely more demaning.


```r
doses_long %>%
  group_by(area, fornitore) %>%
  summarise(
    totale_dosi = sum(dosi_consegnate)
  ) %>%
  ggplot(aes(area, totale_dosi, fill = fornitore)) +
  geom_col() +
  theme(legend.position = 'bottom')
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="2100" />

# Vaccine Deliveries Grouped by Date

We will `group_by(data)` to obtain how many doses have been delivered in the whole of Italy every day by each supplier:


```r
doses_wide %>%
  group_by(data_consegna) %>%
  summarise(
    totale_pfizer = sum(dosi_pfizer),
    totale_moderna = sum(dosi_moderna),
  ) %>%
  mutate(
    totale_dosi_consegnate = totale_pfizer + totale_moderna,
    totale_dosi = cumsum(totale_dosi_consegnate)
  ) %>%
  ggplot(aes(data_consegna, totale_dosi)) +
  geom_area(fill = 'cadetblue')
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="2100" />

This does not really tell much more. But, once again, it can be enriched with other data and have multiple objects (rather than layers/dimensions).
