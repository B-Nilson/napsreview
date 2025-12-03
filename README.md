
<!-- README.md is generated from README.Rmd. Please edit that file -->

# napsreview

<!-- badges: start -->

<!-- badges: end -->

`napsreview` is an R package with the goal of assessing the validity of
the National Air Pollution Surveillance (NAPS) Program archive of
Canadian quality-assured continuous air quality observations.

You can read more about the NAPS Program
[here](https://open.canada.ca/data/en/dataset/1b36a356-defd-4813-acea-47bc3abd859b).

There are two key parts to `napsreview`:

1.  Creation of a local database of NAPS (and comparison) data (see
    [Build database](#build-database))
2.  Validity assessment of NAPS data (see [Assess
    validity](#assess-validity))

## Installation

Using R, you can install the development version of napsreview from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("B-Nilson/napsreview")
```

# Build database

The NAPS dataset is split into annual files by pollutant. Below we
download each of those files and dump them into a `duckdb` database, as
well as format and combine them into a single dataset.

We also want to compare with another source for the same dataset to
check timestamp alignment, so we download and archive data from the BC
Government for the same time period as the NAPS data collected.

``` r
library(napsreview)
desired_years <- 1974:2023
desired_pollutants <- c("PM25", "O3", "NO2")

# Define local paths (change as desired)
raw_data_dir <- "./naps_raw"
db_path <- "./naps.duckdb"

# Create directories if needed
if (!dir.exists(raw_data_dir)) {
  dir.create(raw_data_dir, recursive = TRUE)
}
if (!dir.exists(dirname(db_path))) {
  dir.create(dirname(db_path), recursive = TRUE)
}

# Create (if needed) and connect to local database
db <- connect_to_database(db_path)
on.exit(DBI::dbDisconnect(db))

# Collect data for desired years/pollutants
# *takes ~10 minutes to run on my machine*
naps_data <- desired_years |>
  get_naps_data(
    pollutants = desired_pollutants,
    raw_data_dir = raw_data_dir,
    check_if_raw_exists = TRUE # don't download if already exists locally
  )

# Write raw data to database if needed
if (!DBI::dbExistsTable(db, "raw_data")) {
  db |>
    archive_raw_naps_data(
      naps_data = naps_data,
      raw_data_tbl = "raw_data",
      raw_headers_tbl = "raw_data_headers"
    )
}

# Format data and write to database if needed
# *takes ~60 minutes to run on my machine*
if (!DBI::dbExistsTable(db, "fmt_data")) {
  db |>
    archive_fmt_naps_data(
      naps_data = naps_data,
      fmt_data_tbl = "fmt_data",
      fmt_meta_tbl = "fmt_meta"
    )
}

# Get and archive bcgov data if needed
# *takes ~30-60 minutes to run on my machine*
if (!DBI::dbExistsTable(db, "bcgov_data")) {
  date_range <- db |>
    dplyr::tbl("raw_data") |>
    dplyr::select(date = `Date//Date`) |>
    dplyr::summarise(
      min_date = min(date, na.rm = TRUE),
      max_date = max(date, na.rm = TRUE)
    ) |>
    dplyr::collect()
  date_range <- c(date_range$min_date, date_range$max_date)
  db |>
    get_and_archive_bcgov_data(date_range = date_range)
}
```

And you can check that worked and view the data using the following:

``` r
library(napsreview)

# Connect to database (change path here if you did earlier as well)
db_path <- "./naps.duckdb"
db <- connect_to_database(db_path)
on.exit(DBI::dbDisconnect(db))

# Print the first 10 rows of each table
#   Note - these are lazy tables.
#   You need to pass to dplyr::collect() if
#   you want to query and load the data from the database
db |> dplyr::tbl("raw_data")
db |> dplyr::tbl("raw_data_headers")
db |> dplyr::tbl("fmt_data")
db |> dplyr::tbl("fmt_meta")
db |> dplyr::tbl("bcgov_data")
db |> dplyr::tbl("bcgov_meta")
db |> dplyr::tbl("bcgov_aligned_data")
```

# Assess Validity

Several relatively minor issues have been found in the NAPS dataset:

- A major format change occurred around 2004 (see [Data Format
  Differences](#data-format-differences)).
- Site meta data values are inconsistent between files (see
  [Inconsistent Metadata](#inconsistent-metadata)).
- Some coordinate and concentration values are unrealistic (see
  [Unrealistic Values](#unrealistic-values)).

However, most noteably an inconsistent date mis-aligment was discovered
for several sites/years (see [Date Misalignment](#date-misalignment)).
This is a serious issue that needs to be addressed and will impact
downstream analysis that rely on hourly data.

## Data Format Differences

The NAPS data format changed after 2004 to include French and English
headers and several other format changes. Strangley, the 2002 files for
NO2 and O3 use the new format, unlike the rest of the files for those
pollutants prior to 2005.

Noteably:

- new files are encoded in UTF-8, old files are latin1
- new files have FR and EN headers (i.e. ‘City//Ville’ instead of
  ‘City’)
- new files have slightly different pre-data header format
- new files have a different date format (i.e. “2025-01-01” instead of
  “20040101”)

``` r
library(napsreview)

# Connect to database (change path here if you did earlier as well)
db_path <- "./naps.duckdb"
db <- connect_to_database(db_path)
on.exit(DBI::dbDisconnect(db))

# View encoding differences between versions (change path here if you did earlier as well)
raw_data_dir <- "./naps_raw"
raw_data_dir |>
  file.path("PM25_2002.csv") |> # v1 file
  readLines(encoding = "UTF-8") |> # actually latin1
  stringr::str_subset("\xb5") |> # so non-standard characters are broken
  head()
raw_data_dir |>
  file.path("PM25_2006.csv") |> # same file
  readLines(encoding = "latin1") |> # now we try latin1
  stringr::str_subset("\xb5") |> # so nothing broken
  head()
raw_data_dir |>
  file.path("PM25_2006.csv") |> # v2 file
  readLines(encoding = "UTF-8") |> # UTF-8 as expected
  stringr::str_subset("\xb5") |> # so nothing broken
  head()

# View file differences between versions
# Note: name and row_number are added by `napsreview` to ensure uniqueness in the db
# Note: 'Method' / 'Method Code//Code Méthode' are included for non-PM25 pollutants for consistency, even though they are not found in the raw files
db |> dplyr::tbl("raw_data_v1")
db |> dplyr::tbl("raw_data_v2")

# View pre-data header differences between versions
db |> dplyr::tbl("raw_data_headers_v1")
db |> dplyr::tbl("raw_data_headers_v2")

# View date format differences between versions
db |>
  dplyr::tbl("raw_data_v1") |>
  head(n = 1) |>
  dplyr::pull("Date") |>
  as.character()
db |>
  dplyr::tbl("raw_data_v2") |>
  head(n = 1) |>
  dplyr::pull("Date//Date") |>
  as.character()
```

These files have been identified as having the old format - see
[here](inst/extdata/issues/old_format_files.csv) for the full list:

    #> NO2: 1974-2001, 2003 and 2004
    #> O3: 1974-2001, 2003 and 2004
    #> PM25: 1995-2004

## Inconsistent Metadata

Every entry in the raw NAPS data has latitude, longitude,
province/territory, and city information. However, for many sites these
values are inconsistent between files.

For example, lat/lng values differ between files causing slight/major
shifts in location - see
[here](inst/extdata/issues/multiple_loc_sites.csv) for the full list.

    #> # A tibble: 308 × 4
    #>    site_id lat       lng        files                                           
    #>      <int> <chr>     <chr>      <chr>                                           
    #>  1   10102 47.56038  -52.7115   NO2_1990, NO2_1991, NO2_1992, NO2_1993, NO2_199…
    #>  2   10102 47.56038  -52.71147  NO2_2002, NO2_2005, NO2_2006, NO2_2008, NO2_200…
    #>  3   10301 48.949479 -57.945387 NO2_2001, NO2_2003, NO2_2004, O3_2001, O3_2003,…
    #>  4   10301 48.94948  -57.94539  NO2_2002, NO2_2005, NO2_2006, NO2_2007, NO2_200…
    #>  5   10701 51.02199  -57.09557  O3_2005, O3_2006, O3_2007, O3_2008, O3_2009     
    #>  6   10701 51.021995 -57.095567 O3_2004                                         
    #>  7   30118 44.64642  -63.57338  NO2_2002, O3_2002, O3_2014, O3_2015, O3_2016    
    #>  8   30118 44.646425 -63.573383 NO2_1990, NO2_1991, NO2_1992, NO2_1993, NO2_199…
    #>  9   30118 44.64643  -63.57338  NO2_2005, NO2_2006, NO2_2007, NO2_2008, NO2_201…
    #> 10   30120 44.719799 -63.480754 PM25_2001, PM25_2002, PM25_2003, PM25_2004      
    #> # ℹ 298 more rows

Sometimes this coordinate shift is due to a change in precision (number
of decimals) kept for the latitude/longitude. 5 decimals is the most
common (and the expected level of precision), but a range of decimals
exist. Here is a sample, see
[here](inst/extdata/issues/wrong_precision_coord_sites.csv) for the full
list.

    #> # A tibble: 124 × 6
    #>    site_id lat_precision lng_precision lat       lng        files               
    #>      <int>         <int>         <int> <chr>     <chr>      <chr>               
    #>  1   10301             6             6 48.949479 -57.945387 NO2_2001, NO2_2003,…
    #>  2   10701             6             6 51.021995 -57.095567 O3_2004             
    #>  3   11001             4             4 52.9518   -66.9156   NO2_2014, NO2_2015,…
    #>  4   30118             6             6 44.646425 -63.573383 NO2_1990, NO2_1991,…
    #>  5   30120             6             6 44.719799 -63.480754 PM25_2001, PM25_200…
    #>  6   30501             6             6 44.433611 -65.205833 O3_1985, O3_1986, O…
    #>  7   40103             6             6 45.957318 -66.646719 NO2_1999, NO2_2000,…
    #>  8   40203             6             6 45.308762 -66.008384 NO2_1987, NO2_1988,…
    #>  9   40207             6             6 45.252898 -66.080065 O3_1997, O3_1998, O…
    #> 10   40302             6             6 46.101578 -64.789661 NO2_1998, NO2_1999,…
    #> # ℹ 114 more rows

and here is a sample of all sites with more than 1 level of precision
for their lat/lng values, see
[here](inst/extdata/issues/multiple_precision_coord_sites.csv) for the
full list.

    #> # A tibble: 252 × 7
    #>    site_id     n lat_precision lng_precision lat       lng        files         
    #>      <int> <int>         <int>         <int> <chr>     <chr>      <chr>         
    #>  1   10102     2             5             4 47.56038  -52.7115   NO2_1990, NO2…
    #>  2   10102     2             5             5 47.56038  -52.71147  NO2_2002, NO2…
    #>  3   10301     2             5             5 48.94948  -57.94539  NO2_2002, NO2…
    #>  4   10301     2             6             6 48.949479 -57.945387 NO2_2001, NO2…
    #>  5   10701     2             5             5 51.02199  -57.09557  O3_2005, O3_2…
    #>  6   10701     2             6             6 51.021995 -57.095567 O3_2004       
    #>  7   30118     2             5             5 44.64642  -63.57338  NO2_2002, O3_…
    #>  8   30118     2             5             5 44.64643  -63.57338  NO2_2005, NO2…
    #>  9   30118     2             6             6 44.646425 -63.573383 NO2_1990, NO2…
    #> 10   30120     2             4             5 44.7198   -63.48076  NO2_2006, NO2…
    #> # ℹ 242 more rows

In addition, city names are inconsistently spelled between files - see
[here](inst/extdata/issues/multiple_city_spellings.csv) for the full
list

    #> # A tibble: 27 × 2
    #>    prov_terr cities                             
    #>    <chr>     <chr>                              
    #>  1 AB        Bitumont | Bitumount               
    #>  2 AB        Fort Mackay | Fort Mckay           
    #>  3 BC        Abbotsford | Metro Van - Abbotsford
    #>  4 BC        Burnaby | Metro Van - Burnaby      
    #>  5 BC        Chilliwack | Metro Van-Chilliwack  
    #>  6 BC        Coquitlam | Metro Van - Coquitlam  
    #>  7 BC        Delta | Metro Van - Delta          
    #>  8 BC        Fort St John | Fort St. John       
    #>  9 BC        Hope | Metro Van-Hope              
    #> 10 BC        Langley | Metro Van-Langley        
    #> # ℹ 17 more rows

and some sites have multiple city names across files - see
[here](inst/extdata/issues/multiple_city_sites.csv) for the full list.

    #> # A tibble: 48 × 3
    #>    site_id n_cities cities                                                  
    #>      <int>    <int> <chr>                                                   
    #>  1   53501        3 Saint-François | St-Francois | St-François-Île-D'orléans
    #>  2   10102        2 St Johns | St. John's                                   
    #>  3   10501        2 Grand Falls - Windsor | Grand Falls-Windsor             
    #>  4   30120        2 Dartmouth | Halifax                                     
    #>  5   40401        2 Fundy Nat. Park | Fundy National Park                   
    #>  6   40601        2 Blissville - Sunbury County | Central Blissville        
    #>  7   40701        2 Norton | Norton - Kings County                          
    #>  8   40901        2 Saint Andrews | St. Andrews                             
    #>  9   41201        2 Lower Newscastle | Miramichi                            
    #> 10   50308        2 Quebec | Québec                                         
    #> # ℹ 38 more rows

## Invalid Metadata

Throughout the files, the Yukon territory is marked with the old
abbreviation “YU”, the official abbreviation is now “YT” and should be
used. (see
[here](https://www.noslangues-ourlanguages.gc.ca/en/writing-tips-plus/abbreviations-canadian-provinces-and-territories)
for the source of the official names). See
[here](inst/extdata/issues/non_official_provinces.csv) for the full list
of issues.

    #> # A tibble: 1 × 2
    #>   original official
    #>   <chr>    <chr>   
    #> 1 YU       YT

For some sites/files, the coordinates are outside of the
province/territory they are marked to be in. For some it appears to be a
typo or rounding issue, but for a few it seems the deicimal was not
included in the longitude, producing an extremely negative value. See
[here](inst/extdata/issues/out_of_province_coords.csv) for the full list
of issues.

    #> # A tibble: 5 × 7
    #>   site_id lat       lng         prov_terr expected_prov_terr pollutants    years
    #>     <int> <chr>     <chr>       <chr>     <chr>              <chr>         <chr>
    #> 1  101701 52.981686 -112.493227 BC        AB                 NO2, O3, PM25 2000…
    #> 2  129401 82.45083  -62.5056    NT        NU                 O3            2008…
    #> 3  105604 49        -119.4625   BC        <NA>               NO2, O3, PM25 2004 
    #> 4   64301 42.88472  -8148056    ON        <NA>               O3            1988…
    #> 5   80801 50.34861  -10498333   SK        <NA>               NO2, O3       1992…

## Unrealistic Values

Some values in the raw data are unrealistically high or low.

Here is a sample of files/sites with negative concentrations - see
[here](inst/extdata/issues/invalid_values.csv) for the full list:

    #> # A tibble: 131 × 2
    #>    file_name site_ids                                                           
    #>    <chr>     <chr>                                                              
    #>  1 NO2_1974  030115, 030116, 050102, 050104, 050304, 060204, 060401, 060410, 06…
    #>  2 NO2_1975  030115, 030116, 050102, 050104, 050110, 050111, 050112, 050304, 06…
    #>  3 NO2_1976  030115, 030116, 050102, 050104, 050110, 050111, 050112, 050113, 05…
    #>  4 NO2_1977  030115, 030116, 050102, 050104, 050109, 050110, 050111, 050112, 05…
    #>  5 NO2_1978  030115, 030116, 050102, 050104, 050109, 050110, 050111, 050112, 05…
    #>  6 NO2_1979  030115, 030116, 050102, 050104, 050109, 050110, 050111, 050112, 05…
    #>  7 NO2_1980  030115, 030116, 040202, 050102, 050104, 050109, 050110, 050111, 05…
    #>  8 NO2_1981  030115, 030116, 040202, 050102, 050104, 050109, 050110, 050111, 05…
    #>  9 NO2_1982  030115, 030116, 040202, 050102, 050103, 050104, 050108, 050109, 05…
    #> 10 NO2_1983  030115, 030116, 040202, 050102, 050103, 050104, 050108, 050109, 05…
    #> # ℹ 121 more rows

Here is a sample of files/sites with concentrations above 2000 - see
[here](inst/extdata/issues/invalid_values.csv) for the full list:

    #> # A tibble: 2 × 2
    #>   file_name site_ids                              
    #>   <chr>     <chr>                                 
    #> 1 PM25_2016 090701, 090702, 090801, 090806, 094601
    #> 2 PM25_2023 105504

## Date Misalignment

Inconsistencies have been found in the hourly alignment of NAPS data for
some sites in entirety, and for select years for some sites, when
compared to the same station’s data sourced directly from the BC
Government archive. Due to NAPS recieving their data from BC, this issue
is likely due to an error in how the NAPS data are converted to local
hour and archived.

Given the presence of the BC Government archive with clear documentation
on data timezone, a standardized data format, and the ability to collect
data programmatically, this issue has only been assessed for BC.
However, it is likely that this issue affects data from across the
country.

Below we load in the BCgov/NAPS data alligned by naps_id and date, and
test the correlation between the two datasets for each pollutant at
various time lags. Given that these datasets should be essentially the
same, the correlation should be near perfect (small differences could
occur due to rounding or differing QAQC decisions). Sites/years where
the correlation goes from poor-good to near perfect after lagging the
data have allignment issues.

``` r
library(napsreview)

# Connect to database (change path here if you did earlier as well)
db_path <- "./naps.duckdb"
db <- connect_to_database(db_path)
on.exit(DBI::dbDisconnect(db))

# View the bcgov/naps data aligned by naps_id and date
db |> dplyr::tbl("bcgov_aligned_data")

# Load the aligned data
aligned_data <- db |>
  dplyr::tbl("bcgov_aligned_data") |>
  dplyr::collect() |>
  # Censor negative values, round to nearest integer (NAPS data are integers)
  dplyr::mutate(
    dplyr::across(dplyr::ends_with(c("_naps", "_bcgov")), \(x) {
      ifelse(x < 0, NA_real_, x) |> round(digits = 0)
    })
  )

# Test alignment for each pollutant
passed <- list()
pollutants <- names(aligned_data) |>
  stringr::str_subset("_bcgov$") |>
  sub(pattern = "_bcgov$", replacement = "")
issues_dir <- paste0("extdata/issues") |> # adjust as needed
  system.file(package = "napsreview")
for (pollutant in pollutants) {
  passed[[pollutant]] <- aligned_data |>
    check_date_alignment(
      pollutant = pollutant,
      value_cols = pollutant |> paste0("_", c("bcgov", "naps")),
      name_cols = pollutant |> paste0("_", c("bcgov_lag", "naps_lag")),
      save_issues_to = issues_dir
    )
}
```

Here is a sample of the sites with persistent date misalignment issues
(PM2.5 only, but they exist for other pollutants). “\_lag_1” indicates
that those data are lagged by 1 hour (i.e. those data are incorrectly
shifted 1 hour later). Here all of the sites go from 70-90% correlation
to 93-100% after lagging the NAPS data by 1 hour. See the following for
the full lists for:
[PM2.5](inst/extdata/issues/pm25_site_alignment.csv),
[O3](inst/extdata/issues/o3_site_alignment.csv),
[NO2](inst/extdata/issues/no2_site_alignment.csv).

    #> # A tibble: 9 × 6
    #>   naps_id best_lag_a best_lag_b      best_cor nonlagged_cor mean_count
    #>     <int> <chr>      <chr>              <dbl>         <dbl>      <dbl>
    #> 1  101801 pm25_bcgov pm25_naps_lag_1    0.999         0.889     47164 
    #> 2  101803 pm25_bcgov pm25_naps_lag_1    1             0.745     28084 
    #> 3  103202 pm25_bcgov pm25_naps_lag_1    0.968         0.911     84559.
    #> 4  103203 pm25_bcgov pm25_naps_lag_1    1             0.805     14511.
    #> 5  103204 pm25_bcgov pm25_naps_lag_1    1             0.803     15936.
    #> 6  103205 pm25_bcgov pm25_naps_lag_1    0.933         0.747     16987 
    #> 7  105501 pm25_bcgov pm25_naps_lag_1    1             0.796     18064.
    #> 8  105504 pm25_bcgov pm25_naps_lag_1    1.000         0.897     73324 
    #> 9  106502 pm25_bcgov pm25_naps_lag_1    1             0.709     28518

Here is a sample of the sites with date misalignment issues for specific
years (PM2.5 only, but they exist for other pollutants). Here all of the
sites go from 70-90% correlation to 93-100% after lagging the NAPS data
by 1 hour. See the following for the full lists for:
[PM2.5](inst/extdata/issues/pm25_annual_site_alignment.csv),
[O3](inst/extdata/issues/o3_annual_site_alignment.csv),
[NO2](inst/extdata/issues/no2_annual_site_alignment.csv).

    #> # A tibble: 73 × 7
    #>     year naps_id best_lag_a best_lag_b      best_cor nonlagged_cor mean_count
    #>    <int>   <int> <chr>      <chr>              <dbl>         <dbl>      <dbl>
    #>  1  2001  105501 pm25_bcgov pm25_naps_lag_1    1             0.829      6945.
    #>  2  2002  105501 pm25_bcgov pm25_naps_lag_1    1             0.777      8718.
    #>  3  2003  105501 pm25_bcgov pm25_naps_lag_1    1             0.780      2401.
    #>  4  2005  101803 pm25_bcgov pm25_naps_lag_1    1             0.641      4623.
    #>  5  2005  103203 pm25_bcgov pm25_naps_lag_1    1             0.824      8459.
    #>  6  2005  103204 pm25_bcgov pm25_naps_lag_1    1             0.756      8498.
    #>  7  2005  103205 pm25_bcgov pm25_naps_lag_1    0.933         0.739      8008.
    #>  8  2005  106502 pm25_bcgov pm25_naps_lag_1    1             0.713      7515.
    #>  9  2006  101803 pm25_bcgov pm25_naps_lag_1    1             0.790      7501 
    #> 10  2006  103203 pm25_bcgov pm25_naps_lag_1    1             0.780      6051.
    #> # ℹ 63 more rows

And this issue becomes quite clear when looking at the monthly
correlation between the two datasets. Here is two samples of that, see
[monthly_cor_plots](/inst/extdata/issues/monthly_cor_plots) for the full
set. Note how for some sites, specific years appear to be shifted while
others are not, and for other sites the shift is consistent across
years.

![](/inst/extdata/issues/monthly_cor_plots/100110_pm25_bcgov_pm25_naps.png)
![](/inst/extdata/issues/monthly_cor_plots/100112_no2_bcgov_no2_naps.png)
![](/inst/extdata/issues/monthly_cor_plots/105504_o3_bcgov_o3_naps.png)

By mapping the locations of sites with allignment issues, we can see
that the sites are all either in the metro-vancouver / lower fraser
valley region, or east of the Rockies (where DST is not observed, and MT
is in place instead of PT).

The “east of the Rockies” issue further supports the hypothesis that the
date misalignment occurs in the conversion to local hour when archiving
the NAPS data. For the Vancouver area monitors the reason is less clear,
but could be due to how air quality is managed in that region seperatley
from the rest of the province.

![](/inst/extdata/issues/misalignment_map.png)

Unfortunatley, given that no other province/territory provides an easy
to collect data source for their historic air quality data (that we are
aware of), we are not able to evaluate this issue across Canada. In
addition, due to BC not providing NAPS ID’s for all stations, some BC
sites which have alignment issues may not be represented in this
analysis. If the trend we observed for BC persists, potential issues may
exist for NL and NU due to the presence of multiple timezones in those
provinces/territories. In addition, other provinces/territories with
seperate internal agencies responsible for monitoring air quality (such
as the air sheds in AB) may also have alignment issues, similiar to what
was noted for Metro Vancouver.
