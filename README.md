
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

``` r
library(napsreview)
desired_years <- 1974:2023
desired_pollutants <- c("PM25", "O3", "NO2")

# Define local paths (default is the install dir of napsreview)
#  but you can change that if needed
raw_data_dir <- "extdata/naps_raw" |>
  system.file(package = "napsreview")
db_path <- "extdata" |>
  system.file(package = "napsreview") |>
  file.path("naps.duckdb")

# Create (if needed) and connect to local database
db <- connect_to_database(db_path)
on.exit(DBI::dbDisconnect(db))

# Collect data for desired years/pollutants
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
if (!DBI::dbExistsTable(db, "fmt_data")) {
  db |>
    archive_fmt_naps_data(
      naps_data = naps_data,
      fmt_data_tbl = "fmt_data",
      fmt_meta_tbl = "fmt_meta"
    )
}
```

And you can check that worked and view the data using the following:

``` r
library(napsreview)

# Connect to database (change path here if you did earlier as well)
db_path <- system.file("extdata", package = "napsreview") |>
  file.path("naps.duckdb")
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
db_path <- system.file("extdata", package = "napsreview") |>
  file.path("naps.duckdb")
db <- connect_to_database(db_path)
on.exit(DBI::dbDisconnect(db))

# View encoding differences between versions (change path here if you did earlier as well)
raw_data_dir <- "extdata/naps_raw" |>
  system.file(package = "napsreview")
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

These files have been identified as having the old format:

    #> NO2: 1974-2001, 2003 and 2004
    #> O3: 1974-2001, 2003 and 2004
    #> PM25: 1995-2004

## Inconsistent Metadata

Every entry in the raw NAPS data has latitude, longitude,
province/territory, and city information. However, for many sites these
values are inconsistent between files.

For example, lat/lng values have differing precisions between files
causing slight shifts in location.

    #> # A tibble: 308 × 4
    #>    site_id lat       lng        files                                           
    #>      <int> <chr>     <chr>      <chr>                                           
    #>  1   50116 45.47167  -73.57222  O3_2007, NO2_2014, NO2_2007, O3_2005, NO2_2013,…
    #>  2   50116 45.472854 -73.57296  O3_1982, O3_1987, NO2_1994, NO2_2000, O3_1978, …
    #>  3   60204 42.31578  -83.04367  O3_2006, NO2_2008, O3_2009, O3_2010, PM25_2007,…
    #>  4   60204 42.315778 -83.043667 O3_1977, O3_1991, NO2_1993, NO2_1997, NO2_2000,…
    #>  5   50308 46.82118  -71.22049  O3_2008, PM25_2014, O3_2006, NO2_2008, O3_2010,…
    #>  6   50308 46.821177 -71.220495 O3_1998, O3_2000, NO2_2001, NO2_1999, NO2_2004,…
    #>  7   50308 46.82118  -71.2205   O3_2015, PM25_2019, NO2_2021, O3_2022, O3_2002,…
    #>  8  101401 49.36989  -121.49912 NO2_2007, NO2_2018, PM25_2022, O3_2007, O3_2017…
    #>  9  101401 49.369887 -121.49912 NO2_1998, O3_1997, O3_2003, NO2_2000, O3_1999, …
    #> 10   60430 43.709444 -79.5435   O3_2000, NO2_2004, NO2_2003, PM25_2003, O3_2003…
    #> # ℹ 298 more rows

In addition, city names are inconsistently spelled between files

    #> # A tibble: 27 × 2
    #>    prov_terr cities                             
    #>    <chr>     <chr>                              
    #>  1 AB        Bitumount | Bitumont               
    #>  2 AB        Fort Mackay | Fort Mckay           
    #>  3 BC        Metro Van - Abbotsford | Abbotsford
    #>  4 BC        Metro Van - Burnaby | Burnaby      
    #>  5 BC        Chilliwack | Metro Van-Chilliwack  
    #>  6 BC        Coquitlam | Metro Van - Coquitlam  
    #>  7 BC        Metro Van - Delta | Delta          
    #>  8 BC        Fort St. John | Fort St John       
    #>  9 BC        Metro Van-Hope | Hope              
    #> 10 BC        Langley | Metro Van-Langley        
    #> # ℹ 17 more rows

and some sites have multiple city names across files.

    #> # A tibble: 48 × 3
    #>    site_id n_cities cities                                           
    #>      <int>    <int> <chr>                                            
    #>  1  100135        2 Coquitlam | Metro Van - Coquitlam                
    #>  2   52301        2 Saint-Faustin-Lac-Carré | Saint-Faustin-Lac-Carre
    #>  3  100121        2 Metro Van - North Vancouver | North Vancouver    
    #>  4   10102        2 St. John's | St Johns                            
    #>  5   90801        2 Fort Mckay | Fort Mackay                         
    #>  6   55001        2 Ferme Neuve | Mont Saint-Michel                  
    #>  7  101004        2 Metro Van - Abbotsford | Abbotsford              
    #>  8   50801        2 Trois Rivières | Trois-Rivières                  
    #>  9   51501        2 St-Zépherin-De-Courval | St. Zephirin-De-Courval 
    #> 10  100112        2 Vancouver | Metro Van - Vancouver                
    #> # ℹ 38 more rows

## Unrealistic Values

Some values in the raw data are unrealistically high or low, coordinates
exist that are outside Canada, and some site ids are not properly padded
with leading zeros.

Here is a sample of files/sites with negative concentrations:

    #> # A tibble: 138 × 2
    #>    file_name site_ids                                                           
    #>    <chr>     <chr>                                                              
    #>  1 NO2_1974  050102, 070101, 030116, 060204, 060401, 050104, 050304, 090122, 06…
    #>  2 NO2_1975  050102, 050110, 070101, 090114, 060505, 060602, 030116, 060204, 06…
    #>  3 NO2_1976  060505, 060602, 061301, 050304, 090122, 050102, 060501, 060104, 07…
    #>  4 NO2_1977  060414, 060505, 060602, 061301, 050109, 050304, 090122, 100110, 06…
    #>  5 NO2_1978  060414, 060505, 060602, 061301, 050109, 050304, 090122, 100110, 06…
    #>  6 NO2_1979  060403, 090130, 050104, 060413, 100106, 050115, 060415, 060501, 06…
    #>  7 NO2_1980  030115, 100302, 050203, 061004, 050110, 050113, 060412, 080109, 05…
    #>  8 NO2_1981  050110, 050113, 060105, 060412, 050102, 060418, 030115, 100302, 04…
    #>  9 NO2_1982  060402, 061501, 070119, 090222, 100111, 060101, 040202, 050109, 05…
    #> 10 NO2_1983  050102, 050103, 060418, 050110, 050113, 060105, 060412, 080109, 05…
    #> # ℹ 128 more rows

Here is a sample of files/sites with concentrations above 2000:

    #> # A tibble: 2 × 2
    #>   file_name site_ids                              
    #>   <chr>     <chr>                                 
    #> 1 PM25_2016 094601, 090701, 090702, 090801, 090806
    #> 2 PM25_2023 105504

Here is a sample of files/sites with coordinates outside of Canada:

    #> # A tibble: 7 × 2
    #>   file_name site_ids
    #>   <chr>     <chr>   
    #> 1 NO2_1992  080801  
    #> 2 NO2_1993  080801  
    #> 3 O3_1988   064301  
    #> 4 O3_1989   064301  
    #> 5 O3_1990   064301  
    #> 6 O3_1992   080801  
    #> 7 O3_1993   080801

Here is a sample of files/sites with invalid site ids:

    #> # A tibble: 0 × 2
    #> # ℹ 2 variables: file_name <chr>, site_ids <chr>

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
