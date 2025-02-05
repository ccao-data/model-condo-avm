Table of Contents
================

- [Prior Models](#prior-models)
- [Model Overview](#model-overview)
  - [Differences Compared to the Residential
    Model](#differences-compared-to-the-residential-model)
    - [Features Used](#features-used)
    - [Valuation](#valuation)
    - [Multi-PIN Sales](#multi-pin-sales)
  - [Condo Strata](#condo-strata)
- [Ongoing Issues](#ongoing-issues)
  - [Unit Heterogeneity](#unit-heterogeneity)
  - [Buildings With Few Sales](#buildings-with-few-sales)
  - [Buildings Without Sales](#buildings-without-sales)
- [FAQs](#faqs)
- [Usage](#usage)
  - [Getting Data](#getting-data)
- [License](#license)
- [Contributing](#contributing)

<!-- README.md is generated from README.Rmd. Please edit that file -->

> :warning: **NOTE** :warning:
>
> The [condominium model](https://github.com/ccao-data/model-condo-avm)
> (this repo) is nearly identical to the [residential
> (single/multi-family)
> model](https://github.com/ccao-data/model-res-avm), with a few [key
> differences](#differences-compared-to-the-residential-model). Please
> read the documentation for the [residential
> model](https://github.com/ccao-data/model-res-avm) first.

# Prior Models

This repository contains code, data, and documentation for the Cook
County Assessor’s condominium reassessment model. Information about
prior year models can be found at the following links:

| Year(s) | Triad(s) | Method                                      | Language / Framework       | Link                                                                                                                                       |
|---------|----------|---------------------------------------------|----------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| 2015    | City     | N/A                                         | SPSS                       | [Link](https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev/-/tree/master/code.legacy/2015%20City%20Tri/2015%20Condo%20Models) |
| 2018    | City     | N/A                                         | N/A                        | Not available. Values provided by vendor                                                                                                   |
| 2019    | North    | Linear regression or GBM model per township | R (Base)                   | [Link](https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev)                                                                   |
| 2020    | South    | Linear regression or GBM model per township | R (Base)                   | [Link](https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev)                                                                   |
| 2021    | City     | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-condo-avm/tree/2021-assessment-year)                                                             |
| 2022    | North    | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-condo-avm/tree/2022-assessment-year)                                                             |
| 2023    | South    | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-condo-avm/tree/2023-assessment-year)                                                             |
| 2024    | City     | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-condo-avm/tree/2024-assessment-year)                                                             |

# Model Overview

The duty of the Cook County Assessor’s Office is to value property in a
fair, accurate, and transparent way. The Assessor is committed to
transparency throughout the assessment process. As such, this document
contains:

- [A description of the differences between the residential model and
  this (condominium)
  model](#differences-compared-to-the-residential-model)
- [An outline of ongoing issues specific to condominium
  assessments](#ongoing-issues)

The repository itself contains the [code](./pipeline) for the Automated
Valuation Model (AVM) used to generate initial assessed values for all
condominium properties in Cook County. This system is effectively an
advanced machine learning model (hereafter referred to as “the model”).
It uses previous sales to generate estimated sale values (assessments)
for all properties.

## Differences Compared to the Residential Model

The Cook County Assessor’s Office has started to track a limited number
of characteristics (building-level square footage, unit-level square
footage, bedrooms, and bathrooms) for condominiums, but the data we have
***varies in both the characteristics available and their
completeness*** between triads. Staffing limitations have forced the
office to prioritize smaller condo buildings less likely to have recent
unit sales in certain parts of the county.

Like most assessors nationwide, our office staff cannot enter buildings
to observe property characteristics. For condos, this means we cannot
observe amenities, quality, or any other interior characteristics which
must instead be gathered from listings and a number of additional
third-party sources.

The only *complete* information our office currently has about
individual condominium units is their age, location, sale date/price,
and percentage of ownership. This makes modeling condos particularly
challenging, as the number of usable features is quite small.
Fortunately, condos have two qualities which make modeling a bit easier:

1.  Condos are more homogeneous than single/multi-family properties,
    i.e. the range of potential condo sale prices is much narrower.
2.  Condo are pre-grouped into clusters of like units (buildings), and
    units within the same building usually have similar sale prices.

We leverage these qualities to produce what we call ***strata***, a
feature unique to the condo model. See [Condo Strata](#condo-strata) for
more information about how strata is used and calculated.

### Features Used

Because our individual condo unit characteristics are sparse and
incomplete, we primarily must rely on aggregate geospatial features,
economic features, [strata](#condo-strata), and time of sale to
determine condo assessed values. The features in the table below are the
ones used in the most recent assessment model.

| Feature Name                                                                | Variable Name                                         | Description                                                                                                                                           | Category       | Type      | Unique to Condo Model |
|:----------------------------------------------------------------------------|:------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------|:----------|:----------------------|
| Condominium Building Year Built                                             | char_yrblt                                            | Year the property was constructed                                                                                                                     | Characteristic | numeric   | X                     |
| Total Condominium Building Livable Parcels                                  | char_building_units                                   | Count of livable 14-digit PINs (AKA condo units)                                                                                                      | Characteristic | numeric   | X                     |
| Total Condominium Building Non-Livable Parcels                              | char_building_non_units                               | Count of non-livable 14-digit PINs                                                                                                                    | Characteristic | numeric   | X                     |
| Condominium Building Is Mixed Use                                           | char_bldg_is_mixed_use                                | The 10-digit PIN (building) contains a 14-digit PIN that is neither class 299 nor 399                                                                 | Characteristic | logical   | X                     |
| Total Condominium Building Square Footage                                   | char_building_sf                                      | Square footage of the *building* (PIN10) containing this unit                                                                                         | Characteristic | numeric   | X                     |
| Condominium Unit Square Footage                                             | char_unit_sf                                          | Square footage of the condominium unit associated with this PIN                                                                                       | Characteristic | numeric   | X                     |
| Condominium Unit Bedrooms                                                   | char_bedrooms                                         | Number of bedrooms in the building                                                                                                                    | Characteristic | numeric   | X                     |
| Condominium Unit Half Baths                                                 | char_half_baths                                       | Number of half baths                                                                                                                                  | Characteristic | numeric   | X                     |
| Condominium Unit Full Baths                                                 | char_full_baths                                       | Number of full bathrooms                                                                                                                              | Characteristic | numeric   | X                     |
| Condominium % Ownership                                                     | meta_tieback_proration_rate                           | Proration rate applied to the PIN                                                                                                                     | Meta           | numeric   | X                     |
| Condominium Building Strata 1                                               | meta_strata_1                                         | Condominium Building Strata - 10 Levels                                                                                                               | Meta           | character | X                     |
| Condominium Building Strata 2                                               | meta_strata_2                                         | Condominium Building Strata - 100 Levels                                                                                                              | Meta           | character | X                     |
| Standard Deviation Distance From Parcel Centroid to Vertices (Feet)         | shp_parcel_centroid_dist_ft_sd                        | Standard deviation of the distance from each major parcel vertex to the parcel centroid                                                               | Parcel Shape   | numeric   | X                     |
| Standard Deviation Parcel Edge Length (Feet)                                | shp_parcel_edge_len_ft_sd                             | Standard deviation of the edge length between parcel vertices                                                                                         | Parcel Shape   | numeric   | X                     |
| Standard Deviation Parcel Interior Angle (Degrees)                          | shp_parcel_interior_angle_sd                          | Standard deviation of the interior angles of the parcel polygon                                                                                       | Parcel Shape   | numeric   | X                     |
| Ratio of Parcel Area to Minimum Rotated Bounding Rectangle                  | shp_parcel_mrr_area_ratio                             | Ratio of the parcel’s area to the area of its minimum rotated bounding rectangle                                                                      | Parcel Shape   | numeric   | X                     |
| Ratio of Parcel Minimum Rotated Bounding Rectangle Longest to Shortest Side | shp_parcel_mrr_side_ratio                             | Ratio of the longest to the shortest side of the parcel’s minimum rotated bounding rectangle                                                          | Parcel Shape   | numeric   | X                     |
| Number of Parcel Vertices                                                   | shp_parcel_num_vertices                               | The number of vertices of the parcel                                                                                                                  | Parcel Shape   | numeric   | X                     |
| Nearest Highway Distance (Feet)                                             | prox_nearest_road_highway_dist_ft                     | Distance to nearest highway road                                                                                                                      | Proximity      | numeric   | X                     |
| Nearest Arterial Road Distance (Feet)                                       | prox_nearest_road_arterial_dist_ft                    | Distance to nearest arterial road                                                                                                                     | Proximity      | numeric   | X                     |
| Nearest Collector Road Distance (Feet)                                      | prox_nearest_road_collector_dist_ft                   | Distance to nearest collector road                                                                                                                    | Proximity      | numeric   | X                     |
| Average Daily Traffic Count on Nearest Highway                              | prox_nearest_road_highway_daily_traffic               | Daily traffic of nearest highway road                                                                                                                 | Proximity      | numeric   | X                     |
| Average Daily Traffic Count on Nearest Arterial Road                        | prox_nearest_road_arterial_daily_traffic              | Daily traffic of nearest arterial road                                                                                                                | Proximity      | numeric   | X                     |
| Average Daily Traffic Count on Nearest Collector Road                       | prox_nearest_road_collector_daily_traffic             | Daily traffic of nearest collector road                                                                                                               | Proximity      | numeric   | X                     |
| Nearest New Construction (Feet)                                             | prox_nearest_new_construction_dist_ft                 | Nearest new construction distance (feet)                                                                                                              | Proximity      | numeric   | X                     |
| Nearest Major Stadium (Feet)                                                | prox_nearest_stadium_dist_ft                          | Nearest stadium distance (feet)                                                                                                                       | Proximity      | numeric   | X                     |
| Percent Population Age, Under 19 Years Old                                  | acs5_percent_age_children                             | Percent of the people 17 years or younger                                                                                                             | ACS5           | numeric   |                       |
| Percent Population Age, Over 65 Years Old                                   | acs5_percent_age_senior                               | Percent of the people 65 years or older                                                                                                               | ACS5           | numeric   |                       |
| Median Population Age                                                       | acs5_median_age_total                                 | Median age for whole population                                                                                                                       | ACS5           | numeric   |                       |
| Percent Households Family, Married                                          | acs5_percent_household_family_married                 | Percent of households that are family, married                                                                                                        | ACS5           | numeric   |                       |
| Percent Households Nonfamily, Living Alone                                  | acs5_percent_household_nonfamily_alone                | Percent of households that are non-family, alone (single)                                                                                             | ACS5           | numeric   |                       |
| Percent Population Education, High School Degree                            | acs5_percent_education_high_school                    | Percent of people older than 25 who attained a high school degree                                                                                     | ACS5           | numeric   |                       |
| Percent Population Education, Bachelor Degree                               | acs5_percent_education_bachelor                       | Percent of people older than 25 who attained a bachelor’s degree                                                                                      | ACS5           | numeric   |                       |
| Percent Population Education, Graduate Degree                               | acs5_percent_education_graduate                       | Percent of people older than 25 who attained a graduate degree                                                                                        | ACS5           | numeric   |                       |
| Percent Population Income, Below Poverty Level                              | acs5_percent_income_below_poverty_level               | Percent of people above the poverty level in the last 12 months                                                                                       | ACS5           | numeric   |                       |
| Median Income, Household in Past Year                                       | acs5_median_income_household_past_year                | Median income per household in the past 12 months                                                                                                     | ACS5           | numeric   |                       |
| Median Income, Per Capita in Past Year                                      | acs5_median_income_per_capita_past_year               | Median income per capita in the past 12 months                                                                                                        | ACS5           | numeric   |                       |
| Percent Population Income, Received SNAP in Past Year                       | acs5_percent_income_household_received_snap_past_year | Percent of households that received SNAP in the past 12 months                                                                                        | ACS5           | numeric   |                       |
| Percent Population Employment, Unemployed                                   | acs5_percent_employment_unemployed                    | Percent of people 16 years and older unemployed                                                                                                       | ACS5           | numeric   |                       |
| Median Occupied Household, Total, Year Built                                | acs5_median_household_total_occupied_year_built       | Median year built for all occupied households                                                                                                         | ACS5           | numeric   |                       |
| Median Occupied Household, Renter, Gross Rent                               | acs5_median_household_renter_occupied_gross_rent      | Median gross rent for only renter-occupied units                                                                                                      | ACS5           | numeric   |                       |
| Percent Occupied Households, Owner                                          | acs5_percent_household_owner_occupied                 | Percent of households that are owner-occupied                                                                                                         | ACS5           | numeric   |                       |
| Land Square Feet                                                            | char_land_sf                                          | Square footage of the land (not just the building) of the property                                                                                    | Characteristic | numeric   |                       |
| Longitude                                                                   | loc_longitude                                         | X coordinate in degrees (global longitude)                                                                                                            | Location       | numeric   |                       |
| Latitude                                                                    | loc_latitude                                          | Y coordinate in degrees (global latitude)                                                                                                             | Location       | numeric   |                       |
| Census Tract GEOID                                                          | loc_census_tract_geoid                                | 11-digit ACS/Census tract GEOID                                                                                                                       | Location       | character |                       |
| First Street Factor                                                         | loc_env_flood_fs_factor                               | First Street flood factor The flood factor is a risk score, where 10 is the highest risk and 1 is the lowest risk                                     | Location       | numeric   |                       |
| School Elementary District GEOID                                            | loc_school_elementary_district_geoid                  | School district (elementary) GEOID                                                                                                                    | Location       | character |                       |
| School Secondary District GEOID                                             | loc_school_secondary_district_geoid                   | School district (secondary) GEOID                                                                                                                     | Location       | character |                       |
| CMAP Walkability Score (No Transit)                                         | loc_access_cmap_walk_nta_score                        | CMAP walkability score for a given PIN, excluding transit walkability                                                                                 | Location       | numeric   |                       |
| CMAP Walkability Total Score                                                | loc_access_cmap_walk_total_score                      | CMAP walkability score for a given PIN, including transit walkability                                                                                 | Location       | numeric   |                       |
| Municipality Name                                                           | loc_tax_municipality_name                             | Taxing district name, as seen on Cook County tax bills                                                                                                | Location       | character |                       |
| Township Code                                                               | meta_township_code                                    | Cook County township code                                                                                                                             | Meta           | character |                       |
| Neighborhood Code                                                           | meta_nbhd_code                                        | Assessor neighborhood code                                                                                                                            | Meta           | character |                       |
| Property Tax Bill Aggregate Rate                                            | other_tax_bill_rate                                   | Tax bill rate for the taxing district containing a given PIN                                                                                          | Other          | numeric   |                       |
| Number of PINs in Half Mile                                                 | prox_num_pin_in_half_mile                             | Number of PINs within half mile                                                                                                                       | Proximity      | numeric   |                       |
| Number of Bus Stops in Half Mile                                            | prox_num_bus_stop_in_half_mile                        | Number of bus stops within half mile                                                                                                                  | Proximity      | numeric   |                       |
| Number of Foreclosures Per 1000 PINs (Past 5 Years)                         | prox_num_foreclosure_per_1000_pin_past_5_years        | Number of foreclosures per 1000 PINs, within half mile (past 5 years)                                                                                 | Proximity      | numeric   |                       |
| Total Airport Noise DNL                                                     | prox_airport_dnl_total                                | Estimated DNL for a PIN, assuming a baseline DNL of 50 (“quiet suburban”) and adding predicted noise from O’Hare and Midway airports to that baseline | Proximity      | numeric   |                       |
| Nearest Bike Trail Distance (Feet)                                          | prox_nearest_bike_trail_dist_ft                       | Nearest bike trail distance (feet)                                                                                                                    | Proximity      | numeric   |                       |
| Nearest Cemetery Distance (Feet)                                            | prox_nearest_cemetery_dist_ft                         | Nearest cemetery distance (feet)                                                                                                                      | Proximity      | numeric   |                       |
| Nearest CTA Route Distance (Feet)                                           | prox_nearest_cta_route_dist_ft                        | Nearest CTA route distance (feet)                                                                                                                     | Proximity      | numeric   |                       |
| Nearest CTA Stop Distance (Feet)                                            | prox_nearest_cta_stop_dist_ft                         | Nearest CTA stop distance (feet)                                                                                                                      | Proximity      | numeric   |                       |
| Nearest Hospital Distance (Feet)                                            | prox_nearest_hospital_dist_ft                         | Nearest hospital distance (feet)                                                                                                                      | Proximity      | numeric   |                       |
| Lake Michigan Distance (Feet)                                               | prox_lake_michigan_dist_ft                            | Distance to Lake Michigan shoreline (feet)                                                                                                            | Proximity      | numeric   |                       |
| Nearest Metra Route Distance (Feet)                                         | prox_nearest_metra_route_dist_ft                      | Nearest Metra route distance (feet)                                                                                                                   | Proximity      | numeric   |                       |
| Nearest Metra Stop Distance (Feet)                                          | prox_nearest_metra_stop_dist_ft                       | Nearest Metra stop distance (feet)                                                                                                                    | Proximity      | numeric   |                       |
| Nearest Park Distance (Feet)                                                | prox_nearest_park_dist_ft                             | Nearest park distance (feet)                                                                                                                          | Proximity      | numeric   |                       |
| Nearest Railroad Distance (Feet)                                            | prox_nearest_railroad_dist_ft                         | Nearest railroad distance (feet)                                                                                                                      | Proximity      | numeric   |                       |
| Nearest University Distance (Feet)                                          | prox_nearest_university_dist_ft                       | Nearest university distance (feet)                                                                                                                    | Proximity      | numeric   |                       |
| Nearest Vacant Land Parcel Distance (Feet)                                  | prox_nearest_vacant_land_dist_ft                      | Nearest vacant land (class 100) parcel distance (feet)                                                                                                | Proximity      | numeric   |                       |
| Nearest Water Distance (Feet)                                               | prox_nearest_water_dist_ft                            | Nearest water distance (feet)                                                                                                                         | Proximity      | numeric   |                       |
| Nearest Golf Course Distance (Feet)                                         | prox_nearest_golf_course_dist_ft                      | Nearest golf course distance (feet)                                                                                                                   | Proximity      | numeric   |                       |
| Sale Year                                                                   | time_sale_year                                        | Sale year calculated as the number of years since 0 B.C.E                                                                                             | Time           | numeric   |                       |
| Sale Day                                                                    | time_sale_day                                         | Sale day calculated as the number of days since January 1st, 1997                                                                                     | Time           | numeric   |                       |
| Sale Quarter of Year                                                        | time_sale_quarter_of_year                             | Character encoding of quarter of year (Q1 - Q4)                                                                                                       | Time           | character |                       |
| Sale Month of Year                                                          | time_sale_month_of_year                               | Character encoding of month of year (Jan - Dec)                                                                                                       | Time           | character |                       |
| Sale Day of Year                                                            | time_sale_day_of_year                                 | Numeric encoding of day of year (1 - 365)                                                                                                             | Time           | numeric   |                       |
| Sale Day of Month                                                           | time_sale_day_of_month                                | Numeric encoding of day of month (1 - 31)                                                                                                             | Time           | numeric   |                       |
| Sale Day of Week                                                            | time_sale_day_of_week                                 | Numeric encoding of day of week (1 - 7)                                                                                                               | Time           | numeric   |                       |
| Sale After COVID-19                                                         | time_sale_post_covid                                  | Indicator for whether sale occurred after COVID-19 was widely publicized (around March 15, 2020)                                                      | Time           | logical   |                       |

We maintain a few useful resources for working with these features:

- Once you’ve [pulled the input data](#getting-data), you can inner join
  the data to the CSV version of the data dictionary
  ([`docs/data-dict.csv`](./docs/data-dict.csv)) to filter for only the
  features that we use in the model.
- You can browse our [data
  catalog](https://ccao-data.github.io/data-architecture/#!/overview) to
  see more details about these features, in particular the [condo model
  input
  view](https://ccao-data.github.io/data-architecture/#!/model/model.ccao_data_athena.model.vw_pin_condo_input)
  which is the source of our training data.
- You can use the [`ccao` R package](https://ccao-data.github.io/ccao/)
  or its [Python equivalent](https://ccao-data.github.io/ccao/python/)
  to programmatically convert variable names to their human-readable
  versions
  ([`ccao::vars_rename()`](https://ccao-data.github.io/ccao/reference/vars_rename.html))
  or convert numerically-encoded variables to human-readable values
  ([`ccao::vars_recode()`](https://ccao-data.github.io/ccao/reference/vars_recode.html).
  The [`ccao::vars_dict`
  object](https://ccao-data.github.io/ccao/reference/vars_dict.html) is
  also useful for inspecting the raw crosswalk that powers the rename
  and recode functions.

### Valuation

For the most part, condos are valued the same way as single- and
multi-family residential property. We [train a
model](https://github.com/ccao-data/model-res-avm#how-it-works) using
individual condo unit sales, predict the value of all units, and then
apply any [post-modeling
adjustment](https://github.com/ccao-data/model-res-avm#post-modeling).

However, because the CCAO has so [little information about individual
units](#differences-compared-to-the-residential-model), we must rely on
the [condominium percentage of ownership](#features-used) to
differentiate between units in a building. This feature is effectively
the proportion of the building’s overall value held by a unit. It is
created when a condominium declaration is filed with the County (usually
by the developer of the building). The critical assumption underlying
the condo valuation process is that percentage of ownership correlates
with the relative market value differences between units.

Percentage of ownership is used in two ways:

1.  It is used directly as a predictor/feature in the regression model
    to estimate differing unit values within the same building.
2.  It is used to reapportion unit values directly i.e. the value of a
    unit is ultimately equal to `% of ownership * total building value`.

Visually, this looks like:

![](docs/figures/valuation_perc_owner.png)

For what the office terms “nonlivable” spaces — parking spaces, storage
space, and common area — the breakout of value works differently. See
[this excel sheet](docs/spreadsheets/condo_nonlivable_demo.xlsx) for an
interactive example of how nonlivable spaces are valued based on the
total value of a building’s livable space.

Percentage of ownership is the single most important feature in the
condo model. It determines almost all intra-building differences in unit
values.

### Multi-PIN Sales

The condo model is trained on a select number of “multi-PIN sales” (or
“multi-sales”) in addition to single-parcel sales. Multi-sales are sales
that include more than one parcel. In the case of condominiums, many
units are sold bundled with deeded parking spaces that are separate
parcels. These two-parcel sales are highly reflective of the unit’s
actual market price. We split the total value of these two-parcel sales
according to their relative percent of ownership before using them for
training. For example, for a \$100,000 sale of a unit (4% ownership) and
a parking space (1% ownership), the sale would be adjusted to \$80,000:

$$\frac{0.04}{0.04 + 0.01} * \$100,000 = \$80,000$$

## Condo Strata

The condo model uses an engineered feature called *strata* to deliver
much of its predictive power. Strata is the binned, time-weighted,
5-year average sale price of the building. There are two strata features
used in the model, one with 10 bins and one with 100 bins. Buildings are
binned across each triad using either quantiles or 1-dimensional
k-means. A visual representation of quantile-based strata binning looks
like:

![](docs/figures/strata.png)

To put strata in more concrete terms, the table below shows a sample
5-level strata. Each condominium unit would be assigned a strata from
this table (Strata 1, Strata 2, etc.) based on the 5-year weighted
average sale price of its building. All units in a building will have
the same strata.

| Strata   | Range of 5-year Average Sale Price |
|:---------|:-----------------------------------|
| Strata 1 | \$0 - \$121K                       |
| Strata 2 | \$121K - \$149K                    |
| Strata 3 | \$149K - \$199K                    |
| Strata 4 | \$199K - \$276K                    |
| Strata 5 | \$276K+                            |

Some additional notes on strata:

- Strata is calculated in the [ingest stage](./pipeline/00-ingest.R) of
  this repository.
- Calculating the 5-year average sale price of a building requires at
  least 1 sale. Buildings with no sales have their strata imputed via
  KNN (using year built, number of units, and location as features).
- Number of bins (10 and 100) was chosen based on model performance.
  These numbers yielded the lowest root mean-squared error (RMSE).

# Ongoing Issues

The CCAO faces a number of ongoing issues specific to condominium
modeling. We are currently working on processes to fix these issues. We
list the issues here for the sake of transparency and to provide a sense
of the challenges we face.

### Unit Heterogeneity

The current modeling methodology for condominiums makes two assumptions:

1.  Condos units within the same building are similar and will sell for
    similar amounts.
2.  If units are not similar, the percentage of ownership will
    accurately reflect and be proportional to any difference in value
    between units.

The model process works even in heterogeneous buildings as long as
assumption 2 is met. For example, imagine a building with 8 identical
units and 1 penthouse unit. This building violates assumption 1 because
the penthouse unit is likely larger and worth more than the other 10.
However, if the percentage of ownership of each unit is roughly
proportional to its value, then each unit will still receive a fair
assessment.

However, the model can produce poor results when both of these
assumptions are violated. For example, if a building has an extreme mix
of different units, each with the same percentage of ownership, then
smaller, less expensive units will be overvalued and larger, more
expensive units will be undervalued.

This problem is rare, but does occur in certain buildings with many
heterogeneous units. Such buildings typically go through a process of
secondary review to ensure the accuracy of the individual unit values.

### Buildings With Few Sales

The condo model relies on sales within the same building to calculate
[strata](#condo-strata). This method works well for large buildings with
many sales, but can break down when there are only 1 or 2 sales in a
building. The primary danger here is *unrepresentative* sales,
i.e. sales that deviate significantly from the real average value of a
building’s units. When this happens, buildings can have their average
unit sale value pegged too high or low.

Fortunately, buildings without any recent sales are relatively rare, as
condos have a higher turnover rate than single and multi-family
property. Smaller buildings with low turnover are the most likely to not
have recent sales.

### Buildings Without Sales

When no sales have occurred in a building in the 5 years prior to
assessment, the building’s strata features are imputed. The model will
look at nearby buildings that have similar unit counts/age and then try
to assign an appropriate strata to the target building.

Most of the time, this technique produces reasonable results. However,
buildings without sales still go through an additional round of review
to ensure the accuracy of individual unit values.

# FAQs

**Note:** The FAQs listed here are for condo-specific questions. See the
residential model documentation for [more general
FAQs](https://github.com/ccao-data/model-res-avm#faqs).

**Q: What are the most important features in the condo model?**

As with the [residential
model](https://github.com/ccao-data/model-res-avm), the importance of
individual features varies by location and time. However, generally
speaking, the most important features are:

- Location, location, location. Location is the largest driver of
  county-wide variation in condo value. We account for location using
  [geospatial features like neighborhood](#features-used).
- Condo percentage of ownership, which determines the intra-building
  variation in unit price.
- [Condo building strata](#condo-strata). Strata provides us with a good
  estimate of the average sale price of a building’s units.

**Q: How do I see my condo building’s strata?**

Individual building [strata](#condo-strata) are not included with
assessment notices or shown on the CCAO’s website. However, strata *are*
stored in the sample data included in this repository. You can load the
data
([`input/condo_strata_data.parquet`](./input/condo_strata_data.parquet))
using R and the `read_parquet()` function from the `arrow` library.

**Q: How do I see the assessed value of other units in my building?**

You can use the [CCAO’s Address
Search](https://www.cookcountyassessor.com/address-search#address) to
see all the PINs and values associated with a specific condominium
building, simply leave the `Unit Number` field blank when submitting a
search.

**Q: How do I view my unit’s percentage of ownership?**

The percentage of ownership for individual units is printed on
assessment notices. You may also be able to find it via your building’s
board or condo declaration.

# Usage

Installation and usage of this model is identical to the [installation
and usage of the residential
model](https://github.com/ccao-data/model-res-avm#usage). Please follow
the instructions listed there.

## Getting Data

The data required to run these scripts is produced by the [ingest
stage](pipeline/00-ingest.R), which uses SQL pulls from the CCAO’s
Athena database as a primary data source. CCAO employees can run the
ingest stage or pull the latest version of the input data from our
internal DVC store using:

``` bash
dvc pull
```

Public users can download data for each assessment year using the links
below. Each file should be placed in the `input/` directory prior to
running the model pipeline.

#### 2021

- [assmntdata.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2021/assmntdata.parquet)
- [modeldata.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2021/modeldata.parquet)

#### 2022

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2022/assessment_data.parquet)
- [condo_strata_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2022/condo_strata_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2022/land_nbhd_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2022/training_data.parquet)

#### 2023

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2023/assessment_data.parquet)
- [condo_strata_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2023/condo_strata_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2023/land_nbhd_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2023/training_data.parquet)

#### 2024

Due to a [data
issue](https://github.com/ccao-data/data-architecture/pull/334) with the
initial 2024 model run, there are actually *two* final 2024 models. The
run `2024-02-16-silly-billy` was used for Rogers Park only, while the
run `2024-03-11-pensive-manasi` was used for all subsequent City of
Chicago townships.

The data issue caused some sales to be omitted from the
`2024-02-16-silly-billy` training set, however the actual impact on
predicted values was *extremely* minimal. We chose to update the data
and create a second final model out of an abundance of caution, and,
given low transaction volume in 2023, to include as many arms-length
transactions in the training set as possible.

##### 2024-02-16-silly-billy

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-02-16-silly-billy/assessment_data.parquet)
- [char_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-02-16-silly-billy/char_data.parquet)
- [condo_strata_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-02-16-silly-billy/condo_strata_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-02-16-silly-billy/land_nbhd_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-02-16-silly-billy/training_data.parquet)

##### 2024-03-11-pensive-manasi (final)

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-03-11-pensive-manasi/assessment_data.parquet)
- [char_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-03-11-pensive-manasi/char_data.parquet)
- [condo_strata_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-03-11-pensive-manasi/condo_strata_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-03-11-pensive-manasi/land_nbhd_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/condo/2024/run_id=2024-03-11-pensive-manasi/training_data.parquet)

For other data from the CCAO, please visit the [Cook County Data
Portal](https://datacatalog.cookcountyil.gov/).

# License

Distributed under the AGPL-3 License. See [LICENSE](./LICENSE) for more
information.

# Contributing

We welcome pull requests, comments, and other feedback via GitHub. For
more involved collaboration or projects, please see the [Developer
Engagement Program](https://github.com/ccao-data/people#external)
documentation on our group wiki.
