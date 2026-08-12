Final Project
================
Nithin Varghese
2026-08-12

- [Section 1 Data Description, Aquisition, and
  Wrangling](#section-1-data-description-aquisition-and-wrangling)
- [Section 2 Exploratory Findings That Shaped Your Story and Data
  cleaning](#section-2-exploratory-findings-that-shaped-your-story-and-data-cleaning)
- [Section 3 Storyboard](#section-3-storyboard)
- [Section 4 Visualisations](#section-4-visualisations)
  - [Visualisation 1](#visualisation-1)
  - [Visualisation 2](#visualisation-2)
  - [Visualisation 3](#visualisation-3)
- [Section 5 Written Justification](#section-5-written-justification)

``` r
# Load your packages here
library(tidyverse)
library(dplyr)
library(patchwork)
library(readr)
library(ggmosaic)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Add any additional packages using library() only, not install.packages()
```

------------------------------------------------------------------------

# Section 1 Data Description, Aquisition, and Wrangling

*The dataset contains location and characteristics of crashes within
Queensland for all reported Road Traffic Crashes 1 January 2001 to 30
June 2025 for fatal and non-fatal crashes*

*The data was downloaded from QLD government open data porta* *url-
<https://www.data.qld.gov.au/dataset/crash-data-from-queensland-roads/resource/e88943c0-5968-4972-a15f-38e120d72ec0?inner_span=True>*

``` r
# Load the data
crash_data <- read_csv("data\\crash_locations.csv")
# Example: my_data <- read_csv("data/my_dataset.csv")
```

``` r
# Get an overview of the dataset
glimpse(crash_data)
```

    ## Rows: 408,271
    ## Columns: 52
    ## $ Crash_Ref_Number                <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,…
    ## $ Crash_Severity                  <chr> "Medical treatment", "Property damage …
    ## $ Crash_Year                      <dbl> 2006, 2007, 2008, 2007, 2007, 2008, 20…
    ## $ Crash_Month                     <chr> "November", "August", "May", "August",…
    ## $ Crash_Day_Of_Week               <chr> "Saturday", "Wednesday", "Saturday", "…
    ## $ Crash_Hour                      <dbl> 17, 9, 17, 2, 1, 6, 9, 9, 15, 13, 4, 4…
    ## $ Crash_Nature                    <chr> "Hit parked vehicle", "Rear-end", "Hit…
    ## $ Crash_Type                      <chr> "Single Vehicle", "Multi-Vehicle", "Si…
    ## $ Crash_Longitude                 <dbl> 153.5497, 153.0551, 153.4071, 146.7124…
    ## $ Crash_Latitude                  <dbl> -28.16524, -27.52444, -28.04818, -19.2…
    ## $ Crash_Street                    <chr> "Boundary St", "South East Arterial Rd…
    ## $ Crash_Street_Intersecting       <chr> NA, NA, "Southport - Burleigh Rd", NA,…
    ## $ State_Road_Name                 <chr> NA, "South-East Arterial Road (Pacific…
    ## $ Loc_Suburb                      <chr> "Coolangatta", "Holland Park West", "M…
    ## $ Loc_Local_Government_Area       <chr> "Gold Coast City", "Brisbane City", "G…
    ## $ Loc_Post_Code                   <chr> "4225", "4121", "4218", "4818", "4514"…
    ## $ Loc_Police_Division             <chr> "Coolangatta", "Holland Park", "Broadb…
    ## $ Loc_Police_District             <chr> "Gold Coast", "South Brisbane", "Gold …
    ## $ Loc_Police_Region               <chr> "South Eastern", "Brisbane", "South Ea…
    ## $ Loc_Queensland_Transport_Region <chr> "SEQ South", "SEQ South", "SEQ South",…
    ## $ Loc_Main_Roads_Region           <chr> "South Coast", "Metropolitan", "South …
    ## $ Loc_ABS_Statistical_Area_2      <chr> "Coolangatta", "Tarragindi", "Mermaid …
    ## $ Loc_ABS_Statistical_Area_3      <chr> "Coolangatta", "Nathan", "Broadbeach -…
    ## $ Loc_ABS_Statistical_Area_4      <chr> "Gold Coast", "Brisbane - South", "Gol…
    ## $ Loc_ABS_Remoteness              <chr> "Major Cities", "Major Cities", "Major…
    ## $ Loc_State_Electorate            <chr> "Currumbin", "Miller", "Mermaid Beach"…
    ## $ Loc_Federal_Electorate          <chr> "Mcpherson", "Moreton", "Mcpherson", "…
    ## $ Crash_Controlling_Authority     <chr> "Locally-controlled", "State-controlle…
    ## $ Crash_Roadway_Feature           <chr> "No Roadway Feature", "No Roadway Feat…
    ## $ Crash_Traffic_Control           <chr> "No traffic control", "No traffic cont…
    ## $ Crash_Speed_Limit               <chr> "0 - 50 km/h", "100 - 110 km/h", "60 k…
    ## $ Crash_Road_Surface_Condition    <chr> "Sealed - Dry", "Sealed - Dry", "Seale…
    ## $ Crash_Atmospheric_Condition     <chr> "Clear", "Clear", "Raining", "Clear", …
    ## $ Crash_Lighting_Condition        <chr> "Daylight", "Daylight", "Darkness - Li…
    ## $ Crash_Road_Horiz_Align          <chr> "Straight", "Straight", "Straight", "S…
    ## $ Crash_Road_Vert_Align           <chr> "Grade", "Level", "Level", "Level", "L…
    ## $ Crash_DCA_Code                  <chr> "703", "301", "708", "003", "703", "30…
    ## $ Crash_DCA_Description           <chr> "Off Path-Straight: Left Off Cway Hit …
    ## $ Crash_DCA_Group_Description     <chr> "16: Off Carriageway on Straight Hit O…
    ## $ DCA_Key_Approach_Dir            <chr> "E", "N", "S", "E", "N", "N", "W", "N"…
    ## $ Count_Casualty_Fatality         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Count_Casualty_Hospitalised     <dbl> 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0,…
    ## $ Count_Casualty_MedicallyTreated <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Count_Casualty_MinorInjury      <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,…
    ## $ Count_Casualty_Total            <dbl> 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1,…
    ## $ Count_Unit_Car                  <dbl> 2, 2, 0, 1, 0, 1, 1, 2, 1, 2, 1, 1, 2,…
    ## $ Count_Unit_Motorcycle_Moped     <dbl> 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Count_Unit_Truck                <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Count_Unit_Bus                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Count_Unit_Bicycle              <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Count_Unit_Pedestrian           <dbl> 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0,…
    ## $ Count_Unit_Other                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,…

``` r
range(crash_data$Crash_Year)
```

    ## [1] 2001 2024

*There 52 columns in the dataset and 408,271 rows. Some columns that are
certainly not going to be used are to be removed*.

*There are several columns dedicated to information pertaining to
geographical location. As far as the scope of this study is concerned we
are only interested in local government area, statistical area 3, other
columns related to location were omitted. Statistical area 3 was chosen
as this is classification used for town planning,traffic analysis and
infrastructure development*.

``` r
# use select function to retain only the required columns

crash_filtered <- crash_data |>
  select( "Crash_Ref_Number","Crash_Severity", "Crash_Year", "Crash_Month", "Crash_Day_Of_Week", "Crash_Hour", "Loc_Local_Government_Area","Loc_ABS_Statistical_Area_3", "Crash_Roadway_Feature",)

glimpse(crash_filtered)
```

    ## Rows: 408,271
    ## Columns: 9
    ## $ Crash_Ref_Number           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, …
    ## $ Crash_Severity             <chr> "Medical treatment", "Property damage only"…
    ## $ Crash_Year                 <dbl> 2006, 2007, 2008, 2007, 2007, 2008, 2007, 2…
    ## $ Crash_Month                <chr> "November", "August", "May", "August", "Jun…
    ## $ Crash_Day_Of_Week          <chr> "Saturday", "Wednesday", "Saturday", "Sunda…
    ## $ Crash_Hour                 <dbl> 17, 9, 17, 2, 1, 6, 9, 9, 15, 13, 4, 4, 18,…
    ## $ Loc_Local_Government_Area  <chr> "Gold Coast City", "Brisbane City", "Gold C…
    ## $ Loc_ABS_Statistical_Area_3 <chr> "Coolangatta", "Nathan", "Broadbeach - Burl…
    ## $ Crash_Roadway_Feature      <chr> "No Roadway Feature", "No Roadway Feature",…

``` r
# check for NA values

colSums(is.na(crash_filtered))
```

    ##           Crash_Ref_Number             Crash_Severity 
    ##                          0                          0 
    ##                 Crash_Year                Crash_Month 
    ##                          0                          0 
    ##          Crash_Day_Of_Week                 Crash_Hour 
    ##                          0                          0 
    ##  Loc_Local_Government_Area Loc_ABS_Statistical_Area_3 
    ##                          0                          0 
    ##      Crash_Roadway_Feature 
    ##                          0

- There are no ‘NA’ values seen.

<!-- 
&#10;-->

------------------------------------------------------------------------

# Section 2 Exploratory Findings That Shaped Your Story and Data cleaning

*Exploratory analysis was done to identify the trend over time in the
incidence of accidents across whole of QLD *.

``` r
# Exploratory data analysis

crash_filtered|>
  count(Crash_Year) |>
  ggplot(aes(x = Crash_Year, y = n)) +
  geom_col()
```

![](major_project1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

*It appears there was a sudden drop in number of crashes post 2010. This
prompted further investigation.Comparison was made to check if there are
any changes in how crashes are recorded*.

``` r
# Exploratory data analysis

crash_filtered |>
  filter(Crash_Year <= "2010")|>
  distinct(Crash_Severity)
```

    ## # A tibble: 5 × 1
    ##   Crash_Severity      
    ##   <chr>               
    ## 1 Medical treatment   
    ## 2 Property damage only
    ## 3 Hospitalisation     
    ## 4 Minor injury        
    ## 5 Fatal

``` r
crash_filtered |>
  filter(Crash_Year >= "2011")|>
  distinct(Crash_Severity)
```

    ## # A tibble: 4 × 1
    ##   Crash_Severity   
    ##   <chr>            
    ## 1 Medical treatment
    ## 2 Hospitalisation  
    ## 3 Minor injury     
    ## 4 Fatal

*Looking at the count of unique values it is identified that crashes
resulting in ‘property damage only’ was not being recorded post 2010.
This is further confirmed in the description provided in the data
source.It was also observed the total number of crashes remained mostly
steady until 2020, then there is a gradual upward trend. Therefore,
further exploration focused only on data from 2020 on wards*.

``` r
# Data cleaning and wrangling

crash_filtered <- crash_filtered |>
  filter(Crash_Year >= 2020) 
range(crash_filtered$Crash_Year)
```

    ## [1] 2020 2024

*For the purposes of this analysis we will use only two categories of
severity, “Major” and “Minor”. Crashes that were fatal or required
hospitalization will be clubbed together to form “Major” category and
others will be “Minor”*

``` r
# Data cleaning and wrangling

unique(crash_filtered$Crash_Severity)
```

    ## [1] "Medical treatment" "Minor injury"      "Hospitalisation"  
    ## [4] "Fatal"

``` r
crash_filtered <- crash_filtered |>
  mutate(Crash_Severity = case_when(
    Crash_Severity == "Fatal" ~ "Major",
    Crash_Severity == "Hospitalisation" ~ "Major",
    Crash_Severity == "Medical treatment" ~ "Minor",
    Crash_Severity == "Minor injury" ~ "Minor"
  ))
 
unique(crash_filtered$Crash_Severity)
```

    ## [1] "Minor" "Major"

*The next obvious question was where does most accidents happen. The
data was aggregated at LGA levels based on crash location*.

``` r
# Exploratory data analysis

crash_filtered |>
  select(Loc_Local_Government_Area)|>
  group_by(Loc_Local_Government_Area)|>
  summarise(Total_crash = n())|>
  arrange(desc(Total_crash))|>
  slice_head(n =5)|>
  ggplot(aes(x = Loc_Local_Government_Area, y = Total_crash))+
  geom_col()
```

![](major_project1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

*It is very obvious that Brisbane city council is the biggest
contributor to road accidents*. *Further exploration was done into crash
data of Brisbane alone to identify seasonal/hourly trends*.

``` r
# filter the data frame to include only Brisbane local government data

crash_brisbane <- crash_filtered |>
  filter(Loc_Local_Government_Area =="Brisbane City")
```

*Further exploration into geographical distribution of crashes within
the Brisbane LGA, data aggregated at statistical area 3(sa3) division.
sa3 column was used to aggregate data as it a standard ABS
classification used for statistical analysis, regional planning and
infrastructure development*.

``` r
# Exploratory data analysis

crash_brisbane |>
  count(Loc_ABS_Statistical_Area_3) |>
  mutate(proportion = n / sum(n)) |>
  arrange(desc(proportion)) |>
  slice_max(order_by = proportion, n = 5) |>
  ggplot(aes(x = Loc_ABS_Statistical_Area_3, y = proportion)) +
  geom_col()+
  scale_y_continuous(labels = scales::percent)
```

![](major_project1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Out of all sa3 divisions in Brisbane LGA, sa3 Brisbane inner alone
accounts for nearly 12 percent of total accidents, despite being
smallest geographically.Further exploration therefore zoomed into
patterns within sa3 - Brisbane inner.

``` r
# Exploratory data analysis

crash_seasonal<-crash_brisbane |>
  mutate(
    Crash_Month = factor(
      Crash_Month,levels = c (
        "January", "February", "March", "April","May", "June", "July", "August","September", "October", "November", "December")
      )
    )|>
  filter(Loc_ABS_Statistical_Area_3 == "Brisbane Inner")|>
  group_by(Crash_Year,Crash_Month)|>
  summarise(count = n(),.groups = "drop")
  
crash_seasonal |>
  mutate(Crash_Year = as.factor(Crash_Year))|>
  ggplot(aes(x = Crash_Month, y = count, group = Crash_Year, colour = Crash_Year))+
  geom_line()
```

![](major_project1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

*Crash incidence seems to be higher in the latter half of the year*.

``` r
# Exploratory data analysis

crash_hour <- crash_brisbane |>
  filter(Loc_ABS_Statistical_Area_3 == "Brisbane Inner")|>
  ggplot(aes(x= Crash_Hour))+
  geom_histogram(binwidth = 1,boundary = 0, fill = "#36454f", colour = "white")+
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Crash incidence is higher in the early morning and late afternoon",
    x = "Hour",
    y = "Crash Count"
  ) +
  theme_minimal()
  
crash_hour  
```

![](major_project1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Exploratory data analysis

crash_day <- crash_brisbane |>
  filter(Loc_ABS_Statistical_Area_3 == "Brisbane Inner")|>
  mutate(Crash_Day_Of_Week = factor(Crash_Day_Of_Week, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))|>
  
  ggplot(aes(x= Crash_Day_Of_Week))+
  geom_bar(fill = "#36454F")+
  labs(
    title = "Crash incidence is highest on Friday"
  )+
  theme_minimal()

crash_day
```

![](major_project1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# Exploratory data analysis

crash_brisbane |>
  filter(Loc_ABS_Statistical_Area_3=="Brisbane Inner")|>
  group_by(Crash_Roadway_Feature) |>
  summarise(Count= n())|>
  arrange(desc(Count))
```

    ## # A tibble: 10 × 2
    ##    Crash_Roadway_Feature      Count
    ##    <chr>                      <int>
    ##  1 No Roadway Feature           826
    ##  2 Intersection - Cross         435
    ##  3 Intersection - T-Junction    426
    ##  4 Intersection - Interchange    86
    ##  5 Bridge/Causeway               75
    ##  6 Merge Lane                    15
    ##  7 Bikeway                       13
    ##  8 Intersection - Roundabout     13
    ##  9 Intersection - 5+ way         11
    ## 10 Intersection - Y-Junction      2

*Most accidents seem to happen mid block rather than intersection.
However intersections are scattered across different
categories.Intersection was consolidated into single category get a
broader picture*.

``` r
crash_feature<-crash_brisbane|>
  filter(Loc_ABS_Statistical_Area_3=="Brisbane Inner")|>
  mutate(
    Crash_Roadway_Feature = case_when(
     Crash_Roadway_Feature %in% c(
        "Intersection - Cross","Intersection - T-Junction","Intersection - Interchange","Intersection - 5+ way","Intersection - Roundabout","Intersection - Y-Junction"
      ) ~ "Intersection" , TRUE~Crash_Roadway_Feature
    )
  )

crash_feature |>
  group_by(Crash_Roadway_Feature) |>
  summarise(Count= n())|>
  arrange(desc(Count))
```

    ## # A tibble: 5 × 2
    ##   Crash_Roadway_Feature Count
    ##   <chr>                 <int>
    ## 1 Intersection            973
    ## 2 No Roadway Feature      826
    ## 3 Bridge/Causeway          75
    ## 4 Merge Lane               15
    ## 5 Bikeway                  13

Looking at broader picture, most crashes are happening at intersections.

<!-- 
&#10;
-->

``` r
# Any exploratory plots or summaries you want to include here
```

*Your written EDA reflection goes here. Aim for a short paragraph or
two.*

*There has been a gradual rise in number of accidents since 2020 across
QLD.Brisbane city local government area tops the chart in the number of
road accidents in QLD.Within the Brisbane Local government area, data
was aggregated at statistical area 3(sa3) divisions. This revealed
crashes in Brisbane are heavily concentrated in ‘Brisbane inner’ sa3
despite it being geographically smaller than other sa3 divisions.
Temporal analysis shows Crash incidence is higher during the weekdays
and during peak traffic hours in the early morning and late
afternoon.Most crashes happen at intersections.The monthly variations
did not make into the final visuals as it was less likely to lead into
any actionable insight and hourly pattern was already providing a strong
temporal dimension.*

------------------------------------------------------------------------

# Section 3 Storyboard

<!-- 
&#10;-->

``` r
knitr::include_graphics("images/storyboard.jpg")
```

<img src="images/storyboard.jpg" alt="" width="1221" />

------------------------------------------------------------------------

# Section 4 Visualisations

<!-- 
&#10;-->

## Visualisation 1

``` r
# aggregate the crash count at sa 3 level

crash_map <- crash_brisbane |>
  count(Loc_ABS_Statistical_Area_3)|>
  mutate(Proportion = (n/sum(n)))

# shapefile of sa3 boundaries in Brisbane dowloaded from https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs/edition-3-july-2021-june-2026/access-and-downloads/digital-boundary-files#downloads-for-gda94-digital-boundary-files

# create base map of Brisbane with statistical area 3 boundaries

sa3 <- st_read("data\\SA3\\SA3_2021_AUST_GDA94.shp")    #read the shape file
```

    ## Reading layer `SA3_2021_AUST_GDA94' from data source 
    ##   `D:\QUT\R Projects\Portfolio\Files_for_submission\Final_project\data\SA3\SA3_2021_AUST_GDA94.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 359 features and 14 fields (with 19 geometries empty)
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 96.81694 ymin: -43.74051 xmax: 167.998 ymax: -9.142176
    ## Geodetic CRS:  GDA94

``` r
sa3_filtered<-sa3|>          #    filter to just QLD data and drop unnecessary columns
  filter(STE_NAME21=="Queensland")|>
  select(SA3_NAME21)        

# shape file with sa3 boundaries do not have LGA boundaries, separate shape file for LGA boundaries for whole of QLD obtained from 
# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs/edition-3-july-2021-june-2026/access-and-downloads/digital-boundary-files#downloads-for-gda94-digital-boundary-files

lga<-st_read("data\\LGA\\LGA_2025_AUST_GDA94.shp") # read shape file with LGA boundaries
```

    ## Reading layer `LGA_2025_AUST_GDA94' from data source 
    ##   `D:\QUT\R Projects\Portfolio\Files_for_submission\Final_project\data\LGA\LGA_2025_AUST_GDA94.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 567 features and 7 fields (with 19 geometries empty)
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 96.81694 ymin: -43.74051 xmax: 167.998 ymax: -9.142176
    ## Geodetic CRS:  GDA94

``` r
lga_brisbane<-lga|>         #     filter to just Brisbane data and drop unnecessary columns
  filter(LGA_NAME25=="Brisbane")|>
  select(LGA_NAME25)

brisbane_lga <-st_intersection(sa3_filtered,lga_brisbane) # clip the sa3 shape file to show only the Brisbane LGA area, intersection keeps only the overlapping area

crash_joined <- left_join(brisbane_lga,crash_map,by = c("SA3_NAME21" = "Loc_ABS_Statistical_Area_3")) # create new sf object combining spatial and non-spatial data
```

``` r
ggplot(crash_joined) +
  geom_sf(aes(fill = Proportion)) +
  scale_fill_gradientn(name = "Proportion of Crashes",
  colours = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C"),
  na.value = "transparent",
  labels=scales::percent
) +
  labs(title = "Road crashes in Brisbane LGA are heavily concentrated in inner city",
       subtitle = "Data from 2020 to 2024 aggregated by statistical area 3(sa3) ",
       x = NULL, y = NULL) +
  
  coord_sf(
  xlim = c(152.7, 153.5),
  ylim = c(-27.7, -27.25),
  expand = FALSE)+
  theme_void()+
  theme(
    legend.position = "left",
    legend.title = element_text(size = 14),
    plot.title = element_text(hjust=0.5,margin = margin(b = 20),size = 20,face = "bold"),
    plot.subtitle = element_text(hjust=0.5,margin = margin(b = 10),size = 18)
    )
```

![](major_project1_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Visualisation 2

``` r
 crash_feature|>
  
  ggplot(
       aes(x = fct_rev(fct_infreq(Crash_Roadway_Feature)),
           fill = Crash_Severity)) +
  geom_bar(width = 0.4) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most crashes in sa3-Brisbane Inner happen at intersections",
    x = NULL,
    y = "Number of Crashes"
  )+
  
  scale_fill_manual(name = "Crash Severity",values=c("firebrick","orange"),
                    labels = c("Major - hospitalisation / fatal","Minor-minor injury / medical attention"))+
  theme(panel.grid = element_blank())+
  theme(
    legend.title = element_text(size = 14),
    plot.title = element_text(hjust=-0.2,margin = margin(b = 20),size = 20,face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16)
    )
```

![](major_project1_files/figure-gfm/vis-2-1.png)<!-- -->

## Visualisation 3

``` r
crash_brisbane|>
  filter(Loc_ABS_Statistical_Area_3 == "Brisbane Inner")|>
  mutate(Crash_Day_Of_Week = factor(Crash_Day_Of_Week,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))|>
  group_by(Crash_Day_Of_Week,Crash_Hour)|>
  mutate(Hourly_Total = n())|>
  ggplot(aes(x= Crash_Hour,y= fct_rev(Crash_Day_Of_Week), fill = Hourly_Total))+
  geom_tile()+
  scale_fill_gradientn(name ="Hourly Total",
  colours = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C"))+
  labs(title = "Crash risk in Brisbane Inner peaks on Friday afternoons",
       x  = "Crash Hour", y = NULL)+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.1,size = 20,face = "bold",margin = margin(b=10)),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16)
    )
```

![](major_project1_files/figure-gfm/vis-3-1.png)<!-- -->

# Section 5 Written Justification

*A choropleth map was selected to visualise the relative concentration
of crashes across SA3 regions in Brisbane. As the primary audience is
Brisbane City Council, it is reasonable to assume a strong existing
familiarity with Brisbane’s geography and statistical divisions,
enabling viewers to interpret the spatial patterns quickly and
accurately. The choropleth format allows immediate comparison between
Brisbane Inner and surrounding SA3 regions, making regional differences
apparent immediately*.

*Colour intensity serves as a highly effective pre-attentive visual
attribute, enabling users to rapidly identify areas with higher crash
concentrations without extensive cognitive effort. The visualisation
focuses on a single key measure across geographic regions, limiting the
number of variables displayed and therefore reducing cognitive load.
This directs attention to the central insight that crashes are
disproportionately concentrated in Brisbane Inner. Using semantic colour
scheme was a key design decision, aligning with general perception of
severity*.

*Furthermore, the choropleth effectively communicates that, despite its
relatively small geographic area, Brisbane Inner accounts for the
highest proportion of crashes among all SA3 divisions. By combining
geographic context with quantitative comparison, the map provides a
clear and intuitive representation of the spatial distribution of crash
concentration*.

*Another key design decisions were to avoid labelling the sa3 divisions
as Brisbane Council officials are expected to be able to recognise them
looking at the map. Especially with Brisbane River running through
middle of the city, it is easily recognisable and avoiding labels reduce
clutter*.

*A stacked bar chart was chosen to visualise the distribution of crashes
across different roadway feature categories. Given that the intended
audience is Brisbane City Council, it is reasonable to assume
familiarity with interpreting bar charts. The pre-attentive attribute of
length enables rapid comparison between categories, clearly highlighting
that intersections account for the highest number of crashes. Stacking
the bars further allows viewers to assess both the total number of
crashes and the relative contribution of major and minor crashes within
each roadway feature category. A semantic colour scheme was applied,
with red representing major crashes and orange representing minor
crashes. This is consistent with common perception of severity*.

*A heatmap was selected to visualise the hourly distribution of crashes
because it effectively highlights temporal patterns and variations in
crash frequency across different times of the day. The pre-attentive
attribute of colour intensity allows viewers to quickly distinguish
between low- and high-crash periods without the need to compare
individual values. Identifying temporal patterns and variations is the
priority here and precision is not required. This makes heatmap an ideal
choice. Also, the heatmap is a compact way of displaying the
inter-connection between three variables, which cannot be achieved using
other chart types*.

<!-- 
&#10;-->
