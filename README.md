# service-area-census-estimates

## things you need

Add Data folder from SharePoint, [here](https://txriogrande.sharepoint.com/sites/DataMapsTRLA2/Shared%20Documents/Forms/AllItems.aspx?viewid=ae692460%2D6432%2D478b%2Da201%2D7e2c3bab4667&id=%2Fsites%2FDataMapsTRLA2%2FShared%20Documents%2FProjects%2FService%20Area%20Estimates).

You'll also need a Census API key, which you can get [here](https://api.census.gov/data/key_signup.html).

## what this is for

Use the Census API and the tidycensus package to pull Census data on poverty, race, race & poverty, English proficiency, and educational attainment, specifically in the TRLA service area.

### Attys_Offices_Counties.R

This script creates maps that compare TRLA attorney staffing to the poverty population in each county. It also groups the counties into regions and runs similar calculations by region. It then creates maps to display this information. This was a RD request.

### Closed Cases 2020

This script maps our closed cases by county and compares them to the poverty population in each county, by number and by proportion. This was used for the Annual Report.

### LEP_race_income

This was pulling Census data for low English proficiency (LEP), race, and income in all of Texas. This was specifically requested for a set of comments, and the data was exported and then used in ArcGIS Pro.

### Poverty Estimates / Poverty Estimates with Geo

Pull poverty data and export as csv/xlsx OR map it. 

### Race Ethnicity Estimates

Generate data on race, ethnicity, and poverty in the TRLA service area.

