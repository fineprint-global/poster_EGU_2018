# EGU 2018 Poster

Unfortunately, we cannot make SNL Metals & Mining Data available in our repository. The data can be acquired from S&P Global Market Intelligence:

## Use web based screener tool
* Go to https://www.snl.com/web/client?auth=inherit#office/screener

## Apply search criteria and select variables
* Restrict search criteria to commodities copper, nickel and iron ore
* Select fields (i.e. variables):
  + Property Name	
  + Property ID	
  + Primary Commodity	
  + Activity Status	
  + Country Name	
  + Latitude (degrees)
  + Longitude (degrees)
  + List of Commodities	
  + Commodity Production - tonne[2005Y|(Best Of)|(Copper)]
  + Commodity Production - tonne[2005Y|(Best Of)|(Iron Ore)]
  + Commodity Production - tonne[2005Y|(Best Of)|(Nickel)]
* Export "as values" to .xls and leave the file unchanged  

* Restrict search criteria to commodity gold
* Select fields as above but replace "Commodity Production" field by
  + Commodity Production - oz[2005Y|(Best Of)|(Gold)]
* Export "as values" to .xls and leave the file unchanged


## Import .xls as obtained from web based S&P screener tool to R
```{R} 
data_raw <- readxl::read_excel(file.path(path, filename), na = c("na", "Na", "NA")) 
data_raw_gold <- readxl::read_excel(file.path(path, filename), na = c("na", "Na", "NA"))
```

## Tidy the data using snl_tidy()
```{R} 
data_raw <- snl_tidy(data_raw, meas.unit = "tons")
data_raw_gold <- snl_tidy(data_raw_gold, meas.unit = "oz")
```
