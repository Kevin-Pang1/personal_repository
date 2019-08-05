

library(tidycensus)
library(tidyverse)
library(leaflet)
library(tigris)
library(reshape2)



# Enter API key
census_api_key(api_key)

most_recent_year <- 2017


# Get Tract to PUMA mapping 
tract_to_puma <- read.csv('census_puma.csv')
tract_to_puma$GEOID <- as.character(tract_to_puma$GEOID)

# Get transit access data
transport_access <- read.csv('stop_access_population.csv') %>% mutate(GEOID = as.character(GEOID10))

# Margin of Error for Proportion formula (90% CI)
moe <- function(x,xe,y,ye){if_else((xe/1.645)^2-(x^2/y^2)*(ye/1.645)^2>0,
                            1/y*sqrt((xe/1.645)^2-(x^2/y^2)*(ye/1.645)^2),
                       1/y*sqrt((xe/1.645)^2+(x^2/y^2)*(ye/1.645)^2))*qnorm(.95)*100}

# Get median rent at county level
datalist <- list()

for (year in c(2012: most_recent_year)) {
  
  mydatadf <- get_acs(geography = "county", table = 'B25064', state = "27", county = "053", year = year, output = 'wide', survey='acs1')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
henn_rent <- bind_rows (datalist) %>% 
  mutate(median_rent = .[[3]]) %>%
  select(year, GEOID, NAME, median_rent)


# Get Tenancy Data from ACS
datalist <- list()

for (year in c(most_recent_year-5,most_recent_year)) {

  mydatadf <- get_acs(geography = "tract", table = 'B25008', state = "27", county = "053", year = year, output = 'wide')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
pct_rental_units_tract <- bind_rows (datalist) %>% 
  mutate(pct_rental = .[[7]]/ .[[3]] * 100, pct_rental_moe = moe(.[[7]],.[[8]],.[[3]],.[[4]])) %>%
  select(year, GEOID, NAME, pct_rental, pct_rental_moe)


# Get Racial Breakdown    
datalist <- list()

for (year in c(most_recent_year-5,most_recent_year)) {
  
  mydatadf <- get_acs(geography = "tract", table = 'B02001', state = "27", county = "053", year = year, output = 'wide')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
race <- bind_rows (datalist) %>% mutate(pct_non_white = rowSums(.[seq(7,21,2)])/ .[[3]]*100,
                                        non_white_moe = sqrt(rowSums(.[seq(8,22,2)]^2))) %>%
  mutate(pct_non_white_moe = moe(rowSums(.[seq(7,21,2)]),non_white_moe,.[[3]],.[[4]])) %>% select(year,GEOID,NAME,pct_non_white, pct_non_white_moe)


# Get Educational Attainment Data
datalist <- list()

for (year in c(most_recent_year-5,most_recent_year)) {
  
  mydatadf <- get_acs(geography = "tract", table = 'B15003', state = "27", county = "053", year = year, output = 'wide')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
education <- bind_rows (datalist) %>% mutate(pct_no_hsd = rowSums(.[seq(5,33,2)])/ .[[3]] * 100,
                                             no_hsd_moe = sqrt(rowSums(.[seq(6,34,2)]^2))) %>%
  mutate(pct_no_hsd_moe = moe(rowSums(.[seq(5,33,2)]),no_hsd_moe,.[[3]],.[[4]])) %>%
  select(year, GEOID, NAME, pct_no_hsd, pct_no_hsd_moe)

# Get Poverty Data
datalist <- list()
for (year in c(most_recent_year-5,most_recent_year)) {
  
  mydatadf <- get_acs(geography = "tract", table = 'B17001', state = "27", county = "053", year = year, output = 'wide')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
poverty <- bind_rows (datalist) %>% mutate(pct_poverty = .[[5]]/ .[[3]] * 100,
                                           pct_poverty_moe = moe(.[[5]],.[[6]],.[[3]],.[[4]])) %>%
  select(year, GEOID, NAME, pct_poverty, pct_poverty_moe)

# Get Rent Burden Data
datalist <- list()
for (year in c(most_recent_year-5,most_recent_year)) {
  
  mydatadf <- get_acs(geography = "tract", table = 'B25106', state = "27", county = "053", year = year, output = 'wide', cache_table = T)
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
rent_burden <- bind_rows (datalist) %>% mutate(burden30 = rowSums(.[seq(57,89,8)]), 
                                               burden30_moe = sqrt(rowSums(.[seq(58,90,2)]^2))) %>%
  mutate(pct_burden = burden30 / .[[49]] * 100, pct_burden_moe = moe(burden30, burden30_moe, .[[49]],.[[50]])) %>% 
  select(year, GEOID, NAME, pct_burden, pct_burden_moe)


# Get Overcrowding Data
datalist <- list()
for (year in c(most_recent_year-5,most_recent_year)) {
  
  mydatadf <- get_acs(geography = "tract", table = 'B25014', state = "27", county = "053", year = year, output = 'wide')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
crowding <- bind_rows (datalist) %>% mutate(pct_overcrowd = rowSums(.[seq(23,27,2)])/ .[[17]] * 100,
                                             overcrowd_moe = sqrt(rowSums(.[seq(24,28,2)]^2))) %>%
  mutate(pct_overcrowd_moe = moe(rowSums(.[seq(23,27,2)]),overcrowd_moe,.[[17]],.[[18]])) %>%
  select(year, GEOID, NAME, pct_overcrowd, pct_overcrowd_moe)

# Test for Statistical Significant Difference at 90% Confidence
sig <- function(x,xe,y,ye){
  if_else(abs(x-y) > 1.645 * sqrt((xe/1.645)^2 + (ye/1.645)^2),1,0)
}

# Get Affordable Housing Data for Households making <$50k
datalist <- list()
for (year in c(most_recent_year-5,most_recent_year)) {
  
  mydatadf <- get_acs(geography = "tract", table = 'B25106', state = "27", county = "053", year = year, output = 'wide')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
afford_housing <- bind_rows(datalist) %>% mutate(total_low_units = .[[51]] + .[[59]] + .[[67]] ,
                                            total_unit_moe = sqrt(.[[52]]^2 + .[[60]]^2 + .[[68]]^2),
                                            afford_units =  .[[53]] + .[[55]] + .[[61]] + .[[63]] + .[[69]] + .[[71]],
                                            afford_units_moe =  sqrt(.[[54]]^2 + .[[56]]^2 + .[[64]]^2 + .[[70]]^2 + .[[72]]^2)) %>%
  select(year,GEOID,NAME,total_low_units,total_unit_moe,afford_units,afford_units_moe)


# Split 2 periods to calculate difference and check signifcance
afford_recent <- afford_housing %>% filter(year==most_recent_year) 
afford_last <- afford_housing %>% filter(year==most_recent_year-5) %>% left_join(afford_recent, by = 'GEOID') %>% 
  mutate(diff = afford_units.y - afford_units.x,significant = sig(afford_units.y, afford_units_moe.y, afford_units.x, afford_units_moe.y))


# Get reported rent data
datalist <- list()
for (year in c(most_recent_year-5,most_recent_year)) {
  
  mydatadf <- get_acs(geography = "tract", table = 'B25063', state = "27", county = "053", year = year, output = 'wide')
  mydatadf$year <- year 
  datalist[[year]] <- mydatadf
}
rent <- bind_rows (datalist) %>% left_join(tract_to_puma[,c('GEOID','PUMA')], by=c('GEOID'))

rent[is.na(rent)] <- 0;  

rent2 <- rent %>% select(-NAME,-GEOID) %>% group_by(year,PUMA) %>% summarise_at(vars(-group_cols()),sum) 
  
  select(year,PUMA,seq(7,53,2))


# Rename columns to use midpoints of each rent bracket
colnames(rent2) <- c('YEAR', 'PUMA', 'Less 100', 'R125', 'R175', 'R225', 'R275', 'R325','R375','R425','R475','R525',
                     'R575', 'R625', 'R675','R725','R775','R850','R950','R1125', 'R1375','R1750',
                     'R2250', 'R2750','R3250', 'More 3500')
# convert wide to long 
rent3 <- melt(rent2, id=c('YEAR', 'PUMA')) %>% arrange(YEAR,PUMA)

# Merge all tables 
final <- education %>% left_join(poverty, by = c('year', 'GEOID')) %>% left_join(race, by = c('year', 'GEOID')) %>%
  left_join(rent_burden, by = c('year', 'GEOID')) %>% left_join(pct_rental_units_tract, by = c('year', 'GEOID')) %>%
  left_join(crowding, by= c('year', 'GEOID')) %>% left_join(transport_access, by = c('GEOID')) %>%
  select(year, GEOID,NAME.x, pct_no_hsd, pct_no_hsd_moe, pct_poverty, pct_poverty_moe ,pct_non_white, pct_non_white_moe,
         pct_burden, pct_burden_moe, pct_rental, pct_rental_moe, pct_overcrowd, pct_overcrowd_moe, stop.ratio) 

final$stop.ratio[is.na(final$stop.ratio)] <- 0;  


# Calculate risk measure and margin of error
final_recent <- final %>% filter(year==most_recent_year) %>% mutate(race_risk = if_else(pct_non_white > median(pct_non_white),1,0),
                                                    edu_risk = if_else(pct_no_hsd > median(pct_no_hsd),1,0),
                                                    poverty_risk = if_else(pct_poverty > median(pct_poverty),1,0),
                                                    rental_risk = if_else(pct_rental > median(pct_rental),1,0),
                                                    burden_risk = if_else(pct_burden > median(pct_burden, na.rm = TRUE),1,0),
                                                    overcrowd_risk = if_else(pct_overcrowd > median(pct_overcrowd, na.rm=TRUE),1,0),
                                                    transport_risk = if_else(stop.ratio < median(stop.ratio, na.rm=TRUE),1,0)) %>%
  mutate(race_risk_moe = if_else(race_risk == 1 & (pct_non_white - pct_non_white_moe < median(pct_non_white)), -1,0),
         edu_risk_moe = if_else(edu_risk == 1 & (pct_no_hsd - pct_no_hsd_moe < median(pct_no_hsd)),-1,0),
         poverty_risk_moe = if_else(poverty_risk == 1 & (pct_poverty - pct_poverty_moe < median(pct_poverty)),-1,0),
         rental_risk_moe = if_else(rental_risk == 1 & (pct_rental - pct_rental_moe < median(pct_rental)),-1,0),
         burden_risk_moe = if_else(burden_risk == 1 & (pct_burden - pct_burden_moe < median(pct_burden)),-1,0),
         overcrowd_risk_moe = if_else(pct_overcrowd == 1 & (pct_overcrowd - pct_overcrowd_moe < median(pct_overcrowd)),-1,0),
         race_risk_moe_p = if_else(race_risk == 0 & (pct_non_white + pct_non_white_moe > median(pct_non_white)), 1,0),
         edu_risk_moe_p = if_else(edu_risk == 0 & (pct_no_hsd + pct_no_hsd_moe > median(pct_no_hsd)),1,0),
         poverty_risk_moe_p = if_else(poverty_risk == 0 & (pct_poverty + pct_poverty_moe > median(pct_poverty)),1,0),
         rental_risk_moe_p = if_else(rental_risk == 0 & (pct_rental + pct_rental_moe > median(pct_rental)),1,0),
         burden_risk_moe_p = if_else(burden_risk == 0 & (pct_burden + pct_burden_moe > median(pct_burden)),1,0),
         overcrowd_risk_moe_p = if_else(pct_overcrowd == 0 & (pct_overcrowd + pct_overcrowd_moe > median(pct_overcrowd)),1,0)
         ) %>%
  mutate(risk_index = race_risk + edu_risk + poverty_risk + rental_risk + burden_risk + overcrowd_risk + transport_risk,
         index_moe = race_risk_moe+edu_risk_moe+ poverty_risk_moe+ rental_risk_moe+ burden_risk_moe+ overcrowd_risk_moe,
         index_moe_p = race_risk_moe_p+edu_risk_moe_p+ poverty_risk_moe_p+ rental_risk_moe_p+ burden_risk_moe_p+ overcrowd_risk_moe_p
         ) %>% mutate(risk_index_high = risk_index + index_moe_p, risk_index_low = risk_index + index_moe) %>%
  mutate(risk_range = risk_index_high - risk_index_low) %>% mutate(er = ifelse(risk_range>3, 2, 0))
  
        
# Load latest census tract shapefile
shapefile <- tracts(state='27', county='53', year = most_recent_year)

# Merge data with shapefile
mydatamerged <- geo_join(shapefile, final_recent, "GEOID", "GEOID")

df <- mydatamerged

mypopup <- paste0("GEOID: ", df$GEOID, "<br>", 
                  "Risk Index: ", df$risk_index,"<br>" )

mypal <- colorNumeric(
  palette = "Reds",
  domain = df$risk_index
)


myLAT <- 44.983705
myLNG <- -93.420759

# Generate map of risk measures
mymap<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(myLNG, myLAT, zoom = 10) %>%
  addPolygons(data = df, 
              fillColor = ~mypal(risk_index), 
              stroke = TRUE,
              color = 'black', # you need to use hex colors
              fillOpacity = 0.7, 
              weight = df$er, 
              smoothFactor = 0.2,
              popup = mypopup) %>%
  addLegend(pal = mypal, 
            values = df$risk_index, 
            position = "bottomright", 
            title = "Risk Index",
            labFormat = labelFormat(suffix = ""))

mymap


#### Affordable Units  


mydatamerged2 <- geo_join(shapefile, afford12, "GEOID", "GEOID")

df2 <- mydatamerged2

mypopup <- paste0("GEOID: ", df$GEOID, "<br>", 
                  "Chg in Affordable Units: ", df2$diff,"<br>"
                  )


rc1 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(max(df2$diff, abs(df2$diff)))

## Make vector of colors for values larger than 0 (180 colors)
rc2 <- colorRampPalette(colors = c("white", "green"), space = "Lab")(max(df2$diff, abs(df2$diff)))

## Combine the two color palettes
rampcols <- c(rc1, rc2)

previewColors(colorNumeric(palette = rampcols, domain = NULL), values = min(df2$diff):max(df2$diff))

mypal <- colorNumeric(
  palette = rampcols,
  domain = df2$diff
)

myLAT <- 44.983705
myLNG <- -93.420759


mymap<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(myLNG, myLAT, zoom = 10) %>%
  addPolygons(data = df2, 
              fillColor = ~mypal(diff*significant), 
              stroke = TRUE,
              color = 'black', # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = mypopup) %>%
  addLegend(pal = mypal, 
            values = df2$diff, 
            position = "bottomright", 
            title = "Chg in AH Units",
            labFormat = labelFormat(suffix = ""))

mymap



