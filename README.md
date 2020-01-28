# The Path of Least Resistance
Scripts and data used to produce Sandbag's report: The Path of Least Resistance
The report analyses electricity trade between EU ETS countries and connected non-EU countries. 
Our aim was to investigate possible carbon leakage in the EU Emissions Trading Scheme, through electricity interconnectors. 

## Method
We aggregate electricity flows by country, estimate how much carbon was emitted in generating the traded electricity, 
and hence the carbon revenues avoided by electricity imports into EU ETS countries from non-EU countries

## Data
The principle data source is ENTSO-E. Transmission and price data was extracted from the ENTSO-E transparency platform. 
The emissions factors provided are estimates of the annual average for each country considered in the analysis. 
They were calculated by dividing annual power sector emissions (EDGAR v5) by annual power generation (IEA), by country. 
The EU ETS price data was acquired via the [Sandbag website](https://sandbag.org.uk/carbon-price-viewer/).

## Units
- Time zone: CET
- Electricity: TWh
- Carbon emissions: MtCO2
- Value: million Euro

## Collaboration
Our main motive for sharing these calculations is transparency. However, if you're interested in understanding more, or expanding on our analysis, we'd welcome your ideas!
