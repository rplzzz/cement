# Predictive model for GHG emissions from cement production

## Description of data

The main data table is called master.table.  It is created from the
raw data by the assemble-data.R script and saved to the file
cement-data.dat.  Other programs look for the cement-data.dat file, so
once it's been created you don't need to run assemble-data.R anymore
unless the data changes.

Each line of the table has the data for one country, for one year.
Incomplete entries are trimmed out of the final table.  Any cement
production values of zero are treated as missing and filtered out of
the data.  The reason we do this is that we are assuming that all of
this production data is a proxy for consumption, and we don't really
think that any of these countries consumed no cement at all.

1. **ISO**:  The three-letter ISO code for the country.

2. **year**:  The year of the observation.

3. **pop.tot**:  The total population. <br>
   Units:  thousands<br> 
   Source: http://esa.un.org/unpd/wpp/Excel-Data/population.htm 

4. **urban.growth**:  Urban population growth.  This is actually the
   growth rate in the total urban population, so there is some
   population growth conflated.  In particular, urbanization fraction
   can still go down even when urban.growth > 0, if the non-urban
   population was growing even faster.<br>
   Units:  annual %<br>
   Source:  gapminder.org

5. **cement**:  CO2 emissions from cement production.  This is just the
   CO2 produced directly by calcining the limestone, not including any
   energy inputs.  As such, it is directly proportional to the amount
   of cement produced.<br>
   Units:  kt C<br>
   Source:  GCAM

6. **urban.pop**:  Urban population fraction<br>
   Units:  percent<br>
   Source:  gapminder.org

7. **GDP**:  Gross Domestic Product, Market Exchange Rate<br>
   Units:  billion 2005 US$
   Source:
   http://www.ers.usda.gov/datafiles/International_Macroeconomic_Data/Historical_Data_Files/HistoricalRealGDPValues.xls

8. **pcGDP**:  per-capita GDP<br>
   Units:  million 2005 US$/person<br>
   Source:  Internally calculated

9. **pccement**:  per-capita CO2 emissions from cement<br>
   Units:  Kt C/person<br>
   Source:  Internally calculated

10. **cement.intensity**:  cement CO2 emissions per unit of GDP<br>
    Units:  Kt C / billion 2005 US$<br>
    Source:  Internally calculated

11. **GDP.rate**:  growth rate in total GDP<br>
    Units:  unitless<br>
    Source:  Internally calcualted

12. **cement.stock**:  (very) loose proxy for the total stock of
    cement products to date.  It is calculated as the cumulative sum
    of the cement variable from the earliest date in the raw data.<br>
    Units:  Kt C<br>
    Source:  Internally calculated

13. **pccement.stock**:  cement stock, as above per-capita<br>
    Units:  Kt C/person<br>
    Source:  Internally calculated

14. **pccement.lag5**:  per-capita cement lagged by 5 years<br>
    Units:  Kt C/person<br>
    Source:  Internally calculated

## Analysis techniques

TODO 
