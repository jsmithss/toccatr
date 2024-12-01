
\<!-\* README.md is generated from README.Rmd. Please edit that file –\>

# toccatr

------------------------------------------------------------------------

A Toccata is a musical piece with fre-flowing harmonies. **toccatr** is
a collection of tools for importing data from
[Harmony](%22https://www.revvity.com/gb-en/product/harmony-5-2-office-revvity-hh17000019%22)
high content imaging software, as well as simple data manipulation and
visualisation tools

The main toccatr functions:

- import PlateResults.txt files from multiple subfolders;
- make plate layout maps, and heatmaps of data in 96 well format; and
- reshape the data suitable for graphing in Graphpad Prism.

## <i class="fa fa-cog" aria-hidden="true"></i> Installation

toccatr is not on CRAN. To install the latest version use:

``` r
install.packages("devtools")
library(devtools)
install_github("jsmithss/toccatr")
```

## Using toccatr

A full lit of functions is found below:

**Importing and exporting data**  
\* `harmread_pl()` Read multiple plate results files into one
dataframe  
\* `harmread_ob()` Read multiple object results files  
\* `clipfromxl()` Copy data from clipboard (Excel) to R  
\* `clip2xl()` Copy data from R dataframe to clipboard for pasting to
Excel  
\* `clip2csv()` Copy data from R dataframe to csv format  
\* `prismy_col()` Reformat data for Prism column tables

**Data frame tools**  
\* `jiggle()` Move all text columns to the left of the dataframe  
\* `coslectr()` Interactively select columns - useful for large data
frames  
\* `repspace()` Replace underscore with space  
\* `repunderscore()` Replace space with underscore  
\* `rescale_z()` Z-score a variable  
\* `well2row()` Creates numeric row and column references from Well
name  
\* `squish()` Limit upper and lower values of variable  
\* `%nin%()` Select items Not in a list

**Visualisation**  
\* `platemap()` 96 well plate layouts by compound or treatment  
\* `hotplate()` Heatmap of data by plate position  
\* `barchartr()` Quick barcharts with mean and SD  
\* `majsty()` Interactive Scatter plots, drag to highlight points  
\* `ploxy()` Interactive scatter plots with annotation and zoom  
\* `ploxytime()` Interactive scatter plots with animation  
\* `theme_js()` James ggplot theme  
\* `theme_tt()` Clean ggplot theme  
\* `theme_ttf()` Clean ggplot theme for faceted data  
\* `tubemap()` List of colours based on London Tube map

Toccatr is very much a work in progress, suggestions and contributions
welcome.
