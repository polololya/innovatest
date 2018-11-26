# InnovaTest

### Licence
InnovaTest. Web-based software to process data from ELISA and calculate ED50/LD50/IC50  
based on 4PL sigmoid curve.
Copyright (c) 2018  Olga Poleshchuk, Ruslan Al-Shehadat  

This program is free software: you can redistribute it and/or modify  
it under the terms of the GNU General Public License as published by  
the Free Software Foundation version 3 of the License, or  
any later version.  

This program is distributed in the hope that it will be useful,  
but WITHOUT ANY WARRANTY; without even the implied warranty of  
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the  
GNU General Public License for more details.  

You should have received a copy of the GNU General Public License  
along with this program.  If not, see <http://www.gnu.org/licenses/>.  

### Software for 4PL curve fitting

InnovaTest is a user-friendly application for 4PL curve fitting, commonly used in ELISA, dose-response analysis,
real-time PCR and a lot of other fields. It allows you to calculate 4-parametric equation, based on your data 
and calculate concentrations/response in test samples.
Based on R-Shiny, it provides high-quality reproducible scientific research and ELISA system development

### Table mode
Table mode is easier to use if you are not familiar with essays such ELISA and dose response analysis.  
To use table mode `.csv` of `.xls` table should be created. First column is Concentration, fill the it with values indicationg the concentration of defined calibrators. Next columns should be filled with response values (optical density). Each cell is set for one replicate, so if you have three replicates of one sample in your experiment, than 3 cells in row should be filled.

<img src="https://pp.userapi.com/c847124/v847124719/13f51e/QxRBGiJeUU4.jpg" width="400">

__Maximum number of replicates__ - specify maximum number of replicates in your experiment. If all samples have 2 replicates and one has 3, then select 3.  
__Choose file type__ - choose format of your input file. If it is a `csv`, then `\tab` is considered as column separator and `dot` is a decimal separator
__Output file name__ - is set as a current date by default, but you can change it. 

### Plate mode
This mode is convenient if you are familiar with ELISA essays, so using it, you are able to use raw plate data straight from your spectrophotometer with scheme of plate, which defines positions of calibratiors and test samples in your plate.

#### Options
__Input file__ - in plate mode the only supported format is `xls`  
__Concentrations__ - paste list of calibrators' concentrations with commas in-between (without spaces or any other symbols)

To process the whole plate you need to make a scheme of plate right below your data in excel spreadsheet.  
`C` - Calibrators
`T` - Tests

<img src="https://pp.userapi.com/c847124/v847124719/13f525/7PnqqgRP4Xw.jpg" width="400">

### Results
Results are shown in a convenient format after the `Calculate` button is pushed.
It contains table with calculated concentrations' values, mean ODs, variation coefficient and 4PL curve generated based on resulting table

### Troubleshooting
1. When using table mode and `csv` input file make sure that all columns have proper names displayed (Concentration, OD1, OD2...). In case there are NAs in column names, tune `Maximal number of replicates` parameter;
2. There should be no column headers in your input file;
3. Pay attention to decimal separator and column separator in csv files (decimal - `dot`, between-column - `\tab`).

P.S. in case you have any questions and ideas, feel free to contact:  
poleshchukolala@gmail.com  
