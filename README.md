### Our group
This is the final project of p8105 by Yuhao Chang(yc4585), Yingyu Cui(yc4584), Shangzi Gao(sg4489), Jiawa Zhang(jz3900), and Alice Zhou(az2852), sorted alphabetically by last name. 

### Our website
https://sg4489.github.io/p8105_final_project.github.io/

### Data source
The address of the latest dataset： https://wonder.cdc.gov/controller/datarequest/D176

### The analysis（Nov. 20th）

#### General Analysis 

1. Explore the trends in the number of deaths across the United States (including at least all-cause and natural-cause deaths).  
2. Create general death proportion visualizations (including a pie chart and a stacked bar chart).  

3. Explore the changes in COVID-19 deaths (underlying and multiple) over time.  
4. Create a 3-panel heatmap to examine the correlation between causes of death for each year.

#### Specific Analysis

1. Perform a time series analysis: create a 4-panel line chart (or other suitable visualization) to explore how the number of deaths changes over time across different regions.  

2. Conduct a geographical exploration: create violin plots within each region to compare the differences in death counts between states. The comparison should ultimately include four regions.

3. Conduct a systemic-level analysis: explore the distribution of deaths by different systems (e.g., respiratory, cardiovascular) across various regions.  

### Meeting Record (11.22 19:00 pm)
Overall Advice: 

(1) Use data for 2020-2023 only.

(2) Consider using CDC's web backgrounds and then adjust the image colours accordingly.

(3) For the disease not COVID_19, using annual mortality rates (per 1,000 population) or using monthly mortality rates (per 10,000 population).

(4) Make a pie plot to see which disease accounts for the largest proportion, then analyse the proportion of systemic diseases that the disease belongs to.

(5) Comment on each other's pictures of everyone.

(6) Unify the dataset after filling in the missing values with random numbers and put it in the data folder.


1. Specific: Shangzi Gao
   
(1) Consider calculating mortality rates based on annual population size and display them seprately.

(2) Vertical coordinates using annual mortality rates (per 1,000 population) or using monthly mortality rates (per 10,000 population).


2. Specific: Jiawa Zhang
(1) Vertical coordinates using annual mortality rates (per 1,000 population) or using monthly mortality rates (per 10,000 population).

(2) Draw and resize four 4-panal violin plots.


3. Specific: Yingyu Cui
   
   
(1) Harmonised mortality rate per 1000 population.

(2) Change the colour of the heatmap.

(3) Hypothesis testing the month of peak mortality and analysing.

(4) Hypothesis testing for endemic diseases (septicaemia, etc.).

(5) Add some comments (all of us).


4. General: Yuhao Chang
   
(1) Plot a barplot of mortality rates for a few selected diseases in a row.

(2) Try to plot a line graph of the change in mortality for each disease with the vertical coordinate as the mortality rate and the horizontal coordinate as time.

(3) Draw a plot comparing all cause and natural cause (using a density plot).

(4) Hypothesis testing followed by some tables explaining the statistics in the tables.


5. General: Alice Zhou
    
(1) Plot the difference in mortality rates between MULTIPLE and UNDERLINING for a given number of weeks.

(2) Putting the legend below makes the picture wider and makes it easier to see trends.

(3) Plot a plot of the mortality rates (2020-2023) for each state in Shiny.(Welcome to debug!)

(4) Adjust for covid-19 mortality (per ? person).
