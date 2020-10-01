# CEPR-data-visualizer
Code and help file for the CEPR Visualization tool

# Data
The data used in the CEPR Visualizer is the 2019 March CPS Extracts obtained from the Center for Economic and Policy Research.  The visualizer is built around this dataset, but can still be modified to accept a different dataset.  The code for the application is available here, to change the dataset you will need to follow a few steps:
1. Change line 34 of the code to point to your new dataset, I highly recommend using a dataset that is limited to the variables that you want to give students to analyze
2. Modify line 43 to the variables present in your dataset that you want to feed into the menus
3. Comment out lines 46 through 99
4. Lines 129 through 138 correspond the the subset buttons, change these to what you want to be able to stratify on
5. Lines 154 through 164 correspond to removing different types of unemployed, recommend commenting out this code as its specific to the CEPR dataset
6. Comment out line 179
7. Lines 208 through 226 will require modification depending on your dataset, mainly changing variable names. Further work might be needed to modify variables to match your desired analysis.
8. Lines 231 through 410 control the plots.  There is a lot of logic present to control what feeds into the plots, you will need to go through these and change your logic accordingly
9. Lines 411 through 531 controls the tables, the logic here should match the logic in the plot generator.

# Errors
There are a few known errors present in the app. They are related to how the variables are passed to ggplot and dplyr. The errors come up when a user selects options that present redundant information.  For example, selected Race as an x value and then subsetting by race will cause the table to disappear and an error to be thrown.  The plot will do the desired plot still, but the information is redundant as it just colors each bar.  Trying to modify a categorical variable by median or mean will not generate a plot and throw an errors as there is no meaning to a median value for text input.  A table will still be generated, but has no noticable meaning.

If you discover any additional errors, then please relay that through Github so that I can investigate them
