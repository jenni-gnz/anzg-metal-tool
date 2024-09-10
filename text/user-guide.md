**This is just a draft**

This app **calculates chronic default guideline values for copper, nickel and zinc** as derived for Australian and New Zealand guidelines for marine and fresh water.

*Hint: Find and click the info icons throughout the app to find more information on a particular input.*  

### Step 1: Input your data 

* Data can be provided for all metals at once. 
* Please ensure your file has only one header line in the first row. 
* Each row should contain a different set of data (e.g., different site or different date). 
* Your data must include **DOC** (*labelled "DOC"*) to calculate copper guideline values (*if pH and hardness are provided, the tool will check whether the DGVs are suitable for use*)
* Your data must include **DOC** (*labelled "DOC"*), **pH** (*labelled "pH"*) and **hardness** (*labelled "Hardness"*) to calculate zinc guideline values 
* Your data must include **DOC** (*labelled "DOC"*), **pH** (*labelled "pH"*) and **calcium** (*labelled "Ca"*) and **magnesium** (*labelled "Mg"*) to calculate nickel guideline values 
* Optionally, other columns like Site name and Date can be included, which will be retained with your output. These are not used by any functions.

### Step 2: Select options


### Step 3: Check data
This step checks whether the data you have uploaded meets the basic needs for the tool - that is, the required columns are included and correctly named. Data cells will be highlighted when missing or if not numeric (e.g., < values). 
This step *does not* check whether the pH, DOC, hardness, calcium, or magnesium are within the applicable range for the guideline values. That information is provided *after* you calculate the guideline values, because each metal has a different range of applicability. 
If you have selected the option to calculate bioavailable metals, but you have *not* supplied a column with the name of that metal, this will be shown as an error. Either update your data file (to include metal concentrations), or change your selected option (do not calculate bioavailable concentrations) and then you can run the calculation. 


### Step 4: Review and download results
This page shows your results for review before downloading. 
Metal DGVs are provided in $\mu$g/L






