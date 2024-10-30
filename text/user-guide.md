**This is just a draft**

This app **calculates chronic default guideline values for copper, nickel and zinc** as derived for Australian and New Zealand guidelines for marine and fresh water.

*Hint: Find and click the info icons throughout the app to find more information on a particular input.*  

### Step 1: Input your data 

* Data can be provided for all metals at once. 
* Please ensure your file has only one header line in the first row. 
* Each row should contain a different set of data (e.g., different site or different date). 
* Your data must include **DOC** (*labelled "DOC"*) to calculate copper guideline values (*if pH and hardness are provided, the tool will check whether the DGVs are suitable for use*)
* Your data must include **DOC** (*labelled "DOC"*), **pH** (*labelled "pH"*) and **calcium** (*labelled "Calcium"*) and **magnesium** (*labelled "Magnesium"*) to calculate nickel guideline values 
* Optionally, other columns like Site name and Date can be included, which will be retained with your output. These are not used by any functions.

### Step 2: Select options
The “2. Select options" page displays the uploaded data and requires choices for the analysis.
There are five checkboxes/radio buttons for selection. 
1)	The metal(s) of interest. At least one metal must be selected. The “Check input data” button cannot be clicked until a metal is selected.
2)	Level of species protection: up to four levels can be calculated and provided, in accordance with ANZG (2018). The 95% level of species protection is selected as a default.
3)	Hazard quotients (HQs): These indicate the risks to aquatic organisms at your site. Metal concentrations (in $/mu$g/L) must be provided for this option. 
The HQs are calculated from the metal concentration divided by the adjusted DGVs at each level of species protection selected. 
Values above 1 (metal concentrations greater than the DGVs) indicate potential risk  of toxic effects.
4)	Bioavailable metal concentration: These are used to compare to the tier 1 DGVs, 
enabling a single DGV to be used for all sites (useful for plotting data over time, between sites, 
and for communicating with a broad audience). **This estimate of bioavailable metal is for use only within the context of these DGVs.**
It should not be used for other purposes. 
5)	Country: this is required because there are often different tier 1 DGVs for Australia and New Zealand. Only one option can be selected, and this is for the country where the GVs will be applied (not necessarily where the user is based).


### Step 3: Check data
This step checks whether the uploaded data meet the basic needs for the tool - that is, the required columns are included and correctly named. Data cells will be highlighted when missing or if not numeric (e.g., < values). 
This step *does not* check whether the pH, DOC, hardness, calcium, or magnesium are within the applicable range for the GVs. That information is provided *after* you calculate the guideline values, because each metal has a different range of applicability. 
If you have selected the option to calculate bioavailable metals, but you have *not* supplied a column with the name of that metal, this will be shown as an error. Either update your data file (to include metal concentrations), or change your selected option (do not calculate bioavailable concentrations) and then you can run the calculation. 


### Step 4: Review and download results
This page shows your results for review before downloading. 
Metal GVs and the tier 1 DGVs are provided in $\mu$g/L.






