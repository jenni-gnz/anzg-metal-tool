Adding additional data:
- Water hardness and pH are not required to calculate copper BAGVs.  If provided, the data will be used to check that
the water chemistry is within the applicability range for the copper BAGVs.
- You can include additional information (e.g. site name, stream name, date, time, flow).  There are no naming
requirements for these columns.  If provided, the information will be included in your output file.

*Outputs*

- The results file produced by the Metals Bioavailability Tool will be your output.
- Your results file will also indicate where BAGVs are not applicable because DOC, pH, hardness, calcium and/or
magnesium are outside the applicability range of the bioavailability models.  The results file will provide a 
reason in a specific column for your metal (e.g. for copper 'CuNote').
- The tool will run rapidly for less than 100 rows.  It will take at least 30 minutes to run over 100,000 rows.
- Hazard quotient values greater than one will be highlighted red (indicating a hazard).

If you want to see an example of an input or output file you can try the demo or download the CSV file template.

**Stepwise instructions**

Detailed instructions about what you need to do at each step are provided here.

The instructions follow the 4 steps in the Metals Bioavailability Tool calculation process.

You can also watch videos that show you <a href="https://www.waterquality.gov.au/anz-guidelines/guideline-values/default" target="_blank"> how to use the Metals Bioavailability Tool</a>.
These videos are located on the ANZG website.

1\. Upload your data

- Provide data for one or all metals.
- Insert only one header line in the first row.
- Use the correct column headings for your variables.  The exact column headings are in Table 4.
- Each row should contain a different set of data (e.g. different site or different date).
- Your data must include DOC to calculate copper BAGVs.
- If pH and hardness are provided, the tool will check whether the BAGVs are suitable for use.
- Your data must include DOC, pH, calcium and magnesium to calculate nickel BAGVs.
- If you provide optional columns (e.g. 'Site name' and 'Date') they will not affect the results.  The optional
columns will appear in your output.

2\. Select options

This step allows you to select from a set of options.  The Metals Bioavailability Tool will use these selections
to do the calculations you have chosen.

&nbsp; a. &nbsp;*Which metals do you want to generate BAGVs for?*\
&nbsp; &nbsp; &nbsp; &nbsp; Select at least one metal.  This will activate the 'Check input data' button.\
&nbsp; b. &nbsp;*What level of species protection do you want to include?*\
&nbsp; &nbsp; &nbsp; &nbsp; Select a level of protection.  You can select up to four.\
&nbsp; c. &nbsp;*Do you want to calculate Hazard Quotients?* \
&nbsp; &nbsp; &nbsp; &nbsp; If you select this option, you must provide dissolved metal concentrations (in &micro;g/L).\
&nbsp; d. &nbsp;*Do you want to estimate the bioavailable metal concentration?*\
&nbsp; &nbsp; &nbsp; &nbsp; If you select this option, you must provide dissolved metal concentrations (in &micro;g/L).\
&nbsp; e. &nbsp;*Select your country for application*\
&nbsp; &nbsp; &nbsp; &nbsp; Select the country that the output will be applied in.\
&nbsp; &nbsp; &nbsp; &nbsp; The DGVs at Tier 1 of the <a href="https://www.waterquality.gov.au/anz-guidelines/guideline-values/default" target="_blank"> Tiered Assessment Framework</a>
are associated with 2 different countries: Australia and New Zealand.

3\. Check data

- Your uploaded data is checked.
- Data cells will be highlighted when missing or if not numeric (e.g. `'<' values).
- This step does not check if pH, DOC, hardness, calcium or magnesium are within the applicability ranges
for the outputs.
- Error messages will appear if you have selected an option that requires data that you have not provided.

4\. View and download results

- You can review your results and then download them.
- This step checks if pH, DOC, hardness, calcium or magnesium are within the applicability ranges for the outputs.
