# Inpatient Provider Report Cards

This marks my first attempt to create an entirely reproducible output.  Clinical requested HEDIS metric report cards (7/30 FUH and 30 REA) for both adult and child providers.

I created a proof of concept report card, then parameterized the rmarkdown document and wrote code to iterate through all providers and create a report card for each one.  Files saved to locations specified by clinical. 

Next steps: 
- Figure out how to schedule this process and see if I can also generate an email through outlook once the reports run. 
- Integrate with SQL directly once the new servers are in place 

