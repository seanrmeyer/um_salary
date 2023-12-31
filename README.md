# um_salary

This project aggregates annual salary disclosures from the University of Michigan into a single data source and incorporates supplemental data for CPI adjustments and binary gender probability related to first names.

A second data source containing job description information was scraped from the UM Career Navigator. There may be similarities between this and the salary data, but they don't appear to match completely.

## Data sources

[UM Salary](https://www.dropbox.com/scl/fo/npjaekyajwsn6cruyq2ak/h?rlkey=thajnoo7so3p0tbnj2cssfyz4&dl=0), Sourced from [University of Michigan Library](https://quod.lib.umich.edu/e/errwpc/public/3/3/1/3314612.html)

[Consumer Price Index](https://www.dropbox.com/scl/fi/x1r9uev5sjz3cvhjypr3f/cpi_data_2023.xlsx?rlkey=fhaphju7j6mum6e5rngyvv64v&dl=0), Sourced from [U.S. Bureau of Labor Statistics](https://data.bls.gov/timeseries/CUUR0000SA0?years_option=all_years)

[Gender R Package](https://github.com/lmullen/gender)

[Career Navigator](https://www.dropbox.com/scl/fo/j70k99uicvzmwo26cvhp7/h?rlkey=e8w1dvqdfri7vjvnu39ip8m9e&dl=0), Sourced from [UM Career Navigator Search Function](https://careernavigator.umich.edu/search)

## R Notebooks

[UM Salary Data](https://htmlpreview.github.io/?https://github.com/seanrmeyer/um_salary/blob/main/01%20R%20code/um%20salary%20prep.nb.html)

[Career Navigator](https://htmlpreview.github.io/?https://github.com/seanrmeyer/um_salary/blob/main/01%20R%20code/career%20navigator%20prep.nb.html)

## Prepared data output

[Prepped Salary Data](https://www.dropbox.com/scl/fi/isj68hwx646gz2cpau0tf/salary_data.csv?rlkey=8ohkfwkbbttqrhtz5s1cy8t43&dl=0)

[Career Navigator Job Descriptions](https://www.dropbox.com/scl/fi/eagpul5f5wby0mhq50cgc/career_navigator.csv?rlkey=totd1713yxaukd7hgjqqkiapa&dl=0)
