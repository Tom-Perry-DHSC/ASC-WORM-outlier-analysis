# ASC-WORM-outlier-analysis
Outlier analysis code for the ASC Capacity Tracker

This code is only based on the workforce metrics collected.
No CT data nor filepaths are visible and comes from internal extracts. 
The only columns to have outlier analysis performed on them are headcounts and both hourly and daily metrics.

Our initial input file, wide_data, has columns:
- ExtractDate
- UpdatedWeek, UpdatedMonth
- CqcId, LocalAuthority, Region, Setting
- DirectlyEmployed_Headcount, Agency_Headcount
- DirectlyEmployed_CovidAbsence, DirectlyEmployed_GeneralAbsence, DirectlyEmployed_TotalAbsence
- HoursPaid, HoursAgency, HoursOvertime, HoursAbsence
- DaysAbsence

## Methodology of determining an outlier

- A single value in each of the fields which is an outlier based on raw and percentage change relative to other responses
- Comparing where the hours worked is over 168 hours per person per week. We assume the headcount figures are correct in preference to the hours
- Comparing where the headcounts for the DE and Agency staff are equal
