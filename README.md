# ASC-WORM-outlier-analysis
Outlier analysis code for the ASC Capacity Tracker

This code is only based on the workforce metrics collected.

No CT data is visible and comes from internal extracts. Our initial input file, wide_data, has columns:

- ExtractDate
- UpdatedWeek, UpdatedMonth
- CqcId, LocalAuthority, Region, Setting
- DirectlyEmployed_Headcount, Agency_Headcount
- DirectlyEmployed_CovidAbsence, DirectlyEmployed_GeneralAbsence, DirectlyEmployed_TotalAbsence
- HoursPaid, HoursAgency, HoursOvertime, HoursAbsence
- DaysAbsence