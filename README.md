# NYC Flight Delays: A Logistic Regression Analysis

Identifying key drivers of flight delays at NYC airports (JFK, LGA, EWR) in 2013, using `nycflights13` data. Response variable: whether a flight arrived more than 15 minutes late (FAA delay threshold).

## Methods
Multiple logistic regression with AIC-based model selection, Box-Tidwell transformations for linearity, and standard diagnostics (VIF, DFBETAs, Cook's Distance, Hosmer-Lemeshow, AUC).

## Key Findings
- Afternoon/evening departures have much higher odds of delay than morning flights
- Precipitation nearly doubles the odds of delay
- Newark and LaGuardia have higher delay odds than JFK
- Summer has the highest seasonal delay risk; fall the lowest
- Model AUC ≈ 0.76 — the focus is inference, not prediction

## Contents
- `flight_delay_project.R` — full R analysis
- `Flight_Project_Report.docx` — written report

## Limitations
Results are specific to NYC airports in 2013 and don't generalize to other cities/years; cancelled flights were excluded.
