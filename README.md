# Shared Shiny App Bundle

This folder contains a shareable version of the Shiny app and the fitted model file it depends on.

## Files

- `app.R`: Shiny application
- `personalised_model_190426.RDS`: fitted model object used by the app

## How to run

Open R in this folder and run:

```r
shiny::runApp()
```

You can also run:

```r
source("app.R")
```

## Required R packages

The app uses these packages:

- `shiny`
- `shinydashboard`
- `personalized`
- `xgboost`
- `ggplot2`

Install any missing package before running the app.

## How to read the app

### Patient profile

Use the left sidebar to enter the patient's baseline variables. The app updates automatically after the inputs change.

### Year 1 and Year 2 tabs

The main page has two tabs:

- `Year 1`: estimated outcomes at 1 year
- `Year 2`: estimated outcomes at 2 years

At the top of each tab, the app shows an `overall model recommendation`:

- `Conservative`
- `Surgery`
- `Conservative = Surgery`

Each tab shows four outcomes:

- `NDI`
- `Arm pain`
- `Neck pain`
- `EQ-VAS`

### How to interpret each outcome card

For each outcome, the app compares both treatments side by side:

- `Conservative`
- `Surgery`

Each treatment box shows:

- the predicted outcome value for that treatment
- whether that value meets the published success threshold

Within each pair of treatment boxes, the app highlights which treatment has the better displayed predicted value for that outcome.

### Colour and face meaning

- Green-highlighted treatment box: this treatment has the better displayed predicted value for that outcome
- Grey-highlighted treatment box: both displayed predicted values are equal
- `:)` badge: this treatment meets the success threshold
- `:(` badge: this treatment does not meet the success threshold
- Green note above the boxes: one treatment meets the threshold and the other does not
- Grey note above the boxes: either both treatments meet the threshold or neither treatment meets the threshold

The box highlight and the threshold badge mean different things:

- box highlight = which treatment has the more favourable displayed predicted value
- threshold badge = whether that displayed value crosses the published success threshold

### Threshold direction

For:

- `NDI`
- `Arm pain`
- `Neck pain`

lower values are better.

For:

- `EQ-VAS`

higher values are better.

### Neutral interpretation

Some patients may have:

- both treatments meeting the threshold
- neither treatment meeting the threshold

In these situations, the threshold-based comparison is more neutral, because the published success threshold alone does not clearly separate the two treatments. However, the displayed predicted values may still differ numerically, so one treatment can still be highlighted as better for that outcome.

### How the overall recommendation is calculated

The `overall recommendation` at the top of each Year 1 / Year 2 tab is based on the four outcome cards shown in that tab:

- `NDI`
- `Arm pain`
- `Neck pain`
- `EQ-VAS`

For each outcome, the app identifies which treatment has the better displayed predicted value:

- for `NDI`, `Arm pain`, and `Neck pain`, lower is better
- for `EQ-VAS`, higher is better

The app then counts how many outcomes favour `Conservative` and how many favour `Surgery`:

- if more outcomes favour `Conservative`, the overall recommendation is `Conservative`
- if more outcomes favour `Surgery`, the overall recommendation is `Surgery`
- if the counts are equal, the overall recommendation is `Conservative = Surgery`

The threshold badges do not determine the overall recommendation. They are shown as a separate interpretation aid.

#### Example

If one card shows:

- `Conservative = 47.6` with `:(`
- `Surgery = 13.4` with `:)`

then, for that outcome, Surgery has the better displayed predicted value and also meets the published success threshold, while Conservative does not.

### Validation tab

The `Validation` tab shows the model validation interaction plot for each outcome. This is mainly for reviewing model behaviour and is less important for routine clinical reading than the Year 1 / Year 2 comparison tabs.

## Notes

- Keep `app.R` and `personalised_model_190426.RDS` in the same folder.
