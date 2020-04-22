# API requirements

## Data management

- Maintain "official" versions of core data (e.g. survey, demographics, ISO country)
- Allow "official" versions of data to be retrieved/queried
- Allow input data to be updated
- Allow model outputs to circle-back for runs on subsets (this may not require anything new).

## Core modeling

  - Allow user-defined models **matching pre-defined input/output scenarios**
  - Run models, varying the following parameters
    - Type---i.e. married, unmarried, all women
    - Age range
    - Time frame
    - Prevalence dataNumber of women
    - Prior parameters
  - Allow user-defined lists of arguments to be passed down to the algorithms

- Provide high-level parameter aggregations that make it easier to run "standard" analyses.

- Obtain uncertainty around estimates (e.g. in the form of posterior marginal quantiles)

- Allow results and computationally expensive intermediate data to be 
  extracted/stored and re-loaded. Provide access to the input data used
  to run the model.

## Model and algorithm checking

  - cross-validation
  - out-of-sample validation

## Model validation
  - Run out-of-sample validations for a data/model/algorithm combo.
  - Create goodness-of-fit metrics of times-series for multiple units and metrics
  - Compare runs for different parameters

## Aggregation
  - Create country (or unit)-level aggregates
  - Create time-aggregates for given time-periods
  - Create standard aggregates (following, e.g. UNPD procedure, or a particular paper).

## Reporting

  - Create standard summaries of estimated time-series
  - Create standard table (e.g.-as published by X)

## Visualization

  - Compare results for arbitrary pairs and triples of years probabilistically.
  - Review uncertainty around estimates (e.g. in the form of posterior marginal quantiles)
  - Generate web-friendly and print-friendly and screen-friendly viz. from the
    same basic plots/data.
