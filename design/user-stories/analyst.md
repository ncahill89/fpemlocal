
# User stories

## Items that should be responsibilities of the package (on a first pass).
 
- **As an** analyst, **I want** to be able to model past, current and
  projected use of contraception and unmet need, total demand, demand satisfied,
  demand satisfied by modern methods, in all countries, and country aggregates,
  **so that** I may monitor trends, report on them, and develop new methodology.

### Model API

These are model goals based on usage so far:

- Stratification by age and union status (in-union, not-in-union, married,
  etc.., age-groups)
- Nested stratification by spatial units 
- Multiple-metrics (unmet need, modern use, total demand, etc...)

On the Stan/JAGS end (esp. on the Stan end) the stratification by age/union
can be taken care of by taking a model matrix as input (the sort of thing
that's produced by a formula in R).  Multiple metrics (esp. interacting ones 
like in FPEM need cusom handling.  

The interface (at least for a Stan model) can look like:

```
#' FEPM data
#'
#' This function creates an fpem_data object that includes a tag
#' (string) identifying the type of model this data is for
#' and contains the data.  There is a separate 'registry' in the 
#' package that contians validation code that checks that the 
#' supplied data matches the relevant schema for this model.
#' 
#' @param model_type string identifying the mdoel (this tells you which of the
#'             standard models you are preparing data for).
#' @param ... sorry can't make this more specific ATM.
fpem_data(model_type, ...)

#' Run FPEM 
#'
#' This function runs an FPEM and saves output to a directory
#'
#' @param fpem_data an object that includes the model type,
#'       as well as all the data required to run.
#' @param formulas a named list of formulas, one for each component
#'        required for this model type.
#' @param ... model-specific tuning parameters (this is where you can 
#'        adjust priors, etc...
fpem(data, mm, target_dir, ...)
```

### Public API for control of models
    - **I want** to run the global model to generate estimates for unmarried,
      married, and all women **so that**  they can be published on our website
      and used for SDG reporting.
    - **I want** to be able to select the time period of analysis **so that**
      I can produce estimates and projections for custom time periods of
      interest.
    - **I want** to be able to update input data **so that** I can publish 
      updated results to our website and used for SDG
      reporting in an annual cycle.
    - **I want** to be able to do age-stratified analyses
      **so that** I can produce estimates and projections for different age
      groups (by also supplying required input data).	

### Visualization and model summaries
    - **I want** to review the uncertainty around my estimats and projections
      (e.g., in the form of posterior marginal quantiles) **so that** I know
      whether they are reliable/robust.
    - **I want** to be able to compare results for arbitrary pairs and triples of
      years probabilistically **so that** I can summarize changes and
      changes-in-changes that have occured, or are projected to occur, in key
      indicators.

### Internal API: 
    - **I want** to do quick trial runs **so that** I can check the impact of
      changes to input data and plan releases and reports.

### Items solved by having a pipeline approach and .rds objects on disk
  - **I want** to store intermediate outputs, such as MCMC chains, **so that**
    I can use them to produce customized summaries that may be required from
    time-to-time, or for model development purposes.
  - **I want** to store estimates, projections **so that** I can return to
    study them at a later date.


### Items solved by having solid internal data and schema for external data:
  - **I want** to be able to run out-of-sample validations for global models.
  - **I want** to be able to modify model inputs, such as prevalence data,
    number of women, prior parameters, and compare projections **so that**
    I can see the impact of different assumptions.
  - **I want** the package to store 'default' versions of input files that are
    updated infrequently (e.g,. country classifications) **so that** I don't
    have to manage them myself and risk using the wrong ones.

### Items solved by good programming practices and doc:
  - **I want** to keep up-to-date with model developments from collaborators
    **so that** I can assess any impact revisions might have on our work
    programme and plan for any changes needed. 
  - **I want** to understand the structure of the package code **so that**
    I can ensure any methodological development I do is compatible and
    consistent with it.


## Items that we can be solved by established packages (on a first pass).

To effectively develop a package this big we need to create it in testable
layers.  The goal here is to identify items that are important but 
can initially be be done with other packages.  This lets us create a
milestone.

- **I want** to be able to specify custom country aggregations and get all
  results for these new aggregations. 
    - **Rationale**: aggregation is really easy with long-format
      output in `dplyr`, we **should** provide all data to do this
      (e.g.-weights).
  - **I want** to be able to produce customized versions of figures and tables
    **so that** I can inlcude them  in reports and presentations.
    - **Rationale:**: if we produce `ggplot` objects they can be customized,
      we **should** produce ggplot objects as output.  We **should** provide
      functions for printing/modifying them.
  - **I want** to be able to produce maps **so that** I can visualize country
    differnences in key indicators on a map.
    - **Rationale:**: if we produce the right data there are a range of 
      `ggplot` and web-related packages that can serve this purpose.  We
      **should** produce `ggplot` objects for this purpose.
