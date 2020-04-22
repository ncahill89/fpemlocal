# User stories

## UNPD analyst

As a UNPD analyst, **I want** to be able to model past, current and projected use of contraception and unmet need, total demand, demand satisfied, demand satisfied by modern methods, in all countries, and country aggregates, **so that** I may monitor trends, report on them, and develop new methodology.

- **I want** to run the global model to generate estimates for unmarried, married, and all women **so that** they can be published on our website and used for SDG reporting.

- **I want** to be able to update input data, re-run the global model, **so that** I can publish updated results to our website and used for SDG reporting in an annual cycle.

- **I want** to be able to do the above two steps for different age groups **so that** I can produce estimates and projections for different age groups (by also supplying required input data).

- **I want** to be able to select the time period of analysis **so that** I can produce estimates and projections for custom time periods of interest.

- **I want** to be able to compare results for arbitrary pairs and triples of years probabilistically **so that** I can summarize changes and changes-in-changes that have occured, or are projected to occur, in key indicators.

- **I want** the package to store "default" versions of input files that are updated infrequently (e.g. country classifications) **so that** I don't have to manage them myself and risk using the wrong ones.

- **I want** to review the uncertainty around my estimates and projections (e.g. in the form of posterior marginal quantiles) **so that** I know whether they are reliable/robust.

- **I want** to store estimates, projections **so that** I can return to study them at a later date.

- **I want** to do quick trial runs **so that** I can check the impact of changes to input data and plan releases and reports.

- **I want** to store intermediate outputs, such as MCMC chains, **so that** I can use them to produce customized summaries that may be required from time-to-time, or for model development purposes.

- **I want** to be able to modify model inputs, such as prevalence data, number of women, prior parameters, and compare projections **so that** I can see the impact of different assumptions (e.g. conduct sensitivity analysis).

- **I want** to be able to produce customized versions of figures and tables **so that** I can include them in reports and presentations.

- **I want** to be able to produce maps **so that** I can visualize country differences in key indicators on a map.

- **I want** to be able to run out-of-sample validations for global models.

- **I want** to be able to specify custom country aggregations and get all results for these new aggregations.

### Non-functional requirements

- **I want** to keep up-to-date with model developments from collaborators **so that** I can assess any impact revisions might have on our work programme and plan for any changes needed.

- **I want** to understand the structure of the package code **so that** I can ensure any methodological development I do is compatible and consistent with it.

## Researcher

As a researcher, **I want** to run a variety of multivariate time-series models **so that** I can compare their performance.

- **I want** to provide measurements of times-series for multiple units and metrics **so that** their changes can be modeled.

- **I want** to provide covariates and factors **so that** I can describe the similarity between separate units and time-points.

- **I want** to install JAGS and Stan models **so that** I can analyze the evolution of metrics for multiple units.

- **I want** to define interfaces to models **so that** for a given task (e.g. national models of unmarried women) there is an unambiguous set of requirements for permissible model inputs and required model outputs.

- **I want** to create standard summaries of estimated time-series **so that** a variety of models can feed into pre-defined analyses.

- **I want** to create standard visualizations **so that** I can apply them to outputs of multiple models.

- **I want** to create standard aggregates **so that** I can apply them to outputs of multiple models and create reports at the expected level of detail.

## Supervisor

### Non-functional requirements

- **I want** to create modular tasks **so that** new students and post-docs can learn the package in stages.

- **I want** to separate model building from visualization **so that** researchers with subject-area experience can suggest or implement post-processing without understanding pre-processing or model-building steps.

- **I want** to create data descriptions independent of R code **so that** subject-area experts without R experience can produce compatible data.

## Country analyst

As a country analyst, **I want** to understand current and projected use of contraception in my country **so that** I may inform family planning policy.

- **I want** to review recent survey statistics **so that** I know the current state of contraceptive use.

- **I want** to project contraceptive use into the future **so that** I can see what progress my country will make under current programmes.

  - **I want** to be able to perform quick “trial” runs **so that** I can explore different hypotheses.

  - **I want** to specify the period of my projections **so that** I can focus on the time-frames that my country wishes to prioritize.

  - **I want** to review the results of my projections **so that** I can determine if sufficient progress will be made.

  - **I want** to review the uncertainty around my projections **so that** I know whether they are reliable/robust.

- **I want** to be able to add custom data **so that** I can create projections based on the latest survey data, address erroneous data or perform “what if…” analysis.

- **I want** to store projections **so that** I can return to study them at a later date.

- **I want** to compare projections **so that** I can see the impact of different assumptions (e.g. conduct sensitivity analysis).
