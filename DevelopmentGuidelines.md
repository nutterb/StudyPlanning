## Study Planning Development Guidelines

### Function Expectations

1. Each function should be capable of estimating each study parameter.
2. Each function should provide support for parallel processing
3. Each function should return a tidy data frame of the results.

### Documentation Structure

1. **Title:** Study Planning for [test]
2. **Description:** Estimate a `NULL` study parameter for [test] given the remaining study parameters are known.  The study parameters for [test] are [param1], [param2], ..., [paramk]
3. **Params:** Include the parameter domain of each study parameter in the param definitioni.
4. **Details:** 
    + Formula for the estimation
    + Behavior for out-of-domain values
    + Guidelines for when to use parallel processing (if any)
5. **Default Interval Limits**: A table with three columns giving the default limits for the function.
    + Study parameter
    + Lower interval limit
    + Upper interval limit
6. **Functional Requirements:** Objective, measurable requirements against which unit tests may be written.
7. **Return:** Description of the data frame returned.
8. **sources**
9. **See Also**
10. **Authorship**
11. **Examples**
