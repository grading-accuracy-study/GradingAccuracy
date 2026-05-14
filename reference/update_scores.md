# Update student scores based on metadata rubric items

Computes total scores for each student submission by multiplying rubric
item responses by their corresponding point values defined in a metadata
JSON file.

## Usage

``` r
update_scores(csv, metadata = "./metadata.json", overwrite = TRUE, calibrated)
```

## Arguments

- csv:

  the path to the student CSV file.

- metadata:

  the path to the metadata JSON file

- overwrite:

  whether to overwrite the original CSV file with updated scores

- calibrated:

  which rubric to apply

## Value

A data frame containing the original student data with an updated
`Score` column.
