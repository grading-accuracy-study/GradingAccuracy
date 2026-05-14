# Update Scores in metadata JSON

Reads student and expert grading CSV files, computes `n_submissions` and
`mean_score`, and updates the corresponding fields in a metadata JSON
file.

## Usage

``` r
update_scores_in_metadata(folder = "./", file = "./metadata.json")
```

## Arguments

- folder:

  the directory containing the CSV files

- file:

  the path to the metadata JSON file
