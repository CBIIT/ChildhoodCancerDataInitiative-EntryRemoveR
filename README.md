# ChildhoodCancerDataInitiative-EntryRemoveR
This script will take a CCDI metadata manifest file and template and remove entries based on the TSV of entries given.

To run the script on a CCDI template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CCDI-EntryRemoveR.R -h
```

```
Usage: CCDI-EntryRemoveR.R [options]

EntryRemoveR v1.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		dataset file (.xlsx, .tsv, .csv)

	-e CHARACTER, --entry=CHARACTER
		tsv file, no header, of [node]_id that are to be removed from the submission manifest.

	-t CHARACTER, --template=CHARACTER
		dataset template file, CCDI_submission_metadata_template.xlsx

	-h, --help
		Show this help message and exit
```
