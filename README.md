# anonymizeHandles
a rudimentary R function to replace real names in a data set by anonymized values
untested, no guarantees! check results carefully.


the function's name is <anonymizeHandles>, it takes as parameters
* df: a data frame
* groupings: a list of groupings (sets of variables as character vectors for which the same original values will be replaced by the same anonymized values, for example if the senders and recipients of messages in a data set can be the same people) - obligatory
* ignore: values in the original data set that should not be subjected to anonymization (e.g., missing values, blank spaces, dashes, etc....) - default values are "-", "---", "--", "", " ", NA
* sorting: should anonymous values be assigned in order of occurrence in the original data frame ("occurrence") or alphabetically ("alpha")? default: occurrence
* prefixes: prefixes for anonymous values; a vector of strings, one string per grouping; in the same order as the groupings (same prefix within one grouping) - default: all anonymized values will be constructed as ANONID<order number of grouping>_<order number of unique values in the order specified in "sorting">
* keep: a vector of values 1 or 0, indicating for each grouping whether the original variables with the original values should be kept (=1) or dropped from the resulting data frame (=0); same order as groupings - default: all original variables will be kept and the non-anonymous values still in the resulting data frame

the function returns a list of data frames:
* dataframe - the converted data frame with the new variables containing the anonymized values added (all added variables are named <original name>_anon
* replacelog - a list of all replacements made with each line representing one (possibly duplicate) case of the following variables
	grouping.number - the incremental order number of the grouping in which the replecement occurred
	grouping - the list of variables in the grouping
	original.variable.name - the original variable name (huh!)
	original.variable.kept - whether the original variable is kept (1) or not (0) as specified in the parameter "keep" in the function call
	new.variable.name - the name of the new variable which contains only anonymized values and the values specified in "ignore" in the function call
	original.value - the original, non-anonymous value
	anonymized.value - the new values that original.value is represented as in new.variable.name
	#### repleacelog may contain duplicates of original.value->anonymized.value mappings
* uniquereplacements like replacelog, but with unique cases within each grouping
	original.value
	grouping
	anonymized.value
*vars: a mapping of the original and the anonymized variable names
