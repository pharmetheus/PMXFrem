README:

Status 2025-09-12 of the createFREMmodel work:
- The createFREMmodel function can generate a minimal model taht is equivalent to PsN
- The createFREMdata function needs to filter ans sort the data more intelligently
 a) Apply the IGNORE and ACCEPT statements in the base model. utils-filter_data implements this.
 b) Remove all subjects without observations before computing means and variances and before creating the frem data set.
 c) Retain the sort order of the original data set.
 
 There is a google doc with handover instructions: createFREMmodel handover 20250912
 
 The work is done in the create_FREMmodel branch, which is branched from refactor_updateFREMmodel3, which in turn is branched from main.