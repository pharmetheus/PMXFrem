# PMXFrem 1.2.0

* Made parNames mandatory for createFFEMmodel
* Added an input check to getExplainedVar so that dataI and etas must have the 
  same number of rows if type=1
* Added setupdfCovs fuction that facilitates the creation of the dfCovs for getExplainedVar.
* Fixed the potential issue with missalignment between panels and facet labels.
* Moved getForestDFREM from PMXForest to PMXFrem
