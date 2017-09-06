This repository contains various tools and artifacts useful for exploring open policing data.

Contents currently include:

* `police-data` R project, which in turn includes:
	* R code to read and munge arrest data published by the City of Baltimore and Baltimore Police Department
	* An R notebook that demonstrates visualizations of Baltimore arrest data, combined with datasets and shapefiles from US Census Bureau and various resources from the State of Maryland and
	City of Baltimore
	* R code to load Baltimore arrest data into a dimensional model
* A model for a dimensional database (in the `db` directory) and associated [mondrian](http://community.pentaho.com/projects/mondrian/) schema (in the `mondrian` directory)
* Docker image definitions (dockerfiles and associated artifacts) for building mysql and [saiku](http://community.meteorite.bi/) Docker images for Baltimore arrest data