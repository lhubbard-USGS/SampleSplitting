SampleSplitting
===========

Calculates sample bottle volumes for a given USGS station and storm event(s)

To install this package use the following code:
install.packages(c("SampleSplitting"),repos="http://usgs-r.github.com")

An example workflow script is available at https://github.com/USGS-R/SampleSplitting/blob/master/sampleSplitWorkflow.R

Explanation of the calculations in the labDataOut function follow:

Sample Splitting

User enters data, storm start and end dates, max bottle volumes, max sample volumes, storm names and sub nums (optional)
For each storm:
Discharge data is subset to one value after the storm end date and one value before the storm start date 
Storm discharge data is subset to only non-null discharge values
Volumes are calculated for each discharge value
	if row 1, volume is NA
	if last row, volume is NA
	else volume is (.5*(adaps_data_storm$datetime[i]-adaps_data_storm$datetime[i-1])*(.75*adaps_data_storm$X02_00060[i]+.25*adaps_data_storm$X02_00060[i-1]))+(.5*(adaps_data_storm$datetime[i+1],adaps_data_storm$datetime[i])*(.75*adaps_data_storm$X02_00060[i]+.25*adaps_data_storm$X02_00060[i+1]))
Samples data is subset to only <= storm end date and >= storm start date
If there are no sample rows within the designated storm start and end dates, "Storm event specified which has no samples" is output
If sub nums were entered, the bottle numbers are set to begin with the designated number
If removeDate(s) were input, those samples are removed from the samples data subset
For each sample:
	if row 1, sampStart is minimum date from Discharge data subset
	if not row 1, sampStart is adaps_samp_storm$datetime[i-1]+(.5*(adaps_samp_storm$datetime[i]-adaps_samp_storm$datetime[i-1]))
	if last row, sampEnd is maximum date from Discharge data subset
	if not last row, sampEnd is adaps_samp_storm$datetime[i]+(.5*(adaps_samp_storm$datetime[i+1]-adaps_samp_storm$datetime[i]))
	Sample volume subset is defined as discharge data >= sampStart and <= sampEnd
	if sampEnd corresponds to an exact discharge value datetime, that volume is multiplied by 1/2
	if sampStart corresponds to an exact discharge value datetime, the first row of the existing Sample Volume subset is reduced to half the volume value, because it is a split discharge
	volume for each sample is sum(Sample_volume_subset$volume)
Percent for each sample is round(100*(adaps_samp_storm$volume/sum(adaps_data_storm$volume,na.rm=TRUE))
mL for each sample is adaps_samp_storm$volume*maxBottleV/max(adaps_samp_storm$volume)
If sum(mL)>maxSampV
	mL values are all multipled by the ratio of maxSampV/sum(mL)
	mL values are truncated (not rounded, b/c we don't want to go over allowable lab volume)
else mL values are truncated trunc(adaps_samp_storm$mL*(maxSampV/sum(adaps_samp_storm$mL))
