# brainKCCA
Region-Level Connectivity Network Construction via Kernel Canonical Correlation Analysis 


This readme file provides you a simple example to construct region-level connectivity network. In example below, when the programming is running, the progress bar will be generated.

```{r, fig.show='hold'}
require(brainKCCA)
require(oro.nifti)
# write data into temp file. The generated NIfTI
# provide you a general structure of fMRI data.
filePath <- tempdir()
# the nii.gz fMRI imaging file is created (toy example)
oro.nifti::writeNIfTI(brainKCCA::input_img, paste(filePath, "/",  "temp", sep=""))
#read fMRI data
testcase1 <- nii2RData(niiFile1 = "temp", resolution = "3mm", imgPath = filePath)
result1<-permkCCA_multipleRegion(imageDat = testcase1, region = c(1,5,10))
summary_kcca(result1)
```
