FlexAgg2: An Improved Version of the Data Aggregation Program FlexAgg

The FlexAgg program distributed with the GTAP 7 Data Base (and older) does not work in Windows 7 64-bit (Even after updating the open-source utilities used to interpret the mapping files). 

In order to solve this we have rewritten FlexAgg to be fully compatible with GEMPACK 10. The new FlexAgg, nicknamed FlexAgg2.X, has these new features: 
•Takes advantage of GEMPACK text data files and is totally written in GEMPACK. (It does not use Awk for building aggregation mappings.)
•Can be called from anywhere in the file system provided it has been added to the Windows path.
•Allows databases with names different to those predefined in the GTAP Data Package.
•Easy manipulation of commodity, region, and factor sets makes FlexAgg2 a flexible tool to accommodate and incorporate satellite datasets.
•Produces a Social Accounting Matrix (SAM) view of the GTAP Data Base.
•For ease of reference, mapping sets with description of disaggregated (original) sets are included in the sets file.
•Error messages in the aggregation process are indexed facilitating debugging of larger workflows.

The default behavior of FlexAgg2 is significantly different from its predecessors, so we encourage reading the documentation, available at:
https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=3762

Is FlexAgg2.X compiled using 64 bits? No, we compiled it using Window 32 bits for greater compatibility, but it does work in 64 bits. If you want it compiled in 64 bits you can run "compile.bat" in the source (FlexAgg2/src/) folder. 

Update for v8a (May 2012): We provide all the data files now in full (not sparse) storage format. While this makes the files
 fully compatible with GEMPACK versions older than 10.0, this also means increased storage space. If you want to reduce
the storage, please use the program SPRS2FULL.exe to suit your needs. You may modify the batch file sprsfullall.bat to 
switch between sparse and full storage for all the files in Flexagg.

Questions? Contact gtapsupport@purdue.edu for more information. 