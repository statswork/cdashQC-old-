version 0.1.1
-------------

1.  re-structured `labshift.R` .
2.  rewrote `cdash_ae` function:
    -   `ae1()` argument `bytrt=` is removed.
    -   `ae2()` argument `bytrt=` is removed.
    -   `ae3()` argument `bytrt=` is removed.
    -   fixed an issue in `create_aet()` function that causes "ERROR:
        You are losing observations (Duplicates Maybe?)"

3.  rewrote `cdash_dem.R` :
    -   `dem_summary()` argument `bytrt=` is removed.
    -   `dem_listing()` argument `bytrt=` is removed.

4.  complete `new_create_included.R`.

version 0.1.0
-------------

-   Create the TFLs for QC of cdash format data.
-   currently supports the following tables (or listings):
    -   demographics listing
    -   demographics summary
    -   Adverse Event (AE1, AE2 and AE3)
    -   Lab shift table
    -   Lab out of range values
    -   Vital Signs listing
    -   ECG listing
    -   Concomitant medication listing
    -   Other useful functions
