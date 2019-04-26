rem This is (Windows) batch file   copy_all_dex10stepanalysis_macros.bat      11/07/18 ==> 12/13/18
rem Motivation: For ASQ Webinar 11/07/18 ==> NIST DEX Workshop
rem Purpose: copy      dex10stepanalysis.dp
rem                    (the macro that  carries out the
rem                    entire 10-step sensitivity analysis)
rem                    along with all of its
rem                    supporting macros and submacros.
rem  Note: The copy will be from my current c: directory    (usually c:\0\)
rem                    to  the directory                    c:\0\dex10stepmacros\

rem -----start point-----


rem ---This is the batch file which created the contents of this directory---
rem    Note: For security, this copies to a ...readme.txt file.  Adter the copy, rename ... readme.txt => ... .bat
copy c:copy_all_dex10stepanalysis_macros.bat      c:\0\dex10stepmacros\readme.txt


rem ---dp.bat    is the shortcut that allows you to type in   dp   to execute dataplot---
rem    Note: For security, this copies to   dp.battxt   . After the copy, rename    dp.battxt ==> dp.bat
rem    dp.bat will call    dpcomm.bat   , which in turn will invoke   dataplot.exe---
copy c:\bat\dp.bat                          c:\0\dex10stepmacros\dp.battxt


rem ---dpcomm.bat    is the behind-the-scenes batch file which will invoke   dataplot.exe---
rem    Note: For security, this copies to   dpcomm.battxt   . After the copy, rename    dpcomm.battxt ==> dpcomm.bat
copy c:\progra~2\nist\dataplot\dpcomm.bat     c:\0\dex10stepmacros\dpcomm.battxt


rem ---dataplot_111718.exe   is the dataplot executable.
rem    Note: For security, this copies to   ... .exetxt   . After the copy, rename    ... .exetxt ==> ... .exe
copy c:\progra~2\nist\dataplot\dataplot_110218.exe   c:\0\dex10stepmacros\dataplot_110218.exetxt

rem ---dplogf.tex   is a jjf-preferred dataplot login file of preferences which affect plot appearance---
copy c:dplogf.tex                           c:\0\dex10stepmacros\*.*


rem ---dpmesf.tex and dpsysf. text    are dataplot start-up message and system files (of minor importance)---
copy c:\progra~2\nist\dataplot\dpmesf.tex   c:\0\dex10stepmacros\*.*
copy c:\progra~2\nist\dataplot\dpsysf.tex   c:\0\dex10stepmacros\*.*


rem ---gvdp.bat    is the shortcut to take default dataplot's postscript graphics output (in dppl1f.dat)---
rem                and convert it to a pdf file---
rem    gvdp.bat will invoke   c:\progra~1\ghostgum\gsview\gsview64.exe    .---
rem    for gsview64.exe, you will need to download the (free) ghostscript & ghostview executables.---
rem    to download ghostscript, go to http://gs910w64.exe (the 64-bit 9.1 version of Ghostscript), or---
rem                                   http://gsv50w32.exe (the 32-bit 5.0 version of Ghostscript)    ---
rem    to download ghostview,   go to http://pages.cs.wisc.edu/~ghost/gsview/  ---
rem    Note: this copies to   gvdp.battxt   . After the copy, rename    gvdp.battxt ==> gvdp.bat
copy c:\bat\gvdp.bat                        c:\0\dex10stepmacros\gvdp.battxt

rem ---dd.bat, ddsize.bat, np.bat, and np++.bat are (minor importance) batch files to invoke dos dir command and dos notepad editors---
copy c:\bat\dd.bat                        c:\0\dex10stepmacros\dd.battxt
copy c:\bat\ddsize.bat                    c:\0\dex10stepmacros\ddsize.battxt
copy c:\bat\np.bat                        c:\0\dex10stepmacros\np.battxt

rem ---This is data/script/output for the Box Defective Springs % Acceptable 2**3 (k=3,n=8) example---
copy boxsprings_3_8.dat                     c:\0\dex10stepmacros\*.*
copy boxsprings_3_8.dp                      c:\0\dex10stepmacros\*.*
copy boxsprings_3_8.pdf                     c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the Ray Bowen Dental Polysac Adhesion 2**3 (k=3,n=8) example---
copy bowen_3_8.dat                          c:\0\dex10stepmacros\*.*
copy bowen_3_8.dp                           c:\0\dex10stepmacros\*.*
copy bowen_3_8.pdf                          c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the Washburn Knee Cartilege Regeneration 2**4 (k=4,n=16) example---
copy washburn_4_16.dat                      c:\0\dex10stepmacros\*.*
copy washburn_4_16.dp                       c:\0\dex10stepmacros\*.*
copy washburn_4_16.pdf                      c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the BHH Chemical Reactor Efficiency 2**5 (k=5,n=32) example---
copy boxreactor_5_32.dat                    c:\0\dex10stepmacros\*.*
copy boxreactor_5_32.dp                     c:\0\dex10stepmacros\*.*
copy boxreactor_5_32.pdf                    c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the BHH Chemical Reactor Efficiency 2**(5-1) (k=5,n=16) example---
copy boxreactor_5_16.dat                    c:\0\dex10stepmacros\*.*
copy boxreactor_5_16.dp                     c:\0\dex10stepmacros\*.*
copy boxreactor_5_16.pdf                    c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the BHH Chemical Reactor Efficiency 2**(5-2) (k=5,n=8) example---
copy boxreactor_5_8.dat                     c:\0\dex10stepmacros\*.*
copy boxreactor_5_8.dp                      c:\0\dex10stepmacros\*.*
copy boxreactor_5_8.pdf                     c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the Net-Zero House Energy Consumption 2**7 (k=7,n=128) example---
copy kneifel_7_128.dat                      c:\0\dex10stepmacros\*.*
copy kneifel_7_128.dp                       c:\0\dex10stepmacros\*.*
copy kneifel_7_128.pdf                      c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the John Henry Scott HRTEM Error 2**(8-4) (k=8,n=16) example---
copy scott_8_16.dat                         c:\0\dex10stepmacros\*.*
copy scott_8_16.dp                          c:\0\dex10stepmacros\*.*
copy scott_8_16.pdf                         c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the Ken Inn Trace Plutonium Contamination 2**(8-4) (k=8,n=16) example---
copy inn_8_16.dat                           c:\0\dex10stepmacros\*.*
copy inn_8_16.dp                            c:\0\dex10stepmacros\*.*
copy inn_8_16.pdf                           c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the Li Ma Additive Manufacturing Peak Temperature 2**(10-6) (k=10,n=16) example---
copy ma_10_16.dat                           c:\0\dex10stepmacros\*.*
copy ma_10_16.dp                            c:\0\dex10stepmacros\*.*
copy ma_10_16.pdf                           c:\0\dex10stepmacros\*.*

rem ---This is data/script/output for the World Trade Center 2**(13-9) (k=13,n=16) example---
copy wtc_13_16.dat                          c:\0\dex10stepmacros\*.*
copy wtc_13_16.dp                           c:\0\dex10stepmacros\*.*
copy wtc_13_16.pdf                          c:\0\dex10stepmacros\*.*

rem ---This is the top-level program to test the 10-step program---
copy c:test_ dex10stepanalysis.dp           c:\0\dex10stepmacros\*.*


rem ---This is the top-level program for the dex 10-step analysis---
copy c:dex10stepanalysis.dp                 c:\0\dex10stepmacros\*.*


rem ---1. These are the 10 individual macros for the 10-step analysis---
copy c:dexodp.dp                            c:\0\dex10stepmacros\*.*
copy c:dexsp.dp                             c:\0\dex10stepmacros\*.*
copy c:dexmp.dp                             c:\0\dex10stepmacros\*.*
copy c:dexiem.dp                            c:\0\dex10stepmacros\*.*
copy c:dexbp.dp                             c:\0\dex10stepmacros\*.*
copy c:dexyp.dp                             c:\0\dex10stepmacros\*.*
copy c:dexep.dp                             c:\0\dex10stepmacros\*.*
copy c:dexhpp.dp                            c:\0\dex10stepmacros\*.*
copy c:dexcrsdp.dp                          c:\0\dex10stepmacros\*.*
copy c:dexcp.dp                             c:\0\dex10stepmacros\*.*


rem ---1. These are the sub-macros needed by step 1: dexodp.dp (and other steps)---
copy c:determine_k.dp                       c:\0\dex10stepmacros\*.*
copy c:dexplotinit.dp                       c:\0\dex10stepmacros\*.*
copy c:dexplotdefplotset.dp                 c:\0\dex10stepmacros\*.*
copy c:setcircle.dp                         c:\0\dex10stepmacros\*.*
copy c:dexwriteproject.dp                   c:\0\dex10stepmacros\*.*
copy c:dexknbox.dp                          c:\0\dex10stepmacros\*.*
copy c:dexknboxlee.dp                       c:\0\dex10stepmacros\*.*
copy c:tagcorn.dp                           c:\0\dex10stepmacros\*.*
copy c:annotate.dp                          c:\0\dex10stepmacros\*.*
copy c:dexrightmargin.dp                    c:\0\dex10stepmacros\*.*
copy c:dexgetlevelcoded.dp                  c:\0\dex10stepmacros\*.*
copy c:upperleft.dp                         c:\0\dex10stepmacros\*.*
copy c:dexarrow.dp                          c:\0\dex10stepmacros\*.*
copy c:form_and_write_dex_10_step_conclusions.dp    c:\0\dex10stepmacros\*.*


rem ---2. These are additional sub-macros needed by step 2: dexsp.dp---
copy c:dexwritelevel.dp                     c:\0\dex10stepmacros\*.*
copy       c:dexwritelevelcoded.dp          c:\0\dex10stepmacros\*.*
copy c:dexsp_write_factor_levels.dp         c:\0\dex10stepmacros\*.*


rem ---3. These are additional sub-macros needed by step 3: dexmp.dp---
copy c:dexmp_write_factor_levels.dp         c:\0\dex10stepmacros\*.*
copy c:dexmp_anova.dp                       c:\0\dex10stepmacros\*.*
copy c:dexmp_levene_test.dp                 c:\0\dex10stepmacros\*.*
copy c:computestats.dp                      c:\0\dex10stepmacros\*.*
copy c:ploterrorbars.dp                     c:\0\dex10stepmacros\*.*


rem ---4. These are additional sub-macros needed by step 4: dexiem.dp---
copy c:check_and_fix_original_data_for_2_levels_and_m1_p1.dp  c:\0\dex10stepmacros\*.*
copy c:label_location_and_size.dp           c:\0\dex10stepmacros\*.*


rem ---5. These are additional sub-macros needed by step 5: dexbp.dp---
copy c:get_ranked_list_of_factors.dp        c:\0\dex10stepmacros\*.*
copy c:get_robustness_factors.dp            c:\0\dex10stepmacros\*.*
copy c:annotate_boxplot_subplot.dp          c:\0\dex10stepmacros\*.*
copy      c:number_of_digits.dp             c:\0\dex10stepmacros\*.*


rem ---6. These are additional sub-macros needed by step 6: dexyp.dp---
copy c:compute_effect_estimates_and_confounding.dp  c:\0\dex10stepmacros\*.*
copy      c:est.dp                          c:\0\dex10stepmacros\*.*



rem ---7. These are additional sub-macros needed by step 7: dexep.dp---
copy      c:dexep_box.dp                    c:\0\dex10stepmacros\*.*


rem ---8. These are additional sub-macros (= none) needed by step 8: dexhpp.dp---


rem ---9. These are additional macros needed by step 9: dexcrsdp.dp---
copy      c:dexcrsdp_box.dp                 c:\0\dex10stepmacros\*.*


rem ---10. These are additional sub-macros needed by step 10: dexcp.dp---
copy c:extract_center_points.dp             c:\0\dex10stepmacros\*.*
copy c:sdigit.dp                            c:\0\dex10stepmacros\*.*
copy c:dexcont.dp                           c:\0\dex10stepmacros\*.*
copy c:dexcp_write_augmented_tic_labels.dp  c:\0\dex10stepmacros\*.*

