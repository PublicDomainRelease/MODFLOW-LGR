readme.txt


                      MODFLOW-LGR - Version: 2.0.0
         Three-dimensional finite-difference groundwater flow model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This version of MODFLOW-LGR is packaged for personal computers using
one of the Microsoft Windows operating systems.  This version of MODFLOW-2005 
is packaged for personal computers using the Microsoft Windows XP or 7 operating
systems.  An executable file for personal computers is provided as well as the 
source code.  The source code can be compiled to run on other computers.

IMPORTANT: Users should review the file mflgr.txt for a description of, and
references for, this software. Users should also review the file release.txt,
which describes changes that have been introduced into MODFLOW-LGR with each
official release; these changes may substantially affect users.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following distribution file is for use on personal computers:

         mflgrv2_0_00.zip

The distribution file contains:

          Compiled runfile and source code for MODFLOW-LGR.
          Supplementary MODFLOW-2005 documentation in PDF format
          Test data sets.

The distribution file is a compressed zip file. The following directory
structure is incorporated in the zip file:

   |
   |--mflgr.2_0
   |    |--bin       ; Compiled MODFLOW-LGR executable for personal computers
   |    |--doc       ; Documentation files
   |    |--test-run  ; Input data to run verification tests
   |    |   |--ex3   ; Input data for example 3
   |    |--test-out  ; Output files from running the test problems
   |    |--src       ; MODFLOW-LGR source code for use on any computer

The distribution file is a compressed zip file. The following directory
structure is incorporated in the zip file:

It is recommended that no user files are kept in the mflgr.2_0 directory
structure.  If you do plan to put your own files in the mflgr.2_0
directory structure, do so only by creating additional subdirectories.


B. INSTALLING

To make the executable version of MODFLOW-LGR accessible from any
directory, the directory containing the executable (mflgr.2_0\bin)
should be included in the PATH environment variable.  Also, if a
prior release of MODFLOW-LGR is installed on your system, the
directory containing the executables for the prior release should
be removed from the PATH environment variable.

As an alternative, the executable file, mflgr.exe, in the
mflgr.2_0\bin directory can be copied into a directory already
included in the PATH environment variable.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
                 WINDOWS 7 SYSTEMS
             
From the Start menu, select Control Panel.  Select System and Security,
and within that screen choose the System option. Then select the Advanced
System Settings option.  Select the Environment Variables button.  In the
System Variables pane, select the PATH variable followed by Edit.  In the
Edit window, add ";C:\WRDAPP\mflgr.2_0\bin" to the end of the Variable
Value (ensure that the current contents of the User Value are not deleted)
and click OK. Click OK in the Environment Variables window and then exit
from the control panel windows.  Initiate and use a new Windows Command
window.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             	  WINDOWS XP SYSTEMS
             
From the Start menu, select Settings and then Control Panel.  Double click
System and select the Advanced tab.  Click on Environment Variables.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click Edit.  In the Edit User Variable window, add
";C:\WRDAPP\mflgr.2_0\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click New.  In the New User Variable
window, define a new variable PATH as shown above.  Click OK.  Click OK
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
                 WINDOWS VISTA SYSTEMS
             
From the Start menu, select Settings and then Control Panel.  Select
System & Maintenance followed by System.  Choose the Advanced System
option.  Select the Settings Task, and then select the Environmental
Variables button.  In the System Variables pane, select the PATH
variable followed by Edit.  In the Edit window, add
";C:\WRDAPP\mf2lgr.2_0\bin" to the end of the Variable Value (ensure
that the current contents of the User Value are not deleted) and click
OK. Click OK in the Environment Variables window and then exit from the
control panel windows.  Initiate and use a new Windows Command window.


C. EXECUTING THE SOFTWARE

After the executable file in the mflgr.2_0\bin directory is installed in
a directory that is included in your PATH, MODFLOW-LGR is initiated in
a Windows Command-Prompt window using the command:

          mflgr [Fname]

The optional Fname argument is the MODFLOW Name File or the LGR Control
File.  If no argument is used, the user is prompted to enter the file.
MODFLOW-LGR runs the same as MODFLOW-2005 if a Name File is entered
rather than an LGR Control File.  If a Name File ends in ".nam", then
the file name can be specified without including ".nam".  For example,
if the name file is named abc.nam, then the simulation can be run by
entering:

          mflgr abc

The data arrays in MODFLOW-LGR are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, but this slows computations significantly.

Some of the files written by MODFLOW are unformatted files.  The structure
of these files depends on the compiler and options in the Fortran write
statement.  Any program that reads the unformatted files produced by
MODFLOW must be compiled with a compiler that produces programs that
use the same structure for unformatted files.  For example, Zonebudget
and Modpath use unformatted budget files produced by MODFLOW.  Another
example is head files that are generated by one MODFLOW simulation and
used in a following simulation as initial heads.  Both simulations must
be run using an executable version of MODFLOW that uses the same
unformatted file structure.

This issue of unformatted files is described here so that users will
be aware of the possibility of problems caused by unformatted files. 
Older versions of MODFLOW executables provided by the U.S. Geological
Survey (USGS) may produce different kinds of unformatted files.  The
current form of unformatted file has been used in USGS MODFLOW
executables starting with version 1.2 of MODFLOW-2000.

The current releases of the following support programs distributed by
the USGS use the same unformatted file structure used in the MODFLOW
executable:  Zonebudget, MODPATH, and MODPATH-PLOT.


D. TESTING

Test data for example 3 in the LGR2 documentation (USGS Techniques
and Methods 6-A44), is provided to verify that MODFLOW-LGR is correctly 
installed and running on the system. The directories under 
mflgr.2_0\test-run contain the input data for running the tests.  
Directories under mflgr.2_0\test-out contain the output files from 
running the tests.

Directories under mflgr.2_0\test-run can be used to conveniently run the 
tests without destroying the original results in directories under 
mflgr.2_0\test-out.  To run example 3, MODFLOW-LGR should be run in a 
command-prompt window with the current directory being 
mflgr.2_0\test-run\ex3 . The LGR control file for example 3 is ex3.lgr.
The output files that are created in directories under mflgr.2_0\test-run 
can then be compared to those in directories under mflgr.2_0\test-out. 


E. COMPILING

The executable file provided in mflgr.2_0\bin was created using the Intel
Visual Fortran 13.1 and Microsoft Visual C++ .NET compilers.  Although an 
executable version of the program is provided, the source code is provided 
in the mflgr.2_0\src directory so that MODFLOW-LGR can be recompiled if 
necessary.  However, the USGS cannot provide assistance to those compiling 
MODFLOW-LGR. In general, the requirements are a Fortran compiler, a compatible 
C compiler, and the knowledge of using the compilers.

The C compiler is used for the GMG solver.  If a compatible C compiler is
not available, the GMG solver can be removed so that only a Fortran compiler
is required.  File Nogmg.txt in mflgr.2_0\src contains instructions for
removing GMG from MODFLOW.
