# TRAC3R
A rendering engine that uses a path tracing algorithm to generate 3D images. Shapes can be defined by signed distance functions (SDF), implicit functions, or explicitly.

RUNNING THE PROGRAM:
    1. Open up your computer's command prompt
	2. Navigate to the location of the 'src' folder for TRAC3R 
	    Windows:  cd "C:\Users\John Doe\Documents\GitHub\TRAC3R\src"
	3. For Windows PowerShell, run ".\compile.ps1", for MacOS run ".\compile.sh"

ADDING PROGRAM TO PATH (Windows only)
	1. Open up Control Panel
	2. Go to System and Security < System < Advanced system settings (top left corner)
	3. A window should pop up with a button labeled Environment Variables at the bottom right, click it
	4. Another window should pop up with a section labeled System Variables
	5. Find the system variable called PATH
	6. Click the Edit button, and then the New button
	7. Add the file path of the 'src' folder to the PATH variable
	8. Done! Now, when you open your command prompt, you can simply type in compile.ps1 and the program will run

REQUIRED INSTALLS
    1. Java JDK version 1.8._ or later
	2. Scala Compiler version 2.12._ or later