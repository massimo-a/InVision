# TRAC3R
<<<<<<< Updated upstream
A rendering engine that uses a path tracing algorithm to generate 3D images. Shapes can be defined by signed distance functions (SDF), implicit functions, or explicitly.

*** RUNNING THE PROGRAM ***
    1. Open up your computer's command prompt
	2. Navigate to the location of the 'src' folder for TRAC3R 
	    Windows:  cd "C:\Users\John Doe\Documents\GitHub\TRAC3R\src"
	3. For Windows PowerShell, run ".\compile.ps1", for MacOS run ".\compile.sh"
=======
A rendering engine that uses a path tracing algorithm to generate 3D images.
Shapes can be defined by signed distance functions (SDF), implicit functions, or explicitly as a case class.

## RUNNING THE PROGRAM
You need SBT installed, but simply navigating to the root folder and using sbt run in your preferred terminal works.
>>>>>>> Stashed changes

*** ADDING PROGRAM TO PATH (Windows only) ***
	1. Open up Control Panel
	2. Go to System and Security < System < Advanced system settings (top left corner)
	3. A window should pop up with a button labeled Environment Variables at the bottom right, click it
	4. Another window should pop up with a section labeled System Variables
	5. Find the system variable called PATH
	6. Click the Edit button, and then the New button
	7. Add the file path of the 'src' folder to the PATH variable
	8. Done! Now, when you open your command prompt, you can simply type in "compile.ps1" and the program will run

*** REQUIRED INSTALLS ***
    1. Java JDK version 1.8._ or later
	2. Scala Compiler version 2.12._ or later

*** CURRENT FEATURES ***
    1. Material Properties
        a. Reflective
        b. Glossy
        c. Transparent
        d. Diffuse
        e. Texture mapping

    2. Geometric Features
        a. Shapes bounded by volumes
          i.   Plane
          ii.  Sphere
          iii. Triangle
          iv.  Square
          v.   Polygon
        b. Signed Distance Function (SDF) Shapes
          i.   Sphere
<<<<<<< Updated upstream
	      ii.  Cylinder
	      iii. Box
	      iv.  Torus
	      v.   Ellipsoid
	      vi.  You can union, intersect, and subtract these shapes
	      vii. You can translate and rotate these shapes about the x, y, and z axes
=======
          ii.  Cylinder
          iii. Box
          iv.  Torus
          v.   Ellipsoid
          vi.  You can union, intersect, and subtract these shapes
          vii. You can translate and rotate these shapes about the x, y, and z axes

## PATH TRACING ALGORITHM
Path tracing falls under a general umbrella of rendering algorithms known as ray casting.
It is meant to create a hyperrealistic image based on predefined geometry.
>>>>>>> Stashed changes
