# InVision
A rendering engine that uses a path tracing algorithm to generate 3D images.
Shapes can be defined by signed distance functions (SDF), implicit functions, or explicitly as a case class.

## RUNNING THE PROGRAM
You need SBT installed, but simply navigating to the root folder and using sbt run in your preferred terminal works.

## REQUIRED INSTALLS
    1. Java JDK version 1.8._ or later
    2. Scala Compiler version 2.12._ or later
    3. SBT version 1.1._ or later

## CURRENT FEATURES
    1. Material Properties
        a. Glossy
        b. Transparent
        c. Diffuse
        d. Texture mapping

    2. Geometric Features
        a. Shapes not bounded by volumes
          i.   Plane
          ii.  Sphere
          iii. Triangle
          iv.  Square
          v.   Polygon
        b. Signed Distance Function (SDF) Shapes
          i.   Sphere
          ii.  Cylinder
          iii. Box
          iv.  Torus
          v.   Ellipsoid
          vi.  You can union, intersect, and subtract these shapes
          vii. You can translate and rotate these shapes about the x, y, and z axes
    3. Scene Parser
        a. Parses files with a CSS-like syntax into a scene
    4. Command Line Interface (CLI)

## PATH TRACING ALGORITHM
Path tracing falls under a general umbrella of rendering algorithms known as ray casting.
It is meant to create a hyper realistic image based on predefined geometry.
The general format for any ray casting algorithm is:
```
  for all pixels in the image {
    shoot ray from camera to pixel (i, j)
    if it intersects any geometry {
      do math to get pixel color
    } else return background color
  }
```

## PARSER
InVision includes a parser that simplifies how to specify a scene.
The parser has syntax similar to CSS.

### Primitives
    1. Box
        Parameters: position, color, size, rotate
    2. Cylinder
        Parameters: position, color, radius, height, rotate
    3. Plane
        Parameters: position, color, normal
    4. Quad
        Parameters: vertexOne, vertexTwo, vertexThree, vertexFour, color
    5. Sphere
        Parameters: position, color, radius
    6. Torus
        Parameters: position, color, rotate, centerRadius, tubeRadius
    7. Triangle
        Parameters: vertexOne, vertexTwo, vertexThree, color
    8. Light
        Parameters: position, color, radius

### Example Scene
```
#light {
  radius: 50;
  position: (500, 1000, 500);
  color: (1.0, 1.0, 1.0);
}

#plane {
  normal: (0, 1, 0);
  position: (0, 0, 0);
  color: (0.8, 0.8, 0.8);
  gloss: (1.0, 0.2);
}

#sphere {
  radius: 250;
  position: (500, 500, 1000);
  color: (0.5, 1.0, 0.0);
}
```

Numbers wrapped in parentheses such as `(a, b, c)` are vectors, and
all primitives may also specify a material - Diffuse, Transparent or Gloss.
Diffuse takes a single value, the albedo - can take any number greater than zero,
but should be less than 1.0 for physical realism - which
determines the strength of the diffuse reflection. Gloss and Transparent both
take a second value, the surface roughness, which determines the spread of the
reflection. For example, a roughness of 0.0 on a gloss surface is a perfect mirror.

## Command Line Interface
  `--help` or `-h` will print the help screen for the CLI.
