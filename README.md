# cghs

An haskell way to do 2D computational geometry.

The library provides the following geometric types:

* points
* vectors
* segments
* lines
* triangles
* polygons
* circles

The following algorithms:

* convex hull

It also contains a viewer written using `GLFW-b` and `OpenGL` bindings.It alllows users to easily manipulate geometric objects and apply algorithms.

The following functionalities are supported by the viewer:

* left-click for adding points
* right-click for selecting points
* 'r' to restore the initial state
* 'c' to apply the convex hull algorithm on the selected points
* 'a' to toggle the selected component of the points
* 's' to create a segment between two points
* 'l' to create a line between two points

# TODO

* generalize selection for selecting anything: intersection line / circle
* change selection to select objects for fixed type (can be changed with a key)

* triangulation: polygon (ear) + point set (via convex hull)
* tests

* refactor events handling: state with lens?

# Useful links

* [conversion screen / opengl coordinates](http://stackoverflow.com/questions/4520377/converting-window-coordinates-to-axis-coordinates-in-opengl)
* [point inside polygon](http://stackoverflow.com/questions/217578/point-in-polygon-aka-hit-test/2922778#2922778)
* [point inside triangle](http://codegolf.stackexchange.com/questions/32898/check-if-point-lies-inside-triangle)
* [circle / line intersection](http://mathworld.wolfram.com/Circle-LineIntersection.html)
