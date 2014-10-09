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
* 'p' to create a polygon where the vertices are the selected points
* 'a' to toggle the selected component of the points
* 's' to create a segment between two points
* 'l' to create a line between two points
* 'm' to change the mode of selection

# TODO

* polygon triangulation: debug
* do a better thing for getRenderable stuff
* point set triangulation
* tests

# Useful links

* [conversion screen / opengl coordinates](http://stackoverflow.com/questions/4520377/converting-window-coordinates-to-axis-coordinates-in-opengl)
* [point inside triangle](http://codegolf.stackexchange.com/questions/32898/check-if-point-lies-inside-triangle)
* [point inside polygon](http://stackoverflow.com/questions/217578/point-in-polygon-aka-hit-test/2922778#2922778)
* [circle / line intersection](http://stackoverflow.com/questions/1073336/circle-line-collision-detection)

