# cghs

An haskell way to do 2D computational geometry.

The library provides the following geometric types:

* points
* vectors
* segments
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

# TODO

* point inside triangle
* triangulation: polygon (ear) + point set (via convex hull)
* tests
* generalize selection for selecting anything: intersection line / circle
* change selection to select objects for fixed type (can be changed with a key)

* refactor events handling: state with lens?

# Useful links

* [circle / line intersection](http://mathworld.wolfram.com/Circle-LineIntersection.html)
* [conversion screen / opengl coordinates](http://stackoverflow.com/questions/4520377/converting-window-coordinates-to-axis-coordinates-in-opengl)
