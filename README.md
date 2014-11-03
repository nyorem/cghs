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
* rays
* regions of the plane

The following algorithms:

* convex hull
* triangulation of a convex polygon
* simple triangulation of a point set

It also contains a viewer written using `GLFW-b` and `OpenGL` bindings.It alllows users to easily manipulate geometric objects and apply algorithms.

The following functionalities are supported by the viewer (AZERTY keyboard):

* left-click for adding points
* right-click for selecting points
* 'r' to restore the initial state
* 'c' to apply the convex hull algorithm on the selected points
* 't' to triangulate the selected polygon or point set depending on the current mode
* 'v' to compute the Voronoi diagram of a point set
* 'p' to create a polygon where the vertices are the selected points
* 'a' to toggle the selected component of the points
* 's' to create a segment between two points
* 'l' to create a line between two points
* 'm' to get the next mode of selection
* 'M' to get the previous mode of selection

# TODO

* Voronoi diagram: voronoiDiagram2
* Sweep-line algorithms: conex hull and point set triangulation
* do a better thing for getRenderable stuff

# Useful links

* [conversion screen / opengl coordinates](http://stackoverflow.com/questions/4520377/converting-window-coordinates-to-axis-coordinates-in-opengl)
* [point inside triangle](http://codegolf.stackexchange.com/questions/32898/check-if-point-lies-inside-triangle)
* [point inside polygon](http://stackoverflow.com/questions/217578/point-in-polygon-aka-hit-test/2922778#2922778)
* [circle / line intersection](http://stackoverflow.com/questions/1073336/circle-line-collision-detection)
* [polygon triangulation](http://en.wikipedia.org/wiki/Polygon_triangulation#Ear_clipping_method)
* [point set triangulation](http://en.wikipedia.org/wiki/Point_set_triangulation#Algorithms)
* [Voronoi diagram (french)](http://fr.wikipedia.org/wiki/Diagramme_de_Voronoï#Algorithme_de_Green_et_Sibson)

