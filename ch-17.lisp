;;;; Chapter 17: Line-Diagram Labeling by Constraint Satisfaction

#| This chapter is concerned with a very small component of computer vision: interpretation of the
lines of an image. |#
;

(in-package :constraint-satisfaction)


#| 17.1 The Line-Labeling Problem 
In this chapter, we consider only diagrams that depict one or more polyhedra.
Furthermore, we will be concerned only with `tri-hedral vertices`: each vertex will represent the
intersection of three planes.
Furthermore, no `accidental vertices` will be considered: no vertex will represent the the inter-
section or conjunction of three non-touching polyhedra.

Given these restrictions, our task is to identify each line and place it in one of three classes:

1. Convex line: Separates two visible faces where a line drawn from one face to the other would 
   lie inside the polyhedron. Marked with +
2. Concave line: Separates two faces of two polyhedra where a line drawn from one face to the 
   other would lie in empty space. Marked with -
3. Boundary line: The same physical situation as a convex line, but only one of the two faces is 
   visible (due to the orientation of the polyhedron). These are marked with --> such that, if we
   were standing on the arrow and facing the point of the arrow, the
   polyhedron would be to our right.

We enumerate all types of vertices and all possible labelings for the vertex.

In trihedral polyhedrons, there are only 4 kinds of vertices:
(directions are away from the vertex)

1. L vertices

   Lines: 1 (north), 2 (east)

   Possibilities: 
   a. 1: arrow away from vertex
      2: arrow toward vertex

   b. 1: arrow toward vertex
      2: arrow away from vertex

   c. 1: arrow toward vertex
      2: +

   d. 1: +
      2: arrow away from vertex

   e. 1: -
      2: arrow toward vertex

   f. 1: arrow away from vertex
      2: -

2. Y (aka Fork) vertices
   Lines: 1 (northwest), 2 (northeast) (ne), 3 (south)

   Possibilities:
   a. 1: +
      2: +
      3: +

   b. 1: -
      2: -
      3: -

   c. 1: -
      2: arrow toward vertex
      3: arrow away from vertex

   d. 1: arrow toward vertex
      2: arrow away from vertex
      3: -

   e. 1: arrow away from vertex
      2: -
      3: arrow toward vertex

3. T vertices
     Lines: 1 (west), 2 (east), 3 (south)

   Possibilities:
   a. 1: arrow away from vertex
      2: arrow toward vertex
      3: -

   b. 1: arrow away from vertex
      2: arrow toward vertex
      3: +

   c. 1: arrow away from vertex
      2: arrow toward vertex
      3: arrow toward vertex

   d. 1: arrow away from vertex
      2: arrow toward vertex
      3: arrow away from vertex

4. W (aka Arrow) vertices
   Lines: 1 (northeast), 2 (north), 3 (northwest)

   Possibilities:
   a. 1: +
      2: -
      3: +

   b. 1. Arrow toward vertex
      2. +
      3. Arrow away from vertex

   c. 1. -
      2. +
      3. -

Vertices are encoded like this:

(v L 1 2)
(v Y 1 2 3)
(v T 1 2 3)
(v W 1 2 3)

Each line connects two vertices, and thus must satisfy two constraints.

Mode of attack:
1. Label each vertex with all possibilities of its type
2. Choose a vertex V and a neighboring vertex N. A neighboring vertex shares
   a line.
3. Attempt to reduce the possibilities of the vertices using constraint propagation.
4. Repeat for all neighboring vertices.
5. Stop when each vertex has been visited at least once and all constraints have
   propagated.

Most diagrams will be uniquely identified by this process. However, some will remain
ambiguous. In this case, choose a possibility and rerun constraint 
propagation. Continue until there is a solution or an inconsistency.

We need two structs: diagram and vertex.
   
|#

(defstruct diagram "A diagram is a list of vertexes" vertexes)

(defstruct (vertex (:print-function print-vertex))
  (name nil :type atom)
  (type 'L :type (member L Y W T))
  (neighbors nil :type list) ; of vertex
  (labelings nil :type list)) ; of lists of (member + - L R)

#|

Ambiguous vertex: more than one possibility
Unambiguous vertex: exactly one possibility
Impossible vertex: no possibilities

Labeling is a list and not a set: the order is influenced by the neighbors

R: equivalent to an arrow traveling away from the vertex
L: equivalent to an arrow traveling toward the vertex

|#

(defun ambiguous-vertex-p (vertex)
  "A vertex is ambiguous if it has more than one labeling"
  (> (number-of-labelings vertex) 1))

(defun number-of-labelings (vertex)
  (length (vertex-labelings vertex)))

(defun impossible-vertex-p (vertex)
  "A vertex is impossible if it has no labeling"
  (null (vertex-labelings vertex)))

(defun impossible-diagram-p (diagram)
  "An impossible diagram is a diagram with an impossible vertex"
  (some #'impossible-vertex-p (diagram-vertexes diagram)))

(defun possible-labelings (vertex-type)
  "The list of possible labelings for a vertex type."
  ;; In these labelings, R means an arrow pointing away from the vertex, L means an arrow pointing
  ;; towards it.
  (case vertex-type
    ((L) '((R L) (L R) (+ R) (L +) (- L) (R -)))
    ((Y) '((+ + +) (- - -) (L R -) (- L R) (R - L)))
    ((T) '((R L +) (R L -) (R L L) (R L R)))
    ((W) '((L R +) (- - +) (+ + -)))))

#| 17.2 Combining Constraints and Searching |#
