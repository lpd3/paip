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

;; The following takes a diagram, reduces possibilities and searches
;; for consistent solutions. It prints the progress after each step.|#

(defun print-labelings (diagram)
  "Label the diagram by propagating constraints and then
  searching for solutions if necessary. Print results."
  (show-diagram diagram "~&The iniial diagram is:")
  (every #'propagate-constraints (diagram-vertexes diagram))
  (show-diagram diagram
                "~2&After constraint propagation the diagram is:")
  (let* ((solutions (if (impossible-diagram-p diagram)
                        nil
                        (search-solutions diagram)))
         (n (length solutions)))
    (unless (= n 1)
      (format t "~2&There are ~r solution~:p:" n)
      (mapc #'show-diagram solutions)))
  (values))

;; Now, the propagate-constraints function.

;; Takes a vertex. Considers the neighbors and removes conflicting possibilities.
;; If possibilities were removed, we visit the neighbors and repeat the process.
;; If a vertex is impossible (no possibilities are left), nil is immediately
;; returned, stopping the process. Otherwise, the process stops when there are
;; no more possible changes.

;; 1. In the previous function, we send each vertex to this function once. Since
;;    each call is recursive we need not do more than this.

;; 2. We know that the process will stop because there are a finite number of vertices,
;;    each with a finite number of states. There are therefor a finite number of
;;    system states. Since the process is always subtractive, and stops when there
;;    is nothing more thar can be done, the process must terminate.

(defun propagate-constraints (vertex)
  "Reduce the labelings on vertex by considering neighbors.
  If we can reduce, propagate the constraints to each neighbor."
  ;; Return nil only when the cobstraints lead to an impossibility.
  (let ((old-num (number-of-labelings vertex)))
    (setf (vertex-labelings vertex) (consistent-labelings vertex))
    (every #'propagate-constraints (vertex-neighbors vertex))
    (unless (impossible-vertex-p vertex)
      (when (< (number-of-labelings vertex) old-num)
        (every #'propagate-constraints (vertex-neighbors vertex)))
      t)))

;; consistent-labelings takes a vertex, gathers all the neighbors' labels and
;; checks all of the arg's vertices for consistency with the neighbors.

(defun consistent-labelings (vertex)
  "Return the set of labelings that are consistent with neighbors"
  (let ((neighbor-labels
          (mapcar #'(lambda (neighbor) (labels-for neighbor vertex))
                  (vertex-neighbors vertex))))
    ;; Eliminate labelings that don't have all lines consistent
    ;; with the corresponding line's label from the neighbor.
    ;; Account for the L-R mismatch with reverse-label
    (find-all-if
     #'(lambda (labeling)
         (every #'member (mapcar #'reverse-label labeling)
                neighbor-labels))
     (vertex-labelings vertex))))

;; Often, the process above will either indicate that there
;; is exactly one possible solution, or that there are no
;; possible solutions. But sometimes, there will still be more
;; than one possibility for a given vertex. In this case, we
;; choose one and repopulate all the other vertices, running
;; constraint propagation again. If this is not possible, then
;; we choose another.

(defun search-solutions (diagram)
  "Try all labelings for one ambiguous vertex, and propagate."
  ;; If there is no ambiguous vertex, return the diagram.
  ;; If there is one, return copies of the diagram, trying each of
  ;; the possible labelings. Propagate constraints and append
  ;; all the solutions together.
  (let ((v (find-if #'ambiguous-vertex-p
                    (diagram-vertexes diagram))))
    (if (null v)
        (list diagram)
        (mapcan
         #'(lambda (v-labeling)
             (let* ((diagram2 (make-copy-diagram diagram))
                    (v2 (find-vertex (vertex-name v) diagram2)))
               (setf (vertex-labelings v2) (list v-labeling))
               (if (propate-constraints v2)
                   (search-solutions diagram2)
                   nil)))
         (vetex-labelings v)))))

;; Some auxiliary functions

(defun labels-for (vertex from)
  "Return all the labels for the line going to vertex"
  (let ((pos (position from (vertex-neighbors vertex))))
    (mapcar #'(lambda (labeling) (nth pos labeling))
            (vertex-labelings vertex))))

(defun reverse-label (label)
  "Account for the fact that one label's right is another's left."
  (case label (L 'R) (R 'L) (otherwise label)))

(defun find-vertex (name diagram)
  "Find the vertex in the given diagram with the given name"
  (find name (diagram-vertexes diagram) :key #'vertex-name))

;; Printing utilities

(defun print-vertex (vertex stream depth)
  "Print a vertex in the short form."
  (declare (ignore depth))
  (format stream "-a/~d" (vertex-name vertex)
          (number-of-labelings vertex))
  vertex)

(defun show-vertex (vertex stream depth)
  "Print a vertex in a long form, on a new line."
  (format stream "~&   ~a ~d:" (vertex-type vertex)
          (mapc #'(lambda (neighbor labels)
                    (format stream " ~a~a=[~{~a~}]" (vertex-name vertex)
                            (vertex-name neighbor) labels))
                (vertex-neighbors vertex)
                (matrix-transpose (vertex-labelings vertex)))
          (values)))

(defun show-diagram (diagram &optional (title "~2&Diagram:")
                               (stream t))
  "Print a diagram in long form. Include a title."
  (format stream title)
  (mapc #'show-vertex (diagram-vertexes diagram))
  (let ((n (reduce #'* (mapcar #'number-of-labelings
                               (diagram-vertexes diagram)))))
    (when (> n 1)
      (format stream "~&For ~d: interpretation~:p." n))))

;; Now, a function to transpose a matrix

(defun matrix-transpose (matrix)
  "Turn a matrix on its side."
  (if matrix (apply #'mapcar #'list matrix)))

;; Now we need a way to construct diagrams. In this
;; rather simple implementation, we are going to design
;; operators for constructing diagrams. In the real world,
;; we might instead rely on a line-recognizing algorithm set
;; to concrete images, or a line drawing program using a bitmap
;; linked to the dislay with mouse input.

(defmacro defdiagram (name &rest vertex-descriptors)
  "Define a diagram. A copy can be gotten by (diagram name)."
  `(put-diagram ',name (construct-diagram ',vertex-descriptors)))

(let ((diagrams (make-hash-table)))

  (defun diagram (name)
    "Get a fresh copy of the diagram with this name"
    (make-copy-diagram (gethash name diagrams)))

  (defun put-diagram (name diagram)
    "Store a diagram under a name."
    (setf (gethash name diagrams) diagram)
    name))

(defun construct-diagram (vertex-descriptors)
  "Build a new diagram from a set of vertex-descriptors."
  (let ((diagram (make-diagram)))
    ;; Put in the vertices
    (setf (diagram-vertexes diagram)
          (mapcar #'construct-vertex vertex-descriptors))
    ;; Put in the neighbors of each vertex
    (dolist (v-d vertex-descriptors)
      (setf (vertex-neighbors (find-vertex (first v-d) diagram))
            (mapcar #'(lambda (neighbor)
                        (find-vertex neighbor diagram))
                    (v-d-neighbors v-d))))
    diagram))

(defun construct-vertex (vertex-descriptor)
  "Build the vertex corresponding to the constructor"
  ;; Descriptors are like (x L y z)
  (make-vertex
   :name (first vertex-descriptor)
   :type (second vertex-descriptor)
   :labelings (possible-labelings (second vertex-descriptor))))

(defun v-d-neighbors (vertex-descriptor)
  "The neighboring vertex names in a vertex descriptor."
  (rest (rest vertex-descriptor)))

;; The DEFSTRUCT macro automatically creates a function called COPY-<struct-type-name>.
;; However, this function makes shallow copies. We need a function that returns a deep
;; copy of a diagram:

(defun make-copy-diagram (diagram)
  "Make a copy of the diagram, preserving connectivity."
  (let* ((new (make-diagram
               (mapcar #'copy-vertex
                       (diagram-vertexes diagram)))))
    ;; Put in the neighbors for each vertex.
    (dolist (v (diagram-vertexes new))
      (setf (vertex-neighbors v)
            (mapcar #'(lambda (neighbor)
                        (find-vertex (vertex-name neighbor) new))
                    (vertex-neighbors v))))))
