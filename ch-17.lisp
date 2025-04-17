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
               (if (propagate-constraints v2)
                   (search-solutions diagram2)
                   nil)))
         (vertex-labelings v)))))

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
  (format stream "~a/~d" (vertex-name vertex)
          (number-of-labelings vertex))
  vertex)

(defun show-vertex (vertex &optional (stream t))
  "Print a vertex in a long form, on a new line."
  (format stream "~&   ~a ~d:" vertex (vertex-type vertex)
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
      (format stream "~&For ~:d interpretation~:p." n))))

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
    name)

  (defun query-diagram (name)
    (gethash name diagrams)))


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
  (rest (rest vertex-descriptor)))6

;; The DEFSTRUCT macro automatically creates a function called COPY-<struct-type-name>.
;; However, this function makes shallow copies. We need a function that returns a deep
;; copy of a diagram:

(defun make-copy-diagram (diagram)
  "Make a copy of the diagram, preserving connectivity."
  (let* ((new (make-diagram
               :vertexes
               (mapcar #'copy-vertex
                       (diagram-vertexes diagram)))))
    ;; Put in the neighbors for each vertex.
    (dolist (v (diagram-vertexes new))
      (setf (vertex-neighbors v)
            (mapcar #'(lambda (neighbor)
                        (find-vertex (vertex-name neighbor) new))
                    (vertex-neighbors v))))
    new))

(defdiagram cube
  (a Y b c d)
  (b W g e a)
  (c W e f a)
  (d W f g a)
  (e L c b)
  (f L d c)
  (g L b d))

#| 
CONSTRAINT-SATISFACTION> (print-labelings (diagram 'cube))
The iniial diagram is: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
   A/5 Y: BG=[L-+] BE=[R-+] BA=[++-]
   B/3 W: CE=[L-+] CF=[R-+] CA=[++-]
   C/3 W: DF=[L-+] DG=[R-+] DA=[++-]
   D/3 W: EC=[RL+L-R] EB=[LRR+L-]
   E/6 L: FD=[RL+L-R] FC=[LRR+L-]
   F/6 L: GB=[RL+L-R] GD=[LRR+L-]
   G/6 L:
For 29,160 interpretations.

After constraint propagation the diagram is: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[L-] BE=[R-] BA=[++]
   B/2 W: CE=[L-] CF=[R-] CA=[++]
   C/2 W: DF=[L-] DG=[R-] DA=[++]
   D/2 W: EC=[R-R] EB=[LL-]
   E/3 L: FD=[R-R] FC=[LL-]
   F/3 L: GB=[R-R] GD=[LL-]
   G/3 L:
For 216 interpretations.

There are four solutions:

Diagram: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[L] BE=[R] BA=[+]
   B/1 W: CE=[L] CF=[R] CA=[+]
   C/1 W: DF=[L] DG=[R] DA=[+]
   D/1 W: EC=[R] EB=[L]
   E/1 L: FD=[R] FC=[L]
   F/1 L: GB=[R] GD=[L]
   G/1 L:

Diagram: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[L] BE=[R] BA=[+]
   B/1 W: CE=[L] CF=[R] CA=[+]
   C/1 W: DF=[-] DG=[-] DA=[+]
   D/1 W: EC=[R] EB=[L]
   E/1 L: FD=[-] FC=[L]
   F/1 L: GB=[R] GD=[-]
   G/1 L:

Diagram: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[L] BE=[R] BA=[+]
   B/1 W: CE=[-] CF=[-] CA=[+]
   C/1 W: DF=[L] DG=[R] DA=[+]
   D/1 W: EC=[-] EB=[L]
   E/1 L: FD=[R] FC=[-]
   F/1 L: GB=[R] GD=[L]
   G/1 L:

Diagram: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[-] BE=[-] BA=[+]
   B/1 W: CE=[L] CF=[R] CA=[+]
   C/1 W: DF=[L] DG=[R] DA=[+]
   D/1 W: EC=[R] EB=[-]
   E/1 L: FD=[R] FC=[L]
   F/1 L: GB=[-] GD=[L]
   G/1 L:
; No value

Running (print-labelings (diagram 'cube))
returns four solutions, corresponding to the four
ways that the cube labelings, which are two-dimensional
projections of a possible 3-dimensional object, could be
physically interpreted. These are:

1. A cube that is free-floating
2. A cube that is attached to the floor
3. A cube that is attached to a wall on the right
4. A cube that is attached to a wall on the left

We can narrow the possibilities by choosing a 
position. |#

(defun ground (diagram vertex-a vertex-b)
  "Attach the line between the two vertexes to the ground.
  That is, label the line with a -"
  (let* ((a (find-vertex vertex-a diagram))
         (b (find-vertex vertex-b diagram))
         (i (position b (vertex-neighbors a))))
    (assert (not (null i)))
    (setf (vertex-labelings a)
          (find-all-if #'(lambda (l) (eq (nth i l) '-))
                       (vertex-labelings a)))
    diagram))

#|
CONSTRAINT-SATISFACTION> (print-labelings (ground (diagram 'cube) 'g 'd))
The iniial diagram is: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
   A/5 Y: BG=[L-+] BE=[R-+] BA=[++-]
   B/3 W: CE=[L-+] CF=[R-+] CA=[++-]
   C/3 W: DF=[L-+] DG=[R-+] DA=[++-]
   D/3 W: EC=[RL+L-R] EB=[LRR+L-]
   E/6 L: FD=[RL+L-R] FC=[LRR+L-]
   F/6 L: GB=[R] GD=[-]
   G/1 L:
For 4,860 interpretations.

After constraint propagation the diagram is: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[L] BE=[R] BA=[+]
   B/1 W: CE=[L] CF=[R] CA=[+]
   C/1 W: DF=[-] DG=[-] DA=[+]
   D/1 W: EC=[R] EB=[L]
   E/1 L: FD=[-] FC=[L]
   F/1 L: GB=[R] GD=[-]
   G/1 L:
; No value

It works. Now, we make something more complex:
a cube on a plate. We still need to `ground` it.
|#

(defdiagram cube-on-plate
  (a Y b c d)
  (b W g e a)
  (c W e f a)
  (d W f g a)
  (e L c b)
  (f Y d c i)
  (g Y b d h)
  (h W l g j)
  (i W f m j)
  (j Y h i k)
  (k W m l j)
  (l L h k)
  (m L k i))

#|
CONSTRAINT-SATISFACTION> (print-labelings (ground (diagram 'cube-on-plate) 'k 'm))
The iniial diagram is: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
   A/5 Y: BG=[L-+] BE=[R-+] BA=[++-]
   B/3 W: CE=[L-+] CF=[R-+] CA=[++-]
   C/3 W: DF=[L-+] DG=[R-+] DA=[++-]
   D/3 W: EC=[RL+L-R] EB=[LRR+L-]
   E/6 L: FD=[+-L-R] FC=[+-RL-] FI=[+--RL]
   F/5 Y: GB=[+-L-R] GD=[+-RL-] GH=[+--RL]
   G/5 Y: HL=[L-+] HG=[R-+] HJ=[++-]
   H/3 W: IF=[L-+] IM=[R-+] IJ=[++-]
   I/3 W: JH=[+-L-R] JI=[+-RL-] JK=[+--RL]
   J/5 Y: KM=[-] KL=[-] KJ=[+]
   K/1 W: LH=[RL+L-R] LK=[LRR+L-]
   L/6 L: MK=[RL+L-R] MI=[LRR+L-]
   M/6 L:
For 32,805,000 interpretations.

After constraint propagation the diagram is: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[L] BE=[R] BA=[+]
   B/1 W: CE=[L] CF=[R] CA=[+]
   C/1 W: DF=[-] DG=[-] DA=[+]
   D/1 W: EC=[R] EB=[L]
   E/1 L: FD=[-] FC=[L] FI=[R]
   F/1 Y: GB=[R] GD=[-] GH=[L]
   G/1 Y: HL=[L] HG=[R] HJ=[+]
   H/1 W: IF=[L] IM=[R] IJ=[+]
   I/1 W: JH=[+] JI=[+] JK=[+]
   J/1 Y: KM=[-] KL=[-] KJ=[+]
   K/1 W: LH=[R] LK=[-]
   L/1 L: MK=[-] MI=[L]
   M/1 L:
; No value

It works. What happens if we give it an `impossible` figure?
|#
(defdiagram poiuyt
  (a L b g)
  (b L j a)
  (c L d l)
  (d L h c)
  (e L f i)
  (f L k e)
  (g L a l)
  (h L l d)
  (i L e k)
  (j L k b)
  (k W j i f)
  (l W h g c))

#|
CONSTRAINT-SATISFACTION> (print-labelings (diagram 'poiuyt))
The iniial diagram is: AB=[RL+L-R] AG=[LRR+L-]
   A/6 L: BJ=[RL+L-R] BA=[LRR+L-]
   B/6 L: CD=[RL+L-R] CL=[LRR+L-]
   C/6 L: DH=[RL+L-R] DC=[LRR+L-]
   D/6 L: EF=[RL+L-R] EI=[LRR+L-]
   E/6 L: FK=[RL+L-R] FE=[LRR+L-]
   F/6 L: GA=[RL+L-R] GL=[LRR+L-]
   G/6 L: HL=[RL+L-R] HD=[LRR+L-]
   H/6 L: IE=[RL+L-R] IK=[LRR+L-]
   I/6 L: JK=[RL+L-R] JB=[LRR+L-]
   J/6 L: KJ=[L-+] KI=[R-+] KF=[++-]
   K/3 W: LH=[L-+] LG=[R-+] LC=[++-]
   L/3 W:
For 544,195,584 interpretations.

After constraint propagation the diagram is: AB=[RL+-R] AG=[LRRL-]
   A/5 L: BJ=[RLL-R] BA=[LR+L-]
   B/5 L: CD=[LR] CL=[+-]
   C/2 L: DH=[RL-] DC=[LRL]
   D/3 L: EF=[RLR] EI=[LR-]
   E/3 L: FK=[+-] FE=[RL]
   F/2 L: GA=[RL-R] GL=[L+L-]
   G/4 L: HL=[R+-R] HD=[LRL-]
   H/4 L: IE=[RL-R] IK=[L+L-]
   I/4 L: JK=[R+-R] JB=[LRL-]
   J/4 L: KJ=[L-+] KI=[R-+] KF=[++-]
   K/3 W: LH=[L-+] LG=[R-+] LC=[++-]
   L/3 W:
For 2,073,600 interpretations.

There are zero solutions:
; No value

It correctly spotted our ruse.

How about a much more complex (possible) diagram, which we
will call a `tower`?
|#

(defdiagram tower
  (a Y b c d)
  (b W g e a)
  (c W e f a)
  (d W f g a)
  (e L c b)
  (f Y d c i)
  (g Y b d h)
  (h W l g j)
  (i W f m p)
  (j Y h o k)
  (k W m l j)
  (l L h k)
  (m L k i)
  (n L q o)
  (o W y j n)
  (p L r i)
  (q W n s w)
  (r W s p x)
  (s L r q)
  (t W w x z)
  (u W x y z)
  (v W y w z)
  (w Y t v q)
  (x Y r u t)
  (y Y v u o)
  (z Y t u v))

#|
CONSTRAINT-SATISFACTION> (print-labelings (ground (diagram 'tower) 'l 'k))
The iniial diagram is: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
   A/5 Y: BG=[L-+] BE=[R-+] BA=[++-]
   B/3 W: CE=[L-+] CF=[R-+] CA=[++-]
   C/3 W: DF=[L-+] DG=[R-+] DA=[++-]
   D/3 W: EC=[RL+L-R] EB=[LRR+L-]
   E/6 L: FD=[+-L-R] FC=[+-RL-] FI=[+--RL]
   F/5 Y: GB=[+-L-R] GD=[+-RL-] GH=[+--RL]
   G/5 Y: HL=[L-+] HG=[R-+] HJ=[++-]
   H/3 W: IF=[L-+] IM=[R-+] IP=[++-]
   I/3 W: JH=[+-L-R] JO=[+-RL-] JK=[+--RL]
   J/5 Y: KM=[L-+] KL=[R-+] KJ=[++-]
   K/3 W: LH=[R] LK=[-]
   L/1 L: MK=[RL+L-R] MI=[LRR+L-]
   M/6 L: NQ=[RL+L-R] NO=[LRR+L-]
   N/6 L: OY=[L-+] OJ=[R-+] ON=[++-]
   O/3 W: PR=[RL+L-R] PI=[LRR+L-]
   P/6 L: QN=[L-+] QS=[R-+] QW=[++-]
   Q/3 W: RS=[L-+] RP=[R-+] RX=[++-]
   R/3 W: SR=[RL+L-R] SQ=[LRR+L-]
   S/6 L: TW=[L-+] TX=[R-+] TZ=[++-]
   T/3 W: UX=[L-+] UY=[R-+] UZ=[++-]
   U/3 W: VY=[L-+] VW=[R-+] VZ=[++-]
   V/3 W: WT=[+-L-R] WV=[+-RL-] WQ=[+--RL]
   W/5 Y: XR=[+-L-R] XU=[+-RL-] XT=[+--RL]
   X/5 Y: YV=[+-L-R] YU=[+-RL-] YO=[+--RL]
   Y/5 Y: ZT=[+-L-R] ZU=[+-RL-] ZV=[+--RL]
   Z/5 Y:
For 1,614,252,037,500,000 interpretations.

After constraint propagation the diagram is: AB=[+] AC=[+] AD=[+]
   A/1 Y: BG=[L] BE=[R] BA=[+]
   B/1 W: CE=[L] CF=[R] CA=[+]
   C/1 W: DF=[-] DG=[-] DA=[+]
   D/1 W: EC=[R] EB=[L]
   E/1 L: FD=[-] FC=[L] FI=[R]
   F/1 Y: GB=[R] GD=[-] GH=[L]
   G/1 Y: HL=[L] HG=[R] HJ=[+]
   H/1 W: IF=[L] IM=[R] IP=[+]
   I/1 W: JH=[+] JO=[+] JK=[+]
   J/1 Y: KM=[-] KL=[-] KJ=[+]
   K/1 W: LH=[R] LK=[-]
   L/1 L: MK=[-] MI=[L]
   M/1 L: NQ=[R] NO=[-]
   N/1 L: OY=[+] OJ=[+] ON=[-]
   O/1 W: PR=[L] PI=[+]
   P/1 L: QN=[L] QS=[R] QW=[+]
   Q/1 W: RS=[L] RP=[R] RX=[+]
   R/1 W: SR=[R] SQ=[L]
   S/1 L: TW=[+] TX=[+] TZ=[-]
   T/1 W: UX=[+] UY=[+] UZ=[-]
   U/1 W: VY=[+] VW=[+] VZ=[-]
   V/1 W: WT=[+] WV=[+] WQ=[+]
   W/1 Y: XR=[+] XU=[+] XT=[+]
   X/1 Y: YV=[+] YU=[+] YO=[+]
   Y/1 Y: ZT=[-] ZU=[-] ZV=[-]
   Z/1 Y:
; No value

Think of it: the number of initial combinations
of vertex types if over 1 quadrillion. Yet the
constraint propagation technique narrows all of
these down to one single possible interpretaion
in a split second.

Most of the time is spent printing. So, to get
an idea of the speed of the processing, we 
write a function that returns solutions without
explicit printing. 
|#

(defun find-labelings (diagram)
  "Return a list of all consistent labelings of the diagram."
  (every #'propagate-constraints (diagram-vertexes diagram))
  (search-solutions diagram))

;; It turns out that the impossible figure takes longer to calculate than the
;; tower.
