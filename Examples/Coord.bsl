me Examples.Coord

use Core.Math


sig cartesianXOfSpherical : Real -> Real -> Real -> Real
def cartesianXOfSpherical r@ phi@ th@ =
  r * cos phi * sin th

sig cartesianYOfSpherical : Real -> Real -> Real -> Real
def cartesianYOfSpherical r@ phi@ th@ =
  r * sin phi * sin th

sig cartesianZOfSpherical : Real -> Real -> Real -> Real
def cartesianZOfSpherical r@ phi@ th@ =
  r * cos th

sig cartesianOfSpherical : Real -> Real -> Real -> (Real, Real, Real)
def cartesianOfSpherical r@ phi@ th@ =
  [cartesianXOfSpherical r phi th,
   cartesianYOfSpherical r phi th,
   cartesianZOfSpherical r phi th]


sig cartesianXOfCylindrical : Real -> Real -> Real -> Real
def cartesianXOfCylindrical rho@ phi@ z@ =
  rho * cos phi

sig cartesianYOfCylindrical : Real -> Real -> Real -> Real
def cartesianYOfCylindrical rho@ phi@ z@ =
  rho * sin phi

sig cartesianZOfCylindrical : Real -> Real -> Real -> Real
def cartesianZOfCylindrical rho@ phi@ z@ =
  z

sig cartesianOfCylindrical : Real -> Real -> Real -> (Real, Real, Real)
def cartesianOfCylindrical rho@ phi@ z@ =
  [cartesianXOfCylindrical rho phi z,
   cartesianYOfCylindrical rho phi z,
   cartesianZOfCylindrical rho phi z]


cotype Coord = x : Real | y : Real | z : Real


-- (define (λcoord c (λc λid) (λx λid) (λy λid) (λz λid))
--   (λc
--    (λx (xyz-x c))
--    (λy (xyz-y c))
--    (λz (xyz-z c))))


-- sig xyz : Real -> Real -> Real -> Dyn
-- def xyz x@ y@ z@ = Coord {x = x, y = y, z = z}

-- (define xy
--   (case-lambda
--     ((x y)
--      (xyz x y 0))
--     ((c x y)
--      (struct-copy coord c (x x) (y y)))))

-- (define yz
--   (case-lambda
--     ((y z)
--      (xyz 0 y z))
--     ((c y z)
--      (struct-copy coord c (y y) (z z)))))

-- (define xz
--   (case-lambda
--     ((x z)
--      (xyz x 0 z))
--     ((c x z)
--      (struct-copy coord c (x x) (z z)))))

-- (define x
--   (case-lambda
--     ((x)
--      (xyz x 0 0))
--     ((c x)
--      (struct-copy coord c (x x)))))

-- (define y
--   (case-lambda
--     ((y)
--      (xyz 0 y 0))
--     ((c y)
--      (struct-copy coord c (y y)))))

-- (define z
--   (case-lambda
--     ((z)
--      (xyz 0 0 z))
--     ((c z)
--      (struct-copy coord c (z z)))))

-- (define xyz-x coord-x)
-- (define xyz-y coord-y)
-- (define xyz-z coord-z)

-- (define (xyz-r c)
--   (sqrt (+ (^2 (xyz-x c)) (^2 (xyz-y c)) (^2 (xyz-z c)))))


-- (define (cyl rho phi z)
--   (apply xyz (cartesian<-cylindrical rho phi z)))

-- (define (cyl-rho c)
--   (sqrt (+ (^2 (xyz-x c)) (^2 (xyz-y c)))))

-- (define (cyl-phi c)
--   (sph-phi c))

-- (define (cyl-z c)
--   (xyz-z c))


-- (define (pol rho phi)
--   (cyl rho phi 0))

-- (define (pol-rho c)
--   (cyl-rho (struct-copy coord c (z 0))))

-- (define (pol-phi c)
--   (cyl-phi (struct-copy coord c (z 0))))


-- (define (sph r phi th)
--   (apply xyz (cartesian<-spherical r phi th)))

-- (define (sph-r c)
--   (xyz-r c))

-- (define (sph-phi c)
--   (if (= (xyz-x c) (xyz-y c) 0)
--       0
--       (atan (xyz-y c) (xyz-x c))))

-- (define (sph-th c)
--   (if (= (sph-r c) 0)
--       0
--       (acos (/ (xyz-z c) (sph-r c)))))


-- (define u0 (xyz 0 0 0))

-- (define ux (xyz 1 0 0))
-- (define uy (xyz 0 1 0))
-- (define uz (xyz 0 0 1))

-- (define uxy (xy 1 1))
-- (define uyz (yz 1 1))
-- (define uxz (xz 1 1))

-- (define uxyz (xyz 1 1 1))


-- (define (is-u0 c)
--   (and
--    (coord? c)
--    (= (xyz-x c) 0)
--    (= (xyz-y c) 0)
--    (= (xyz-z c) 0)))

-- (define (eq-c c1 c2 (threshold-d 0))
--   (define (eq-p)
--     (and
--      (= (xyz-x c1) (xyz-x c2))
--      (= (xyz-y c1) (xyz-y c2))
--      (= (xyz-z c1) (xyz-z c2))))
  
--   (if (= threshold-d 0)
--       (eq-p)
--       (or (eq-p) (<= (distance c1 c2) threshold-d))))

-- (define add-c
--   (let ((add-coord
--          (λ (c1 c2)
--            (xyz
--             (+ (xyz-x c1) (xyz-x c2))
--             (+ (xyz-y c1) (xyz-y c2))
--             (+ (xyz-z c1) (xyz-z c2))))))
--     (case-lambda
--       ((c1 c2)
--        (add-coord c1 c2))
--       ((c1 c2 . cs)
--        (reduce add-coord (list* c1 c2 cs))))))

-- (define sub-c
--   (let* ((sub-coord
--           (λ (c1 c2)
--             (xyz
--              (- (xyz-x c1) (xyz-x c2))
--              (- (xyz-y c1) (xyz-y c2))
--              (- (xyz-z c1) (xyz-z c2)))))
--          (sub-coord-fn
--           (λ (c1 c2)
--             (sub-coord c2 c1))))
--     (case-lambda
--       ((c1 c2)
--        (sub-coord c1 c2))
--       ((c1 c2 . cs)
--        (foldl sub-coord-fn c1 (cons c2 cs))))))

-- (define (dot-c c1 c2)
--   (+
--    (* (xyz-x c1) (xyz-x c2))
--    (* (xyz-y c1) (xyz-y c2))
--    (* (xyz-z c1) (xyz-z c2))))

-- (define (cross-c c1 c2)
--   (xyz
--    (-
--     (* (xyz-y c1) (xyz-z c2))
--     (* (xyz-z c1) (xyz-y c2)))
--    (-
--     (* (xyz-z c1) (xyz-x c2))
--     (* (xyz-x c1) (xyz-z c2)))
--    (-
--     (* (xyz-x c1) (xyz-y c2))
--     (* (xyz-y c1) (xyz-x c2)))))

-- (define (collinear-cross-c c)
--   (define (collinear-pxp-1)
--     (xyz 0 (xyz-z c) (- (xyz-y c))))
  
--   (define (collinear-pxp-2)
--     (xyz (xyz-z c) 0 (- (xyz-x c))))
  
--   (define (collinear-pxp-3)
--     (xyz (xyz-y c) (- (xyz-x c)) 0))
  
--   (let ((n (collinear-pxp-1)))
--     (if (is-u0 n)
--         (let ((n (collinear-pxp-2)))
--           (if (is-u0 n)
--               (collinear-pxp-3)
--               n))
--         n)))

-- (define (angle-c c1 c2 (normal #f))
--   (define (aux)
--     (acos
--      (dot-c
--       (mult-c c1 (/ (sph-r c1)))
--       (mult-c c2 (/ (sph-r c2))))))
  
--   (define (continuous-aux)
--     (let ((p (cross-c c1 c2)))
--       (if (or
--            (eq-c u0 p)
--            (> (dot-c normal p) 0))
--           (aux)
--           (- 2pi (aux)))))
  
--   (if normal
--       (continuous-aux)
--       (aux)))

-- (define (collinearity-c c1 c2)
--   (define (n//n n1 n2)
--     (cond ((= n1 n2 0) #t)
--           ((= n2 0) 0)
--           (else (/ n1 n2))))
  
--   (define (ns//ns)
--     (filter
--      (cλ (λ. not eqv?) true)
--      (list
--       (n//n (xyz-x c1) (xyz-x c2))
--       (n//n (xyz-y c1) (xyz-y c2))
--       (n//n (xyz-z c1) (xyz-z c2)))))
  
--   (let ((ns (ns//ns)))
--     (cond ((empty? ns) 0)
--           ((apply = (cons (first ns) ns)) (first ns))
--           (else 0))))


-- (define (mult-c c/r1 c/r2)
--   (define (mult-coord c r)
--     (xyz
--      (* (xyz-x c) r)
--      (* (xyz-y c) r)
--      (* (xyz-z c) r)))
  
--   (if (coord? c/r1)
--       (mult-coord c/r1 c/r2)
--       (mult-coord c/r2 c/r1)))


-- (define (distance c1 c2)
--   (xyz-r (sub-c c1 c2)))

-- (define (midcoord c1 c2)
--   (let ((axis (sub-c c2 c1)))
--     (add-xyz-c c1 (/2 (xyz-x axis)) (/2 (xyz-y axis)) (/2 (xyz-z axis)))))

-- (define (norm c)
--   (mult-c c (/ (sph-r c))))

-- (define (symmetric c)
--   (λcoord c xyz - - -))

-- ; rotates 'c2' by 'φ' with center in 'c1'
-- (define (rotate-c c1 φ c2)
--   (add-c c1 (add-phi-c (sub-c c2 c1) φ)))

-- ; scales 'c1' by 's' in the direction of 'c2'
-- (define (scale-c c1 s c2)
--   (add-c c1 (mult-c (sub-c c2 c1) s)))


-- (define (add-x-c c r)
--   (λcoord c xyz (cλ + r) λid λid))

-- (define (add-y-c c r)
--   (λcoord c xyz λid (cλ + r) λid))

-- (define (add-z-c c r)
--   (λcoord c xyz λid λid (cλ + r)))

-- (define (add-xy-c c x y)
--   (λcoord c xyz (cλ + x) (cλ + y) λid))

-- (define (add-yz-c c y z)
--   (λcoord c xyz λid (cλ + y) (cλ + z)))

-- (define (add-xz-c c x z)
--   (λcoord c xyz (cλ + x) λid (cλ + z)))

-- (define (add-xyz-c c x y z)
--   (λcoord c xyz (cλ + x) (cλ + y) (cλ + z)))


-- (define (add-cyl-c c rho phi z)
--   (apply add-xyz-c c (cartesian<-cylindrical rho phi z)))

-- (define (add-rho-c c rho)
--   (cyl (+ (cyl-rho c) rho) (cyl-phi c) (cyl-z c)))

-- (define (add-phi-c c phi)
--   (cyl (cyl-rho c) (+ (cyl-phi c) phi) (cyl-z c)))


-- (define (add-pol-c c rho phi)
--   (add-cyl-c c rho phi 0))

-- (define (add-sph-c c r phi th)
--   (apply add-xyz-c c (cartesian<-spherical r phi th)))

-- (define (add-r-c c r)
--   (sph (+ (sph-r c) r) (sph-phi c) (sph-th c)))

-- (define (add-th-c c th)
--   (sph (sph-r c) (sph-phi c) (+ (sph-th c) th)))


-- (define (list-of-coord c)
--   (λcoord c list))

-- (define (vector-of-coord c)
--   (λcoord c vector))

-- (define (coord-of-vector v)
--   (apply xyz (vector->list v)))