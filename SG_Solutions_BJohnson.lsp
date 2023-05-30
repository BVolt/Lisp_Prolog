;; Brenden Johnson 3520.01 Study Guide LISP Solutions


; 1 compute x^y
(defun power (x y)
  (if (= y 0)
      1
      (* x (power x (- y 1)))
      )
    )

;2 Create a function that computes x^2 without using multiplication
(defun square (n)
  (if (= n 0)
      0
      (+ (square (- n 1)) n n -1)
      )
  )


;3 Create a function that takes a list of pairs of integers, and orders the elements of each pair in non-increasing order.
(defun order-pairs (lst)
  (if (eq lst nil)
      nil
      (let* ((pair (car lst))
             (p1 (car pair))
             (p2 (cadr pair)))
        (cons (if (< p1 p2)
                  (list p2 p1)
                  pair)
              (order-pairs (cdr lst))
        )
      )
  )
  )



;4 Create a function that takes a list of integers and returns a pair consisting of the sum of the numbers in even positions and the sum of the numbers in odd positions
(defun sum-even-odd-pos (lst)
  (let ((sum-even 0) (sum-odd 0) (pos 0))
    (do ((i lst (cdr i)))
        ((eq i nil) (list sum-even sum-odd))
        (if (eq (mod pos 2) 0)
            (setf sum-even (+ sum-even (car i)))
            (setf sum-odd (+ sum-odd (car i))))
        (setf pos (+ pos 1))
    )
  )
  )




; 5 Create a function that takes a list of pairs of integers and returns a list with the sum of the elements of every pair
(defun sum-pairs (lst)
  (if (eq lst nil)
      nil
      (cons (+ (car (car lst)) (cadr (car lst)))
            (sum-pairs (cdr lst))
      )
  )
)


; 6 Create a function that takes a list of pairs of integers and returns a pair with the sum of the elements in the first position and the sum of the elements in the second position.
(defun sum-pair-elements (lst)
  (let ((sum1 0) (sum2 0))
    (do ((i lst (cdr i)))
        ((eq i nil) (list sum1 sum2))
        (setf sum1 (+ sum1 (car (car i))))
        (setf sum2 (+ sum2 (cadr (car i))))
    )
  )
)

; 7 Polynomials may be represented as lists of integer, where each integer is the coefficient of the corresponding monomial
(defun evalPoly (poly val)
  (cond
    ((null poly) 0)
    ((null (cdr poly)) (car poly))
    (t (+ (evalMono (car poly)
                    (- (length poly) 1)
                    val
                    )
          (evalPoly (cdr poly) val)
          )
       )
    )
  )

(defun evalMono (coeff expo val)
  (* coeff (expt val expo))
  )

; 8 Create a function that eliminates all duplicate elements from a list
(defun remove-dups (lst)
  (if (eq lst nil)
      nil
      (cons (car lst) 
            (remove-duplicates (remove (car lst) (cdr lst))))
  )
)

; 9 Create a function that packs consecutive duplicate elements of a list into sublists
(defun pack (lst)
  (if (null lst)
      nil
      (let ((val (car lst)))
        (cons (pack-group val (cdr lst))
              (pack (drop-group val (cdr lst)))))
      )
  )

(defun pack-group (val lst)
  (if (or (null lst) (not (eq (car lst) val)))
      (list val)
      (cons val (pack-group val (cdr lst)))
      )
  )

(defun drop-group (val lst)
  (if (or (null lst) (not (eq (car lst) val)))
      lst
      (drop-group val (cdr lst))
      )
  )



; 10  Create a function that packs all duplicate elements of a list into sublists
(defun fullPack (lst)
  (if (null lst)
      nil
      (let ((hpar (headPackAndRem (car lst) lst nil nil)))
        (cons (car hpar) (fullPack (cdr hpar)))
        )
      )
  )

(defun headPackAndRem (elem lst acc1 acc2)
  (if (null lst)
      (cons acc1 (reverse acc2))
      (let ((filterC (equal elem (car lst))))
        (headPackAndRem elem
                        (cdr lst)
                        (if filterC
                            (cons elem acc1)
                            acc1
                            )
                        (if filterC
                            acc2
                            (cons (car lst) acc2)
                            )
                        )
        )
      )
  )


; 11 Create a function that computes the length encoding of a list, which is a list of pairs with every elements and times it appears consecutively at a given position.
(defun length-encode (lst)
  (if (null lst)
      nil
      (let ((val (car lst))
            (run-length (count-run lst)))
        (cons (list run-length val)
              (length-encode (nthcdr run-length lst))))
      )
  )

(defun count-run (lst)
  (if (or (null (cdr lst)) (not (eq (car lst) (cadr lst))))
      1
      (+ 1 (count-run (cdr lst)))
      )
  )


; 12 Create a function that decodes length encoding. 
(defun decode (codeL)
  (if (null codeL)
      nil
      (let ((numC (car (car codeL)))
            (elem (cdr (car codeL))))
        (append (decodeSingCode numC elem nil) (decode (cdr codeL)))
        )
      )
  )
(defun decodeSingCode (count elem acc)
  (if (zerop count)
      acc
      (decodeSingCode (decf count) elem (cons elem acc))
      )
  )



; 13 Create a function that takes a list and two integers a and b and returns the sublist that starts on a and ends on b
(defun sublist (lst a b)
  (if (> a b)
      nil
      (do ((i a (+ i 1))
           (result nil))
          ((> i b) (nreverse result))
        (push (nth i lst) result)
        )
      )
  )


; 14 Create a version of the function in problem (13) that when a > b it doesn't return an empty list, but the sublist that starts at a and ends in b wrapping around the end of the list.
(defun sublist-wrap (lst a b)
  (if (< a b)
      (sublist lst a b)
      (let ((tail (sublist lst a (- (length lst) 1)))
            (head (sublist lst 0 b)))
        (append tail head))
      )
  )


; 15 Create a a function that takes a pair of integers a and b, and returns a list with every integer between a and b.
(defun range (a b)
  (if (> a b)
      nil
      (cons a (range (+ a 1) b))
  )
)


; 16 Create a function that takes a number k and a list, and returns a list of lists with all the combinations of k distinct elements from the list
(defun combination (l k)
  (cond
    ((= k 0) (list nil))
    ((= k (length l)) (list l))
    (t
     (let ((combkm1 (combination l (decf k))))
       (combination-h l combkm1 nil)
       )
     )
    )
  )

(defun combination-h (l pcomb acc)
  (if (null l)
      acc
      (let ((proj-u (proj-union (car l) pcomb nil))
            )
        (combination-h (cdr l) pcomb (sp-union proj-u acc))
        )
      )
  )


(defun proj-union (x s acc)
  (if (null s)
      acc
      (proj-union x
                  (cdr s) 
                  (if (not (member x (car s)))
                      (cons (cons x (car s)) acc)
                      acc
                      )
                  )
      )
  )

(defun sp-union (s1 s2)
  (cond
    ((null s1) s2)
    ((null s2) s1)
    (t (sp-union (cdr s1) (if (not (sp-member (car s1) s2))
                              (cons (car s1) s2)
                              s2
                              )
                 )
       )
    )
  )

(defun sp-member (x s)
  (if (null s)
      nil
      (if (set-eq x (car s))
          t
          (sp-member x (cdr s))
          )
      )
  )

(defun set-eq (s1 s2)
  (and (subsetp s1 s2) (subsetp s2 s1))
  )


; 17 Create a function that takes a list and returns a pair with two lists: one with elements that are less than or equal to the first element, and the other with the elements that are greater than the first element.
(defun split-list (lst)
  (if (eq lst nil)
      '(nil nil)
      (let ((pivot (car lst)))
        (list
         (cons pivot (remove-if (lambda (x) (> x pivot)) (cdr lst)))
         (remove-if (lambda (x) (<= x pivot)) (cdr lst))
        )
      )
))


; 18 Create a function that takes two sorted lists and merges them into a single sorted list.
(defun merge-sorted-lists (lst1 lst2)
  (cond ((eq lst1 nil) lst2)
        ((eq lst2 nil) lst1)
        ((<= (car lst1) (car lst2)) (cons (car lst1) (merge-sorted-lists (cdr lst1) lst2)))
        (t (cons (car lst2) (merge-sorted-lists lst1 (cdr lst2))))
  )
)

; 19 Create a function that takes a list of integers and returns a pair with the least and greatest elements in the list.
(defun min-max (lst)
  (if (eq lst nil)
      '(nil nil)
      (do ((i (cdr lst) (cdr i))
           (min (car lst) (if (< (car i) min) (car i) min))
           (max (car lst) (if (> (car i) max) (car i) max)))
          ((eq i nil) (list min max))
      )
  )
)

; 20 Create a function that takes a list and an element and removes all copies of this element from the list
(defun remove-all (elem lst)
  (if (null lst)
      nil
      (if (eq elem (car lst))
          (remove-all elem (cdr lst))
          (cons (car lst) (remove-all elem (cdr lst)))
          )
      )
  )


