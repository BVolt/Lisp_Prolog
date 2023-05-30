;;
;;Brenden Johnson
;;CS 3520.01
;;Assignment 2
;;

;;PROBLEM ONE
;;Palindromep takes a list and returns if it is a palindrome
(defun palindromep (lst)
  (palindromep-h lst (reverse lst))
  )


(defun palindromep-h (lst rlst)
  (if (null (cdr lst))
      T
      (if (eq (car lst) (car rlst))
          (palindromep-h (cdr lst) (cdr rlst))
          nil
          )
      )
  )


;;PROBLEM TWO
;;produces dotted pairs that count number of occurences per element
(defun occr (lst)
  (occr-h lst (reverse (unique lst nil)))
  )


(defun occr-h (lst ulst)
  (if (null ulst)
      nil
      (cons (dotcount (car ulst) lst 0) (occr-h lst (cdr ulst)))
      )
  )


;; Returns list with only unique values
(defun unique (lst acc)
  (if (null lst)
      acc
      (unique (cdr lst) (adjoin (car lst) acc))
      )
  )

;;returns dotted pair with count of instance in list
(defun dotcount (i lst acc)
  (if (null lst)
      (cons i acc)
      (if (= i (car lst))
          (dotcount i (cdr lst) (+ acc 1))
          (dotcount i (cdr lst) acc)
          )
      )
  )

;;PROBLEM THREE
;;removes consecutive duplicates from a list
(defun nodups (lst)
  (nodups-h lst (car lst))
  )

(defun nodups-h (lst n)
  (if (null lst)
      nil
      (if (eq n (car (cdr lst)))
          (nodups-h (cdr lst) n)
          (cons (car lst) (nodups-h (cdr lst) (car (cdr lst))))
          ) 
      )
  )


;;PROBLEM FOUR
;;returns a list containing lists of prime factors for each element
(defun factorsL (lst)
  (if (null lst)
      nil
      (cons (pfacts (car lst) 2) (factorsL (cdr lst)))
      )

  )

(defun pfacts(n i)
  (if (< n i)
      nil
      (if (= (mod n i) 0)
          (if (null (isPrime i 2))
              (pfacts n (+ i 1))
              (cons i (pfacts (/ n i) (+ i 1)))
              )     
          (pfacts n (+ i 1))
          )
      )
  )

(defun isPrime (n i)
  (if (<= n i)
      T
      (if (= (mod n i) 0)
          nil
          (isPrime n (+ i 1))
          )
      )
  )


;;PROBLEM FIVE
;;duplicates each element in a list a specified number of times
(defun dups (lst x)
      (dup-h lst x 1) 
  )


(defun dup-h (lst x i)
  (if (null lst)
      nil
      (if (= i x)
          (cons (car lst) (dup-h (cdr lst) x 1))
          (cons (car lst) (dup-h lst x (+ i 1)))
          )
      )
  )
