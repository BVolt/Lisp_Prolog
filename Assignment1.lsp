;Flip Function
(defun flip(lst) 
  (if (null  (cdr lst))
      (cons (car lst) nil)
      (cons (car (cdr lst)) (cons (car lst)
                                   (if (null (cdr (cdr lst)))
                                       nil
                                       (flip (cdr(cdr lst)))
                                       )
                                   )
            )
      )
  )


;remove function
(defun remove-i (i lst)
  (if (= i 1)
          (if (null (cdr lst))
              nil
              (cons (car(cdr lst)) (remove-i i (cdr lst)))
          )
          (if (null (cdr lst))
              (cons (car lst) nil)
              (cons (car lst) (remove-i  (- i 1) (cdr lst)))
          )
  )
  )

(defun prodL(l)
  (if (null l)
      1
      (* (car l) (prodl (cdr l)))
      )
  )

(defun headsubl(term l)
  (if (null (cdr l))
      (cons (- term (car l)) nil)
      (cons (- term (car l)) (headsubl term (cdr l)))
      )
  )

(defun podif-h(l)
  (if (null (cdr (cdr l)))
      (cons (- (car l) (car(cdr l))) nil)
      (cons (prodl (headsubl (car l) (cdr l))) (podif-h (cdr l)) )
      )
  )

(defun product-of-diff(l)
  (if (null l)
      1
      (if (null (cdr l))
          (car l)     
          (prodl (podif-h l))
          )
      )
  )
