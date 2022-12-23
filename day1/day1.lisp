(defun get-carlories (line)
  (let ((r (nth-value 0 (parse-integer line :junk-allowed t))))
    (if (null r)
        0
        r)))

(defun step-total (total carlories)
  (if (= 0 carlories)
      0
      (+ total carlories)))

(defun step-max (max total)
  (if (> max total)
      max
      total))

(defun max-calories ()
  (with-open-file (stream "./input")
                       (setq x (do ((line (read-line stream nil) (read-line stream nil))
                                    (total 0 (step-total total (get-carlories line)))
                                    (m -1 (step-max m total)))
                                   ((null line) (max total m))
                                 ()))
    (print x)))

(defun firsts (n list)
  (if (= 0 n)
      nil
      (cons
       (car list)
       (firsts (- n 1) (cdr list)))))

(defun top-n-calories (n)
  (with-open-file (stream "./input")
    (let ((calories nil))
      (progn
        (do ((line (read-line stream nil) (read-line stream nil))
             (c 0 (get-carlories line))
             (total 0 (if (= 0 c)
                          (progn
                            (push total calories)
                            0)
                          (+ total c))))
            ((null line))
          (print (list c total)))        
        (reduce '+ (firsts 3 (sort calories #'>)))))))


(print (top-n-calories 3))



