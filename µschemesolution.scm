;; Count returns the number of "top-level" elements
;; that equal x.

(define count (x xs)
    (if (null? xs)
        0
        (if (equals? x (car xs))
            (+ 1 (count x (cdr xs)))
            (count x (cdr xs)))))

(check-expect (count 'a '(1 b a (c a))) 1)

;; Countall returns the total number of elements in
;; a list that are equal to x

(define countall (x xs)
    (if (null? xs)
        0
        (if (pair? (car xs))
          (+ (countall x (cdr xs)) (countall x (car xs)))
            (if (equal? x (car xs))
                (+ 1 (countall x (cdr xs)))
                (countall x (cdr xs)))
        (countall x (car xs)))))

(check-expect (countall 'a '(1 b a (c a))) 2)

;; Flatten returns a list in where all other sublists
;; of that list are combined into just one 'flat' list 
;; in the same order. 

(define flatten (xs)
  (if (null? xs)
    xs
    (if (pair? (car xs))
      (append (flatten (car xs)) (flatten (cdr xs)))
      (cons (car xs) (flatten (cdr xs))))))

(check-expect (flatten '((I Ching) (U Thant) (E Coli))) 
(I Ching U Thant E Coli))

;;contig-vals? contig-sublist? helper function
;; Contig-Vals returns true if the specified number of 
;; elements of ys are equal to xs in value and order.

(define contig-vals? (xs ys)
  (if (null? xs)
    #t
    (if (equal? (car xs) (car ys))
      (contig-vals? (cdr xs) (cdr ys))
        #f)))

;;Contig-sublist returns true if the xs
;; is a contiguous subsequence of ys

(define contig-sublist? (xs ys)
  (if (equal? (car xs) (car ys))
    (contig-vals? xs ys)
    (contig-sublist? xs (cdr ys))))

;;(check-expect (contig-sublist? '(a b c) '(x a y b z c)) #f)
;;(check-expect (contig-sublist? '(a y b) '(x a y b z c)) #t)
;;(check-expect (contig-sublist? '(x) '(x a y b z c)) #t)

(define sublist? (xs ys)
  (if (null? xs)
    #t
    (if (null? ys)
      #f
      (if (equal? (car xs) (car ys))
        (sublist? (cdr xs) (cdr ys))
        (sublist? xs (cdr ys))))))

(check-expect (sublist? '(a b c) '(x a y b z c)) #t)
(check-expect (sublist? '(a y b) '(x a y b z c)) #t)
(check-expect (sublist? '(a z b) '(x a y b z c)) #f)
(check-expect (sublist? '(x y z) '(x a y b z c)) #t)

;; Takewhile returns the longest prefix of xs
;; where every element satisfies the condition p?


(define takewhile (p? xs)
  (if (p? (car xs))
    (cons (car xs) (takewhile p? (cdr xs)))
    '()))

(define even? (x) (= (mod x 2) 0))

(check-expect (takewhile even? '(2 4 6 7 8 10 12)) (2 4 6))

;;Dropwhile removes the longest prefix from a given
;;string and returns what is left.

(define dropwhile (p? xs)
  (if (p? (car xs))
    (dropwhile p? (cdr xs))
    (append xs '())))

(check-expect (dropwhile even? '(2 4 6 7 8 10 12)) (7 8 10 12))
;; Take returns the longest prefix of a list that
;; has at most n elements.

(define take (n xs)
  (if (not (= n 0))
    (if (not (null? xs))
       (cons (car xs) (take (- n 1) (cdr xs)))
    '())
  '())


;; Drop removes n elements from the front of a
;; list and returns what is left.

(define drop (n xs)
  (if (not (= n 0))
    (if (not (null? xs))
      (drop (- n 1)(cdr xs))
    '())
  '(append xs ())))


(check-expect (equal? (append (take 1 '(a b)) (drop 1 '(a b)))) (a b))

;; A: vector-length takes the square of both x and y and adds
;; their respective products together, then finally takes the
;; square root of the sum.

 
;; B:
;;
;; Zip converts xs and ys to an association list. The behavior is
;; unspecified when lists are not the same length

(define zip (xs ys) 
    (if (null? xs)
        '()
        (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

(check-expect (zip '(1 2 3) '(a b c)) ((1 a) (2 b) (3 c)))
;; Rebuild First List takes in an association list xs and 
;; rebuilds the original first list

(define rebuildFirstList (xs) 
    (if (null? xs)
        '()
        (cons (car (car xs)) (rebuildFirstList (cdr xs)))))

;; Rebuild Second List takes in an association list xs and 
;; rebuilds the original second list

(define rebuildSecondList (xs)
    (if (null? xs)
        '()
        (cons (car (cdr (car xs))) (rebuildSecondList (cdr xs)))))

;; Unzip takes in an association list xs and 
;; converts it to a pair of lists

(define unzip (xs) 
    (if (null? xs)
        '()
        (list2 (rebuildFirstList xs) (rebuildSecondList xs))))

(check-expect (unzip '((I Magnin) (U Thant) (E Coli))) ((I U E) 
(Magnin Thant Coli)))

;;
;; D:
;;
;; Returns the element of a for which (f a) is as large as possible
;; Expects a function f and a nonempty list a. URE if these 
;; are not provided
 
(define arg-max (f a) 
    (if (null? a)
        1
        (if (> (f (car a)) (f (arg-max f (cdr a))))
            (car a)
            (arg-max f (cdr a)))))

(define square (a) (* a a))
(define invert (a) (/ 1000 a))

(check-expect (arg-max square '(5 4 3 2 1)) 5)
(check-expect (arg-max invert '(5 4 3 2 1)) 1)

 ;;
 ;; D:
 ;;
 ;;Returns #t if element a does not appear more than once in list ls

(define checkNodeUp? (x ls)
  (if (null? ls)
    #t
    (if (equal? x (car (car ls)))
      #f
      (checkNodeUp? x (cdr ls)))))

(define addUnpaired (nl fl)
  (if (null? fl)
    '()
    (if (checkNodeUp? (car (cdr (car fl))) nl)
      (append nl (cons (cdr (car fl)) '()))
      (addUnpaired nl (cdr fl)))))
 
;; Nodes From Edges searches an edgelist es for nodes that point 
;; to other nodes and returns a list of these nodes

 (define nodesFromEdges (es)
    (if (null? es)
        '()
        (if (checkNodeUp? (car (car es)) (nodesFromEdges(cdr es)))
            (cons (list1(car(car es))) (nodesFromEdges (cdr es)))
            (if (checkNodeUp? (car (car (cdr es))) (nodesFromEdges (cdr es)))
                (cons (list1(car (car (cdr es)))) (nodesFromEdges (cdr es)))
                (nodesFromEdges (cdr es))))))

 ;; Make Edge List finds all nodes in es that n points to and returns a list

 (define makeEdgeList (n es) 
    (if (null? es)
        '()
        (if (equal? n (car (car es)))
            (append (cdr (car es)) (makeEdgeList n (cdr es)))
            (makeEdgeList n (cdr es)))))

 ;;Adds edges from edgelist fl to the successors map nl by 
 ;;calling makeEdgeList on each node in successors map nl

(define addEdges (nl fl) 
    (if (null? nl)
        '()
         (cons (list2(car (car nl)) (makeEdgeList (car (car nl))) fl)) 
         (addEdges (cdr nl) fl))))

;; successors-map-of-edge-list accepts a graph es in edgelist representation 
;; and returns a representation of the same graph in successors-map 
;; representation

 (define successors-map-of-edge-list (es) 
    (if( null? es)
        '()
        (addUnpaired (addEdges (nodesFromEdges es) es) es)))

;; Create Pair returns all elements e points to within es as a list of pairs

 (define createPair (e es) 
    (if( null? es)
    '()
    (cons (list2 e (car es)) (createPair e (cdr es)))))

 ;;edge-list-of-successors-map accepts a graph in successors map representation
 ;; and returns a representation of the same graph in edge-list representation

 (define edge-list-of-successors-map (es) 
    (if (null?  es)
    '()
    (append (createPair (car (car es) (car (cdr (car es))))
    (edge-list-of-successors-map (cdr es)))))

 ;; E:
 ;;
 ;;Merge returns a single sorted list containing same # of elements 
 ;; as xs and ys together
 
(define merge (xs ys) 
    (if(null? xs)
        (append ys '())
        (if(null? ys)
            (append xs '())
            (if( > (car xs) (car ys))
                (cons (car ys) (merge (cdr ys) xs))
                (cons (car xs) (merge (cdr xs) ys))))))

(check-expect (merge '(1 2 3) '(4 5 6)) (1 2 3 4 5 6))
(check-expect (merge '(1 3 5) '(2 4 6)) (1 2 3 4 5 6))

 ;;
 ;; F:
 ;;
 ;;Interleave returns a single list of alternating elements
 ;;If one list ends appends the end of the second list to the returned list

 (define interleave (xs ys) 
    (if(null? xs)
        (append ys '())
        (if(null? ys)
            (append xs '())
            (cons (car xs) (cons (car ys) (interleave (cdr xs) (cdr ys)))))))

(check-expect (interleave '(1 2 3) '(a b c)) (1 a 2 b 3 c))
(check-expect (interleave '(1 2 3) '(a b c d e f)) (1 a 2 b 3 c d e f))
(check-expect (interleave '(1 2 3 4 5 6) '(a b c)) (1 a 2 b 3 c 4 5 6)) 
# microscheme
