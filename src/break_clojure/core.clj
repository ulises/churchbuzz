(ns break-clojure.core)

;;; The whole point of this exercise is to write the well-known FizzBuzz
;;; algorithm. This algorithm iterates over a sequence of numbers in a range
;;; and every time it comes across a number which is a multiple of 3 or
;;; 5 it prints the words Fizz or Buzz respectively. If the number is
;;; multiple of both numbers it prints FizzBuzz.

;;; An initial version, with many constructs of clojure could be written as
;;; follows:
;; (defn fizzbuzz [n]
;;   (cond (and (zero? (mod n 3))
;;              (zero? (mod n 5))) "FizzBuzz"
;;              (zero? (mod n 3)) "Fizz"
;;              (zero? (mod n 5)) "Buzz"
;;              :default n))
;; (defn run []
;;   (map fizzbuzz (range 1 101)))

;;; This implementation relies on many things including:
;;;   - all natural numbers between 1 and 100
;;;   - the ability to iterate over these
;;;   - the ability to check whether a number is 0
;;;   - the ability to branch depending on a particular predicate

;;; And now we want to build all these things from scratch and implement
;;; FizzBuzz with them.

;;; Natural numbers
;;; the idea is that counting is based on comparing two bags of apples and oranges
;;; if you take an apple, followed by taking an orange, and you find yourself with
;;; two empty bags at the same time, you can conclude that both have the same
;;; "number" of things in them (regardless of what these things are) although
;;; you don't know what that number is.
;;; Hence, numbers can be defined as applying an operation to an element many
;;; times.

(def ^{:doc "Utility function to get better reporting for NUMBERs.
  Use like (to-integer NUMBER), e.g.

  user> (to-integer ONE) ; 1"}
  to-integer (fn [f] ((f inc) 0)))

(def ZERO (fn [f] (fn [x] x)))
(def ONE (fn [f] (fn [x] (f x))))
(def TWO (fn [f] (fn [x] (f (f x)))))
(def THREE (fn [f] (fn [x] (f (f (f x))))))

(def FIVE
  (fn [f] (fn [x] (f (f (f (f (f x))))))))

(def FIFTEEN
  (fn [f]
    (fn [x]
      (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))
(def ONE-HUNDRED
  (fn [f]
    (fn [x]
      (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

;;; Booleans
;;;
;;; Booleans can be implemented as two functions which simply choose one parameter over the other

(def to-boolean (fn [f] ((f true) false)))

(def TRUE (fn [x] (fn [_] x)))
(def FALSE (fn [_] (fn [y] y)))

;;; IF-THEN-ELSE
;;;
;;; Implementing booleans is easy, and so we can implement if-then-else control
;;; blocks on top of them rather easily
;;; Initially one can implement IF along the lines of
;;; (def IF (fn [pred] (fn [then] (fn [else] ((pred then) else)))))
;;; however, this can be simplified as follows:
;;; IF is written as a sequence of functions that take a single
;;; value and return functions. In the end, a call to the function
;;; returned by the predicate on the then-clause (pred then) is called
;;; with the else clause. So, in principle, IF doesn't do much
;;; other than call pred with the then- and else-clauses.
;;; And so it can be simplified to be just the pred.

(def IF (fn [pred] pred))


;;; Predicates
;;; As stated before, we need a way of checking whether a number is zero
;;; or not.
;;; If you recall, the way we defined the number zero was that given
;;; a function, it would call it 0 times with the parameter given
;;; so we can call NUMBER (the function) with a function that always
;;; returns FALSE when invoked and with TRUE as its parameter, then
;;; effectively we're checking whether whether NUMBER the function
;;; is the ZERO function.

(def ZERO? (fn [number] ((number (fn [_] FALSE)) TRUE)))

;;; Numeric operations
;;; Now we need the ability to calculate the remainder of a division, and
;;; for that we need to know how to increment and decrement a NUMBER.

(def INC (fn [number] (fn [f] (fn [x] (f ((number f) x))))))
(def ADD (fn [n] (fn [m] (fn [f] (fn [x] ((m f) ((n f) x)))))))
(def MULT (fn [n] (fn [m] ((m (ADD n)) ZERO))))
(def POW (fn [n] (fn [m] ((m (MULT n)) ONE))))

(def DEC (fn [number] (fn [f] (fn [x] (((number (fn [g] (fn [h] (h (g f)))))  (fn [y] x)) (fn [y] y))))))
(def SUB (fn [n] (fn [m]
                  ;; (println "N:" (to-integer n) " - M:" (to-integer m))
                  ((m DEC) n))))

;;; Modulo
;;; To implement FizzBuzz we need to be able to calculate the mod of two
;;; numbers. A simple algorithm for doing so is as follows:
;;;

;; (defn my-mod [n m]
;;   (if (>= n m)
;;     (my-mod (- n m) m) n))

;;; however this implementation relies on <= which we're not allowed to use!
;;; Don't despair, because we can implement <= rather easily by rewriting <=
;;; to: (defn <= [n m] (<= (- n m) 0))
;;; (cheating by using <= again)
;;; This rewrite shows that we can check for leq (lower-equal) checking
;;; that if the SUBstraction of two numbers is <= 0, then n <= m. Our definition of
;;; SUB is such that if we try to substract a larger number from a smaller
;;; one we'll get ZERO so all is good.

(def LEQ (fn [n] (fn [m]
                  ;; (println "N:" (to-integer n) "<= M:" (to-integer m))
                  (ZERO? ((SUB n) m)))))

;;; With LEQ we can now implement modulo rather easily:
;;; We'll rewrite

;; (defn mymod [n m]
;;   (if (<= m n) (mymod (- n m) m) n))

;;; using our LEQ implementation and our SUB implementation.
;;;
;;; Initially we may want to write as such:
;;;
;; (def MOD (fn [n]
;;            (fn [m]
;;              (((IF ((LEQ m) n)) (((MOD ((SUB n) m)) m) x)) n))))

;;; However this will quickly result in a nast StackOverflow :-o
;;; The reason for this is that in clojure, (if...) is lazy in that if
;;; the predicate is false, then the then-clause is not evaluated. Our
;;; implementation of IF instead is eager, i.e. it will always evaluate
;;; both the then-clause and the else-clause regardless of the truthiness
;;; of the predicate. To work around this we use the fact we used to simplify
;;; our implementation of IF but in reverse:
;;; If we have a function f which takes a single parameter x and calls
;;; a function g with it, i.e. (defn f [x] (g x)), then we can
;;; replace f with g in all our code.
;;; Here we do the opposite, where we have a call to g, we introduce f
;;; so that we are lazy in the evaluation of g. Our g in this case is MOD and
;;; so we replace ((MOD ((SUB n) m)) m) with (fn [x] (((MOD ((SUB n) m)) m) x))
;;; which is equivalent in nature but lazy-er! \o/

(def MOD-CHEAT
  (fn [n]
    (fn [m]
      (((IF ((LEQ m) n)) (fn [x] (((MOD-CHEAT ((SUB n) m)) m) x))) n))))

;;; Unfortunately, albeit correct, our implementation of MOD relies on
;;; a feature of the language we should not rely on: the fact that to define
;;; MOD we use MOD as if it had been defined already.
;;; To solve this, we make use of the Y-combinator
;;; (http://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator).
;;; Briefly, the Y-combinator solves the assignment problem since
;;; when you call the Y-combinator with a function, it will in turn
;;; call the function with the function itself as its first argument. Hence
;;; the function can now recurse (by calling itself again) without
;;; relying on itself being defined. The Y-combinator is implemented as follows:

;;;   (def Y-combinator (fn [f] (fn [x] ((f (x x)) (fn [x] (f (x x)))))))

;;; Sadly, just as our first version of MOD produced a StackOverflow, using
;;; the Y-combinator will also produce a StackOverflow due to eager
;;; evaluation of its parameters. Again, indirection saves the day and we
;;; use the same trick we used to fix MOD: we introduce a function f which
;;; will simply call g with the parameter passed (this is called the
;;; Z-combinator):

;; Z = -> f { -> x { f[-> y { x[x][y] }] } [-> x { f[-> y { x[x][y] }] }] }

(def Z-combinator
  (fn [f]
    ((fn [x] (f (fn [y] ((x x) y))))
     (fn [x] (f (fn [y] ((x x) y)))))))


(def MOD
  (Z-combinator
   (fn [f]
     (fn [n]
       (fn [m]
         (println "M:" (to-integer m) " N:" (to-integer n))
         (if (<= (to-integer m) (to-integer n))
           (do (println "THEN")
               (fn [x] (((f ((SUB n) m)) n) x)))
           (do (println "ELSE:")
               m)))))))
;; (def MOD
;;   (Z-combinator
;;    (fn [f]
;;      (fn [m]
;;        (fn [n]
;;          (((IF ((LEQ m) n)) (fn [x] (((f ((SUB m) n)) n) x))) m))))))

;;; Ok, now we have a bunch of building blocks for our barebones FizzBuzz
;;; however we still need: ranges, map, string literals and a way of
;;; converting NUMBER to a string. Let's start with ranges and map.

;;; To implement ranges and map we need to be able to build lists. The
;;; building block of lists is a pair (also known as tuple). A pair
;;; is simply a pair of items (two - not "pair" in the many sense) and
;;; is sometimes implemented as an array of 2 items. In our case however
;;; we can only implement this with functions:

(def PAIR (fn [x] (fn [y] (fn [f] ((f x) y)))))

;;; A PAIR will take items x and y and a function f. A PAIR then becomes
;;; the result of calling as a function the result of (f x) with y.

;;; Extracting items from a pair then becomes:

(def LEFT (fn [pair] (pair (fn [x] (fn [_] x)))))
(def RIGHT (fn [pair] (pair (fn [_] (fn [y] y)))))

;;; So, LEFT will call pair with a function that returns its first element
;;; (in this case x) and ignore the second (naming the parameter of the
;;; returned function as _ denotes that it's not important). And RIGHT
;;; will do the same thing except that it'll return the second value (y).

;;; Now that we have pairs, we can implement linked lists. However,
;;; instead of using a pointer to the next item (as it is normally done)
;;; we store the actual pair.

(def EMPTY ((PAIR TRUE) TRUE))
(def EMPTY? LEFT)
(def CONJ (fn [l] (fn [x] ((PAIR FALSE) ((PAIR x) l)))))
(def FIRST (fn [l] (LEFT (RIGHT l))))
(def REST (fn [l] (RIGHT (RIGHT l))))

;;; now we write a convenience function (just like to-integer and
;;; to-boolean) that will make our lives easier when inspecting
;;; our lists:

(defn to-vector
  "Convenience function to represent a list as a vector"
  ([l] (to-vector l []))
  ([l acc]
     ;; (println "EMPTY?" (to-boolean (EMPTY? l)))
     ;; (println "acc:" acc)
     (if (to-boolean (EMPTY? l))
       (reverse acc)
       (recur (REST l) (cons (FIRST l) acc)))))

;;; Now we want to build ranges of NUMBERs. A slightly contrived
;;; way of doing so is to think as (range m n) as (conj (range (inc m) n) m)
;;; which is saying that the range of numbers between m and n is just
;;; m followed by the range of numbers between m+1 and n (provided that m and
;;; (m + 1) aren't larger than n.
;;; This observation will make it easier for us to write RANGE in terms of
;;; the basic operations we've just built.

;; RANGE =
;;   Z[-> f {
;;     -> m { -> n {
;;       IF[IS_LESS_OR_EQUAL[m][n]][
;;         -> x {
;;           UNSHIFT[f[INCREMENT[m]][n]][m][x]
;;         }
;;       ][EMPTY]
;;     } }
;;   }]


(def RNG (fn [m n] (if (to-boolean ((LEQ m) n)) ((CONJ (RNG (INC m) n)) m) EMPTY)))

(def RANGE
  (Z-combinator
   (fn [f]
     (fn [m]
       (fn [n] (((IF ((LEQ m) n)) (fn [x] (((CONJ ((f (INC m)) n)) m) x))) EMPTY))))))
