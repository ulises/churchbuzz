(ns break-clojure.test.core
  (:use midje.sweet
        break-clojure.core))

(unfinished)

(facts "to-integer produces the right integers"
  (to-integer ZERO) => 0
  (to-integer ONE) => 1
  (to-integer TWO) => 2
  (to-integer THREE) => 3
  (to-integer FIVE) => 5
  (to-integer FIFTEEN) => 15
  (to-integer (INC FIFTEEN)) => 16)

(facts "to-boolean produces the right booleans"
  (to-boolean TRUE) => truthy
  (to-boolean FALSE) => falsey)

(fact "if has the right behaviour"
  (((IF TRUE) :foo) :bar) => :foo
  (((IF FALSE) :foo) :bar) => :bar)

(fact "ZERO? is truthy for ZERO and not for any other number"
  (to-boolean (ZERO? ZERO)) => truthy
  (to-boolean (ZERO? ONE-HUNDRED)) => falsey)

(fact "INC increments a number"
  (to-integer (INC ONE)) => 2)

(fact "ADDition adds two numbers"
  (to-integer ((ADD ONE) TWO)) => 3)

(fact "MULTiplication multiplies two numbers"
  (to-integer ((MULT FIVE) FIVE)) => 25)

(fact "TWO to the POWer of THRE is 8"
  (to-integer ((POW TWO) THREE)) => 8)

(fact "DEC decrements a number"
  (to-integer (DEC ONE)) => 0)

(fact "SUBstraction substracts two numbers n and m. It also returns ZERO when n > m"
  (to-integer ((SUB FIVE) FIFTEEN)) => 0
  (to-integer ((SUB FIFTEEN) FIVE)) => 10)

(fact "FIVE is LEQ than FIFTEEN"
  (to-boolean ((LEQ FIVE) FIFTEEN)) => truthy
  (to-boolean ((LEQ TWO) TWO)) => truthy
  (to-boolean ((LEQ FIFTEEN) FIVE)))

(fact "FIVE MOD-CHEAT TWO is 1. FIFTEEN MOD-CHEAT FIVE is 0."
  (to-integer ((MOD-CHEAT THREE) TWO)) => 1
  (to-integer ((MOD-CHEAT FIFTEEN) FIVE)) => 0)

(fact "Z-combinator: FIVE MOD TWO is 1. FIFTEEN MOD FIVE is 0."
  (to-integer ((MOD FIVE) TWO)) => 1
  (to-integer ((MOD THREE) TWO)) => 1
  (to-integer ((MOD FIFTEEN) FIVE)) => 0)

(fact "FIRST and SECOND of a PAIR return the requested element"
  (LEFT ((PAIR :foo) :bar)) => :foo
  (RIGHT ((PAIR :foo) :bar)) => :bar)

(fact "list operations"
  (to-boolean (EMPTY? EMPTY)) => true
  (to-integer (FIRST ((CONJ EMPTY) ONE))) => 1
  (to-integer (FIRST (REST ((CONJ ((CONJ EMPTY) TWO)) ONE)))) => 2)

(fact "to-vector returns a vector with the elements in the list in the same order"
  (map to-integer (to-vector ((CONJ ((CONJ EMPTY) ONE)) TWO))) => '(2 1))

(fact "RANGE produces a range of numbers between m and n (inclusive)"
  (map to-integer (to-vector ((RANGE ZERO) THREE))) => '(0 1 2 3))

(fact "FOLD works!"
  (to-integer (((FOLD ((RANGE ZERO) THREE)) ZERO) ADD)) => 6
  (to-integer (((FOLD ((RANGE ONE) FOUR)) ONE) MULT)) => 24)

(fact "MAP based on FOLD maps a given function over a LIST of items"
  (to-integer-vector ((MAP ((RANGE ZERO) FIVE)) INC)) => '(1 2 3 4 5 6)
  (to-integer-vector ((MAP ((RANGE ZERO) FIVE)) DEC)) => '(0 0 1 2 3 4))

(fact "The words Fizz, Buzz and FizzBuzz are encoded correctly"
  (to-integer-vector Fizz) => '(11 12 14 14)
  (to-integer-vector Buzz) => '(10 13 14 14)
  (to-integer-vector FizzBuzz) => '(11 12 14 14 10 13 14 14))

(fact "helper function to-string converts encoded strings to regular clojure strings"
  (to-string Fizz) => "Fizz"
  (to-string Buzz) => "Buzz"
  (to-string FizzBuzz) => "FizzBuzz")

(fact "DIV divides m by n"
  (to-integer ((DIV FIFTEEN) FIVE)) => 3
  (to-integer ((DIV FIVE) TWO)) => 2)

(fact "REVERSE reverses lists"
  (let [l ((CONJ ((CONJ EMPTY) ONE)) TWO)]
    (to-integer-vector (REVERSE l)) => '(1 2)
    (to-integer-vector (REVERSE (REVERSE l))) => (to-integer-vector l)))

(fact "TODIGITS splits a natural number into a sequence of its digits"
  (to-integer-vector (TODIGITS FIFTEEN)) => '(1 5)
  (to-integer-vector (TODIGITS ONE-HUNDRED)) => '(1 0 0))

(fact "to-string-vector returns a seq of strings from the given list"
  (to-string-vector ((CONJ ((CONJ EMPTY) (TODIGITS ONE))) Buzz)) => '("Buzz" "1"))

(fact "the FIZZBUZZ function returns FizzBuzz for numbers multiple of 3 and 5,
Fizz for multiples of 5, Buzz for multiples of 3, and the number itself
otherwise"
  (to-string (FIZZBUZZ ONE)) => "1"
  (to-string (FIZZBUZZ THREE)) => (to-string Buzz)
  (to-string (FIZZBUZZ FIVE)) => (to-string Fizz)
  (to-string (FIZZBUZZ FIFTEEN)) => (to-string FizzBuzz))
