import coursera.progfun.week4._

val zero = Zero
val one = zero.successor
val two = one.successor
val three = two.successor

(three - one) isZero;
(three - two) isZero;
(one - zero) isZero;
(three - two - one) isZero;
(three - one - one - one) isZero;
(one - three) isZero