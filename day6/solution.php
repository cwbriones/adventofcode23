<?php
$times = array_values(array_filter(explode(" ", rtrim(explode(":", fgets(STDIN))[1]))));
$distances = array_values(array_filter(explode(" ", rtrim(explode(":", fgets(STDIN))[1]))));

// We want to know when
//
// x^2 - xT + D < 0
//
// Which will give us the valid times that result in a win.
//
// From the quadratic formula we get:
//
// x = 0.5 * (T +/- sqrt(T**2 - 4*D))
//
// We can assume that the discriminant is positive, otherwise
// there would either be 1 or 0 roots, either of which would likely
// result in 0 wins (unless the 1 root is exactly an integer).
//
// Another thing to note is that f"(x) = 2 > 0 so the parabola is
// shaped upwards. This means that the range we are looking for is
// [r1, r2]. Since the times must be integers we take floor/ceil.
function solve($times, $distances) {
  $product = 1;
  foreach ($times as $i => $time) {
    $distance = $distances[$i];
    $sqr_dis = sqrt($time**2 - 4*$distance);
    $r1 = ($time - $sqr_dis) / 2;
    $r2 = ($time + $sqr_dis) / 2;
    $product *= floor($r2) - ceil($r1) + 1;
  }
  return $product;
}
echo solve($times, $distances), "\n";
$t = implode("", $times);
$d = implode("", $distances);
echo solve(array($t), array($d)), "\n";
