<?php

function calculate_fuel(int $mass, bool $recursive = false) : int {
  $result = max(floor($mass / 3) - 2, 0);
  if ($recursive && $result > 0) {
    return $result + calculate_fuel($result, true);
  }
  return $result;
}

function read_file() : array {
  $content = file_get_contents('./a1.input');
  return array_filter(array_map(function ($r) {
    return (int) $r;
  }, explode("\n", $content)));
}

print_r(array_sum(array_map('calculate_fuel', read_file())));

print_r("\n");

print_r(array_sum(array_map(function ($mass) {
  return calculate_fuel($mass, true);
}, read_file())));