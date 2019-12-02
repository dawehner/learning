<?php

function execute(array $memory) {
  $instruction_pointer = 0;
  while (true) {
    $opcode = $memory[$instruction_pointer];
    switch ($opcode) {
      case 1:
        $pos1 = $memory[$instruction_pointer + 1];
        $pos2 = $memory[$instruction_pointer + 2];
        $pos3 = $memory[$instruction_pointer + 3];
        $memory[$pos3] = $memory[$pos1] + $memory[$pos2];
        $instruction_pointer += 4;
        break;
      case 2:
        $pos1 = $memory[$instruction_pointer + 1];
        $pos2 = $memory[$instruction_pointer + 2];
        $pos3 = $memory[$instruction_pointer + 3];
        $memory[$pos3] = $memory[$pos1] * $memory[$pos2];
        $instruction_pointer += 4;
        break;
      case 99:
        return $memory;
      default:
        print_r("wrong opcode $opcode, position  $pos");
        exit(1);
    }
  }
}

function read_file() : array {
  $content = file_get_contents('./a2.input');
  return array_map(function ($r) {
    return (int) $r;
  }, explode(",", $content));
}

$input = $original_input = read_file();

$input[1] = 12;
$input[2] = 2;

print_r(execute($input)[0]);
print_r("\n");

for ($noun = 0; $noun < 100; $noun++) {
  for ($verb = 0; $verb < 100; $verb++) {
    $new_input = $original_input;
    $new_input[1] = $noun;
    $new_input[2] = $verb;
    $result = execute($new_input);
    if ($result[0] === 19690720) {
      print_r(100 * $noun + $verb);
    }
  }
}