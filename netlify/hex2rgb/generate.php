<?php

$keys = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

mkdir('/tmp/hex2rgb/output');
foreach ($keys as $i0) {
  foreach ($keys as $i1) {
    mkdir ('/tmp/hex2rgb/output/' . $i0  . $i1);
    foreach ($keys as $j0) {
      foreach ($keys as $j1) {
        mkdir('/tmp/hex2rgb/output/' . $i0  . $i1 . '/' . $j0 . $j1);
        foreach ($keys as $k0) {
          foreach ($keys as $k1) {
            $r = hexdec($i0 . $i1);
            $g = hexdec($j0 . $j1);
            $b = hexdec($k0 . $k1);

            file_put_contents('/tmp/hex2rgb/output/' . $i0  . $i1 . '/' . $j0 . $j1 . '/' . $k0 . $k1 . '.html', $r . $g . $b);
          }
        }
      }
    }
  }
}
