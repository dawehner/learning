//
//  day_10.swift
//  adventofcode
//
//  Created by Daniel Wehner on 13/12/2020.
//

import Foundation

func align_jolts (all: Array<Int>) -> Int {
    let sorted = all.sorted()
    var histogram = [1, 0, 1]
    for index in (0...sorted.count - 2) {
        let prev = sorted[index]
        let next = sorted[index + 1]
        histogram[next - prev - 1] += 1
    }
    return histogram[2] * histogram[0]
}

func tribonacci (n: Int64) -> Int64 {
    switch n {
    case 0 : return 1
    case 1: return 1
    case 2: return 2
    case 3: return 4
    default: return tribonacci(n: n - 1) + tribonacci(n: n - 2) + tribonacci(n: n - 3)
    }
}

func day_10_b(all : Array<Int>) -> Int64 {
    let sorted = [0] + all.sorted()
    
    var consecutive_ones : Int64 = 1
    var consecutives : Array<Int64> = []
    for index in (0...sorted.count - 2) {
        let prev = sorted[index]
        let next = sorted[index + 1]
        if (next - prev) == 1 {
            consecutive_ones += 1
        }
        else {
            consecutives.append(consecutive_ones - 1)
            consecutive_ones = 1
        }
    }

    print(sorted)
    print(consecutives)
    return consecutives.reduce(1) { agg, c in
        return agg * tribonacci(n: c )
    }
}

func day_10_main() {
    let numbers = loadArrayOfNumbers(path: "input/10.txt")

    print(align_jolts(all: numbers))
    print(day_10_b(all: numbers))
}
