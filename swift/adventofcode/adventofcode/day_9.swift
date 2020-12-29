//
//  day_9.swift
//  adventofcode
//
//  Created by Daniel Wehner on 13/12/2020.
//

import Foundation

func is_missing_number(numbers: Array<Int>, number: Int) -> Int? {

    for index in 0...(numbers.count - 1) {
        for index2 in index...(numbers.count - 1) {
            if numbers[index] + numbers[index2] == number {
                return number
            }
        }
    }
    return nil
}

func find_missing_number(all: Array<Int>, range: Int) -> Int? {
    for start in 0...(all.count - range - 1) {
        let number = all[range + start]
        let view = all[start...(start + range - 1)]
        if is_missing_number(numbers: Array(view), number: number) == nil {
            return number
        }
    }
    return nil
}

func find_encryption_weakness(all: Array<Int>, target : Int) -> Int? {
    for index in 2...(all.count - 1) {
        for startIndex in 0...(index - 1) {
            let view = all[startIndex...index]
            let sum = view.reduce(0, +)
            if sum == target {
                return view.min()! + view.max()!
            }
        }
    }
    return nil
}

func day_9_main() {
    let numbers = loadArrayOfNumbers(path: "input/9.txt")

    if let target = find_missing_number(all: numbers, range: 25) {
        print(target)
        print(find_encryption_weakness(all: numbers, target: target) ?? 0)
    }
}
