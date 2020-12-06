//
//  day_1.swift
//  adventofcode
//
//  Created by Daniel Wehner on 04/12/2020.
//

import Foundation

func day_1_main() {
    let numbers = loadArrayOfNumbers(path: "input/1-1.txt")
    
    numbers.forEach { (x: Int) in
        numbers.forEach { (y: Int) in
            if x + y == 2020 {
                print(x*y)
            }
        }
    }
}

func day_1_main_2() {
    let numbers = loadArrayOfNumbers(path: "input/1-1.txt")
    
    numbers.forEach { (x: Int) in
        numbers.forEach { (y: Int) in
            numbers.forEach { (z: Int) in
                if (x + y + z) == 2020 {
                    print(x*y*z)
                }
            }
        }
    }
}
