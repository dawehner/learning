//
//  day_13.swift
//  adventofcode
//
//  Created by Daniel Wehner on 18/12/2020.
//

import Foundation

func findShortedTime(time : Int, times : Array<Int>) -> (Int, Int) {
    outerloop : for d in Array(0...time) {
        for t in times {
            if (time + d) % t == 0 {
                print(time, d, t)
                return (d, t)
            }
        }
    }
    return (0,0)
}

func day_13_input() {
    let lines = loadArrayOfStrings(path: "input/13.txt")

    let time = Int(lines[0]) ?? 0
    let times = lines[1]
        .split(separator: ",")
        .compactMap { $0 != "x" ? $0: nil }
        .compactMap { Int($0) }

    let resA = findShortedTime(time: time, times: times)
    print(resA.0 * resA.1)
    
    let timesB = lines[1]
        .split(separator: ",")
        .enumerated()
        .compactMap{ ($0.0, Int($0.1)) }
    print(timesB)
    
}
