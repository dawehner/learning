//
//  day_14.swift
//  adventofcode
//
//  Created by Daniel Wehner on 19/12/2020.
//

import Foundation

struct MaskMemPair {
    let mask: Array<Optional<Bool>>
    let memory: Array<(Int, Int)>

    func applyMaskToNumber(number: Int, mask: Array<Optional<Bool>>) -> Int {
        let numberString = String(number, radix: 2)
        let paddedString = pad(string: numberString, toSize: mask.count)
        let str = Array(mask.enumerated()).reduce(Array(paddedString)) { (string: Array<Character>, tuple: (Int, Optional<Bool>)) -> Array<Character> in
            let index = tuple.0
            let m = tuple.1
            switch m {
            case nil: return string
            case .some(false):
                var str = string
                str[index] = "0"
                return str
            case .some(true):
                var str = string
                str[index] = "1"
                return str
            }
        }

        return Int(String(str), radix: 2)!
    }
}

func pad(string: String, toSize: Int) -> String {
    var padded = string
    for _ in 0..<(toSize - string.count) {
        padded = "0" + padded
    }
    return padded
}

func day14ParseInput(lines: Array<String>) -> Array<MaskMemPair> {
    lines.reduce([]) { agg, line in
        if line.starts(with: "mask = ") {
            let maskString = line.replacingOccurrences(of: "mask ", with: "")
            let mask: Array<Optional<Bool>> = Array(maskString).map { (char: Character) in
                switch char {
                case "0": return false
                case "1": return true
                case "X": return nil
                default: return nil
                }
            }

            let maskPair = MaskMemPair(mask: mask, memory: [])

            return agg + [maskPair]
        }
        else {
            var agg = agg
            let regex = #"mem\[(\d+)\] = (\d+)"#
            let memString = line.matchingString(regex: regex)!
            let maskPair = agg.last!
            let newMarkPair = MaskMemPair(mask: maskPair.mask, memory: maskPair.memory +
                    [
                    (Int(memString[1]) ?? 0, Int(memString[2]) ?? 0)
                ])
            agg[agg.endIndex - 1] = newMarkPair

            return agg
        }
    }
}

func day_14_main() {
    let lines = loadArrayOfStrings(path: "input/14.txt")

    let maskMemPairs = day14ParseInput(lines: lines)

    var mem: [Int: Int] = [:]

    for pair in maskMemPairs {
        for memory_element in pair.memory {
            print(mem)
            mem[memory_element.0] = pair.applyMaskToNumber(number: memory_element.1, mask: pair.mask)
        }
    }
    
    print(mem.values.reduce(0, +))
}
