//
//  main.swift
//  adventofcode
//
//  Created by Daniel Wehner on 04/12/2020.
//

import Foundation

//day_1_main()
//day_1_main_2()
//day_2_main()
//day_3_main()
//day_4_main()
//day_5_main()
day_6_main()

extension Sequence {
    public func filterMap<T>(map: (Self.Iterator.Element) throws -> (Bool, T)) rethrows -> [T] {
        var array : [T] = []
        for element in self {
            do {
                let (result, value) = try map(element)
                if result {
                    array.append(value)
                }
            }
        }
        return array
    }
    
    public func filterMap<T>(includeElement: (Self.Iterator.Element) throws -> Bool, map: (Self.Iterator.Element) throws -> T) rethrows -> [T] {
        var array : [T] = []
        for element in self {
            do {
                if try includeElement(element) {
                    array.append(try map(element))
                }
            }
        }
        return array
    }
}

func loadArrayOfNumbers(path: String) -> [Int] {
    do {
        let path: String = "/Users/dawehner/Documents/Projects/learning/swift/adventofcode/adventofcode/\(path)"
        let file = try String(contentsOfFile: path)
        let text: [String] = file.components(separatedBy: "\n")
        
        let numbers = text.filterMap { (x: String) -> (Bool, Int) in
            if let intx = Int(x) {
                return (true, intx)
            }
            else {
                return (false, 0)
            }
        }
        return numbers
        
    } catch let error {
        Swift.print("Fatal Error: \(error.localizedDescription)")
        return []
    }
}

func loadArrayOfStrings(path: String) -> [String] {
    do {
        let path: String = "/Users/dawehner/Documents/Projects/learning/swift/adventofcode/adventofcode/\(path)"
        let file = try String(contentsOfFile: path)
        let text: [String] = file.components(separatedBy: "\n")
        return Array(text[0..<text.count - 1])
    } catch let error {
        Swift.print("Fatal Error: \(error.localizedDescription)")
        return []
    }
}

func loadArrayOfStringsSeparatedByEmptyLine(path: String) -> [String] {
    do {
        let path: String = "/Users/dawehner/Documents/Projects/learning/swift/adventofcode/adventofcode/\(path)"
        let file = try String(contentsOfFile: path)
        let text: [String] = file.components(separatedBy: "\n\n")
        return text
    } catch let error {
        Swift.print("Fatal Error: \(error.localizedDescription)")
        return []
    }
}

