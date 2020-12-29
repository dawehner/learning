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
//day_6_main()
//day_7_main()
//day_8_main()
//day_9_main()
//day_10_main()
//day_11_main()
//day_12_main()
//day_13_input()
//day_14_main()
//day_15_main()
//day_16_main()
//day_17_main()
day_18_main()

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

extension String {
    func matchingStrings(regex: String) -> [[String]] {
        guard let regex = try? NSRegularExpression(pattern: regex, options: []) else { return [] }
        let nsString = self as NSString
        let results  = regex.matches(in: self, options: [], range: NSMakeRange(0, nsString.length))
        return results.map { result in
            (0..<result.numberOfRanges).map {
                result.range(at: $0).location != NSNotFound
                    ? nsString.substring(with: result.range(at: $0))
                    : ""
            }
        }
    }
    
    func matchingString(regex: String) -> [String]? {
        let result = self.matchingStrings(regex: regex)
        return result.count > 0 ? result[0] : nil
    }
}

func compareTuples <T: Equatable> (tuple1: (T, T), tuple2: (T, T)) -> Bool {
    return (tuple1.0 == tuple2.0) && (tuple1.1 == tuple2.1)
}
