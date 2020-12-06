//
//  day_6.swift
//  adventofcode
//
//  Created by Daniel Wehner on 06/12/2020.
//

struct Group {
    let lines : [String]
    
    func uniqChars () -> Set<Character> {
        return Set(self.lines.flatMap {x in Array(x)})
    }
    
    func intersectionChars() -> Set<Character> {
        let combine = self.lines.map { x in Array(x)};
        
        let result = combine
            .reduce(nil) { (acc : Optional<Set<Character>>, x : Array<Character> ) -> Optional<Set<Character>> in
                return acc != nil ? acc?.intersection(x) : Set(x)
            }
        
        return result ?? Set([])
    }
}

import Foundation

func day_6_main() {
    let lines_groups = loadArrayOfStringsSeparatedByEmptyLine(path: "input/6.txt")
 
    let groups = lines_groups.map { lines in
        Group(lines: lines.split(separator: "\n").map { x in String(x)})
    }
    
    let sum = groups.map { $0.uniqChars().count } .reduce(0, +)
    print(sum)
    
    let intersection_sum = groups.map { $0.intersectionChars().count }.reduce(0, +)
    print(intersection_sum)
    
}
