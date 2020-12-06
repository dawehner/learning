//
//  day_3.swift
//  adventofcode
//
//  Created by Daniel Wehner on 04/12/2020.
//

import Foundation

func day_3_main() {
    let lines = loadArrayOfStrings(path: "input/3.txt")
    let totalLines = lines.count - 1;

    let rules = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    var trees : [Int] = []
    for rule in rules {
    
        var dx = 0
        var dy = 0

        var linesWithTree = 0;
        while ((dy + rule.1) < totalLines) {
            dx += rule.0
            dy += rule.1

            let line = lines[dy]
                    
            let width = line.count
            let index = dx % width
            
            let posXY = String(line[String.Index(encodedOffset: index)])
                        
            if posXY == "#" {
                linesWithTree += 1;
            }
        }
        
        
        trees.append(linesWithTree)
        
        print(rule, linesWithTree)
    }
    print(trees.reduce(1, { (acc, x) in acc * x}))
}
