//
//  day_2.swift
//  adventofcode
//
//  Created by Daniel Wehner on 04/12/2020.
//

import Foundation

struct Rule {
    let string: String
    let min: Int
    let max: Int
}

struct RuleAndPassword {
    let password : String
    let rule : Rule
    
    func passwordValid() -> Bool {
        let tok =  password.components(separatedBy: rule.string)
        let count = tok.count - 1
        return count >= rule.min && count <= rule.max;
    }
    
    func passwordValid2() -> Bool {
        let hasMin = String(password[String.Index(encodedOffset: rule.min - 1)]) == rule.string
        let hasMax = String(password[String.Index(encodedOffset: rule.max - 1)]) == rule.string
        return (hasMin && !hasMax) || (hasMax && !hasMin)
    }
}

func day_2_main() {
    let lines = loadArrayOfStrings(path: "input/2-1.txt")
    
    let pattern = #"(\d+)-(\d+) ([a-z]): (.+)"#
    let regex = try? NSRegularExpression(
      pattern: pattern,
      options: .caseInsensitive
    )
    
    var lines_valid_password = 0
    for line in lines {
        if let match = regex?.firstMatch(in: line, options: [], range: NSRange(location: 0, length: line.utf16.count)) {
            // TODO: Figure out how to better extract regex matches.
            if let range1 = Range(match.range(at: 1), in: line) , let x1 = Int(line[range1]) {
              if let range2 = Range(match.range(at: 2), in: line) , let x2 = Int(line[range2]) {
                if let range3 = Range(match.range(at: 3), in: line) {
                    if let range4 = Range(match.range(at: 4), in: line) {
                        
                        let rule = Rule(string: String(line[range3]), min: x1, max: x2)
                        let rule_and_password = RuleAndPassword(password: String(line[range4]), rule: rule)
                        
                        if rule_and_password.passwordValid2() {
                            lines_valid_password += 1;
                        }
                    }
                }
              }
            }
        }
        
    }

    print(lines_valid_password)
}
