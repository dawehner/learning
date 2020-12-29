//
//  day_16.swift
//  adventofcode
//
//  Created by Daniel Wehner on 23/12/2020.
//

import Foundation

struct Day16Rule {
    let name: String
    let r1Min: Int
    let r1Max: Int
    let r2Min: Int
    let r2Max: Int

    func allowed(_ int: Int) -> Bool {
        return (r1Min <= int && int <= r1Max) || (r2Min <= int && int <= r2Max)
    }
}

func day_16_main() {
    let strings = loadArrayOfStrings(path: "input/16.txt")

    let your_ticket_split = strings.split(separator: "your ticket:")

    let regex = #"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)"#
    let rules: [Day16Rule] = Array(your_ticket_split[0]).compactMap {
        String($0).matchingString(regex: regex)
    }.compactMap {
        guard $0.count == 6 else {
            return nil
        }
        return Day16Rule(name: $0[1], r1Min: Int($0[2])!, r1Max: Int($0[3])!, r2Min: Int($0[4])!, r2Max: Int($0[5])!)
    }

    let your_tickets = Array(your_ticket_split[1])[0].split(separator: ",").compactMap { Int($0) }
    var nearby_ticket_rows = Array(your_ticket_split[1])
    nearby_ticket_rows = Array(nearby_ticket_rows[3 ... nearby_ticket_rows.count - 1])

    let nearby_tickets = nearby_ticket_rows
        .map { $0.split(separator: ",").compactMap { Int($0) } }

    let invalidNumbers : Array<Array<Int>> = nearby_tickets.map { ticket in
        let invalidNumbers : Array<Int> = ticket.filter({ number in
            let inAnyRule = rules.contains(where: { rule in rule.allowed(number)})
            return !inAnyRule
        })
        
        return invalidNumbers
    }.filter { $0.count != 0 }
    
    print(Array(invalidNumbers.joined()).reduce(0, +))

//    let rules =
//
//    let nearby_tickets_split = your_ticket_split[1].split(separator: "nearby tickets:")
//
}
