//
//  day_5.swift
//  adventofcode
//
//  Created by Daniel Wehner on 05/12/2020.
//

import Foundation

struct Seat {
    let row : Array<Character>;
    let column : Array<Character>;
    
    func number_from_column(zero : Character, one: Character, keys: Array<Character>) -> Int {
        let result = keys.reduce((0, 0), {acc, x in
            let counter = acc.0;
            let sum = acc.1;
            return (counter + 1, x == zero ? sum : sum + (1 << counter));
        })
        
        return result.1;
    }
    
    init(def: String) {
        let array = Array(def)
        let row = Array(array[0...6])
        let column = Array(array[7..<def.count])
        
        self.row = Array(row.reversed())
        self.column = Array(column.reversed())
    }
}

func day_5_main() {
    let lines = loadArrayOfStrings(path: "input/5.txt")

    let ids = lines.map { (line : String) -> Int in
        let seat = Seat.init(def: line)

        let row = seat.number_from_column(zero: "F", one: "B", keys: seat.row)
        let column = seat.number_from_column(zero: "L", one: "R", keys: seat.column)
        let id = row * 8 + column
        return id
    }

    print(ids.max() ?? 0)
    
    let ids_without_front_or_back : Set<Int> = Set(Array(8..<1015))
    let missing_ids : Array<Int> = Array(ids_without_front_or_back.subtracting(ids))
    
    for n in 8...1015 {
        if missing_ids.contains(n) && ids.contains(n - 1) && ids.contains(n + 1) {
            print("Your id: ", n)
        }
    }
}
