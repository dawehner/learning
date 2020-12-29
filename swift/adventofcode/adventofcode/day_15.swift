//
//  day_15.swift
//  adventofcode
//
//  Created by Daniel Wehner on 23/12/2020.
//

import Foundation

struct MemoryB {
    let lastNumber: Int
    let lastPos: Int
    var lastPosMem: [Int: Int]

    func nextNumber() -> Int {
        let lastNumber = self.lastNumber

        let currentPos = self.lastPos
        let lastPos = lastPosMem[lastNumber]

        if lastPos == nil {
            return 0
        }
        else {
            return currentPos - lastPos!
        }
    }

    mutating func addHistory(_ int: Int) -> Self {
        self.lastPosMem[lastNumber] = lastPos
        return MemoryB(lastNumber: int, lastPos: lastPos + 1, lastPosMem: self.lastPosMem)
    }
}

struct Memory {
    let history: [Int]

    func nextNumber() -> Int {
        let lastNumber = history.last!

        let currentPos = history.count
        let lastIndex = history[0...history.count - 2].enumerated().reversed().firstIndex(where: { $0.1 == lastNumber })

        if lastIndex == nil {
            return 0
        }
        else {
            let lastPos = history.count - 1 - lastIndex!

            return currentPos - lastPos
        }
    }

    func addHistory(_ int: Int) -> Self {
        return Memory(history: history + [int])
    }
}

func day_15_main() {
//    _ = [11, 0, 1, 10, 5, 19]
//    let memory = MemoryB(lastNumber: 19, lastPos: 6, lastPosMem: [11: 1, 0: 2, 1: 3, 10: 4, 5: 5])
    let memory = MemoryB(lastNumber: 6, lastPos: 3, lastPosMem: [0: 1, 1: 2])

    let result = Array(7...30000000).reduce(into: memory) { agg, i in
        if i % 100000 == 0 {
            print(i)
        }
        agg.addHistory(agg.nextNumber())
    }
    print(result.lastNumber)
}
