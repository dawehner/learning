//
//  day_11.swift
//  adventofcode
//
//  Created by Daniel Wehner on 13/12/2020.
//

import Foundation

enum State {
    case Floor
    case EmptySeat
    case OccupiedSeat
}
struct Seating {
    let dimension: (Int, Int)
    var seats: Array<State>

    func index(i: Int, j: Int) -> Int {
        return i * dimension.0 + j
    }
    func get(i: Int, j: Int) -> State {
        return seats[index(i: i, j: j)]
    }
    mutating func set(i: Int, j: Int, s: State) {
        self.seats[index(i: i, j: j)] = s
    }
//
    func printSeating() {
        var string = ""
        for index_w in 0...(dimension.0 - 1) {
            for index_h in 0...(dimension.1 - 1) {
                switch seats[index(i: index_h, j: index_w)] {
                case .Floor: string += "."
                case .EmptySeat: string += "L"
                case .OccupiedSeat: string += "#"
                }
            }
            string += "\n"
        }
        print(string)
    }

    func fillStep() -> (Seating, Int) {
        var copy = self
        var count_changes = 0
        for index_w in 0...(dimension.0 - 1) {
            for index_h in 0...(dimension.1 - 1) {
                var neighborsOccupied = 0

                let currentPos = self.get(i: index_h, j: index_w)
                if currentPos == State.Floor {
                    continue
                }

                if (index_w - 1) >= 0 && (index_h - 1) >= 0 && self.get(i: index_h - 1, j: index_w - 1) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }
                if (index_w - 1) >= 0 && (index_h) >= 0 && self.get(i: index_h, j: index_w - 1) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }
                if (index_w - 1) >= 0 && (index_h + 1) < dimension.1 && self.get(i: index_h + 1, j: index_w - 1) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }
                if (index_w) >= 0 && (index_h - 1) >= 0 && self.get(i: index_h - 1, j: index_w) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }
                if (index_w) >= 0 && (index_h + 1) < dimension.1 && self.get(i: index_h + 1, j: index_w) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }
                if (index_w + 1) < dimension.0 && (index_h - 1) >= 0 && self.get(i: index_h - 1, j: index_w + 1) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }
                if (index_w + 1) < dimension.0 && (index_h) >= 0 && self.get(i: index_h, j: index_w + 1) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }
                if (index_w + 1) < dimension.0 && (index_h + 1) < dimension.1 && self.get(i: index_h + 1, j: index_w + 1) == State.OccupiedSeat {
                    neighborsOccupied += 1
                }

                if currentPos == State.EmptySeat && neighborsOccupied == 0 {
                    copy.set(i: index_h, j: index_w, s: State.OccupiedSeat)
                    count_changes += 1
                }
                else {
                    if currentPos == State.OccupiedSeat && neighborsOccupied >= 4 {
                        copy.set(i: index_h, j: index_w, s: State.EmptySeat)
                        count_changes += 1
                    }
                }
            }
        }

        return (copy, count_changes)
    }

    /**func fillStepb() -> (Seating, Int) {
        var copy = self
        var count_changes = 0
        print(dimension)
        for index_w in 0...(dimension.0 - 1) {
            for index_h in 0...(dimension.1 - 1) {
                let neighborsOccupied = 0

                let currentPos = self.get(i: index_h, j: index_w)
                if currentPos == State.Floor {
                    continue
                }

                var left: Array<State> = []
                var right: Array<State> = []

                if index_w > 0 {
                    left = stride(from: index(i: index_h, j: 0), to: index(i: index_h, j: max(index_w - 1, 0)), by: dimension.0).map { seats[$0] }.reversed()
                }
                if index_w < dimension.0 {
                    right = stride(from: index(i: index_h, j: min(index_w + 1, dimension.0 - 1)), to: (min(index_h + 1, dimension.1) * dimension.0 - 1), by: dimension.0).map { seats[$0] }
                }

                var up: Array<State> = []
                var down: Array<State> = []
                if index_h > 0 {
                    up = stride(from: index_w, to: index(i: index_h - 1, j: index_w), by: dimension.0).map { seats[$0] }.reversed()
                }
                if index_h < dimension.1 {
                    down = stride(from: index(i: index_h + 1, j: index_w), to: index(i: dimension.1 - 1, j: index_w), by: dimension.0).map { seats[$0] }
                }

                var left_up: Array<State> = []
                var left_down: Array<State> = []
                var right_up: Array<State> = []
                var right_down: Array<State> = []
                if (index_h > 0 && index_w > 0) {
                    // ...
                    // ..# x: 1, y: 3  | 3 : 1
                    // => .#. x: 0, y: 2 | 2: 0
                    //    ...
                    let end_tuple: (Int, Int) = (index_h - index_w) > 0 ? (index_h - index_w, 0) : (0, index_w - index_h)
                    left_up = stride(from: index(i: end_tuple.0, j: end_tuple.1), to: index(i: index_h - 1, j: index_w - 1), by: dimension.0 + 1).map { seats[$0] }.reversed()
                }
                if (index_h > 0 && index_w < dimension.0 - 1) {
                    let end_tuple: (Int, Int) = (index_h - index_w) > 0 ? (index_h - index_w, 0) : (0, index_w - index_h)

                }
                // @todo diagonal left down, diagonal left up, diagonal right up, diagonal right down

                if currentPos == State.EmptySeat && neighborsOccupied == 0 {
                    copy.set(i: index_h, j: index_w, s: State.OccupiedSeat)
                    count_changes += 1
                }
                else {
                    if currentPos == State.OccupiedSeat && neighborsOccupied >= 5 {
                        copy.set(i: index_h, j: index_w, s: State.EmptySeat)
                        count_changes += 1
                    }
                }
            }
        }

        return (copy, count_changes)
    }*/


    func iterative() -> Seating {
        var seating = self
        var count_changes = 1

        while (count_changes > 0) {
            let result = seating.fillStep()

            seating = result.0
            count_changes = result.1
        }
        return seating
    }

//    func iterativeb() -> Seating {
//        var seating = self
//        var count_changes = 1
//
//        while (count_changes > 0) {
//            let result = seating.fillStepb()
//
//            seating = result.0
//            count_changes = result.1
//        }
//        return seating
//    }

    func countOccupied() -> Int {
        return seats.filter { $0 == State.OccupiedSeat }.count
    }
}

func day_11_main() {
    let lines = loadArrayOfStrings(path: "input/11.txt")
    let seats: Array<Array<State>> = lines.map { line in
        let enums: Array<State> = Array(line).map { char in
            switch char {
            case "#": return State.OccupiedSeat
            case ".": return State.Floor
            case "L": return State.EmptySeat
            default: return State.Floor
            }
        }
        return enums
    }
    let seating = Seating(dimension: (seats[0].count, seats.count), seats: seats.flatMap { $0 })

    print(seating.iterative().countOccupied())
//    print(seating.iterativeb().countOccupied())



}
