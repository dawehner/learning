//
//  day_12.swift
//  adventofcode
//
//  Created by Daniel Wehner on 17/12/2020.
//

import Foundation

enum Direction {
    case N
    case S
    case W
    case E

    func rotateDirection(_ int: Int) -> Direction {
        let loop = [Direction.N, Direction.E, Direction.S, Direction.W]
        guard let pos = loop.firstIndex(of: self) else { return Direction.N }
        let index_move: Int = int / 90
        let new_index = (pos + index_move + loop.count) % loop.count

        return loop[new_index]
    }
}

struct Day12State {
    let direction: Direction
    let pos: (Int, Int)

    func advance(input: Input) -> Day12State {
        switch input {
        case .N(let n): return self.withPos((self.pos.0, self.pos.1 - n))
        case .S(let n): return self.withPos((self.pos.0, self.pos.1 + n))
        case .W(let n): return self.withPos((self.pos.0 - n, self.pos.1))
        case .E(let n): return self.withPos((self.pos.0 + n, self.pos.1))
        case .F(let n):
            switch self.direction {
            case .N: return self.advance(input: Input.N(n))
            case .E: return self.advance(input: Input.E(n))
            case .S: return self.advance(input: Input.S(n))
            case .W: return self.advance(input: Input.W(n))
            }
        case .R(let n): return self.withDirection(self.direction.rotateDirection(n))
        case .L(let n): return self.withDirection(self.direction.rotateDirection(-1 * n))
        }
    }

    func withDirection (_ dir: Direction) -> Day12State {
        return Day12State(direction: dir, pos: self.pos)
    }
    func withPos (_ pos: (Int, Int)) -> Day12State {
        return Day12State(direction: self.direction, pos: pos)
    }
}

struct Day12StateB {
    let shipPos: (Int, Int)
    let waypointPos: (Int, Int)

    func advance(input: Input) -> Day12StateB {
        switch input {
        case .N(let n): return withWaypointPos((self.waypointPos.0, self.waypointPos.1 - n))
        case .S(let n): return withWaypointPos((self.waypointPos.0, self.waypointPos.1 + n))
        case .W(let n): return withWaypointPos((self.waypointPos.0 - n, self.waypointPos.1))
        case .E(let n): return withWaypointPos((self.waypointPos.0 + n, self.waypointPos.1))

        case .R(let n): return withWaypointPos(rotateWaypoint(n))
        case .L(let n): return withWaypointPos(rotateWaypoint(-1 * n))

        case .F(let n):
            return withShipPos((self.shipPos.0 + n * self.waypointPos.0, self.shipPos.1 + n * self.waypointPos.1))
        }

    }

    func rotateWaypoint(_ n: Int) -> (Int, Int) {
        let rotationMatrix = [
            (cos(Double(n) * Double.pi / 180.0), -1 * sin(Double(n) * Double.pi / 180)),
            (sin(Double(n) * Double.pi / 180.0), cos(Double(n) * Double.pi / 180))
        ]

        let a = Double(self.waypointPos.0)
        let b = Double(self.waypointPos.1)
        let res : (Int, Int) = (Int(round(rotationMatrix[0].0 * a + rotationMatrix[0].1 * b)), Int(round(rotationMatrix[1].0 * a + rotationMatrix[1].1 * b)))
        return res
    }

    func withWaypointPos (_ pos: (Int, Int)) -> Day12StateB {
        return Day12StateB(shipPos: self.shipPos, waypointPos: pos)
    }

    func withShipPos (_ pos: (Int, Int)) -> Day12StateB {
        return Day12StateB(shipPos: pos, waypointPos: self.waypointPos)
    }
}

enum Input {
    case N(Int)
    case S(Int)
    case W(Int)
    case E(Int)
    case F(Int)
    case R(Int)
    case L(Int)

    static func fromString(_ string: String) -> Optional<Input> {
        let regex = #"(N|S|W|E|F|R|L)(\d+)"#
        if let result = string.matchingString(regex: regex) {
            if let int = Int(result[2]) {
                switch result[1] {
                case "N": return Input.N(int)
                case "S": return Input.S(int)
                case "W": return Input.W(int)
                case "E": return Input.E(int)
                case "F": return Input.F(int)
                case "R": return Input.R(int)
                case "L": return Input.L(int)
                default:
                    return nil
                }

            }
        }
        return nil
    }
}

func day_12_main() {
    let lines = loadArrayOfStrings(path: "input/12.txt")

    let inputs = lines.map(Input.fromString).compactMap { $0 }

    let state = Day12State.init(direction: Direction.E, pos: (0, 0))
    let result: Day12State = inputs.reduce(state) { agg, input in
        let res = agg.advance(input: input)
        return res
    }
    print(abs(result.pos.0) + abs(result.pos.1))

    let stateB = Day12StateB(shipPos: (0, 0), waypointPos: (10, -1))
    let resultB: Day12StateB = inputs.reduce(stateB) { agg, input in
        let res = agg.advance(input: input)
        return res
    }
    print(abs(resultB.shipPos.0) + abs(resultB.shipPos.1))
}
