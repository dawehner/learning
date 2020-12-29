//
//  day_17.swift
//  adventofcode
//
//  Created by Daniel Wehner on 23/12/2020.
//

import Foundation

struct Coord: Hashable {
    let x: Int
    let y: Int
    let z: Int

    func add(_ int: Int) -> Coord {
        Coord(x: x + int, y: y + int, z: z + int)
    }
}

struct Grid {
    var states: Set<Coord>
    var frame: (Coord, Coord)

    init(states: Set<Coord>, frame: (Coord, Coord)?) {
        self.states = states
        self.frame = frame == nil ? Grid.calcMinMax(states: states) : frame!
    }

    static func calcMinMax(states : Set<Coord>) -> (Coord, Coord) {
        let minX = Array(states).map(\.x).reduce(10000, min) - 1
        let maxX = Array(states).map(\.x).reduce(0, max) + 1

        let minY = Array(states).map(\.y).reduce(10000, min) - 1
        let maxY = Array(states).map(\.y).reduce(0, max) + 1

        let minZ = Array(states).map(\.z).reduce(10000, min) - 1
        let maxZ = Array(states).map(\.z).reduce(0, max) + 1

        return (Coord(x: minX, y: minY, z: minZ), Coord(x: maxX, y: maxY, z: maxZ))
    }

    func countActiveInactiveNeighbords(coord: Coord) -> Int {
        let neighbords = CoordNeighbours(coord: coord)
        let intersection = Set(neighbords).intersection(states)

        return intersection.count
    }

    func nextCyle() -> Grid {
        var newStates = states

        let allCoordsAndNeighBours = Array(frame.0.x...frame.1.x).flatMap { x in
            Array(frame.0.y...frame.1.y).flatMap { y in
                Array(frame.0.z...frame.1.z).compactMap { z in
                    Coord(x: x, y: y, z: z)
                }
            }
        }

        allCoordsAndNeighBours.forEach { coord in
            let active = self.countActiveInactiveNeighbords(coord: coord)

            if states.contains(coord) == true {
                if active == 2 || active == 3 {
                    newStates.insert(coord)
                }
                else {
                    newStates.remove(coord)
                }
            }
            else {
                if active == 3 {
                    newStates.insert(coord)
                }
                else {
                    newStates.remove(coord)
                }
            }
        }

        let nextGrid = Grid(states: newStates, frame: (frame.0.add(-1), frame.1.add(1)))

        return nextGrid
    }
}

struct CoordNeighbours: Sequence, IteratorProtocol {
    let coord: Coord
    let neighbours: [Coord]
    var index: Int

    init(coord: Coord) {
        self.coord = coord
        self.neighbours = [
            Coord(x: coord.x - 1, y: coord.y - 1, z: coord.z - 1),
            Coord(x: coord.x - 1, y: coord.y - 1, z: coord.z),
            Coord(x: coord.x - 1, y: coord.y - 1, z: coord.z + 1),

            Coord(x: coord.x - 1, y: coord.y, z: coord.z - 1),
            Coord(x: coord.x - 1, y: coord.y, z: coord.z),
            Coord(x: coord.x - 1, y: coord.y, z: coord.z + 1),

            Coord(x: coord.x - 1, y: coord.y + 1, z: coord.z - 1),
            Coord(x: coord.x - 1, y: coord.y + 1, z: coord.z),
            Coord(x: coord.x - 1, y: coord.y + 1, z: coord.z + 1),

            Coord(x: coord.x, y: coord.y - 1, z: coord.z - 1),
            Coord(x: coord.x, y: coord.y - 1, z: coord.z),
            Coord(x: coord.x, y: coord.y - 1, z: coord.z + 1),

            Coord(x: coord.x, y: coord.y, z: coord.z - 1),
//            Coord(x: coord.x , y: coord.y , z : coord.z ),
            Coord(x: coord.x, y: coord.y, z: coord.z + 1),

            Coord(x: coord.x, y: coord.y + 1, z: coord.z - 1),
            Coord(x: coord.x, y: coord.y + 1, z: coord.z),
            Coord(x: coord.x, y: coord.y + 1, z: coord.z + 1),

            Coord(x: coord.x + 1, y: coord.y - 1, z: coord.z - 1),
            Coord(x: coord.x + 1, y: coord.y - 1, z: coord.z),
            Coord(x: coord.x - 1, y: coord.y - 1, z: coord.z + 1),

            Coord(x: coord.x + 1, y: coord.y, z: coord.z - 1),
            Coord(x: coord.x + 1, y: coord.y, z: coord.z),
            Coord(x: coord.x + 1, y: coord.y, z: coord.z + 1),

            Coord(x: coord.x + 1, y: coord.y + 1, z: coord.z - 1),
            Coord(x: coord.x + 1, y: coord.y + 1, z: coord.z),
            Coord(x: coord.x + 1, y: coord.y + 1, z: coord.z + 1),
        ]

        self.index = -1
    }

    mutating func next() -> Coord? {
        index += 1

        if index != neighbours.count {
            return neighbours[index]
        }
        return nil
    }
}

func day_17_main() {
//    let lines = loadArrayOfStrings(path: "input/17.txt")
    let lines = [
        ".#.",
        "..#",
        "###",
    ]

    var states : Set<Coord> = Set()
    lines.enumerated().forEach { row in
        Array(row.1).enumerated().forEach { element in
            if element.1 == "#" {
                states.insert(Coord(x: element.0, y: row.0, z: 0))
            }
        }
    }
    let grid = Grid(states: states, frame: (Coord(x: -1, y: -1, z: -1), Coord(x: 3, y: 3, z: 1)))


    let final = Array(1...6).reduce(grid) { agg, _ in agg.nextCyle() }
    print(final.states.count)
    
    print(workingSolution())
}

// Copied from https://topaz.github.io/paste/#XQAAAQDvCwAAAAAAAAA0m0pnuFI8c4GDemuKS6NnNxbsTu3UkdcIGik0pcIVv2dKhKRZiXT8Mf3K+cjgvvkdN1YKWimKJ5qWruj3TiNUI1AX9/ZL+rTY2WDGcPRCmnKNljWvzq+j1R//8t8sbSis7JYEYI983kHIwVSSpWQqoQgiUjjdaowGCaoo0JQixzWKAggLLgiZND6/jW11q9p5E37QkXNdrXb5erBvPuvBQZn4bLPpmr2eeuZif1aQC1WhyasuQf2kZ6lw/Ju1DWuTMEGuro9OdH1BNGS8LFbQJQra3DXLZEagBUPSOX7i/6Fzeqy8zrp7MW0+jV+VCbGsgjbiiqgrnUGhCJfdhW3r6vJLaEk63ogh3iPxt1nzl/KYpaUbu4/K/I+FQPArzGAzUbTselmqL2zn3f7UuZqx9gRvPv1mjvSbEyG3kt5Zs7lX+utHoYcnZu1e2ruJmUzakYEr9Mz04E8aql7uBFQSfE/kEfYH+2IBOrD736exsnWJcV9moKbMl1LQuy4TBBLEks9JK1Mni/w3lJIwmTYGrrYrdBgBz239oZdyxjJNYEC7ZoZ9vZ/suyomOu7atcJpaoEXnf8JI+TIOSaabJH97IdiXPSMlv8++4zMISfNml7cjVI9L+Q2oMWCiYhvrARM+fZisVohLWz6+6i+uGLgg3EGO6FxBivav7p793MjavFS6HpMr2z1ai1yRnmsEdfLMcvDUAEnkm8hKIxadw1PrZACYLPtzpadKu9r8mf1PPYmzjj//GqCsLni5N4Z1ynsNrMlAKtsNeWWOBmjmposKgp4XkEWfq1ekBPUBLVVtU1yUfenCv9vfmPc0k83MCcFEi/lpe+hbMh/hLhWQuAJPpZTnFyt89Pl3IezCedA30o3HcnOX9R3aLkGOU42bM0z+NCJAwja6HWbB7c+azupWLoEdKCo0wYdzt3WCgYQEaIt6a6+0sJwCsYBg7jEw+jbC3tAmrCRGdlAB6jbFHZ1Ly1z8PBAqyz0Rn8mEAVVLmjcwubiI2kXo7F4l6PUaLiT9UXoAzhrw4q5o4lB5YzXIINRBCigYim9ixUItBBzqU8pa0YUGLr5XJCcrryxy6YBwFXKCFPmaY+K77f1cEVDtBVXar3loeJm5NJ6rwPNGFIl+R7XREBoShaKfD/LlzHWX9XPDUrk9WDypw0Z0lRwosbzlxAURXxdpzHfzdAH+do81NlNETtugR4RCz54oX09q8HBaqxuTd7eyMQAETz8xJzsHhLiB8PNgxJ99KyemsIUpX1ONl6ocS0Fusa6nPWE6WTN4g026Pv++q/iyA==
func workingSolution() -> Int {

    // --- Day 17: Conway Cubes --- PART TWO!!

    //guard let url = Bundle.main.url(forResource: "input", withExtension: "txt") else { fatalError()}

    struct Cube: Equatable, Hashable {let x,y,z: Int}

    struct PocketDimension {
        var actives : Set<Cube> = []
        var inactives: Set<Cube> = []
        var nextActives : Set<Cube> = []
        var nextInactives: Set<Cube> = []
        static var cycles = 0

        init() {
            let bootSector = loadArrayOfStrings(path: "input/17.txt")
            for (yIndex, y) in bootSector.enumerated() {
                for (xIndex,x)  in y.enumerated() {
                    if x == "#" { actives.insert(Cube(x: xIndex, y: yIndex, z: 0))} else {
                        inactives.insert(Cube(x: xIndex, y: yIndex, z: 0)); continue}
                }
            }
        }

        func printBootSector() {
            for w in -Self.cycles...Self.cycles {
                for z in -Self.cycles...Self.cycles {
                    print("\nLayer \(z), \(w)")
                    for y in -10...10 {
                        for x in -10...10 {
                            let cube = Cube(x: x, y: y, z: z)
                            if actives.contains(cube) {print("#", terminator: "")} else {print(".",terminator: "")}
                        }
                        print("\n", terminator: "")
                    }
                }
            }
        }

        mutating func runCycle() {
            for cube in actives {
                var count = 0
                    for z in -1...1 {
                        for y in -1...1 {
                            for x in -1...1 {
                                // this is my current position so skip
                                if x == 0 && y == 0 && z == 0 { continue }
                                // create a neighbour cube
                                let neighbour = Cube(x: (cube.x + x), y: (cube.y + y), z: (cube.z + z))
                                // if this position is active count, if not active insert in inactives
                                if actives.contains(neighbour) {
                                    count += 1
                                    continue
                                } else {
                                    inactives.insert(neighbour)
                                }
                            }
                        }
                }
                if 2...3 ~= count { nextActives.insert(cube) } else {
                    nextInactives.insert(cube)
                }
            }

            for cube in inactives {
                var count = 0
                    for z in -1...1 {
                        for y in -1...1 {
                            for x in -1...1 {
                                    // this is my current position so skip
                                if x == 0 && y == 0 && z == 0 { continue }
                                //create a neighbour cube
                                let neighbour = Cube(x: (cube.x + x), y: (cube.y + y), z: (cube.z + z))
                                // if this position is not active insert in next inactive
                                if actives.contains(neighbour) {
                                    count += 1
                                } else {
                                    nextInactives.insert(neighbour)
                                }
                            }
                        }
                }
                if count == 3 {nextActives.insert(cube)} else {nextInactives.insert(cube)}
            }
            actives = nextActives; nextActives = []
            inactives = nextInactives
            nextInactives = []
            Self.cycles += 1
            print("---------------- cycle \(Self.cycles) -------------")
        }
    }

    var boot = PocketDimension()
    print(boot.printBootSector())
    boot.runCycle()
    boot.runCycle()
    boot.runCycle()
    boot.runCycle()
    boot.runCycle()
    boot.runCycle()
    print("Solution Part Two : ",boot.actives.count)

    return boot.actives.count
}
