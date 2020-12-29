//
//  day_8.swift
//  adventofcode
//
//  Created by Daniel Wehner on 12/12/2020.
//

import Foundation

enum Instruction {
    case Nop(Int)
    case Acc(Int)
    case Jmp(Int)
}

typealias Pointer = Int
typealias Value = Int

enum Day8Error: LocalizedError {
    case InvalidPointer
    case ParseError
    case EndlessLoop

    var errorDescription: String? {
        switch self {
        case .InvalidPointer: return "Invalid pointer"
        case .ParseError: return "Parse error"
        case .EndlessLoop: return "no solution"
        }
    }
}

enum MachineResult {
    case Loop(Int)
    case Terminate(Int)
}

struct Machine {
    let instructions: Array<Instruction>
    let history: Array<Pointer>

    let pointer: Pointer
    let value: Value


    func addHistoryAndValue(pointer: Pointer, value: Value) -> Machine {
        return Machine(instructions: self.instructions, history: self.history + [pointer], pointer: pointer, value: value)
    }

    func executeStep() throws -> (Pointer, Value) {
        guard self.instructions.count >= pointer else {
            throw Day8Error.InvalidPointer
        }
        let instruction = self.instructions[pointer]

        return {
            switch instruction {
            case .Nop: return (pointer + 1, self.value)
            case .Acc(let diff): return (pointer + 1, self.value + diff)
            case .Jmp(let pointerDiff): return (pointer + pointerDiff, self.value)
            }
        }()
    }

    func executeUntilLoop() throws -> MachineResult {
        var machine = self
        repeat {
            let (pointer, value) = try machine.executeStep()

            if machine.history.contains(pointer) {
                return MachineResult.Loop(value)
            }
            else {
                if (pointer >= machine.instructions.count) {
                    return MachineResult.Terminate(value)
                }
                else {
                    machine = machine.addHistoryAndValue(pointer: pointer, value: value)
                }
            }
        }
        while (true)
    }

    func tryToTerminate() throws -> Value {
        let sequence = InstructionSwappedSequence(instructions: self.instructions, pointer: self.pointer)
        for newInstructions in sequence {
            let machine = Machine(instructions: newInstructions, history: [], pointer: 0, value: 0)

            let executeResult = try machine.executeUntilLoop()
            switch executeResult {
            case .Terminate(let x): return x
            case .Loop(_): continue
            }
        }
        throw Day8Error.EndlessLoop
    }
}

struct InstructionSwappedSequence: Sequence, IteratorProtocol {
    let instructions: Array<Instruction>
    var pointer: Pointer

    mutating func next() -> Array<Instruction>? {

        if pointer >= instructions.count {
            return nil
        } else {
            var newInstructions = instructions
            let instruction: Instruction = newInstructions[pointer]
            let swappedInstruction: Instruction = {
                switch instruction {
                case .Nop(let x): return Instruction.Jmp(x)
                case .Jmp(let x): return Instruction.Nop(x)
                case .Acc(let x): return Instruction.Acc(x)
                }
            }()
            newInstructions[pointer] = swappedInstruction
            
            self.pointer += 1

            return newInstructions
        }
    }
}

func day_8_parse(lines: Array<String>) throws -> Array<Instruction> {
    let instructions: Array<Instruction> = lines.compactMap { line in
        let regex = #"(nop|acc|jmp) (\+|-)(\d+)"#
        if let result = line.matchingString(regex: regex) {
            if result.count != 4 {
                return nil
            }

            let parsedNumber = Int(result[3])
            if let parsedNumber = parsedNumber {

                let argument = result[2] == "+" ? parsedNumber : -1 * parsedNumber

                let result: Instruction? = { switch result[1] {
                        case "jmp": return Instruction.Jmp(argument)
                        case "acc": return Instruction.Acc(argument)
                        case "nop": return Instruction.Nop(argument)
                        default: return nil
                    }
                }()
                return result
            }
        }
        return nil
    }
    return instructions
}

func day_8_main() {
    let lines = loadArrayOfStrings(path: "input/8.txt")

    do {
        let instructions = try day_8_parse(lines: lines)

        let machine = Machine(instructions: instructions, history: [], pointer: 0, value: 0)
        let result = try machine.executeUntilLoop()

        print(result)

        let resultTerminate = try machine.tryToTerminate()
        print(resultTerminate)

    }
    catch {
        print(error)
    }

}
