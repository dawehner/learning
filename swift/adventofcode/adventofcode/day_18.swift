//
//  day_18.swift
//  adventofcode
//
//  Created by Daniel Wehner on 25/12/2020.
//

import Foundation

enum Day18Token {
    case Numb(Int)
    case Plus
    case Multi
    case OpenP
    case CloseP
    case EOL
}

indirect enum Day18Expr {
    case Plus(Day18Expr, Day18Expr)
    case Mult(Day18Expr, Day18Expr)
    case Numb(Int)
    case Parathesis(Day18Expr)

    static func eval(_ expr: Day18Expr) -> Int {
        switch expr {
        case .Plus(let ex1, let ex2): return eval(ex1) + eval(ex2)
        case .Mult(let ex1, let ex2): return eval(ex1) * eval(ex2)
        case .Numb(let number): return number
        case .Parathesis(let ex1): return eval(ex1)
        }
    }

    static func tokenize(_ string: String) -> Array<Day18Token> {
        let regex = #"(\d+)|(\()|(\))|([\+*])"#
        let result = string.matchingStrings(regex: regex)
        let tokens : Array<Day18Token> = result.flatMap { (tokenstr : Array<String>) in
            if tokenstr[1] != "" {
                if let number = Int(tokenstr[1]) {
                    return Day18Token.Numb(number)
                }
                return nil
            }
            else if tokenstr[2] == "(" {
                return Day18Token.OpenP
            }
            else if tokenstr[3] == ")" {
                return Day18Token.CloseP
            }
            else if tokenstr[4] == "*" {
                return Day18Token.Multi
            }
            else if tokenstr[4] == "+" {
                return Day18Token.Plus
            }
            else {
                return nil
            }
        } + [Day18Token.EOL]
        
        return tokens
    }
    
    static func parse(_ tokens : Array<Day18Token>) {
        
    }
}

func day_18_main() {
    let lines = loadArrayOfStrings(path: "input/18.txt")

    let tokens = Day18Expr.tokenize(lines[0])
    

//    let a1 = Day18Expr.Mult(Day18Expr.Numb(2), Day18Expr.Numb(2))
//
//    print(Day18Expr.eval(a1))
}
