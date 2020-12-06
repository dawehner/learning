//
//  day_4.swift
//  adventofcode
//
//  Created by Daniel Wehner on 04/12/2020.
//

import Foundation

typealias Fieldname = String
typealias PassportFieldValue = String

typealias Passport = [Fieldname : PassportFieldValue]

let requiredFields : [Fieldname] = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

func isValidPassport(passport: Passport) -> Bool {
    let fields = passport.keys
    let missing_fields = Set(requiredFields).subtracting(fields)
    
    let result = missing_fields.count == 0
        || (missing_fields.count == 1 && missing_fields.contains("cid"))

    return result
}

func validateYear(_ value: String.SubSequence, low: Int, high: Int) -> Bool {
    if let year = Int(value) {
        return year >= low && year <= high
    }
    return false
}

func validateHeight(_ value: String.SubSequence, low: Int, high: Int) -> Bool {
    if let value = Int(value.prefix(value.count-1)) {
        return value >= low && value <= high
    }
    return false
}

func validateHair(_ value: String.SubSequence) -> Bool {
    let valueContents = Set(value.dropFirst())
    return value.first! == "#" && value.count == 7 &&
        valueContents.intersection(Set("0123456789abcdef")).count == valueContents.count
}

func validField(_ field: String) -> String? {
    let parts = field.split(separator: ":")
    let field = String(parts.first!)
    let value = parts.last!
    let valid: Bool = {
        switch field {
        case "byr" : return validateYear(value, low: 1920, high: 2002)
        case "iyr" : return validateYear(value, low: 2010, high: 2020)
        case "eyr": return validateYear(value, low: 2020, high: 2030)
        case "hgt":
            switch value.suffix(2) {
            case "cm": return validateHeight(value, low: 150, high: 193)
            default : return validateHeight(value, low: 59, high: 76)
            }
        case "hcl": return validateHair(value)
        case "ecl":
            return Set(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]).contains(value)
        case "pid": return value.count == 9 && Int(value) != nil
        case "cid": return true
        default: return false
        }
    }()
    return valid ? field : nil
}

func isValidPassportWithFields2(passport: Passport) -> Bool {
    let fields = passport.keys
    
    for field in fields {
        let line = "\(field):\(passport[field] ?? "")"
        if validField(line) == nil {
            return false
        }
    }
    return true
}

func isValidPassportWithFields(passport: Passport) -> Bool {
    var conditions : [Bool] = [];

    if let byr = passport["byr"] {
        if let byr_number = Int(byr) {
            conditions.append(byr_number >= 1920 && byr_number <= 2002)
        }
        else {
            conditions.append(false)
        }
    }
    
    if let iyr = passport["iyr"] {
        if let iyr_number = Int(iyr) {
            conditions.append(iyr_number >= 2010 && iyr_number <= 2020)
        }
        else {
            conditions.append(false)
        }
    }
    
    if let eyr = passport["eyr"] {
        if let eyr_number = Int(eyr) {
            conditions.append(eyr_number >= 2020 && eyr_number <= 2030)
        }
        else {
            conditions.append(false)
        }
    }
    
    if let hgt = passport["hgt"] {
        let pattern = #"(\d+)(cm|in)"#
        let regex = try? NSRegularExpression(
          pattern: pattern,
          options: .caseInsensitive
        )
        if let match = regex?.firstMatch(in: hgt, options: [], range: NSRange(location: 0, length: hgt.utf16.count)) {
            if let range1 = Range(match.range(at: 1), in: hgt) , let x1 = Int(hgt[range1]) {
              if let range2 = Range(match.range(at: 2), in: hgt) {
                let unit = String(hgt[range2])
                conditions.append(
                    (unit=="cm" && x1 >= 150 && x1 <= 193)
                    ||
                    (unit == "in" && x1 >= 59 && x1 <= 76)
                )
              }
              else {
                conditions.append(false)
              }
            }
            else {
                conditions.append(false)
            }
        }
        else {
            conditions.append(false)
        }
    }
    
    if let hcl = passport["hcl"] {
        let pattern = #"#[0-9a-f]{6}"#
        let regex = try? NSRegularExpression(
          pattern: pattern,
          options: .caseInsensitive
        )
        if regex?.firstMatch(in: hcl, options: [], range: NSRange(location: 0, length: hcl.utf16.count)) != nil {
            conditions.append(true)
        }
        else {
            conditions.append(false)
        }
    }

    if let ecl = passport["ecl"] {
        conditions.append(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(ecl))
    }
    
    if let pid = passport["pid"] {
        let pattern = #"[0-9]{9}"#
        let regex = try? NSRegularExpression(
          pattern: pattern,
          options: .caseInsensitive
        )
        if regex?.firstMatch(in: pid, options: [], range: NSRange(location: 0, length: pid.utf16.count)) != nil {
            conditions.append(true)
        }
        else {
            conditions.append(false)
        }
    }
    
    return conditions.allSatisfy { $0 }
}

func day_4_main() {
    let passport = ["byr": "123"]
    _ = isValidPassport(passport: passport)
    
    let lines = loadArrayOfStringsSeparatedByEmptyLine(path: "input/4.txt")
    
//    let lines = """
//pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
//hcl:#623a2f
//
//eyr:2029 ecl:blu cid:129 byr:1989
//iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
//
//hcl:#888785
//hgt:164cm byr:2001 iyr:2015 cid:88
//pid:545766238 ecl:hzl
//eyr:2022
//
//iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
//""".components(separatedBy: "\n\n")
    
    var total_valid = 0
    for line in lines {
        let replaced_line = line.replacingOccurrences(of: "\n", with: " ")
        let field_pairs = replaced_line.split(separator: " ")
        
        var passport : Passport = [:]
        for field_pair in field_pairs {
            let field_value_split = field_pair.split(separator: ":")
            
            passport[String(field_value_split[0])] = String(field_value_split[1])
        }
        if isValidPassportWithFields2(passport: passport) != isValidPassportWithFields(passport: passport) {
            print(passport)
        }
        if isValidPassport(passport: passport) && isValidPassportWithFields2(passport: passport) {
            total_valid += 1;
        }
        
    }
    print(total_valid)
}
