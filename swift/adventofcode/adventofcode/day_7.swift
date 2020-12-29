//
//  day_7.swift
//  adventofcode
//
//  Created by Daniel Wehner on 10/12/2020.
//

import Foundation

struct Bags {
    var definitions: [String: Bag]

    init(definitions: [String: Bag]) {
        self.definitions = definitions
    }

    init(definitions: Array<Bag>) {
        self.definitions = definitions.reduce([:]) { dict, bag in
            var dict = dict
            dict[bag.parent] = bag
            return dict
        }
    }

    mutating func resolveSingleChild(parent: String, bags: Bags) -> Bags {
        var bs = bags
        guard bs.definitions[parent] != nil else { return bags }
        let current_bag = bs.definitions[parent]!

        if current_bag.children.count == 0 {
            bs.definitions[parent]!.resolvedChildren = []
            return bs
        }

        let bag_children = current_bag.children.map { $0.1 }
        let resolved_children = (
            bag_children.flatMap { (child: String) -> Array<String> in
                guard bs.definitions[child] != nil else { return [] }

                if bs.definitions[child]!.resolvedChildren == nil {
                    let bs_changed = resolveSingleChild(parent: child, bags: bs)
                    bs.definitions = bs_changed.definitions
                }

                return bs.definitions[child]!.resolvedChildren ?? []
            })
        bs.definitions[parent]?.resolvedChildren = bag_children + resolved_children

        return bs
    }

    mutating func resolveChildren() -> Bags {
        self.definitions.keys.forEach { parent in
            self.definitions = self.resolveSingleChild(parent: parent, bags: self).definitions
        }
        return self
    }

    func printBags() {
        self.definitions.forEach { (key, value) in
            print(key, value.resolvedChildren ?? "none")
        }
    }
    
    func countContainedBags(parent : String) -> Int {
        guard definitions[parent] != nil else {
            return 0
        }
        return definitions[parent]!.children.map({ (count, child) in
            return count * (1 + self.countContainedBags(parent: child))
        }).reduce(0) { $0 + $1 }
    }

}

struct Bag {
    let parent: String
    let children: Array<(Int, String)>
    var resolvedChildren: Optional<Array<String>>
    
    func containsBag(bag: String) -> Bool {
        return (resolvedChildren ?? []).contains(bag)
    }
}

func day_7_calc(lines: Array<String>, search_bag : String) -> Int {
    let bagDefinitions: Array<Bag> = lines.compactMap({ line in

        let no_other_bags_pattern = #"(.+) bags contain no other bags"#
        let no_other_result = line.matchingStrings(regex: no_other_bags_pattern)
        if no_other_result.count > 0 {
            return Bag(parent: no_other_result[0][1], children: [], resolvedChildren: [])
        }

        let pattern = #"(.+) bags contain (\d+ .+ bags?)(,\s*\d+ .+ bags?)*"#

        let result = line.matchingStrings(regex: pattern)

        if result.count > 0 {

            let splitted_bags = result[0][2].components(separatedBy: ", ")
            let removed_bags: Array<(Int, String)> = splitted_bags.compactMap { x in
                let without_bags = x.replacingOccurrences(of: " bags", with: "").replacingOccurrences(of: " bag", with: "")
                let number_result = without_bags.matchingStrings(regex: #"(\d+) (.+)"#)
                guard number_result.count > 0 else {
                    return nil
                }
                if let parsed_number = Int(number_result[0][1]) {
                    return (parsed_number, number_result[0][2])
                }
                return nil
            }

            let bag = Bag(parent: result[0][1], children: removed_bags, resolvedChildren: nil)
            return bag
        }
        return nil
    })

    var bags = Bags(definitions: bagDefinitions)
    _ = bags.resolveChildren()
//    bags.printBags()
    
//    return bags.definitions.map { $0.1 }.reduce(0) { (agg, bag) in
//        return agg + (bag.containsBag(bag: search_bag) ? 1 : 0)
//    }
    return bags.countContainedBags(parent: search_bag)
}

func day_7_main() {
    let lines = loadArrayOfStrings(path: "input/7.txt")

//    let lines = [
//        "light red bags contain 1 bright white bag, 2 muted yellow bags.",
//        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
//        "bright white bags contain 1 shiny gold bag.",
//        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
//        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
//        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
//        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
//        "faded blue bags contain no other bags.",
//        "dotted black bags contain no other bags."
//    ]
    print(day_7_calc(lines: lines, search_bag: "shiny gold"))
}
