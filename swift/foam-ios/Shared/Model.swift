//
//  Model.swift
//  foam-ios
//
//  Created by Daniel Wehner on 28/08/2020.
//

import Foundation


public struct Note {
    var id : Int
    var name : String
}

public struct Notes {
    var notes : [Note];
}

public func exampleNotes() -> Notes {
    return Notes(notes: [
        Note(id: 2, name: "test2"),
        Note(id: 3, name: "test3"),
        Note(id: 4, name: "test4")
    ])
}
