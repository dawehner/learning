//
//  foam_iosApp.swift
//  Shared
//
//  Created by Daniel Wehner on 28/08/2020.
//

import SwiftUI

@main
struct foam_iosApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView(notes: exampleNotes(), fullText: "test")
        }
    }
}
