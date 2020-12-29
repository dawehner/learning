//
//  ContentView.swift
//  reminder-move
//
//  Created by Daniel Wehner on 11/12/2020.
//

import SwiftUI

class Configuration : ObservableObject {
    @Published var reminders : [Reminder]
    
    init(reminders: [Reminder]) {
        self.reminders = reminders
    }
}

struct Reminder : Identifiable {
    var id: Int
    
    let name : String
    let time : Int
}

struct ContentView: View {

    @ObservedObject var configuration : Configuration = Configuration(reminders: [])

    var body: some View {
        NavigationView {
            List {
                ForEach(configuration.reminders) { reminder in
                    HStack {
                        Text(verbatim: reminder.name)
                        Text(Date().addingTimeInterval(TimeInterval(reminder.time + 1)), style: .relative)
                    }
                }
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    
    static var previews: some View {
        let configuration = Configuration(reminders: [
            Reminder(id: 1, name: "Stand up", time: 3600*2),
            Reminder(id: 2, name: "Move", time: 60 * 20)
        ])
        var view = ContentView()
        view.configuration = configuration
        return view
    }
}
