//
//  Sidebar.swift
//  foam-ios
//
//  Created by Daniel Wehner on 28/08/2020.
//

import SwiftUI

struct Sidebar: View {
    var notes : Notes
    
    init(notes : Notes) {
        self.notes = notes
      
        UITableView.appearance().separatorStyle = .none
        UITableViewCell.appearance().backgroundColor = .black
        UITableView.appearance().backgroundColor = .black
    }

    var body: some View {
        VStack(alignment: .leading, minWidth: .infinity) {
            List(notes.notes, id: \.id) { note in
                Text(note.name)
                    .foregroundColor(.gray)
                    .background(Color.black)
            }
            .listRowBackground(Color.black)
            .background(Color.black)
            .frame(maxHeight: .infinity)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .edgesIgnoringSafeArea(.all)
        .background(Color.black)
    }
}

struct Sidebar_Previews: PreviewProvider {
    static var previews: some View {
        Sidebar(notes: exampleNotes())
    }
}
