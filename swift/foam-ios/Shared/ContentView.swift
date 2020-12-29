//
//  ContentView.swift
//  Shared
//
//  Created by Daniel Wehner on 28/08/2020.
//

import SwiftUI

struct ContentView: View {
    
    var notes : Notes;
    
    @State var fullText : String
    @State var showMenu : Bool = false

    var body: some View {
        let drag = DragGesture()
            .onEnded {
                if $0.translation.width < -100 {
                    withAnimation {
                        self.showMenu = false
                    }
                }
            }

        return GeometryReader { geometry in
            ZStack(alignment: .leading) {
                VStack {
                    Button(action: {
                        withAnimation {
                           self.showMenu = true
                        }
                    }) {
                        Text("Show Menu")
                    };
                    TextEditor(text: $fullText)
                        .frame(width: geometry.size.width, height: geometry.size.height)
                        .offset(x: self.showMenu ? geometry.size.width/2 : 0)
                            .disabled(self.showMenu ? true : false)
                }
                if self.showMenu {
                    Sidebar(notes : notes)
                        .frame(width: geometry.size.width/2)
                        .transition(.move(edge: .leading))
                }
            }.gesture(drag)
        }
    }
}

struct ContentView_Previews: PreviewProvider {

    static var previews: some View {
        ContentView(notes: exampleNotes(), fullText: "test")
    }
}
