//
//  ContentView.swift
//  music-for-programming
//
//  Created by Daniel Wehner on 25/05/2020.
//  Copyright © 2020 Daniel Wehner. All rights reserved.
//

import SwiftUI
import FeedKit

extension RSSFeedItem : Hashable {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))
    }
}

struct ContentView: View {
    @EnvironmentObject var appState : AppState

    var body: some View {
        ScrollView(.vertical) {
            VStack(spacing: 20) {
                ForEach(appState.feed.rssFeed!.items!, id: \.self) { item in
                    Button (action: {
                        self.appState.activeMusic = item.enclosure!.attributes!.url
                    }) {
                        Text(item.title!)
                    }
//                        self.appState.activeMusic = item.enclosure!.attributes()!.url
//                    }) {
//                    }
                    
                }
            }
        }
    }
}


struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
