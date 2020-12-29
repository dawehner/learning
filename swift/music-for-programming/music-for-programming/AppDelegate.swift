//
//  AppDelegate.swift
//  music-for-programming
//
//  Created by Daniel Wehner on 25/05/2020.
//  Copyright © 2020 Daniel Wehner. All rights reserved.
//

import Cocoa
import SwiftUI
import FeedKit
import AVKit


class AppState : ObservableObject {
    @Published var feed : Feed
    @Published var activeMusic : String?
    
    init(feed : Feed) {
        self.feed = feed
    }
    
    
}

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    var window: NSWindow!



    func applicationDidFinishLaunching(_ aNotification: Notification) {
        let feedUrl = URL(string: "https://www.musicforprogramming.net/rss.php")!
        let parser = FeedParser(URL: feedUrl)
        
        let result = parser.parse()        
        
        // Create the SwiftUI view that provides the window contents.
        let contentView = ContentView()
        
       

        // Create the window and set the content view. 
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 480, height: 300),
            styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
            backing: .buffered, defer: false)
        window.center()
        window.setFrameAutosaveName("Main Window")
        
        switch result {
               case .success(let feed):
                   debugPrint(feed)
                   
                    var appState = AppState(feed: feed)
                   appState.objectWillChange.sink { v in
                        print(v.activeMusic)
                   }
                   
                   window.contentView = NSHostingView(rootView: contentView.environmentObject(appState))
               case .failure(_):
                   exit(1)
               }

        window.makeKeyAndOrderFront(nil)
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }


}

