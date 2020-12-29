//
//  AppDelegate.swift
//  translate-live
//
//  Created by Daniel Wehner on 02/08/2020.
//  Copyright © 2020 Daniel Wehner. All rights reserved.
//

import Cocoa
import SwiftUI
import AudioKit

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    var window: NSWindow!

    var oscillator = AKOscillator()

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Create the SwiftUI view that provides the window contents.
        let contentView = ContentView()
        
        AudioKit.output = oscillator
        do {
            try AudioKit.start()
        } catch {
            AKLog("AudioKit did not start!")
        }
        
        oscillator.amplitude = random(in: 0.5 ... 1)
        oscillator.frequency = random(in: 220 ... 880)
        oscillator.start()
        
        AKNodeRecorder(node: <#T##AKNode?#>, file: <#T##AKAudioFile?#>, bus: <#T##Int#>)

        // Create the window and set the content view. 
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 480, height: 300),
            styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
            backing: .buffered, defer: false)
        window.center()
        window.setFrameAutosaveName("Main Window")
        window.contentView = NSHostingView(rootView: contentView)
        window.makeKeyAndOrderFront(nil)
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }


}

