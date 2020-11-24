//
//  File.swift
//  
//
//  Created by Daniel Wehner on 20/11/2020.
//

import Foundation
import Vapor

struct AddUserCommand : Command {
    struct Signature : CommandSignature { }
    
    var help : String {
        "Creates a user"
    }
    
    func run(using context: CommandContext, signature: Signature) throws {
        
        let name = context.console.ask("What is your \("name", color: .blue)?")
        context.console.print("Hello, \(name) 👋")
        
        let token = context.console.ask("What should be the \("token", color: .blue)?")
        
        let user = User(name: name, token: token)
        context.console.print("Hello, \(user)")
    }
}
