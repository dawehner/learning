import Fluent
import Vapor

func routes(_ app: Application) throws {
    
    let protected = app.grouped(UserAuthenticator())
    
    app.get { req in
        return req.view.render("index", ["title": "Hello Vapor!"])
    }
    app.get { req in
        return "It works!"
    }

    protected.get("hello") { req -> String in
        let user = try req.auth.require(User.self)
        return "Hello: " + user.name
    }
}
