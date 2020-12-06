import Fluent
import FluentPostgresDriver
import Leaf
import Vapor

// configures your application
public func configure(_ app: Application) throws {
    // uncomment to serve files from /Public folder
    // app.middleware.use(FileMiddleware(publicDirectory: app.directory.publicDirectory))

    // register routes
    
    // app.http.server.configuration.port = 8080;
    
    // Commands
    app.commands.use(AddUserCommand(), as: "users:add")
    app.commands.use(ImportRedditWallpaper("wallpaper"), as: "import:reddit:wallpaper")
    app.commands.use(ImportRedditWallpaper("wallpapers"), as: "import:reddit:wallpapers")
    app.commands.use(ImportRedditWallpaper("EarthPorn"), as: "import:reddit:earthporn")
    
    // Database credentials
    app.databases.use(.postgres(hostname: "localhost", username: "vapor_username", password: "vapor_password", database: "vapor_database"), as: .psql)

    // Migrations
    
    app.migrations.add(CreateUsers())
    app.migrations.add(CreateWallpaper())
    
    try routes(app)
}
