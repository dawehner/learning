import Fluent

struct CreateWallpaper: Migration {
    func prepare(on database: Database) -> EventLoopFuture<Void> {
        return database.schema("wallpaper")
            .id()
            .field("source_id", .string, .required)
            .field("url", .string, .required)
            .field("thumbnail_url", .string)
            .field("full_url", .string, .required)
            .create()
    }

    func revert(on database: Database) -> EventLoopFuture<Void> {
        return database.schema("wallpaper").delete()
    }
}
