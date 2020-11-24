import Fluent
import Vapor

final class Wallpaper: Model, Content {
    static let schema = "wallpaper"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "source_id")
    var source_id: String
    
    @Field(key: "url")
    var url: String
    
    @OptionalField(key: "thumbnail_url")
    var thumbnail_url: String?
    
    @Field(key: "full_url")
    var full_url: String

    init() { }

    init(id: UUID? = nil, source_id : String, url: String, thumbnail_url : String?, full_url: String) {
        self.id = id
        self.source_id = source_id
        self.url = url
        self.thumbnail_url = thumbnail_url
        self.full_url = full_url
    }
}
