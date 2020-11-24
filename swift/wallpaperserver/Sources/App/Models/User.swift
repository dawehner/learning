import Fluent
import Vapor

final class User: Model, Content, Authenticatable {
    static let schema = "users"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "name")
    var name: String
    
    @Field(key: "token")
    var token: String?

    init() { }

    init(id: UUID? = nil, name: String, token: String? = nil) {
        self.id = id
        self.name = name
        self.token = token
    }
}
