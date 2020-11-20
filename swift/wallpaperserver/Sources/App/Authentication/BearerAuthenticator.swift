import Vapor

struct UserAuthenticator: BearerAuthenticator {
    typealias User = App.User

    func authenticate(
        bearer: BearerAuthorization,
        for request: Request
    ) -> EventLoopFuture<Void> {
       if bearer.token == "foo" {
           request.auth.login(User(name: "Vapor"))
            return request.eventLoop.makeSucceededFuture(())
       }
        return request.eventLoop.makeFailedFuture(Abort(.nonAuthoritativeInformation))
   }
}
