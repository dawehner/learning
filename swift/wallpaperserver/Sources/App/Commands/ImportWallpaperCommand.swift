//
//  File.swift
//  
//
//  Created by Daniel Wehner on 23/11/2020.
//

import Foundation
import Vapor
import Fluent

struct RedditWallpaperResponse : Content {
    var data: Array<RedditWallpaperItem>
}

struct RedditWallpaperItem : Content {
    var url: String
    var thumbnail : String
    var full_link: String
    var id: String
    var created_utc : Int
}

class RedditSequence : Sequence, IteratorProtocol {
    let subreddit : String

    var total = 0
    let total_limit = 100000
    let items_per_page = 500

    var current = 0
    var currentItems : Array<RedditWallpaperItem> = []
    var lastCreated : Int? = nil
    
    let client : Client
    
    init(_ subreddit : String, client : Client) {
        self.subreddit = subreddit
        self.client = client
    }
    
    func next() -> RedditWallpaperItem? {
        var shouldFetchAdditional = false
        if lastCreated == nil {
            shouldFetchAdditional = true
        }
        if current > 0 && current >= currentItems.count {
            shouldFetchAdditional = true
        }
        if total > total_limit {
            return nil
        }

        if shouldFetchAdditional {
            do {
                try self.fetchNextItems(self.lastCreated)
            }
            catch {
            }
        }


        let item = self.currentItems[current]
        self.current += 1;
        self.total += 1;
        return item
    }
    
    func fetchNextItems(_ lastCreated : Int? = nil) throws {
        
        var filter = ""
        if let created = lastCreated {
            filter = "&before=\(String(created))"
        }
        else {
            filter = ""
        }

        let url = "https://api.pushshift.io/reddit/search/submission/?subreddit=\(self.subreddit)&sort=desc&sort_type=created_utc&size=500\(filter)"
        let result = try client.get(URI(string: url))
            .flatMapThrowing { res -> RedditWallpaperResponse in
                return try res.content.decode(RedditWallpaperResponse.self)
            }
            .map { res -> Array<RedditWallpaperItem> in
                return res.data
            }
            .unwrap {
                []
            }
            .wait()
    
        self.current = 0
        self.currentItems = result
        self.lastCreated = result.last?.created_utc ?? nil
    }
}

struct ImportRedditWallpaper : Command {
    struct Signature : CommandSignature { }
    
    let subreddit : String
    
    init(_ subreddit : String) {
        self.subreddit = subreddit
    }
    
    
    var help : String {
        "Imports wallpapers from r/wallpaper"
    }
    
    func run(using context: CommandContext, signature: Signature) throws {
        
        for var item in RedditSequence(self.subreddit, client: context.application.client) {
            item.id = "reddit/\(self.subreddit)/\(item.id)"
            
            try context.application.db.query(Wallpaper.self)
                .filter(\.$source_id == item.id)
                .first()
                .map { wallpaperM -> EventLoopFuture<Void> in
                    
                    let wallpaper = wallpaperM ?? Wallpaper(source_id: item.id, url: item.url, thumbnail_url: item.thumbnail, full_url: item.full_link)
                    
                    context.console.info("Writing item with source ID \(item.id)")
                    return wallpaper.save(on: context.application.db)
                }
        }
    }
}
