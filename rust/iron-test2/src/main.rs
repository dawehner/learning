extern crate iron;
extern crate time;
extern crate router;

use iron::prelude::*;
use iron::{BeforeMiddleware, AfterMiddleware, typemap};
use iron::status;
use router::{Router};
use time::precise_time_ns;

// use std::string::String;
// use std::collections::HashMap;

struct ResponseTime;

impl typemap::Key for ResponseTime { type Value = u64; }

/**
trait EntityType {
    fn link_collecions(&self) -> std::collections::HashMap<String, String>;
}

struct ExampleEntityType {
}

impl EntityType for ExampleEntityType {
    fn link_collections(&self) -> std::vec::Vec<String> {
        let mut collections = HashMap::new();

        collections.push("/node/:node", "canonical");

        return collections;
    }
}
*/

impl BeforeMiddleware for ResponseTime {
    fn before(&self, req: &mut Request) -> IronResult<()> {
        req.extensions.insert::<ResponseTime>(precise_time_ns());
        Ok(())
    }
}

impl AfterMiddleware for ResponseTime {
    fn after(&self, req: &mut Request, res: Response) -> IronResult<Response> {
        let delta = precise_time_ns() - *req.extensions.get::<ResponseTime>().unwrap();
        println!("Request took: {} ms", (delta as f64) / 1000000.0);
        Ok(res)
    }
}

fn hello_world(_: &mut Request) -> IronResult<Response> {
    Ok(Response::with((iron::status::Ok, "Hello World")))
}

 fn handler(req: &mut Request) -> IronResult<Response> {
    let ref query = req.extensions.find::<Router>().unwrap().find("query").unwrap_or("/");
    Ok(Response::with((status::Ok, *query)))
}

fn main() {
    let mut router = Router::new();
    router.get("/", handler);
    router.get("/:query", handler);
    router.get("/hello-world", hello_world);



    let mut chain = Chain::new(router);

    chain.link_before(ResponseTime);
    chain.link_after(ResponseTime);

    Iron::new(chain).http("localhost:3000").unwrap();
}
