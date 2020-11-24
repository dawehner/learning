@testable import App
import XCTVapor

final class AppTests: XCTestCase {
    func testHelloWorld() throws {
        let app = Application(.testing)
        defer { app.shutdown() }
        try configure(app)

        try app.test(.GET, "hello", afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            XCTAssertEqual(res.body.string, "Hello, world!")
        })
    }
    
}

final class PipeTests : XCTestCase {
    func testPipeFilterTrue() throws {
        let item : Array<Int> = []
        let filterPipe = FilterStep<Array<Int>>()
        filterPipe.add(callable: { (item : Array<Int>) -> Bool in
                       return true
        })
        
        let expectation = XCTestExpectation(description: "Run process function after filter.")
        
        XCTAssertTrue(filterPipe.process(item: item, callable: {(item) -> Bool in
            XCTAssertNotNil(item)
            expectation.fulfill()
            return true
        }))
        
    }
    
    func testPipeFilterFalse() throws {
        let item : Array<Int> = []
        let filterPipe = FilterStep<Array<Int>>()
        filterPipe.add(callable: { (_ : Array<Int>) -> Bool in
                       return false
        })
        
        XCTAssertFalse(filterPipe.process(item: item, callable: {(item) -> Bool in
            return true
        }))
    }
    
    func testConverterStep() throws {
        let item : Array<Int> = []
        let converterPipe = ConverterStep(converter: {(_ : Array<Int>) -> Array<String> in ["test"]});

        let expectation = XCTestExpectation(description: "Run process converter function")

        converterPipe.process(item: item, callable: {(item2 : Array<String>) in
            XCTAssertEqual(["test"], item2);
            expectation.fulfill()
            return true
        })
    }
}
