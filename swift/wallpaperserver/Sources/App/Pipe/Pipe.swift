//
//  File.swift
//  
//
//  Created by Daniel Wehner on 21/11/2020.
//

import Foundation

protocol PipeStep {
    associatedtype T
    associatedtype S

    func process(item : T, callable : (S) -> Bool) -> Bool
}

class FilterStep<T> : PipeStep {
    
    var filters : [(T) -> Bool] = [];
    
    func add(callable : @escaping (T) -> Bool) -> FilterStep {
        self.filters.append(callable)
        return self
    }
    
    func process(item: T, callable : (T) -> Bool) -> Bool {
        for filter in filters {
            if !filter(item) {
                return false
            }
        }
        
        return callable(item);
    }
}

struct ConverterStep<T, S> : PipeStep {
    var converter : (T) -> (S);
    
    
    
    func process(item: T, callable: (S) -> Bool) -> Bool {
        return callable(converter(item))
    }
}

//class StepAggregator<T, R: Sequence, P : PipeStep, W : StepWriter> {
//    var reader : R;
//    
//    var steps : Dictionary<Int, Array<P>>
//    
//    var writers : Array<W>
//    
//    init(reader : R) {
//        self.reader = reader
//        self.steps = Dictionary.init()
//        self.writers = []
//    }
//    
//    func addStep(step: P, priority : Int?) {
//        let prio = priority ?? 0
//        
//        if self.steps[prio] != nil {
//            self.steps[prio]?.append(step)
//        }
//        else {
//            self.steps[prio] = []
//            self.steps[prio]?.append(step)
//        }
//    }
//    
//    func addWriter(writer : W) {
//        self.writers.append(writer)
//    }
//    
//    func process() {
//        
//        for writer in self.writers {
//            writer.prepare()
//        }
//        
//        for item in self.reader {
//            
//        }
//        
//        for writer in self.writers {
//            writer.finish()
//        }
//    }
//    
//    func sortedSteps() -> Array<P> {
//        var sortedSteps : Array<P> = []
//        let keys = Array(steps.keys)
//        let sortedKeys = keys.sorted()
//        
//        for key in sortedKeys {
//            if let innerSortedSteps = self.steps[key] {
//                for step in innerSortedSteps {
//                    sortedSteps.append(step)
//                }
//            }
//        }
//
//        return sortedSteps
//    }
//    
//    func buildPipeline() -> ((_: T) -> Bool) {
//
//        let sortedSteps = self.sortedSteps()
//        
//        let finalCallable = {(item: T) -> Bool in
//            return true
//        }
//
//        var nextCallable = finalCallable;
//        
//        for step in sortedSteps {
//            nextCallable : ((_: T) -> Bool ) = {(item: T) in
//                return step.process(item: T, callable: nextCallable)
//            }
//        }
//
//        return nextCallable
//    }
//}


protocol StepWriter {
    associatedtype S

    func prepare()

    func writeItem(items : Array<S>)

    func finish()
}
