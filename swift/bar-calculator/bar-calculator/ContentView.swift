//
//  ContentView.swift
//  bar-calculator
//
//  Created by Daniel Wehner on 09/05/2020.
//  Copyright © 2020 Daniel Wehner. All rights reserved.
//

import SwiftUI

class CalculatorModel : ObservableObject {
    enum Op {
        case one;
        case two;
        case three;
        case four;
        case five;
        case six;
        case seven;
        case eight;
        case nine;
        case plus;
        case minus;
    }
    
    @Published var state : [Op] = [];
    
   func eval() -> Int {
        var value = 0;
        
        return value;
    }
}

struct ContentView: View {
    @ObservedObject private var model : CalculatorModel = CalculatorModel.init();

    var body: some View {
        Text("Hello, World!")
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .focusable()
            .touchBar {
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.one);
                    print("1")
                }) {
                    Text("1")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.two);
                    print("2")
                }) {
                    Text("2")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.three);
                    print("3")
                }) {
                    Text("3")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.four);
                    print("4")
                }) {
                    Text("4")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.five);
                    print("5")
                }) {
                    Text("5")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.six);
                    print("6")
                }) {
                    Text("6")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.seven);
                    print("7")
                }) {
                    Text("7")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.eight);
                    print("8")
                }) {
                    Text("8")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.nine);
                    print("9")
                }) {
                    Text("9")
                }
                Button(action: {
                    self.model.state.append(CalculatorModel.Op.plus);
                    print("+")
                }) {
                    Text("+")
                }
        }
    }


}


struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
