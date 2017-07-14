import Foundation

enum ReversiState: Int {
    case none
    case white
    case black
    case sentinel
    
    func oppositeState() -> ReversiState {
        return ReversiState(rawValue: (2 - self.rawValue) + 1)
    }
    
    func showState() -> String? {
        switch self {
        case .white:
            return "WHITE"
        case .black:
            return "BLACK"
        default:
            return nil
        }
    }
}

protocol BoardSystem {
    associatedtype State
    associatedtype Board

    func putC(s: State) -> String
    func putBoardLine()
    func putBoard(board: Board)
}

class Reversi {
    fileprivate var board: Board = [[State]]()
    
    init() {
        // initialize board
        (0...9).map { (y) -> () in
            board.append((0...9).map { x in
                if (x == 4 || x == 5) && (y == 4 || y == 5) {
                    return x == y ? .white : .black
                }
                return .sentinel
            })
        }
        
    }
}

extension Reversi: BoardSystem {
    typealias State = ReversiState
    typealias Board = [[State]]

    func putC(s: State) -> String {
        switch s {
        case .none:
            return " "
        case .white:
            return "O"
        case .black:
            return "X"
        }
    }
    
    func putBoard(board: Board) {
        print("|A B C D E F G H \n-+----------------\n", terminator: "")
        
    }
}
