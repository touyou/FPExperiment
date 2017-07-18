//
//  Reversi.swift
//  Reversi
//
//  Created by 藤井陽介 on 2017/07/18.
//
//

import Foundation

// MARK: - Base Class
class Reversi {
    fileprivate var board: Board = [[State]]()
    fileprivate var dirs: [Pos] = [Pos]()
    
    init() {
        // initialize board
        _ = (0...9).map { (y) -> () in
            _ = board.append((0...9).map { x in
                if (x == 4 || x == 5) && (y == 4 || y == 5) {
                    return x == y ? .white : .black
                }
                return .sentinel
            })
        }
        // initialize direction
        _ = [1, 0, -1].map { (i) -> () in
            _ = [1, 0, -1].map { (j) -> () in
                guard i != j else { return }
                dirs.append(Pos(i, j))
            }
        }
    }
}

// MARK: - Game Logic
extension Reversi {
    func isValidMove(board: Board, color: ReversiState, pos: Pos) -> Bool {
        return board[pos.1][pos.0] == .none ? isEffective(board: board, color: color, pos: pos) : false
    }
    
    func isEffective(board: Board, color: ReversiState, pos: Pos) -> Bool {
        return !flippableIndices(board: board, color: color, pos: pos).isEmpty
    }
    
    func checkLine() {}
    
    func flippableIndiceLine(board: Board, color: ReversiState, dir: Pos, next: Pos) -> [Pos] {
        return []
    }
    
    func flippableIndices(board: Board, color: ReversiState, pos: Pos) -> [Pos] {
        return dirs.flatMap { (di, dj) in
            flippableIndiceLine(board: board, color: color, dir: Pos(di, dj), next: Pos(pos.0+di, pos.1+dj))
        }
    }
    
    func doMove(board: Board, com: ComputerState, color: ReversiState) -> Board {
        switch com {
        case .giveUp:
            return board
        case .pass:
            return board
        case let .move(i, j):
            let ms = flippableIndices(board: board, color: color, pos: Pos(i, j))
            return zip(board, 0...9).map { line, y in
                zip(line, 0...9).map { s, x in
                    if ms.contains(where: { (px, py) in px == x && py == y}) || (x == i && y == j) {
                        return color
                    } else {
                        return s
                    }
                }
            }
        }
    }
    
    func validMoves(board: Board, color: ReversiState) -> [Pos] {
        return []
    }
}

// MARK: - Utility
extension Reversi: BoardSystem {
    typealias State = ReversiState
    typealias Board = [[State]]
    typealias Pos = (Int, Int)
    
    func putC(s: State) -> String {
        switch s {
        case .none:
            return " "
        case .white:
            return "O"
        case .black:
            return "X"
        default:
            return " "
        }
    }
    
    func putBoardLine(line: [State]) {
        print(line.reduce("|") { $0 + putC(s: $1) + " " })
    }
    
    func putBoard(board: Board) {
        print("|A B C D E F G H ")
        print("-+----------------")
        _ = board.map { (line) -> () in
            putBoardLine(line: line)
        }
        print("  (X: Black,  O: White)")
    }
}
