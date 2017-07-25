//
//  Reversi+BoardSystem.swift
//  Reversi
//
//  Created by 藤井陽介 on 2017/07/19.
//
//

import Foundation

// MARK: - Game Logic
extension Reversi {
    func isValidMove(board: Board, color: ReversiState, pos: Pos) -> Bool {
        return board[pos.1][pos.0] == .none ? isEffective(board: board, color: color, pos: pos) : false
    }
    
    func isEffective(board: Board, color: ReversiState, pos: Pos) -> Bool {
        return !flippableIndices(board: board, color: color, pos: pos).isEmpty
    }
    
    func flippableIndiceLine(board: Board, color: ReversiState, dir: Pos, next: Pos) -> [Pos] {
        let opColor = color.oppositeState()
        
        var checkLineRec: ((Pos, Pos, [Pos]) -> [Pos])!
        checkLineRec = { (dir, now, acc) -> [Pos] in
            if board[now.1][now.0] == opColor {
                return checkLineRec(dir, Pos(now.0 + dir.0, now.1 + dir.1), [now] + acc)
            } else if board[now.1][now.0] == color {
                return acc
            } else {
                return []
            }
        }
        let checkLine = { (dir: Pos, now: Pos, acc: [Pos]) -> [Pos] in
            if board[now.1][now.0] == opColor {
                return checkLineRec(dir, Pos(now.0 + dir.0, now.1 + dir.1), [now] + acc)
            } else {
                return []
            }
        }
        return checkLine(dir, next, [])
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
        return zip(1...8, 1...8).filter {
            isValidMove(board: board, color: color, pos: $0)
        }
    }
    
    func count(board: Board, color: ReversiState) -> Int {
        return board.reduce([], { $0 + $1 }).filter({ $0 == color }).count
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
