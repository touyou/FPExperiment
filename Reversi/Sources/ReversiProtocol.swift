//
//  ReversiProtocol.swift
//  Reversi
//
//  Created by 藤井陽介 on 2017/07/18.
//
//

import Foundation

enum ReversiState: Int {
    case none
    case white
    case black
    case sentinel
    
    func oppositeState() -> ReversiState {
        return ReversiState(rawValue: (2 - self.rawValue) + 1) ?? .sentinel
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

enum ComputerState {
    case giveUp
    case pass
    case move(Int, Int)
}

protocol BoardSystem {
    associatedtype State
    associatedtype Board
    associatedtype Pos
    
    // print board
    func putC(s: State) -> String
    func putBoardLine(line: [State])
    func putBoard(board: Board)
    
    // game logic
    func isValidMove(board: Board, color: ReversiState, pos: Pos) -> Bool
    func isEffective(board: Board, color: ReversiState, pos: Pos) -> Bool
    func flippableIndices(board: Board, color: ReversiState, pos: Pos) -> [Pos]
    func doMove(board: Board, com: ComputerState, color: ReversiState) -> Board
    func validMoves(board: Board, color: ReversiState) -> [Pos]
    func count(board: Board, color: ReversiState) -> Int
}
