//
//  Reversi.swift
//  Reversi
//
//  Created by 藤井陽介 on 2017/07/18.
//
//

import Foundation

// MARK: - Base Class
final class Reversi {
    internal var board: Board = [[State]]()
    internal var dirs: [Pos] = []
    
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
    
    // MARK: - AI
    func play(board: Board, color: ReversiState) -> ComputerState {
        let ms: [Pos] = validMoves(board: board, color: color)
        guard ms.count != 0 else {
            return .pass
        }
        
        var bestMove = ComputerState.pass
        
        let _ = ms.reduce(Int.min) { (premin, now) in
            let cost = eval(board: board, color: color, pos: now)
            if premin < cost {
                bestMove = .move(now.0, now.1)
                return cost
            }
            return premin
        }
        
        return bestMove
    }
    
    func eval(board: Board, color: ReversiState, pos: Pos) -> Int {
        var cost = flippableIndices(board: board, color: color, pos: pos).count
        cost -= validMoves(board: doMove(board: board, com: .move(pos.0, pos.1), color: color), color: color.oppositeState()).count
        
        return cost
    }
}
