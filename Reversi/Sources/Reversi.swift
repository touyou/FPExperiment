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
    internal var dirs: [Pos] = [Pos]()
    
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
        guard let ms = validMoves(board: board, color: color), ms != [] else {
            return .pass
        }
        
        return .pass
    }
}
