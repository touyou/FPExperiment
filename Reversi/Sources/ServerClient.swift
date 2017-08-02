//
//  ServerClient.swift
//  Reversi
//
//  Created by 藤井陽介 on 2017/07/26.
//
//

import Foundation

protocol ConnectionDelegate {
    
    func didReceivedResponseData(_ response: String)
}

class Game: ConnectionDelegate {
    let reversi: Reversi!
    let server = ServerClient.shared
    
    init() {
    }
    
    init(addr: String, port: Int, name: String) {
        reversi = Reversi()
        server.setAddr(addr: addr, port: port)
        server.connect()
        server.send("OPEN \(name)\n")
    }
    
    func didReceivedResponseData(_ response: String) {
        
    }
}

class ServerClient: NSObject {
    
    static let shared = ServerClient()
    fileprivate var addr: CFString = "localhost" as CFString
    fileprivate var port: UInt32 = 8080
    var isConnected = false
    
    fileprivate var inputStream: InputStream!
    fileprivate var outputStream: OutputStream!
    fileprivate var inputQueue = Data()
    let BUFFER_MAX = 2048
    var delegate: ConnectionDelegate! = nil
    static let ERR_MSG = "Connection Failed"
    
    private override init() {}
    
    func setAddr(addr: String, port: Int) {
        self.addr = addr as CFString
        self.port = UInt32(port)
    }
    
    func connect() {
        print("Connecting...")
        var readStream: Unmanaged<CFReadStream>?
        var writeStream: Unmanaged<CFWriteStream>?
        
        CFStreamCreatePairWithSocketToHost(kCFAllocatorDefault, addr, port, &readStream, &writeStream)
        
        if inputStream != nil {
            inputStream.delegate = nil
            inputStream.close()
            inputStream.remove(from: .current, forMode: .defaultRunLoopMode)
        }
        
        if outputStream != nil {
            outputStream.delegate = nil
            outputStream.close()
            outputStream.remove(from: .current, forMode: .defaultRunLoopMode)
        }
        
        inputStream = readStream!.takeRetainedValue() as InputStream
        outputStream = writeStream!.takeRetainedValue() as OutputStream
        
        inputStream.delegate = self
        outputStream.delegate = self
        
        inputStream.schedule(in: .current, forMode: .defaultRunLoopMode)
        outputStream.schedule(in: .current, forMode: .defaultRunLoopMode)
        
        inputStream.open()
        outputStream.open()
    }
    
    func disConnect() {
        print("Disconnect.")
        inputStream.delegate = nil
        outputStream.delegate = nil
        
        inputStream.close()
        outputStream.delegate = nil
        
        inputStream.remove(from: .current, forMode: .defaultRunLoopMode)
        outputStream.remove(from: .current, forMode: .defaultRunLoopMode)
        
        isConnected = false
    }
    
    func send(_ msg: String) {
        if !isConnected {
            connect()
        }
        
        let request = msg.data(using: .utf8, allowLossyConversion: false)
        let requestLength = request!.count
        
        var timeout = 5 * 100000
        while !outputStream.hasSpaceAvailable {
            usleep(1000)
            timeout -= 100
            
            if timeout < 0 {
                print("time out")
                delegate.didReceivedResponseData(ServerClient.ERR_MSG)
                return
            } else if self.outputStream.streamError != nil {
                print("disconnect stream")
                delegate.didReceivedResponseData(ServerClient.ERR_MSG)
                return
            }
        }
        
        print("SEND.")
        _ = request?.withUnsafeBytes {
            outputStream.write($0, maxLength: requestLength)
        }
    }
    
    fileprivate func getResponse() {
        var buffer: UnsafeMutablePointer<UInt8>!
        var data = Data(capacity: BUFFER_MAX)
        data.withUnsafeMutableBytes { buffer = $0 }
        let length = inputStream.read(buffer, maxLength: BUFFER_MAX)
        
        guard length != -1 else {
            print("length: -1")
            return
        }
        
        let streamText = NSString(data: Data(bytes: buffer, count: length), encoding: String.Encoding.utf8.rawValue)
        
        guard delegate != nil else {
            return
        }
        
        // ここらへんは不安かも？
        delegate.didReceivedResponseData(streamText! as String)
    }
}

extension ServerClient: StreamDelegate {
    func stream(_ aStream: Stream, handle eventCode: Stream.Event) {
        if aStream === inputStream {
            switch eventCode {
            case Stream.Event.errorOccurred:
                print("input: Error Occurred: \(aStream.streamError.debugDescription)")
            case Stream.Event.openCompleted:
                print("input: Open Completed")
            case Stream.Event.hasBytesAvailable:
                print("input: Has Bytes Available")
                // getResponse()
            case Stream.Event.endEncountered:
                print("input: End Encountered")
            default:
                break
            }
        } else if aStream === outputStream {
            switch eventCode {
            case Stream.Event.errorOccurred:
                print("output: Error Occurred: \(aStream.streamError.debugDescription)")
            case Stream.Event.openCompleted:
                print("output: Open Completed")
            case Stream.Event.hasSpaceAvailable:
                print("output: Has Space Available")
                isConnected = true
            case Stream.Event.endEncountered:
                print("output: End Encountered")
                disConnect()
            default:
                break
            }
        }
    }
}
