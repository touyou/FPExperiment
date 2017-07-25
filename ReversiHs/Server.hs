{-# LANGUAGE BangPatterns #-}
module Main where 

import Network 
import System.Environment 
import System.Random 
import System.IO
import System.CPUTime

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar 

import qualified Data.Map as M 

import Data.List (intercalate)
import Data.Time 
import Data.Maybe 
import Color 
import Command 
import Play 

import System.Console.GetOpt

data Config = 
    Config { port :: PortNumber
           , timeout :: Int -- in milliseconds 
           , playersTime :: Int -- in milliseconds
           , byoyomi :: Int 
           , numberOfPlayers :: Int 
           , numberOfRounds  :: Int 
           , verbose :: Int 
           , helpMode :: Bool 
           , concurrency :: Int 
           }

instance Show Config where
    show conf =
        unlines ["Server Configuration:"
                , "\tListening:           " ++ show (port conf) 
                , "\tTime out:            " ++ show (fromIntegral (timeout conf) / 1000) ++ " s" 
                , "\tEach Player's Time:  " ++ show (fromIntegral (playersTime conf) / 1000) ++ " s" 
                , "\tByoyomi:             " ++ show (byoyomi conf) ++ " ms"
                , "\t#Players:            " ++ show (numberOfPlayers conf) 
                , "\t#Rounds:             " ++ show (numberOfRounds conf) 
                , "\tVerbosity:           " ++ show (verbose conf) 
                , "\tDig. of Concurrency: " ++ show (concurrency conf) ++ " (effective if #Players >= 4)"]


defaultConf = Config 3000 (60 * 1000) (10 * 60 * 1000) 500 2 4 0 False 2

options :: [OptDescr (Config -> Config)]
options =
    [ Option ['v'] ["verbose"]
             (OptArg (\m conf -> maybe (conf { verbose = 1 }) (\s -> conf {verbose = read s}) m) "LEVEL")
             "verbose mode"
    , Option ['p'] ["port"]
             (ReqArg (\s conf -> conf { port = fromIntegral (read s :: Int) }) "PORT")
             "port number"
    , Option ['t'] ["playerstime"]
             (ReqArg (\s conf -> conf { playersTime = max (read s * 60 * 1000) 0 }) "TIME") 
             "time assigned to each player (in second)"
    , Option [] ["timeout"]
             (ReqArg (\s conf -> conf { timeout = read s }) "TIME")
             "time out (in millisecond)"
    , Option ['T'] ["byoyomi"]
             (ReqArg (\s conf -> conf { byoyomi = read s }) "TIME")
             "byoyomi time (in millisecond)"
    , Option ['n'] ["players"]
             (ReqArg (\s conf -> conf { numberOfPlayers = max (read s) 2 }) "NUM")
             "number of players"
    , Option ['h','?'] ["help"]
             (NoArg (\conf -> conf { helpMode = True }))
             "show this help"
    , Option ['r'] ["rounds"]
             (ReqArg (\s conf -> conf { numberOfRounds = (max (read s) 1 + 1) `div` 2 * 2 }) "NUM")
             "number of rounds (multiple of 2)"
    , Option ['c'] ["concurrency"]
             (ReqArg (\s conf -> conf { concurrency = max 1 (read s) }) "NUM")
             "number of maximum concurrent matches (EXPERIMENTAL)"
    ]

usageMessage :: String 
usageMessage = usageInfo header options 
    where
      header =
          "Usage: \n" 
          ++ "    reversi-serv -p PORT ...\n" 

parseArg :: [String] -> IO (Config, [String])
parseArg args =
    case getOpt Permute options args of 
      (o, n, [])  -> return (foldl (flip ($)) defaultConf o, n)
      (_, _, err) -> ioError (userError (concat err ++ usageMessage))


hGetCommand :: Handle -> Int -> Config -> IO (Maybe Command)
hGetCommand h t conf = 
    do { b <- hWaitForInput h t 
       ; if b then  
             do r <- hGetLine h 
                when (verbose conf >= 1) (putStrLn $ "Received: " ++ r)
                return $ either (const Nothing) Just $ parseCommand r 
         else
             return Nothing                 
       }

hGetCommand' :: Handle -> Int -> Config -> IO (Maybe Command) 
hGetCommand' h t conf = 
    do { c <- hGetCommand h t conf 
       ; case c of 
           Just Empty -> hGetCommand' h t conf 
           _          -> return c }

hPutCommand :: Handle -> Command -> Config -> IO ()
hPutCommand h c conf = 
    do hPutStr h (show c)
       hPutStr h "\n"
       hFlush h 
       when (verbose conf >= 1) (putStrLn $ "Sent: " ++ show c) 
                        

isValidMv Pass board color = 
    fmap null $ validMoves board color

isValidMv (M i j) board color =
    isValidMove board color (i,j) 


roundRobin []     = []
roundRobin (x:xs) = map (\y -> (x,y)) xs ++ roundRobin xs 

-- Naive Scheduling at most n parallel
greedySchedule :: Eq a => Int -> [(a,a)] -> [[(a,a)]]
greedySchedule n xs = go [] [] [] xs 
    where
      go rs p []   []         = [rs]
      go rs p rest []         = rs : go [] [] [] rest 
      go rs p rest ((x,y):xs) 
          | length rs >= n = 
              rs : go [(x,y)] [x,y] [] (rest++xs)
          | x `elem` p || y `elem` p = 
              go rs p ((x,y):rest) xs 
          | otherwise = 
              go ((x,y):rs) (x:y:p) rest xs

type PlayerName  = String 
type PlayerInfo  = (Handle, PlayerName, HostName, PortNumber)

type Score = Int
type WinCount  = Int
type LoseCount = Int 

type Result = M.Map PlayerName (Score, WinCount, LoseCount)

winScore :: Score
winScore  =  1 

loseScore :: Score 
loseScore = -1

tieScore :: Score 
tieScore  =  0 

add3 (a,b,c) (x,y,z) = (a+x,b+y,c+z) 

server :: Config -> IO () 
server conf = 
    do socket <- listenOn (PortNumber (port conf))
       loop socket 
    where
      putWaitingMsg :: Int -> IO () 
      putWaitingMsg n 
          | n == 0 = return ()
          | n == 1 =
              putStrLn "Waiting 1 connection ..."
          | otherwise = 
              putStrLn $ "Waiting " ++ show n ++ " connections..."

      waitPlayers :: Socket -> Int -> IO [PlayerInfo]
      waitPlayers socket 0 = return []
      waitPlayers socket n = 
          do (ph1, cHost1, cPort1) <- accept socket 
             c <- hGetCommand' ph1 (timeout conf) conf 
             case c of 
                 Just (Open name) -> 
                     do putWaitingMsg (n-1)
                        plist <- waitPlayers socket (n-1)
                        return $ (ph1,name,cHost1,cPort1):plist 
                 _ -> 
                     waitPlayers socket n 

      loop :: Socket -> IO () 
      loop !socket =
          do putWaitingMsg (numberOfPlayers conf) 
             players <- waitPlayers socket (numberOfPlayers conf) 
             forkIO (startSessions players) 
             loop socket 

      -- 総当たり戦の実行
      startSessions :: [PlayerInfo] -> IO () 
      startSessions players =
          do let playerNames = map (\(_,n,_,_) -> n) players 
             putStrLnWithTime $ "Session started (" ++ intercalate ", " playerNames ++ ")."
             let matches = roundRobin players 
             let mss = greedySchedule (concurrency conf) matches 
             res <- concurrentMatch mss 
             mapM_ (\(h,n, _, _) -> 
                         do hPutCommand h (Bye $ M.toList res) conf 
                            hClose h) players 
             putStrLnWithTime $ "Session finished (" ++ intercalate ", "  playerNames ++ ")."
             

      putStrLnWithTime s = 
          do lt <- getZonedTime 
             putStrLn $ "[" ++ show lt ++ "] " ++ s 


      -- 並行に対戦
      concurrentMatch :: [[(PlayerInfo,PlayerInfo)]] -> IO Result 
      concurrentMatch []       = return M.empty 
      concurrentMatch (ms:mss) = 
          do mvs  <- mapM (const newEmptyMVar) ms
             rref <- newMVar M.empty 
             mapM_ (\(mvar,m) -> forkFinally (oneStep rref m)
                                             (\_ -> putMVar mvar ())) $ zip mvs ms 
             mapM_ takeMVar mvs  -- wait for child processes  
             r1 <- takeMVar rref 
             r2 <- concurrentMatch mss 
             return $ M.unionWith add3 r1 r2 
          where
            oneStep rref m = 
                do res <- startMatch m 
                   modifyMVar_ rref (return . M.unionWith add3 res)


      -- 対戦を先手後手を入れ替えつつcount回行う
      startMatch :: (PlayerInfo,  PlayerInfo) -> IO Result
      startMatch ((ph1, name1, _, _), (ph2, name2, _, _)) =
          do putStrLnWithTime $ name1 ++ " vs. " ++ name2 ++ " started. "
             k <- getStdRandom $ randomR (0, 1)
             let [(h1,n1),(h2,n2)] = 
                       map ([(ph1,name1), (ph2,name2)]!!) [k,1-k]
             startMatches (n1,h1) (n2,h2) (numberOfRounds conf) 

      -- 上の補助関数
      startMatches :: (PlayerName,Handle) -> (PlayerName,Handle) -> Int -> IO Result
      startMatches (n1,h1) (n2,h2) count =
          if count > 0 then 
              do board <- initBoard 
                 let t = playersTime conf 
                 hPutCommand h1 (Start black n2 t) conf 
                 hPutCommand h2 (Start white n1 t) conf 
                 rr1 <- interact (n1,h1,t) (n2,h2,t) black board False 
                 rr2 <- startMatches (n2,h2) (n1,h1) (count - 1) 
                 return $ M.unionWith add3 rr1 rr2 
          else
              return M.empty 
              

      -- 実際の対戦処理
      interact :: (PlayerName,Handle,Int) -> (PlayerName,Handle,Int) -> Color -> Board -> Bool -> IO Result
      interact (n1,h1,t1) (n2,h2,t2) color board passFlag =
          do { startTime <- getCurrentTime 
             ; command   <- hGetCommand' h1 (timeout conf) conf 
             ; endTime   <- getCurrentTime 
             ; let diff = round (1000 * realToFrac(endTime `diffUTCTime` startTime)) 
             ; when (verbose conf >= 1) (putStrLn $ "Passed: " ++ show diff ++ " ms")
             ; let t1' = t1 - diff 
             ; if (t1' <= 0) && (diff > byoyomi conf) then 
                   suddenEnd (n1,h1,t1) (n2,h2,t2) color board "TIME_UP"
               else
                   case command of 
                     Just (Move m) ->
                         do { isValid <- isValidMv m board color 
                            ; if not isValid then 
                                  suddenEnd (n1,h1,t1) (n2,h2,t2) color board "ILLEGAL_MOVE"
                              else
                                  case m of 
                                    Pass | passFlag -> 
                                         do { count1 <- count board color
                                            ; count2 <- count board (op color)
                                            ; let (r1,r2,rr) 
                                                   | count1 == count2 = 
                                                       (Tie,Tie, M.fromList [(n1,tieResult),(n2,tieResult)])
                                                   | count1 >  count2 = 
                                                       (Win,Lose,M.fromList [(n1,winResult),(n2,loseResult)])
                                                   | count1 <  count2 = 
                                                       (Lose,Win,M.fromList [(n1,loseResult),(n2,winResult)])
                                            ; hPutCommand h1 (End r1 count1 count2 "DOUBLE_PASS") conf 
                                            ; hPutCommand h2 (End r2 count2 count1 "DOUBLE_PASS") conf 
                                            ; return rr }
                                    _ -> 
                                        do { board <- doMove board m color
                                           ; hPutCommand h1 (Ack t1') conf 
                                           ; hPutCommand h2 (Move m) conf 
                                           ; interact (n2,h2,t2) (n1,h1,t1') (op color) board (case m of { Pass -> True; _ -> False }) }}
                     
                     _ -> 
                         suddenEnd (n1,h1,t1) (n2,h2,t2) color board "ILLEGAL_COMMAND"
             }

      -- 変なコマンドが送られてきたときの処理
      suddenEnd (n1,h1,t1) (n2,h2,t2) color board desc =
          do count1 <- count board color 
             count2 <- count board (op color) 
             hPutCommand h1 (End Lose count1 count2 desc) conf
             hPutCommand h2 (End Win  count2 count1 desc) conf 
             return $ M.fromList [ (n1, loseResult), (n2, winResult) ]
          
      op = oppositeColor 

      winResult  = (winScore,1,0)
      loseResult = (loseScore,0,1)
      tieResult  = (tieScore,0,0)
 
      -- parallelMatchmodify mss = 
      --     do { counter <- newMVar 0 
      --        ; go counter mss }
      --     where
      --       go counter [] = return M.empty 
      --       go counter (ms:mss) =
      --           do { modifyMVar_ counter (\_ -> return $ length ms)
      --              ; rref <- newMVar M.empty 
      --              ; mapM_ (oneStep counter rref) ms
      --              ; r1 <- checkAfterZero counter (readMVar rref) 
      --              ; r2 <- go counter mss 
      --              ; return $ M.unionWith (+) r1 r2 }
      --           where
      --             checkAfterZero counter action =
      --                 do { c <- readMVar counter 
      --                    ; if c == 0 then 
      --                          action
      --                      else 
      --                          do { threadDelay (100 * 1000) 
      --                             ; checkAfterZero counter action }}
      --             oneStep counter rref m = forkIO $ 
      --                do { res <- startMatch m 
      --                   ; modifyMVar_ rref (return . M.unionWith (+) res)
      --                   ; modifyMVar_ counter (\x -> return $ x - 1) }
                                                                  


main :: IO ()                                 
main = withSocketsDo $ 
       do args <- getArgs 
          (conf,rest) <- parseArg args
          if helpMode conf then 
              putStrLn usageMessage 
          else
              do print conf 
                 server conf
