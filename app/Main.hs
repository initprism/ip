module Main where

import Options.Applicative as Opt
import Control.Monad.Catch
import System.Exit

import Lookup.IPTypes
import Lookup.ParseIP
import Lookup.LookupIP

data Params = Params FilePath String

mkParams :: Opt.Parser Params
mkParams = Params
    <$> argument str (metavar "FILE" <> help "IP range database")
    <*> argument str (metavar "IP"   <> help "IP address to check")

run :: Params -> IO ()
run (Params fp ipstr) = do
    iprs <- parseIPRanges <$> readFile fp 
    case (iprs, parseIP ipstr) of 
        (_, Nothing) -> throwM $ InvalidIP ipstr
        (Left pe, _) -> throwM $ LoadIPRangesError pe 
        (Right iprdb, Just ip ) -> putStrLn $ reportIPs iprdb [ip]

main :: IO ()
main = (execParser opts >>= run)
       `catches` [Handler parseExit]

    where 
        opts = 
            info (mkParams <**> helper)
                 (fullDesc <> 
                  progDesc ("Answers YES/NO depedning on where " ++ 
                            "an IP address belongs to the IP range database"))
        parseExit :: ExitCode -> IO ()
        parseExit _ = pure ()