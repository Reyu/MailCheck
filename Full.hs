import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.IMAP.Types (MailboxName)
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)

main :: IO ()
main = do
    args <- getArgs
    boxes <- getUnreadBoxes
    if not (null args) && head args == "-v"
        then mapM_ (putStrLn . formatOutput) $ filter notEmpty boxes
        else print . sum $ map snd boxes
    where
        formatOutput (box,count) = box ++ ": " ++ show count
        notEmpty (_,count) = count /= 0

getAuth :: IO [String]
getAuth = do
    homeDir <- getHomeDirectory
    config <- readFile (homeDir ++ "/.mailauth")
    return $ words config

getUnreadBoxes :: IO [(MailboxName, Int)]
getUnreadBoxes = do
    (server:username:password:_) <- getAuth
    ic <- connectIMAPSSL server
    authenticate ic PLAIN username password
    allBoxes <- lsub ic
    mapM (getUnread ic . snd) (getSelectable allBoxes)
    where
        getSelectable = filter (notElem Noselect . fst)
        getUnread ic box = do
            select ic box
            messages <- search ic [NOTs (FLAG Seen)]
            return (box, length messages)
