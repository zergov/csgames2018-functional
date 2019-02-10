import Data.Char

bytesToDigit :: String -> Int
bytesToDigit = fst . foldr go (0, 0)
  where
    go b (val, 0) = (val + (if b == '0' then 0 else 1), 1)
    go b (val, 1) = (val + (if b == '0' then 0 else 2), 2)
    go b (val, 2) = (val + (if b == '0' then 0 else 4), 3)
    go b (val, 3) = (val + (if b == '0' then 0 else 8), 4)
    go b (val, 4) = (val + (if b == '0' then 0 else 16), 5)
    go b (val, 5) = (val + (if b == '0' then 0 else 32), 6)
    go b (val, 6) = (val + (if b == '0' then 0 else 64), 7)
    go b (val, 7) = (val + (if b == '0' then 0 else 128), 8)

streamToBytes :: String -> [String]
streamToBytes [] = []
streamToBytes stream = [(take 8 stream)] ++ (streamToBytes (drop 8 stream))

streamToAscii :: String -> String
streamToAscii = map chr . filter predicate . map bytesToDigit . streamToBytes
  where predicate n = n >= 32 && n <= 126

binToType :: String -> String
binToType "00000000" = "hello"
binToType "00000001" = "name"
binToType "00000010" = "pass"
binToType "00000011" = "ack"
binToType "00000100" = "ping"
binToType "00000101" = "pong"

decode :: String -> String
decode s
  | stype == "name" = "type name --> data: " ++ (streamToAscii payload)
  | stype == "pass" = "type pass --> data: " ++ (streamToAscii payload)
  | stype == "ack" = "type message --> data: " ++ (streamToAscii payload)
  | stype == "hello" = "type hello --> name's data: " ++ (streamToAscii namedata) ++ "and pass's data: " ++ (streamToAscii namepass)
  -- | stype == "ping" = decodePing
  -- | stype == "pong" = decodePong
  where
    stype = binToType . take 8 $ s
    payload = drop 8 s
    namedata = take (length payload `div` 2) payload
    namepass = drop (length payload `div` 2) payload
