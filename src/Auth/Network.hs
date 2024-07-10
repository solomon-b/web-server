module Auth.Network where

--------------------------------------------------------------------------------

import Network.IP.Addr
import Network.Socket

--------------------------------------------------------------------------------

sockAddrToNetAddr :: SockAddr -> Maybe (NetAddr IP)
sockAddrToNetAddr (SockAddrInet _ host) = Just . fromNetAddr46 . IPv4 $ net4Addr (uncurry4 ip4FromOctets $ hostAddressToTuple host) 0
  where
    uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
    uncurry4 f (a, b, c, d) = f a b c d
sockAddrToNetAddr (SockAddrInet6 _ _ host _) = Just . fromNetAddr46 . IPv6 $ net6Addr (uncurry8 ip6FromWords $ hostAddress6ToTuple host) 0
  where
    uncurry8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> (a, b, c, d, e, f, g, h) -> i
    uncurry8 op (a, b, c, d, e, f, g, h) = op a b c d e f g h
sockAddrToNetAddr (SockAddrUnix _) = Nothing
