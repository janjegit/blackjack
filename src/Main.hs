module Main where

import Data.List (splitAt)
import Data.Maybe (fromJust)
import System.Random

data Player = Guest | Dealer
  deriving Show

type Winner = Player
data Outcome = Draw | Outcome Winner

data Card = Card Rank Suit

instance Eq Card where
  (==) (Card rankA suitA) (Card rankB suitB) = 
    rankA == rankB && suitA == suitB  

instance Show Card where
  show (Card r s) = toEnum (fromJust (lookup s suitMap) + offset r) : " "
    where
      suitMap = [ (Spade,   127137)
                , (Heart,   127153)
                , (Diamond, 127169)
                , (Club,    127185) ]
      offset (NumberCard n) = n - 1
      offset Jack  = 10
      offset Queen = 12
      offset King  = 13
      offset Ace   = 0 

pretty :: Hand -> String
pretty []           = ""
pretty (card:cards) = (show card) ++ (pretty cards)

instance Show Outcome where
  show Draw        = "Draw"
  show (Outcome w) = show w

data Suit = Heart | Diamond | Club | Spade 
  deriving (Show, Eq)

data Rank = NumberCard Int | Jack | King | Queen | Ace
  deriving Show

type Deck = [Card]
type Hand = [Card]

instance Eq Rank where
  (==) (NumberCard a) (NumberCard b) = a == b
  (==) Ace   Ace   = True
  (==) King  King  = True
  (==) Queen Queen = True
  (==) Jack  Jack  = True
  (==) _     _     = False

fullDeck :: Hand
fullDeck = [ Card r s | r <- ranks, s <- [Heart, Diamond, Club, Spade] ]
  where ranks = map NumberCard [2 .. 10] ++ [Jack, King, Queen, Ace]

numOfAces :: Hand -> Int
numOfAces = foldr ((+) . isAce) 0
  where 
    isAce (Card Ace _) = 1
    isAce (Card _   _) = 0

getCardValue :: Card -> Int
getCardValue card = case card of
  (Card (NumberCard n) _) -> n
  (Card  Ace           _) -> 11
  (Card  _             _) -> 10

getValue :: Hand -> Int
getValue hand | total > 21 = total - (10 * numOfAces hand)
              | otherwise = total
  where total = foldr ((+) . getCardValue) 0 hand

shuffle :: [a] -> IO [a]
shuffle xs = do newStdGen >>= return . (go xs [])
  where
    go [] acc _ = acc 
    go ls acc g = let (i, g') = randomR (0, length ls - 1) g
                      (lft, rht)  = splitAt i ls
                  in case rht of 
                    []     -> go lft acc g'
                    (y:ys) -> go (lft ++ ys) (y:acc) g'

drawCardToHand :: Deck -> Hand -> (Maybe Deck, Hand)
drawCardToHand []     hand = (Nothing, hand)
drawCardToHand (c:cs) hand = (Just cs, c:hand)

-- Rules

isOver21 :: Hand -> Bool
isOver21 = (<) 21 . getValue

chooseWinner :: Hand -> Hand -> Outcome
chooseWinner guest dealer | isOver21 guest && isOver21 dealer = Draw
                          | isOver21 guest                    = dealerWins
                          | isOver21 dealer                   = guestWins
                          | totalGuest <= totalDealer         = dealerWins
                          | otherwise                         = guestWins
  where 
    totalGuest  = getValue guest
    totalDealer = getValue dealer 
    dealerWins  = Outcome Dealer
    guestWins   = Outcome Guest 
chooseWinner _ _ = error "chooseWinner: Decks not valid."  

playDealer :: Deck -> Hand
playDealer deck = go deck []
  where
    go d h | getValue h >= 16 = h
           | otherwise        = let (d',h') = drawCardToHand d h
                                in case d' of
                                  Nothing      -> h'
                                  Just newDeck -> go newDeck h'

yesNoQuestion :: String -> IO Bool
yesNoQuestion msg = 
  do
    putStr msg
    c <- getChar
    case c of
      'y' -> return True
      'n' -> return False
      _   -> do 
              putStrLn "\nUnknown action. Try again."
              yesNoQuestion msg


main :: IO ()
main = do 
       putStrLn "Let's play Blackjack!"
       shuffle fullDeck >>= flip gameLoop []
       return ()

endGame :: IO ()
endGame = do
  again <- yesNoQuestion "Play again? (y)es | (n)o: "
  putStrLn " "
  if again 
    then shuffle fullDeck >>= flip gameLoop [] 
    else return ()

gameLoop :: Deck -> Hand -> IO ()
gameLoop deck guestHand = do
  putStrLn ("Your cards: " ++ (pretty guestHand) 
    ++ " : " ++ (show (getValue guestHand)))
  if isOver21 guestHand
    then do putStrLn "Dealer wins."
            endGame
    else do
      result <- yesNoQuestion "Draw card? (y)es | (n)o: "
      putStrLn " "
      if result
        then do
          let (Just deck', guestHand') = drawCardToHand deck guestHand
          gameLoop deck' guestHand'
        else do
          let dealerDeck  = playDealer   deck
          let winner      = chooseWinner guestHand dealerDeck
          let dealerScore = getValue     dealerDeck
          putStrLn ("Dealer's cards: " ++ (pretty dealerDeck) 
            ++ " : " ++ (show dealerScore))
          case winner of
            Draw           -> putStrLn "Draw."
            Outcome Dealer -> putStrLn "Dealer wins."
            Outcome Guest  -> putStrLn "Guest wins."
          endGame

