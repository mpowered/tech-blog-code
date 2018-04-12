{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

-- This is a program for extracting a call graph from a Ruby call trace.
--
-- First produce the call trace by running `tracer.rb` (see instructions in that
-- file).
--
-- Then run this program; e.g. using Cabal:
--
--     cabal new-build
--     cabal new-run
--
-- (or the corresponding command for `stack`).
--
-- That should result in the file `graph.dot`. The graph can be rendered as
-- follows:
--
--     dot -T svg -o graph.svg graph.dot
--
-- If the call graph is very big, it may be preferred to import into a Neo4j
-- database instead of generating a picture. That can be done by generating CSV
-- files, which is done by the `main2` definition below.
--
-- Once the CSV files have been generated, they should be copiedto Neo4j's
-- import directory. After that, run the following in the Cypher shell:
--
--     MATCH (n) DETACH DELETE n;
--
--     LOAD CSV FROM "file:///nodes.csv" AS line
--     CREATE (:Binding {name: line[2], line: line[1], className: line[3], bid: line[0]});
--
--     LOAD CSV FROM "file:///dispatchers.csv" AS line
--     CREATE (:Dispatcher {name: line[2], line: line[1], className: line[3], bid: line[0]});
--
--     LOAD CSV FROM "file:///edges.csv" AS line
--     MATCH (caller {bid: line[0]})
--     MATCH (callee {bid: line[1]})
--     CREATE UNIQUE (caller)-[:Calls {contexts: line[2]}]->(callee);

module Main where

import Protolude

import Data.Char (toUpper)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text



data CallGraphError
  = MalformedEvent Text
  | MissingHeading
  | NonEmptyCallStackOnLastEvent [Binding]
  | EmptyCallStack
  | ReturnDoesNotMatchCall
      Binding -- Call
      Binding -- Return
  deriving (Show)

instance Exception CallGraphError



--------------------------------------------------------------------------------
-- * Bindings
--------------------------------------------------------------------------------

newtype Method = Method {unMethod :: Text}
  deriving (Eq, Show, IsString, Hashable)

newtype ClassName = ClassName {unClassName :: Text}
  deriving (Eq, Show, IsString, Hashable)

data Binding = Binding
  { line :: Int
  , method :: Method
  , className :: ClassName
  } deriving (Eq, Show, Generic)

instance Hashable Binding

sameBinding :: Binding -> Binding -> Bool
sameBinding b1 b2 = and
  [ method b1 == method b2
  , className b1 == className b2
  ]



--------------------------------------------------------------------------------
-- * Execution trace
--------------------------------------------------------------------------------

data EventType
  = Call
  | Return
  deriving (Eq, Show, Read)

data Event = Event
  { eventType :: EventType
  , binding :: Binding
  } deriving (Eq, Show)

parseEvent :: Text -> Maybe Event
parseEvent t = do
  [e, l, Method -> method, ClassName -> className] <-
    return $ Text.splitOn "," t
  eventType <-
    readMaybe . toS =<< fmap (uncurry Text.cons . first toUpper) (Text.uncons e)
  line <- readMaybe $ toS l
  let binding = Binding {..}
  return $ Event {..}

newtype Heading = Heading {unHeading :: Text}
  deriving (Eq, Show, IsString, Hashable)

groupSections :: [Text] -> Either CallGraphError [(Heading, [Text])]
groupSections [] = return []
groupSections (l:ls) = do
  e <- note MissingHeading $ Text.stripPrefix "#### " l
  let ls1 = takeWhile (not . ("#### " `Text.isPrefixOf`)) ls
      ls2 = dropWhile (not . ("#### " `Text.isPrefixOf`)) ls
  gs <- groupSections ls2
  return ((Heading e, ls1) : gs)

parseTrace :: Text -> Either CallGraphError [(Heading, [Event])]
parseTrace =
  traverse (traverse (traverse parseEvent')) <=< groupSections . Text.lines
  where
    parseEvent' t = note (MalformedEvent t) $ parseEvent t



--------------------------------------------------------------------------------
-- * Call graph
--------------------------------------------------------------------------------

type CallGraph = HashMap

insertNode ::
     (Eq n, Hashable n, Monoid e) => n -> CallGraph n e -> CallGraph n e
insertNode n = HM.insertWith mappend n mempty

insertEdge ::
     (Eq n, Hashable n, Eq e, Hashable e)
  => n
  -> e
  -> CallGraph n (HashSet e)
  -> CallGraph n (HashSet e)
insertEdge src dst = HM.insertWith HS.union src (HS.singleton dst)

traceToGraph ::
     [Event] -> Either CallGraphError (CallGraph Binding (HashSet Binding))
traceToGraph = go []
  where
    go [] [] = return HM.empty
    go stk [] = Left $ NonEmptyCallStackOnLastEvent stk
    go stk (Event {eventType = Call, ..}:es) = do
      let addEdge =
            case stk of
              top:_ -> insertEdge top binding
              [] -> identity
      addEdge . insertNode binding <$> go (binding : stk) es
    go stk (Event {eventType = Return, ..}:es) =
      case stk of
        [] -> Left EmptyCallStack
        top:s' ->
          if sameBinding top binding
            then go s' es
            else Left $ ReturnDoesNotMatchCall top binding

mergeGraphs ::
     (Eq n, Hashable n, Eq e, Hashable e, Eq a, Hashable a)
  => [(a, CallGraph n (HashSet e))]
  -> CallGraph n (HashMap e (HashSet a))
mergeGraphs gs =
  foldr
    (HM.unionWith (HM.unionWith HS.union))
    HM.empty
    [fmap (map (const $ HS.singleton t) . HS.toMap) g | (t, g) <- gs]

dispatchCalls ::
     CallGraph Binding (HashMap Binding (HashSet Heading))
  -> CallGraph Binding (HashMap Binding (HashSet Heading))
dispatchCalls = HM.fromList . concatMap (uncurry dispatch) . HM.toList
  where
    dispatch ::
         Binding
      -> HashMap Binding (HashSet Heading)
      -> [(Binding, HashMap Binding (HashSet Heading))]
    dispatch n calls =
      concat
        [ -- Insert the original node `n` with some calls altered
          [(n, HM.fromList $ map (uncurry mkCall) $ HM.toList callsByMethod)]
          -- Insert new dispatcher nodes
        , [ (dispBinding m, bs)
          | (m, bs) <- HM.toList callsByMethod
          , HM.size bs > 1
          ]
        ]
      where
        callsByMethod :: HashMap Method (HashMap Binding (HashSet Heading))
        callsByMethod =
          HM.fromListWith
            (HM.unionWith HS.union)
            [(method b, HM.singleton b ecs) | (b, ecs) <- HM.toList calls]

        -- Make a unique binding for dispatching a call from method `n` to `m`
        dispBinding m =
          Binding
            { line = line n
            , method = m
            , className =
                ClassName ("dispatcher_from_" <> (unMethod $ method n))
            }

        mkCall ::
             Method
          -> HashMap Binding (HashSet Heading)
          -> (Binding, HashSet Heading)
        mkCall m bs =
          case HM.toList bs of
            [becs] -> becs -- No dispatch when there's only a single choice
            becss -> (dispBinding m, HS.unions $ map snd becss)

parseTraceToGraph ::
     Text
  -> Either CallGraphError (CallGraph Binding (HashMap Binding (HashSet Heading)))
parseTraceToGraph =
  fmap (dispatchCalls . mergeGraphs) . traverse (traverse traceToGraph) <=<
  parseTrace

bindingID :: Binding -> Text
bindingID = show . abs . hash

dotNode :: Text -> Binding -> Text
dotNode attr n@Binding {..} = Text.concat
  [ "  "
  , bindingID n
  , " [label = \"{"
  , unMethod method
  , "|class: "
  , unClassName className
  , "\\lline: "
  , show line
  , "\\l}\""
  , attr
  , "]"
  ]

isDispatcher :: Binding -> Bool
isDispatcher Binding {..} =
  "dispatcher_from_" `Text.isPrefixOf` unClassName className

graphToDot :: CallGraph Binding (HashMap Binding (HashSet Heading)) -> Text
graphToDot g = Text.unlines $ concat
  [ [ "digraph G {"
    , "  fontname = \"Bitstream Vera Sans\""
    , "  fontsize = 8"
    , ""
    , "  node ["
    , "    fontname = \"Bitstream Vera Sans\""
    , "    fontsize = 8"
    , "    shape = \"record\""
    , "  ]"
    , ""
    , "  edge ["
    , "    fontname = \"Bitstream Vera Sans\""
    , "    fontsize = 8"
    , "  ]"
    ]
  , [""]
  , nodes
  , [""]
  , dispatchers
  , [""]
  , edges
  , ["}"]
  ]
  where
    red = ", style = rounded, color = red"
    nodes = [dotNode "" n | (n, _) <- HM.toList g, not $ isDispatcher n]
    dispatchers = [dotNode red n | (n, _) <- HM.toList g, isDispatcher n]
    edges =
      [ "  " <> bindingID n <> " -> " <> bindingID n' <> cxt
      | (n, es) <- HM.toList g
      , (n', cs) <- HM.toList es
      , let cxt = " [label = \"" <> Text.intercalate "\\l" (renderContexts cs) <> "\"]"
      ]
    renderContexts cs = map unHeading $ HS.toList cs

csvNode :: Binding -> Text
csvNode n@Binding {..} =
  Text.intercalate
    ","
    [ bindingID n
    , show line
    , show $ unMethod method
    , show $ unClassName className
    ]

nodesToCSV :: CallGraph Binding (HashMap Binding (HashSet Heading)) -> Text
nodesToCSV g =
  Text.unlines [csvNode n | (n, _) <- HM.toList g, not $ isDispatcher n]

dispatchersToCSV ::
     CallGraph Binding (HashMap Binding (HashSet Heading)) -> Text
dispatchersToCSV g =
  Text.unlines [csvNode n | (n, _) <- HM.toList g, isDispatcher n]

edgesToCSV :: CallGraph Binding (HashMap Binding (HashSet Heading)) -> Text
edgesToCSV g = Text.unlines
  [ Text.intercalate ","
    [ bindingID n
    , bindingID n'
    , "\"" <> Text.intercalate "," (renderContexts cs) <> "\""
    ]
  | (n, es) <- HM.toList g
  , (n', cs) <- HM.toList es
  ]
  where
    renderContexts cs = map unHeading $ HS.toList cs

exportCSVGraph :: CallGraph Binding (HashMap Binding (HashSet Heading)) -> IO ()
exportCSVGraph g = do
  writeFile "nodes.csv" $ nodesToCSV g
  writeFile "dispatchers.csv" $ dispatchersToCSV g
  writeFile "edges.csv" $ edgesToCSV g



--------------------------------------------------------------------------------
-- * Main
--------------------------------------------------------------------------------

-- Export call graph in the Dot format to stdout
main = do
  g <- either throwIO return . parseTraceToGraph =<< Text.getContents
  putText $ graphToDot g

-- Export call graph as CSV files
main2 = do
  g <- either throwIO return . parseTraceToGraph =<< Text.getContents
  exportCSVGraph g
