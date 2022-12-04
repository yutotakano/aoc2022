{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
module Runner
    ( mkRun
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | @mkRun@ creates a function body that pattern matches on two integer inputs,
-- the day number and the part number, and calls the matching function if it is
-- implemented. The search domain is the imported module list in the module where
-- @mkRun@ is called.
mkRun :: ExpQ
mkRun = do
    -- Get the imported modules for this module
    (ModuleInfo importedModules) <- thisModule >>= reifyModule
    -- Extract the string module names
    let moduleNames = map (\(Module _pkgName (ModName modName)) -> modName) importedModules

    -- Find only the modules that start with Day. We assume those contain
    -- part1 and part2 functions.
    let implementedDays = map (\x -> (x, read $ drop 3 x :: Integer))
            $ filter ((== "Day") . (take 3)) moduleNames

    -- Create variables for pattern matching on
    day <- newName "day"
    part <- newName "part"

    -- Create a lambda function taking in a day and a part integer, and
    -- pattern match on its tuple representation for each implemented day and
    -- each part.
    lamE [varP day, varP part] $ caseE (tupE [varE day, varE part])
        $ flip concatMap implementedDays
        $ \(moduleName, implementDay) ->
            [ match
                (tupP [litP $ integerL implementDay, litP $ integerL 1])
                (normalB $ varE $ mkName $ moduleName <> "." <> "part1")
                []
            , match
                (tupP [litP $ integerL implementDay, litP $ integerL 2])
                (normalB $ varE $ mkName $ moduleName <> "." <> "part2")
                []
            ]
