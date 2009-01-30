module Scion.Types.ProtocolDaemonInstance where
-- the scion daemon parses the editor request.
-- if the daemon can't handle the request it serializes the request using
-- these datatyes and hands in over to the scion isntance.

-- using the most simple serialization: Read, Show

data ScionInstanceCommand = 
    SICInstanceInfo -- TODO 
    | SICOpenProject  -- TODO 
        ProjectInfo

    | SICListSupportedLanguages -- TODO
    | SICListSupportedPragmas -- TODO
    | SICListSupportedFlags -- TODO
    | SICListRdrNamesInScope -- TODO
        FilePath -- names in scope of which file? 
        (Maybe String) -- not yet saved edited file contents 
    | SICListExposedModules -- TODO
    | SICSetGHCVerbosity -- TODO
    | SICBackgroundTypecheckFile -- TODO
        FilePath -- names in scope of which file? 
    | SICForceUnload -- TODO
    | SICAddCmdLineFlag -- TODO
    | SICThingAtPoint -- TODO
        FilePath -- file 
        Int -- line 
        Int -- col 
    | SICThingAtPointWithType -- TODO experimental
        FilePath -- file 
        Int -- line 
        Int -- col 
    | SICModuleCompletion -- TODO
    | SICDumpSources -- TODO
        FilePath

    -- only valid for cabal projects 
    | SICCabalLoadComponent  -- TODO
        CabalComponent
    | SICListCabalTargets -- TODO

    -- only valid for single file projects: 
    | SICCompile -- TODO write executable

    -- only used to force an exception to test error recovery 
    | SICTestError -- TODO
    | SICTestKill-- TODO
  deriving (Show, Read)

--  
data ScionInstanceReply =
    SIRInstanceInfo Int -- pid
    | SIROpenCabalProject
    | SIRLoadComponent
    | SIRListSupportedLanguages
    | SIRListSupportedPragmas
    | SIRListSupportedFlags
    | SIRListRdrNamesInScope
    | SIRListExposedModules
    | SIRSetGHCVerbosity
    | SIRBackgroundTypecheckFile
    | SIRForceUnload
    | SIRAddSIRLineFlag
    | SIRThingAtPoint
    | SIRModuleCompletion
    | SIRThingAtPointMoreInfo
    | SIRDumpSources
    | SIRListCabalTargets
  deriving (Show, Read)
