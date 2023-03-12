module CmdArgs(getCmd, Cmd(..)) where
    
data Cmd = 
        CmdGrmFiles [String] -- a.grm b.grm c.grm ...
    |   CmdGrmWithOption (Maybe String) String String String 
            -- a.grm prod_rules.txt action_table.txt goto_table.txt
    |   CmdShowItems [String] -- a.grm b.grm c.grm ...
    |   CmdError String -- error message

getCmd :: [String] -> Cmd
getCmd args = 
    let cmd = collectInfo args 
    in  case cmd of
            CmdGrmWithOption Nothing _ _ _ -> 
                CmdError $ "No grammar file is given for -output"
            _ -> cmd

collectInfo :: [String] -> Cmd
collectInfo ("-output":ss) = 
    if length ss < 3 then
        CmdError $ "Specify three file names after -output: " ++ show ss
    else 
        let prod_rules : action_table : goto_table : ss' = ss
            cmd = collectInfo ss' 
        in  case cmd of
                CmdGrmFiles [] ->
                    CmdGrmWithOption Nothing prod_rules action_table goto_table
                CmdGrmFiles [fileName] -> 
                    CmdGrmWithOption (Just fileName) prod_rules action_table goto_table
                CmdGrmFiles _ -> 
                    CmdError $ "Only one grammar file can be applied with -output"
                CmdShowItems _ -> 
                    CmdError $ "Mixed use of -output with -show-items is not allowed"
                CmdGrmWithOption _ _ _ _ -> 
                    CmdError $ "Only one use of -output is allowed"
                CmdError msg -> cmd

collectInfo ("-show-items":ss) =
    let cmd = collectInfo ss in
        case cmd of
            CmdGrmFiles [] ->
                CmdShowItems []
            CmdGrmFiles [fileName] -> 
                CmdShowItems [fileName]
            CmdGrmFiles fileNames -> 
                CmdShowItems fileNames
            CmdShowItems _ -> 
                    CmdError $ "Only one use of -show-items is allowed"
            CmdGrmWithOption _ _ _ _ -> 
                CmdError $ "Mixed use of -show-items with -output is not allowed"
            CmdError msg -> cmd

collectInfo (s:ss) = 
    let cmd = collectInfo ss
    in  case cmd of
            CmdGrmFiles fileNames -> CmdGrmFiles (s:fileNames)
            CmdShowItems fileNames -> CmdShowItems (s:fileNames)
            CmdGrmWithOption Nothing f1 f2 f3 -> 
                CmdGrmWithOption (Just s) f1 f2 f3
            CmdGrmWithOption (Just _) _ _ _ -> 
                CmdError $ "Only one grammar file can be applied with -output"
            CmdError msg -> cmd

collectInfo [] = CmdGrmFiles []