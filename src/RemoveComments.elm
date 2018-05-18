module RemoveComments exposing (removeComments)

import Regex

removeComments : String -> String
removeComments =
    Regex.replace Regex.All (Regex.regex "--.*") (always "")
        >> Regex.replace Regex.All (Regex.regex "{-[\\s\\S]*-}") (always "")
        >> Regex.replace Regex.All (Regex.regex "\n +\\w+ : .*") (always "")
