module ReviewConfig exposing (config)

import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Modules
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoExposingEverything.rule
    , NoImportingEverything.rule [ "Test" ]
    , NoMissingTypeAnnotation.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Modules.rule
    , NoUnused.Variables.rule
    ]
