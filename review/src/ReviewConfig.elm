module ReviewConfig exposing (config)

import CognitiveComplexity
import Docs.NoMissing
import Docs.ReviewAtDocs
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Modules
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes
import ReviewPipelineStyles.Predicates
import Rule.Coding.NoBoolPatternMatching as NoBoolPatternMatching
import Rule.Coding.NoCaseOfNothingToNothing as NoCaseOfNothingToNothing
import Rule.Coding.NoFunctionInLet as NoFunctionInLet
import Rule.Coding.NoInvalidPipelines as NoInvalidPipelines
import Rule.Coding.NoPipeRecordGetter as NoPipeRecordGetter
import Rule.Coding.NoSingleFieldRecord as NoSingleFieldRecord
import Rule.Coding.NoSingleLineRecords as NoSingleLineRecord
import Simplify


config : List Rule
config =
    [ NoExposingEverything.rule
    , NoImportingEverything.rule [ "Test" ]
    , NoMissingTypeAnnotation.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Modules.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule

    --
    , Simplify.rule Simplify.defaults

    --
    , Docs.NoMissing.rule
        { document = Docs.NoMissing.onlyExposed
        , from = Docs.NoMissing.allModules
        }
        |> Review.Rule.ignoreErrorsForDirectories [ "tests" ]
    , Docs.ReviewAtDocs.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests" ]

    --
    , CognitiveComplexity.rule 4

    --
    , NoInvalidPipelines.rule
    , NoBoolPatternMatching.rule
    , NoCaseOfNothingToNothing.rule
    , NoFunctionInLet.rule
    , NoPipeRecordGetter.rule
    , NoSingleFieldRecord.rule
    , NoSingleLineRecord.rule
    ]
