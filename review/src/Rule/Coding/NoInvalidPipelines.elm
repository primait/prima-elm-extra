module Rule.Coding.NoInvalidPipelines exposing (rule)

import Review.Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes
import ReviewPipelineStyles.Predicates


removeSimpleLeftPizzas : ReviewPipelineStyles.PipelineRule ()
removeSimpleLeftPizzas =
    ReviewPipelineStyles.leftPizzaPipelines
        |> ReviewPipelineStyles.forbid
        |> ReviewPipelineStyles.that (ReviewPipelineStyles.Predicates.haveFewerStepsThan 2)
        |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToParentheticalApplication
        |> ReviewPipelineStyles.andCallThem "forbidden <| pipeline"


removeComplexLeftPizzas : ReviewPipelineStyles.PipelineRule ()
removeComplexLeftPizzas =
    ReviewPipelineStyles.leftPizzaPipelines
        |> ReviewPipelineStyles.forbid
        |> ReviewPipelineStyles.that (ReviewPipelineStyles.Predicates.haveMoreStepsThan 1)
        |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToRightPizza
        |> ReviewPipelineStyles.andCallThem "forbidden <| pipeline"


removeLeftComposition : ReviewPipelineStyles.PipelineRule ()
removeLeftComposition =
    ReviewPipelineStyles.leftCompositionPipelines
        |> ReviewPipelineStyles.forbid
        |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToRightComposition
        |> ReviewPipelineStyles.andCallThem "forbidden << application"


{-| Forbids invalid usage of pipeline


### Fail

        f <| value


### Success

        f value

---


### Fail

        f <| g <| value


### Success

        value
            |> g
            |> f

---


### Fail

        f << g << k


### Success

        k >> g >> f

-}
rule : Rule
rule =
    ReviewPipelineStyles.rule
        [ removeSimpleLeftPizzas
        , removeComplexLeftPizzas
        , removeLeftComposition
        ]
