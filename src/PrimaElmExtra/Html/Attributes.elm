module PrimaElmExtra.Html.Attributes exposing
    ( autoCompleteBirthDateDay, autoCompleteBirthDateMonth, autoCompleteBirthDateYear
    , autoCompleteEmail, autoCompleteNewPassword, autoCompleteCurrentPassword
    , autoCompleteOneTimeCode, autoCompleteSex, autoCompleteTel
    , contentSquareOverrideId, dataTestId
    )

{-| Convenience HTML attributes used across forms and UI components.

This module exposes preconfigured `VirtualDom.Attribute` values for common
`autocomplete` tokens and custom data attributes.


# Autocomplete attributes

@docs autoCompleteBirthDateDay, autoCompleteBirthDateMonth, autoCompleteBirthDateYear

@docs autoCompleteEmail, autoCompleteNewPassword, autoCompleteCurrentPassword

@docs autoCompleteOneTimeCode, autoCompleteSex, autoCompleteTel


# Custom attributes

@docs contentSquareOverrideId, dataTestId

-}

import VirtualDom


{-| Sets the `cs-override-id` attribute used by ContentSquare.
-}
contentSquareOverrideId : String -> VirtualDom.Attribute msg
contentSquareOverrideId =
    VirtualDom.attribute "cs-override-id"


{-| Sets `autocomplete="bday-day"`.
-}
autoCompleteBirthDateDay : VirtualDom.Attribute msg
autoCompleteBirthDateDay =
    VirtualDom.attribute "autocomplete" "bday-day"


{-| Sets `autocomplete="bday-month"`.
-}
autoCompleteBirthDateMonth : VirtualDom.Attribute msg
autoCompleteBirthDateMonth =
    VirtualDom.attribute "autocomplete" "bday-month"


{-| Sets `autocomplete="bday-year"`.
-}
autoCompleteBirthDateYear : VirtualDom.Attribute msg
autoCompleteBirthDateYear =
    VirtualDom.attribute "autocomplete" "bday-year"


{-| Sets `autocomplete="email"`.
-}
autoCompleteEmail : VirtualDom.Attribute msg
autoCompleteEmail =
    VirtualDom.attribute "autocomplete" "email"


{-| Sets `autocomplete="new-password"`.
-}
autoCompleteNewPassword : VirtualDom.Attribute msg
autoCompleteNewPassword =
    VirtualDom.attribute "autocomplete" "new-password"


{-| Sets `autocomplete="current-password"`.
-}
autoCompleteCurrentPassword : VirtualDom.Attribute msg
autoCompleteCurrentPassword =
    VirtualDom.attribute "autocomplete" "current-password"


{-| Sets `autocomplete="one-time-code"`.
-}
autoCompleteOneTimeCode : VirtualDom.Attribute msg
autoCompleteOneTimeCode =
    VirtualDom.attribute "autocomplete" "one-time-code"


{-| Sets `autocomplete="sex"`.
-}
autoCompleteSex : VirtualDom.Attribute msg
autoCompleteSex =
    VirtualDom.attribute "autocomplete" "sex"


{-| Sets `autocomplete="tel"`.
-}
autoCompleteTel : VirtualDom.Attribute msg
autoCompleteTel =
    VirtualDom.attribute "autocomplete" "tel"


{-| Sets a `data-test-id` attribute, useful for UI tests.
-}
dataTestId : String -> VirtualDom.Attribute msg
dataTestId =
    VirtualDom.attribute "data-test-id"
