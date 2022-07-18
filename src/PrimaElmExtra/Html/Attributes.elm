module PrimaElmExtra.Html.Attributes exposing
    ( autoCompleteBirthDateDay
    , autoCompleteBirthDateMonth
    , autoCompleteBirthDateYear
    , autoCompleteCurrentPassword
    , autoCompleteEmail
    , autoCompleteNewPassword
    , autoCompleteOneTimeCode
    , autoCompleteSex
    , autoCompleteTel
    , contentSquareOverrideId
    , dataTestId
    )

import VirtualDom


contentSquareOverrideId : String -> VirtualDom.Attribute msg
contentSquareOverrideId =
    VirtualDom.attribute "cs-override-id"


autoCompleteBirthDateDay : VirtualDom.Attribute msg
autoCompleteBirthDateDay =
    VirtualDom.attribute "autocomplete" "bday-day"


autoCompleteBirthDateMonth : VirtualDom.Attribute msg
autoCompleteBirthDateMonth =
    VirtualDom.attribute "autocomplete" "bday-month"


autoCompleteBirthDateYear : VirtualDom.Attribute msg
autoCompleteBirthDateYear =
    VirtualDom.attribute "autocomplete" "bday-year"


autoCompleteEmail : VirtualDom.Attribute msg
autoCompleteEmail =
    VirtualDom.attribute "autocomplete" "email"


autoCompleteNewPassword : VirtualDom.Attribute msg
autoCompleteNewPassword =
    VirtualDom.attribute "autocomplete" "new-password"


autoCompleteCurrentPassword : VirtualDom.Attribute msg
autoCompleteCurrentPassword =
    VirtualDom.attribute "autocomplete" "current-password"


autoCompleteOneTimeCode : VirtualDom.Attribute msg
autoCompleteOneTimeCode =
    VirtualDom.attribute "autocomplete" "one-time-code"


autoCompleteSex : VirtualDom.Attribute msg
autoCompleteSex =
    VirtualDom.attribute "autocomplete" "sex"


autoCompleteTel : VirtualDom.Attribute msg
autoCompleteTel =
    VirtualDom.attribute "autocomplete" "tel"


dataTestId : String -> VirtualDom.Attribute msg
dataTestId =
    VirtualDom.attribute "data-test-id"
