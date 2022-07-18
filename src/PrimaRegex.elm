module PrimaRegex exposing
    ( email
    , hasALowerCaseChar
    , hasANumericChar
    , hasASpecialChar
    , hasAUpperCaseChar
    , hasAtLeastSixChar
    , hasNonAlphanumeric
    , hasNonDigitChar
    , mobileNumber
    , numbersOnly
    , plateNumber
    )

import Regex exposing (Regex)


plateNumber : Regex
plateNumber =
    "^((AG|AL|AN|AO|AR|AP|AT|AV|BA|BL|BN|BG|BI|BO|BZ|BS|BR|CA|CL|CB|CE|CT|CZ|CH|CO|CS|CR|KR|CN|EN|FE|FI|FG|FO|FR|GE|GO|GR|IM|IS|AQ|SP|LT|LE|LC|LI|LO|LU|MC|MN|MS|MT|ME|MI|MO|NA|NO|NU|OR|PD|PA|PR|PV|PG|PS|PE|PC|PI|PT|PN|PZ|PO|RG|RA|RC|RE|RI|RN|RM|RO|SA|SS|SV|SI|SR|SO|TA|TE|TR|TO|TP|TN|TV|TS|UD|VA|VE|VB|VC|VR|VV|VI|VT)[0-9]{6}|(MI|RM|TO|GE|BG|BS|CO|VA|PD|VR|TV|BO|MO|FI|NA|BA|PA|CT)[A-Z][0-9]{5}|(MI|RM|TO)[0-9]{5}[A-Z]|(MI|RM)[0-9][A-Z][0-9]{4}|MI[0-9]{2}[A-Z][0-9]{3})$|^([ABCDEFGHJKLMNPRSTVWXYZ]{2}[0-9]{3}[ABCDEFGHJKLMNPRSTVWXYZ]{2})$|^((AG|AL|AN|AO|AR|AP|AT|AV|BA|BL|BN|BG|BI|BO|BZ|BS|BR|CA|CL|CB|CE|CT|CZ|CH|CO|CS|CR|KR|CN|EN|FE|FI|FG|FO|FR|GE|GO|GR|IM|IS|AQ|SP|LT|LE|LC|LI|LO|LU|MC|MN|MS|MT|ME|MI|MO|NA|NO|NU|OR|PD|PA|PR|PV|PG|PS|PE|PC|PI|PT|PN|PZ|PO|RG|RA|RC|RE|RI|RN|RM|RO|SA|SS|SV|SI|SR|SO|TA|TE|TR|TO|TP|TN|TV|TS|UD|VA|VE|VB|VC|VR|VV|VI|VT)[0-9]{6}|[A-Z]{2}[0-9]{5}|[A-Z][0-9][A-Z]{4})$|^X[BCDFGHJKLMNPRSTVWXYZ2-9]{5}$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


email : Regex
email =
    "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


mobileNumber : Regex
mobileNumber =
    "^(([+]|00)39)?((3[1-9][0-9]))(\\d{6,7})$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


hasASpecialChar : Regex.Regex
hasASpecialChar =
    "[!@#$%^£€&*(),.?\":{}|<>]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


hasNonAlphanumeric : Regex.Regex
hasNonAlphanumeric =
    Regex.fromString "[^a-zA-Z\\d]"
        |> Maybe.withDefault Regex.never


hasANumericChar : Regex.Regex
hasANumericChar =
    "[0-9]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


hasNonDigitChar : Regex.Regex
hasNonDigitChar =
    "\\D"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


hasALowerCaseChar : Regex.Regex
hasALowerCaseChar =
    "[a-z]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


hasAUpperCaseChar : Regex.Regex
hasAUpperCaseChar =
    "[A-Z]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


numbersOnly : Regex.Regex
numbersOnly =
    "[^0-9]+"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


hasAtLeastSixChar : Regex.Regex
hasAtLeastSixChar =
    ".{6,}"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never
