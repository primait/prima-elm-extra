module PrimaElmExtra.Regex exposing
    ( plateNumber, email, mobileNumber
    , hasALowerCaseChar, hasANumericChar, hasASpecialChar, hasAUpperCaseChar
    , hasAtLeastSixChar, hasNonAlphanumeric, hasNonDigitChar, numbersOnly
    )

{-| Reusable regular expressions for common validation tasks.

This module collects patterns that are already compiled with
`Regex.fromString`, making it easier to centralize frequently used validation
rules and reuse them throughout an application.


# Identifiers and contacts

@docs plateNumber, email, mobileNumber


# Character constraints

@docs hasALowerCaseChar, hasANumericChar, hasASpecialChar, hasAUpperCaseChar


# Format constraints

@docs hasAtLeastSixChar, hasNonAlphanumeric, hasNonDigitChar, numbersOnly

-}

import Regex exposing (Regex)


{-| Regex for validating Italian vehicle license plates.

It includes several historical and current formats.

-}
plateNumber : Regex
plateNumber =
    "^((AG|AL|AN|AO|AR|AP|AT|AV|BA|BL|BN|BG|BI|BO|BZ|BS|BR|CA|CL|CB|CE|CT|CZ|CH|CO|CS|CR|KR|CN|EN|FE|FI|FG|FO|FR|GE|GO|GR|IM|IS|AQ|SP|LT|LE|LC|LI|LO|LU|MC|MN|MS|MT|ME|MI|MO|NA|NO|NU|OR|PD|PA|PR|PV|PG|PS|PE|PC|PI|PT|PN|PZ|PO|RG|RA|RC|RE|RI|RN|RM|RO|SA|SS|SV|SI|SR|SO|TA|TE|TR|TO|TP|TN|TV|TS|UD|VA|VE|VB|VC|VR|VV|VI|VT)[0-9]{6}|(MI|RM|TO|GE|BG|BS|CO|VA|PD|VR|TV|BO|MO|FI|NA|BA|PA|CT)[A-Z][0-9]{5}|(MI|RM|TO)[0-9]{5}[A-Z]|(MI|RM)[0-9][A-Z][0-9]{4}|MI[0-9]{2}[A-Z][0-9]{3})$|^([ABCDEFGHJKLMNPRSTVWXYZ]{2}[0-9]{3}[ABCDEFGHJKLMNPRSTVWXYZ]{2})$|^((AG|AL|AN|AO|AR|AP|AT|AV|BA|BL|BN|BG|BI|BO|BZ|BS|BR|CA|CL|CB|CE|CT|CZ|CH|CO|CS|CR|KR|CN|EN|FE|FI|FG|FO|FR|GE|GO|GR|IM|IS|AQ|SP|LT|LE|LC|LI|LO|LU|MC|MN|MS|MT|ME|MI|MO|NA|NO|NU|OR|PD|PA|PR|PV|PG|PS|PE|PC|PI|PT|PN|PZ|PO|RG|RA|RC|RE|RI|RN|RM|RO|SA|SS|SV|SI|SR|SO|TA|TE|TR|TO|TP|TN|TV|TS|UD|VA|VE|VB|VC|VR|VV|VI|VT)[0-9]{6}|[A-Z]{2}[0-9]{5}|[A-Z][0-9][A-Z]{4})$|^X[BCDFGHJKLMNPRSTVWXYZ2-9]{5}$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex for validating an email address in a common format.

It accepts an alphanumeric local part with a few common symbols and requires a
domain with an alphabetic TLD between 2 and 6 characters long.

-}
email : Regex
email =
    "^[a-zA-Z0-9_.+-]+(?:\\.[a-zA-Z0-9_.+-]+)*@(?:[a-zA-Z0-9-]+\\.)+[A-Za-z]{2,6}$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex for Italian mobile phone numbers.

It accepts the optional `+39` or `0039` prefix, followed by an Italian mobile
prefix and 6 or 7 trailing digits.

-}
mobileNumber : Regex
mobileNumber =
    "^(([+]|00)39)?((3[1-9][0-9]))(\\d{6,7})$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex that checks whether at least one special character is present.

The recognized symbols are the ones explicitly listed in the pattern.

-}
hasASpecialChar : Regex.Regex
hasASpecialChar =
    "[!@#$%^£€&*(),.?\":{}|<>]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex that matches at least one non-alphanumeric character.

This can be useful, for example, to detect spaces or symbols in a string.

-}
hasNonAlphanumeric : Regex.Regex
hasNonAlphanumeric =
    Regex.fromString "[^a-zA-Z\\d]"
        |> Maybe.withDefault Regex.never


{-| Regex that checks whether at least one numeric digit is present.
-}
hasANumericChar : Regex.Regex
hasANumericChar =
    "[0-9]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex that matches at least one character that is not a digit.
-}
hasNonDigitChar : Regex.Regex
hasNonDigitChar =
    "\\D"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex that checks whether at least one lowercase letter is present.
-}
hasALowerCaseChar : Regex.Regex
hasALowerCaseChar =
    "[a-z]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex that checks whether at least one uppercase letter is present.
-}
hasAUpperCaseChar : Regex.Regex
hasAUpperCaseChar =
    "[A-Z]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex to use with functions such as `Regex.replace` to remove everything
that is not numeric.

The pattern matches one or more occurrences of characters other than `0-9`.

-}
numbersOnly : Regex.Regex
numbersOnly =
    "[^0-9]+"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Regex that checks whether a string has at least six characters.
-}
hasAtLeastSixChar : Regex.Regex
hasAtLeastSixChar =
    ".{6,}"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never
