module App

open Elmish
open Elmish.React
open Feliz

[<RequireQualifiedAccess>]
type Page =
  | Counter
  | TextInput

type State =
  { Count: int
    InputText: string
    IsUpperCase: bool
    CurrentPage : Page }

type Msg =
  | Increment
  | Decrement
  | InputTextChanged of string
  | UppercaseToggled of bool
  | SwitchPage of Page

let init() =
  { Count = 0
    InputText = ""
    IsUpperCase = false
    CurrentPage = Page.Counter }

let update (msg: Msg) (state: State): State =
  match msg with
  | Increment -> { state with Count = state.Count + 1 }
  | Decrement -> { state with Count = state.Count - 1 }
  | InputTextChanged text -> { state with InputText = text }
  | UppercaseToggled upperCase -> { state with IsUpperCase = upperCase }
  | SwitchPage page -> { state with CurrentPage = page }

let divider = Html.div [ prop.style [ style.margin 10 ] ]

let render (state: State) (dispatch: Msg -> unit) =
  match state.CurrentPage with
  | Page.Counter ->
      Html.div [
        Html.button [
          prop.text "Show Text Input"
          prop.onClick (fun _ -> dispatch (SwitchPage Page.TextInput))
        ]

        divider

        Html.button [
          prop.onClick (fun _ -> dispatch Increment)
          prop.text "Increment"
        ]

        Html.button [
          prop.onClick (fun _ -> dispatch Decrement)
          prop.text "Decrement"
        ]

        Html.h1 state.Count
      ]

  | Page.TextInput ->
      Html.div [
        Html.button [
          prop.text "Show counter"
          prop.onClick (fun _ -> dispatch (SwitchPage Page.Counter))
        ]

        divider

        Html.input [
          prop.valueOrDefault state.InputText
          prop.onChange (InputTextChanged >> dispatch)
        ]

        divider

        Html.input [
          prop.id "uppercase-checkbox"
          prop.type'.checkbox
          prop.isChecked state.IsUpperCase
          prop.onChange (UppercaseToggled >> dispatch)
        ]

        Html.label [
          prop.htmlFor "uppercase-checkbox"
          prop.text "Uppercase"
        ]

        Html.h3 (if state.IsUpperCase then state.InputText.ToUpper() else state.InputText)
      ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run