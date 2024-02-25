namespace p1eXu5.FSharp.Testing.TestBuilder

open p1eXu5.FSharp.Testing.ShouldExtensions
open FsUnit

type TestAggregate<'Event, 'State, 'Command, 'Error> =
    {
        Events: 'Event list
        State: 'State
        Commands: 'Command list
        Errors: 'Error list
        Assertions: (TestAggregate<'Event, 'State, 'Command, 'Error> -> unit) list
    }

module TestAggregate =

    let shouldNotBeErrors (errors: 'Error list) =
        errors |> should be Empty

    let runTests testAggregate =
        testAggregate.Assertions
        |> List.iter (fun f -> f testAggregate)

    let events testAggregate =
        testAggregate.Events

    let hasErrors testAggregate =
        testAggregate.Errors.Length > 0

[<AutoOpen>]
module TestBuilderCE =
    type OfSameCaseConstraint = CustomConstraints.OfSameCaseConstraint

    [<RequireQualifiedAccess>]
    type ReplaceEvents =
        | Yes
        | No

    type TestBuilder<'Event, 'State, 'Command, 'Error when 'State : equality and 'Event : equality >(
        initState: unit -> 'State,
        apply: 'Event list -> 'State -> 'State,
        evolve: 'State -> 'Command -> Result<'State * 'Event list, 'Error>)
        =
        let stateShouldBe (expected: 'State) (actual: 'State) =
            actual |> should equal expected

        member _.Yield(_) =
            {
                Events = []
                State = initState ()
                Commands = []
                Errors = []
                Assertions = []
            }

        [<CustomOperation("Given")>]
        member _.Given(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, state: 'State) =
            { aggregate with State = state }

        [<CustomOperation("Given")>]
        member _.Given(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, events: 'Event list) =
            { aggregate with
                Events = events
                State = aggregate.State |> apply events
            }

        [<CustomOperation("WhenMany")>]
        member this.WhenMany(
            aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>,
            commands: Choice<'Command, ('Command -> 'Command)> list,
            ?replaceEvents: bool)
            =
            let replaceEvents = defaultArg replaceEvents false
            commands
            |> List.fold (fun a -> function
                | Choice1Of2 c -> this.When(a, c, replaceEvents)
                | Choice2Of2 f -> this.When(a, f, replaceEvents)
            ) aggregate

        [<CustomOperation("When")>]
        member this.When(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, fromPreviousCommand: 'Command -> 'Command, ?replaceEvents: bool) =
            if aggregate.Commands.IsEmpty then
                invalidOp "No Commands"
            else
                let command = aggregate.Commands |> List.last |> fromPreviousCommand
                let replaceEvents = defaultArg replaceEvents false
                this.When(aggregate, command, replaceEvents)

        [<CustomOperation("When")>]
        member _.When(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, command: 'Command, ?replaceEvents: bool) =
            let replaceEvents = defaultArg replaceEvents false
            
            let result = command |> evolve aggregate.State

            match result with
            | Ok (newState, events) ->
                { aggregate with
                    Commands = aggregate.Commands @ [command]
                    State = newState
                    Events =
                        match replaceEvents with
                        | true -> events
                        | false -> aggregate.Events @ events
                }
            | Error error ->
                { aggregate with
                    Commands = aggregate.Commands @ [command]
                    Errors = aggregate.Errors @ [error]
                }

        [<CustomOperation("When")>]
        member this.When(
            aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>,
            command: Choice<'Command, ('Command -> 'Command)>,
            ?replaceEvents: bool)
            =
            let replaceEvents = defaultArg replaceEvents false
            command
            |> function
                | Choice1Of2 c -> this.When(aggregate, c, replaceEvents)
                | Choice2Of2 fromPreviousCommand -> this.When(aggregate, fromPreviousCommand, replaceEvents)

        [<CustomOperation("Then")>]
        member _.Then(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, f: 'State -> unit) =
            { aggregate with
                Assertions = aggregate.Assertions @ [ fun a -> f a.State ] }

        [<CustomOperation("Then")>]
        member _.Then(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, f: 'Error list -> unit) =
            { aggregate with
                Assertions = aggregate.Assertions @ [ fun a -> f a.Errors ] }

        [<CustomOperation("Then State is")>]
        member _.ThenStateIs(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, expectedState: 'State) =
            { aggregate with
                Assertions =
                    aggregate.Assertions
                    @ [ fun a -> a.State |> stateShouldBe expectedState ] }

        [<CustomOperation("and State is")>]
        member this.AndStateIs(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, expectedState: 'State) =
            this.ThenStateIs(aggregate, expectedState)

        [<CustomOperation("Then State is")>]
        member _.ThenStateIs(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, f: 'State -> unit) =
            { aggregate with
                Assertions =
                    aggregate.Assertions
                    @ [ fun a -> a.State |> f ] }

        [<CustomOperation("and State is")>]
        member this.AndStateIs(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, f: 'State -> unit) =
            this.ThenStateIs(aggregate, f)

        [<CustomOperation("and State is")>]
        member _.AndStateIs(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, expected: OfSameCaseConstraint) =
            { aggregate with
                Assertions =
                    aggregate.Assertions
                    @ [ fun a -> a.State |> should be expected ] }

        [<CustomOperation("and Events are")>]
        member _.AndEventsAre(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, expectedEvents: 'Event list) =
            { aggregate with
                Assertions =
                    aggregate.Assertions
                    @ [ fun a -> a.Events |> should equal expectedEvents ] }

        [<CustomOperation("and Events are")>]
        member _.AndEventsAre(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, expectedEvents: OfSameCaseConstraint list) =
            { aggregate with
                Assertions =
                    aggregate.Assertions
                    @ [ fun a ->
                        a.Events |> shouldL haveLength (expectedEvents.Length) "TestAggregate Event length assertion failed"
                        (a.Events, expectedEvents)
                        ||> List.zip
                        |> List.iter (fun (e, c) -> e |> should be c)
                    ] }

        /// <summary>
        /// Asserts last events. Event in <paramref name="expectedEvents" /> ordered as emitted.
        /// </summary>
        /// <param name="aggregate"></param>
        /// <param name="expectedEvents">First event in that list is last event.</param>
        [<CustomOperation("and last Events are")>]
        member _.AndLastEventsAre(aggregate: TestAggregate<'Event, 'State, 'Command, 'Error>, expectedEvents: OfSameCaseConstraint list) =
            { aggregate with
                Assertions =
                    aggregate.Assertions
                    @ [ fun a ->
                        a.Events.Length |> should greaterThan (expectedEvents.Length)
                        (a.Events |> List.rev |> List.take (expectedEvents.Length) |> List.rev, expectedEvents)
                        ||> List.zip
                        |> List.iter (fun (e, c) -> e |> should be c)
                    ] }

        member _.Delay(delayed) = delayed

        member _.Run(delayed) = delayed ()

    let createTestCE
        (initState: unit -> 'State)
        (apply: 'Event list -> 'State -> 'State)
        (evolve: 'State -> 'Command -> Result<'State * 'Event list, 'Error>)
        =
        TestBuilder(initState, apply, evolve)
