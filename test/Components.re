/**
 * The simplest component. Composes nothing!
 */
module Box = {
  open ReasonReact;
  let component = statefulNativeComponent("Box");
  let printState = x => x;
  let make = (~title="ImABox", ~onClick as _=?, _children) => {
    ...component,
    /* didUpdate: ({state} as oldSelf, _) =>
       RenderLog.add(
         RenderLog.global,
         ComponentDidUpdate("Box", printState(state))
       ), */
    initialState: () => title,
    willReceiveProps: (_) => title,
    printState,
    render: (_) => {
      children: ReasonReact.listToElement([]),
      make: () => Implementation.Text(title),
      updateInstance: (_) => ()
    }
  };
  let createElement = (~key=?, ~title=?, ~children as _children, ()) =>
    ReasonReact.element(~key?, make(~title?, ()));
};

module Text = {
  open ReasonReact;
  /**
   * FIXME: If a different prop is supplied as title, the change is not picked up by React.
   * It's because make returns a host element and there's no way to know if a Host element
   * is not changed.
   * */
  let component = statefulNativeComponent("Text");
  let make = (~title="ImABox", _children) => {
    ...component,
    initialState: () => title,
    willReceiveProps: (_) => title,
    printState: (_) => title,
    render: (_) => {
      children: listToElement([]),
      make: () => Implementation.Text(title),
      updateInstance: (_) => ()
    }
  };
  let createElement = (~key=?, ~title=?, ~children as _children, ()) =>
    element(~key?, make(~title?, ()));
};

let stringToElement = string => <Text title=string />;

module Div = {
  open ReasonReact;
  let component = statelessNativeComponent("Div");
  let make = children => {
    ...component,
    /* didUpdate: ({state}) =>
       RenderLog.add(RenderLog.global, ComponentDidUpdate("Div", "")), */
    render: (_) => {
      children: listToElement(children),
      make: () => Implementation.View,
      updateInstance: (_) => ()
    }
  };
  let createElement = (~key=?, ~children, ()) =>
    ReasonReact.element(~key?, make(children));
};

module BoxWrapper = {
  let component = ReasonReact.statelessComponent("BoxWrapper");
  let make =
      (~title="ImABox", ~twoBoxes=false, ~onClick as _=?, _children)
      : ReasonReact.component(
          ReasonReact.stateless,
          unit,
          ReasonReact.reactElement
        ) => {
    ...component,
    initialState: () => (),
    /* didUpdate: ({state}) =>
       RenderLog.add(renderLog, ComponentDidUpdate("BoxWrapper", "")), */
    render: _self =>
      twoBoxes ?
        <Div> <Box title /> <Box title /> </Div> : <Div> <Box title /> </Div>
  };
  let createElement = (~key=?, ~title=?, ~twoBoxes=?, ~children, ()) =>
    ReasonReact.element(~key?, make(~title?, ~twoBoxes?, ~onClick=(), ()));
};

/**
 * Box with dynamic keys.
 */
module BoxWithDynamicKeys = {
  let component =
    ReasonReact.statelessComponent(~useDynamicKey=true, "BoxWithDynamicKeys");
  let make = (~title="ImABox", _children: list(ReasonReact.reactElement)) => {
    ...component,
    /* didUpdate: ({state}) =>
       RenderLog.add(renderLog, ComponentDidUpdate("BoxWithDynamicKeys", "")), */
    render: _self => ReasonReact.listToElement([])
  };
  let createElement = (~title, ~children, ()) =>
    ReasonReact.element(make(~title, children));
};

module BoxList = {
  type action =
    | Create(string)
    | Reverse;
  let component = ReasonReact.reducerComponent("BoxList");
  let make = (~rAction, ~useDynamicKeys=false, _children) => {
    ...component,
    initialState: () => [],
    reducer: (action, state) =>
      switch action {
      | Create(title) =>
        ReasonReact.Update([
          useDynamicKeys ? <BoxWithDynamicKeys title /> : <Box title />,
          ...state
        ])
      | Reverse => ReasonReact.Update(List.rev(state))
      },
    /* didUpdate: ({state}) =>
       RenderLog.add(
         renderLog,
         ComponentDidUpdate("BoxList", printState(state))
       ), */
    /* printState, */
    render: ({state, act}) => {
      ReasonReact.RemoteAction.subscribe(~act, rAction);
      ReasonReact.listToElement(state);
    }
  };
  let createElement = (~rAction, ~useDynamicKeys=false, ~children, ()) =>
    ReasonReact.element(make(~rAction, ~useDynamicKeys, children));
};

/**
 * This component demonstrates several things:
 *
 * 1. Demonstration of making internal state hidden / abstract. Components
 * should encapsulate their state representation and should be free to change
 * it.
 *
 * 2. Demonstrates an equivalent of `componentWillReceiveProps`.
 * `componentWillReceiveProps` is like an "edge trigger" on props, and the
 * first item of the tuple shows how we implement that with this API.
 */
module ChangeCounter = {
  type state = {
    numChanges: int,
    mostRecentLabel: string
  };
  let printState = ({numChanges, mostRecentLabel}) =>
    "[" ++ string_of_int(numChanges) ++ ", " ++ mostRecentLabel ++ "]";
  let component = ReasonReact.reducerComponent("ChangeCounter");
  let make = (~label, _children) => {
    ...component,
    initialState: () => {mostRecentLabel: label, numChanges: 10},
    reducer: ((), state) =>
      ReasonReact.Update({...state, numChanges: state.numChanges + 1000}),
    willReceiveProps: ({state, reduce}) =>
      label != state.mostRecentLabel ?
        {
          print_endline("Will receive props");
          reduce(() => (), ());
          reduce(() => (), ());
          {mostRecentLabel: label, numChanges: state.numChanges + 1};
        } :
        state,
    /* didUpdate: ({state}) =>
       RenderLog.add(
         renderLog,
         ComponentDidUpdate("ChangeCounter", printState(state))
       ), */
    printState,
    render: ({state: {numChanges, mostRecentLabel}}) => ReasonReact.Flat([])
  };
  let createElement = (~label, ~children, ()) =>
    ReasonReact.element(make(~label, ()));
};

module StatelessButton = {
  let component = ReasonReact.statelessComponent("StatelessButton");
  let make =
      (~initialClickCount as _="noclicks", ~test as _="default", _children) => {
    ...component,
    /* didUpdate: ({state}) =>
       RenderLog.add(renderLog, ComponentDidUpdate("StatelessButton", "")), */
    render: _self => <Div />
  };
  let createElement = (~initialClickCount=?, ~test=?, ~children, ()) =>
    ReasonReact.element(make(~initialClickCount?, ~test?, ()));
};

module ButtonWrapper = {
  type state = {buttonWrapperState: int};
  let component = ReasonReact.statefulComponent("ButtonWrapper");
  let make = (~wrappedText="default", _children) => {
    ...component,
    initialState: () => {buttonWrapperState: 0},
    /* didUpdate: ({state}) =>
       RenderLog.add(renderLog, ComponentDidUpdate("ButtonWrapper", "")), */
    render: ({state}) =>
      <StatelessButton
        initialClickCount=("wrapped:" ++ wrappedText ++ ":wrapped")
      />
  };
  let createElement = (~wrappedText=?, ~children, ()) =>
    ReasonReact.element(make(~wrappedText?, ()));
};

module ButtonWrapperWrapper = {
  let buttonWrapperJsx = <ButtonWrapper wrappedText="TestButtonUpdated!!!" />;
  let component = ReasonReact.statefulComponent("ButtonWrapperWrapper");
  let make = (~wrappedText="default", _children) => {
    ...component,
    initialState: () => "buttonWrapperWrapperState",
    /* didUpdate: ({state}) =>
       RenderLog.add(renderLog, ComponentDidUpdate("ButtonWrapperWrapper", "")), */
    render: ({state}) =>
      <Div>
        (stringToElement(state))
        (stringToElement("wrappedText:" ++ wrappedText))
        buttonWrapperJsx
      </Div>
  };
  let createElement = (~wrappedText=?, ~children, ()) =>
    ReasonReact.element(make(~wrappedText?, ()));
};

module UpdateAlternateClicks = {
  type action =
    | Click;
  let printState = state => string_of_int(state);
  let component = ReasonReact.reducerComponent("UpdateAlternateClicks");
  let make = (~rAction, _children) => {
    ...component,
    initialState: () => 0,
    reducer: (Click, state) => Update(state + 1),
    shouldUpdate: ({newSelf: {state}}) => state mod 2 === 0,
    /* didUpdate: ({state}) =>
       RenderLog.add(
         renderLog,
         ComponentDidUpdate("ButtonWrapperWrapper", printState(state))
       ), */
    printState,
    render: ({state, act}) => {
      ReasonReact.RemoteAction.subscribe(~act, rAction);
      stringToElement(string_of_int(state));
    }
  };
  let createElement = (~rAction, ~children, ()) =>
    ReasonReact.element(make(~rAction, ()));
};
