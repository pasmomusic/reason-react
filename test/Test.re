open React_ios;

module ReasonReact = {
  module Implementation = {
    let map = Hashtbl.create(1000);
    type hostView =
      | Text(string)
      | View;
    let getInstance = (id) =>
      if (Hashtbl.mem(map, id)) {
        Some(Hashtbl.find(map, id))
      } else {
        None
      };
  };
  include ReactCore_Internal.Make(Implementation);
  module Text = {
    let component = statelessNativeComponent("Text");
    let make = (~title="ImABox", ~onClick as _=?, _children) => {
      ...component,
      printState: (_) => title,
      render: (_) => {
        setProps: (_) => (),
        children: listToElement([]),
        style: Layout.defaultStyle,
        make: (id) => {
          let elem = Implementation.Text(title);
          Hashtbl.add(Implementation.map, id, elem);
          elem
        }
      }
    };
    let createElement = (~key=?, ~title=?, ~children as _children, ()) =>
      element(~key?, make(~title?, ()));
  };
  let stringToElement = (string) => <Text title=string />;
};

module TestRenderer = {
  open ReasonReact;
  type opaqueComponent =
    | Component(componentSpec('a, 'b, 'c, 'd)): opaqueComponent
    | InstanceAndComponent(component('a, 'b, 'c), instance('a, 'b, 'c)): opaqueComponent;
  let compareComponents = false;
  type testInstance = {
    component: opaqueComponent,
    id: Key.t,
    subtree: t,
    state: string
  }
  and t = list(testInstance);
  let rec convertInstance =
    fun
    | Instance({component, id, instanceSubTree, iState} as instance) => {
        component: InstanceAndComponent(component, instance),
        id,
        subtree: convertElement(instanceSubTree),
        state: component.printState(iState)
      }
    | NativeInstance(_, {component, id, instanceSubTree, iState} as instance) => {
        component: InstanceAndComponent(component, instance),
        id,
        subtree: convertElement(instanceSubTree),
        state: component.printState(iState)
      }
  and convertElement =
    fun
    | IFlat(instances) => List.map(convertInstance, instances)
    | INested(_, elements) => List.flatten(List.map(convertElement, elements));
  let render = (element) => convertElement(RenderedElement.render(element));
  let update = (element, next) => {
    let (nextElement, updateLog) = RenderedElement.update(element, next);
    nextElement
  };
  let compareComponents = (left, right) =>
    switch (left, right) {
    | (Component(_), Component(_))
    | (InstanceAndComponent(_, _), InstanceAndComponent(_, _)) => assert false
    | (Component(justComponent), InstanceAndComponent(comp, instance)) =>
      comp.handedOffInstance := Some(instance);
      let result =
        switch justComponent.handedOffInstance^ {
        | Some(_) => true
        | None => false
        };
      comp.handedOffInstance := None;
      result
    | (InstanceAndComponent(comp, instance), Component(justComponent)) =>
      comp.handedOffInstance := Some(instance);
      let result =
        switch justComponent.handedOffInstance^ {
        | Some(_) => true
        | None => false
        };
      comp.handedOffInstance := None;
      result
    };
  let rec compareElement = (left, right) =>
    switch (left, right) {
    | (le, re) =>
      if (List.length(le) != List.length(re)) {
        false
      } else {
        List.fold_left(
          (&&),
          true,
          List.map(compareInstance, List.combine(le, re))
        )
      }
    }
  and compareInstance = ((left, right)) =>
    left.id == right.id
    && left.state == right.state
    && compareComponents(left.component, right.component)
    && compareElement(left.subtree, right.subtree);
  let printList = (indent, lst) => {
    let indent = String.make(indent, ' ');
    "["
    ++ String.concat(",\n", List.map((s) => s, lst))
    ++ "\n"
    ++ indent
    ++ "]"
  };
  let componentName = (component) =>
    switch component {
    | InstanceAndComponent(component, _) => component.debugName
    | Component(component) => component.debugName
    };
  let printElement = (formatter) => {
    let rec pp = () => Fmt.brackets(Fmt.list(~sep=Fmt.comma, printInstance()))
    and printInstance = () =>
      Fmt.braces(
        Fmt.hvbox(
          (formatter, instance) =>
            Fmt.pf(
              formatter,
              "@,id: %s,@ name: %s,@ state: %s,@ subtree: %a@,",
              string_of_int(instance.id),
              componentName(instance.component),
              instance.state,
              pp(),
              instance.subtree
            )
        )
      );
    Fmt.pf(formatter, "%a", pp())
  };
};


/***
 * The simplest component. Composes nothing!
 */
module Box = {
  open ReasonReact;
  let component = statelessNativeComponent("Box");
  let make = (~title="ImABox", ~onClick as _=?, _children) => {
    ...component,
    printState: (_) => title,
    render: (_) => {
      setProps: (_) => (),
      children: ReasonReact.listToElement([]),
      style: Layout.defaultStyle,
      make: (id) => {
        let elem = Implementation.Text(title);
        Hashtbl.add(Implementation.map, id, elem);
        elem
      }
    }
  };
  let createElement = (~key=?, ~title=?, ~children as _children, ()) =>
    ReasonReact.element(~key?, make(~title?, ()));
};

module Div = {
  open ReasonReact;
  let component = statelessNativeComponent("Div");
  let make = (children) => {
    ...component,
    render: (_) => {
      setProps: (_) => (),
      children: listToElement(children),
      style: Layout.defaultStyle,
      make: (id) => {
        let elem = Implementation.View;
        Hashtbl.add(Implementation.map, id, elem);
        elem
      }
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
    render: (_self) =>
      twoBoxes ?
        <Div> <Box title /> <Box title /> </Div> : <Div> <Box title /> </Div>
  };
  let createElement = (~key=?, ~title=?, ~twoBoxes=?, ~children, ()) =>
    ReasonReact.element(~key?, make(~title?, ~twoBoxes?, ~onClick=(), ()));
};


/***
 * Box with dynamic keys.
 */
module BoxWithDynamicKeys = {
  let component =
    ReasonReact.statelessComponent(~useDynamicKey=true, "BoxWithDynamicKeys");
  let make = (~title="ImABox", _children: list(ReasonReact.reactElement)) => {
    ...component,
    printState: (_) => title,
    render: (_self) => ReasonReact.listToElement([])
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
    render: ({state, act}) => {
      ReasonReact.RemoteAction.subscribe(~act, rAction);
      ReasonReact.listToElement(state)
    }
  };
  let createElement = (~rAction, ~useDynamicKeys=false, ~children, ()) =>
    ReasonReact.element(make(~rAction, ~useDynamicKeys, children));
};


/***
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
          {mostRecentLabel: label, numChanges: state.numChanges + 1}
        } :
        state,
    render: ({state: {numChanges, mostRecentLabel}}) => ReasonReact.Flat([]),
    printState: ({numChanges, mostRecentLabel}) =>
      "[" ++ string_of_int(numChanges) ++ ", " ++ mostRecentLabel ++ "]"
  };
  let createElement = (~label, ~children, ()) =>
    ReasonReact.element(make(~label, ()));
};

module StatelessButton = {
  let component = ReasonReact.statelessComponent("StatelessButton");
  let make =
      (~initialClickCount as _="noclicks", ~test as _="default", _children) => {
    ...component,
    render: (_self) => <Div />
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
    render: ({state}) =>
      <StatelessButton
        initialClickCount=("wrapped:" ++ (wrappedText ++ ":wrapped"))
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
    render: ({state}) =>
      <Div>
        (ReasonReact.stringToElement(state))
        (ReasonReact.stringToElement("wrappedText:" ++ wrappedText))
        buttonWrapperJsx
      </Div>
  };
  let createElement = (~wrappedText=?, ~children, ()) =>
    ReasonReact.element(make(~wrappedText?, ()));
};

module UpdateAlternateClicks = {
  type action =
    | Click;
  let component = ReasonReact.reducerComponent("UpdateAlternateClicks");
  let make = (~rAction, _children) => {
    ...component,
    initialState: () => 0,
    printState: (state) => string_of_int(state),
    reducer: (Click, state) => Update(state + 1),
    shouldUpdate: ({newSelf: {state}}) => state mod 2 === 0,
    render: ({state, act}) => {
      ReasonReact.RemoteAction.subscribe(~act, rAction);
      ReasonReact.stringToElement(string_of_int(state))
    }
  };
  let createElement = (~rAction, ~children, ()) =>
    ReasonReact.element(make(~rAction, ()));
};

let renderedElement =
  Alcotest.testable(
    (formatter, t) => TestRenderer.printElement(formatter, t),
    TestRenderer.compareElement
  );

let suite = [
  (
    "First Render",
    `Quick,
    () => {
      open TestRenderer;
      ReasonReact.GlobalState.reset();
      let component = BoxWrapper.make();
      let rendered = render(ReasonReact.element(component));
      let expected = [
        {
          component: Component(BoxWrapper.component),
          id: 1,
          state: "",
          subtree: [
            {
              component: Component(Div.component),
              id: 2,
              state: "",
              subtree: [
                {
                  state: "ImABox",
                  component: Component(Box.component),
                  id: 3,
                  subtree: []
                }
              ]
            }
          ]
        }
      ];
      Alcotest.check(renderedElement, "", expected, rendered)
    }
  ),
  (
    "Top level update",
    `Quick,
    () => {
      open TestRenderer;
      ReasonReact.GlobalState.reset();
      let component = BoxWrapper.component;
      let rendered =
        update(
          ReasonReact.RenderedElement.render(<BoxWrapper />),
          <BoxWrapper twoBoxes=true />
        );
      let expected = [
        {
          component: Component(component),
          id: 1,
          state: "",
          subtree: [
            {
              component: Component(Div.component),
              id: 2,
              state: "",
              subtree: [
                {
                  state: "ImABox",
                  component: Component(Box.component),
                  id: 4,
                  subtree: []
                },
                {
                  state: "ImABox",
                  component: Component(Box.component),
                  id: 5,
                  subtree: []
                }
              ]
            }
          ]
        }
      ];
      Alcotest.check(
        renderedElement,
        "",
        expected,
        TestRenderer.convertElement(rendered)
      )
    }
  ),
  (
    "Change counter test",
    `Quick,
    () => {
      ReasonReact.GlobalState.reset();
      let rendered0 =
        ReasonReact.RenderedElement.render(
          <ChangeCounter label="defaultText" />
        );
      TestRenderer.convertElement(rendered0)
      |> Alcotest.check(
           renderedElement,
           "",
           [
             {
               component:
                 Component(ChangeCounter.make(~label="defaultText", ())),
               id: 1,
               state: "[10, defaultText]",
               subtree: []
             }
           ]
         );
      let rendered1 =
        TestRenderer.update(rendered0, <ChangeCounter label="defaultText" />);
      TestRenderer.convertElement(rendered1)
      |> Alcotest.check(
           renderedElement,
           "",
           [
             {
               component:
                 Component(ChangeCounter.make(~label="defaultText", ())),
               id: 1,
               state: "[10, defaultText]",
               subtree: []
             }
           ]
         );
      let rendered2 =
        TestRenderer.update(rendered1, <ChangeCounter label="updatedText" />);
      TestRenderer.convertElement(rendered2)
      |> Alcotest.check(
           renderedElement,
           "",
           [
             {
               component:
                 Component(ChangeCounter.make(~label="defaultText", ())),
               id: 1,
               state: "[11, updatedText]",
               subtree: []
             }
           ]
         );
      let (rendered2f, _) =
        ReasonReact.RenderedElement.flushPendingUpdates(rendered2);
      TestRenderer.convertElement(rendered2f)
      |> Alcotest.check(
           renderedElement,
           "",
           [
             {
               component:
                 Component(ChangeCounter.make(~label="defaultText", ())),
               id: 1,
               state: "[2011, updatedText]",
               subtree: []
             }
           ]
         );
      let (rendered2f_mem, _) =
        ReasonReact.RenderedElement.flushPendingUpdates(rendered2f);
      Alcotest.(
        check(bool, "it is memoized", rendered2f_mem === rendered2f, true)
      );
      let (rendered2f_mem, _) =
        ReasonReact.RenderedElement.flushPendingUpdates(rendered2f_mem);
      Alcotest.(
        check(bool, "it is memoized", rendered2f_mem === rendered2f, true)
      );
      let rendered3 =
        TestRenderer.update(
          rendered2f_mem,
          <ButtonWrapperWrapper wrappedText="updatedText" />
        );
      TestRenderer.convertElement(rendered3)
      |> Alcotest.check(
           renderedElement,
           "Switching Component Types from: ChangeCounter to ButtonWrapperWrapper",
           [
             {
               id: 2,
               component: Component(ButtonWrapperWrapper.component),
               state: "",
               subtree: [
                 {
                   id: 3,
                   component: Component(Div.component),
                   state: "",
                   subtree: [
                     {
                       id: 4,
                       component: Component(ReasonReact.Text.component),
                       state: "buttonWrapperWrapperState",
                       subtree: []
                     },
                     {
                       id: 5,
                       component: Component(ReasonReact.Text.component),
                       state: "wrappedText:updatedText",
                       subtree: []
                     },
                     {
                       id: 6,
                       component: Component(ButtonWrapper.component),
                       state: "",
                       subtree: [
                         {
                           id: 7,
                           component: Component(StatelessButton.component),
                           state: "",
                           subtree: [
                             {
                               component: Component(Div.component),
                               state: "",
                               id: 8,
                               subtree: []
                             }
                           ]
                         }
                       ]
                     }
                   ]
                 }
               ]
             }
           ]
         );
      let rendered4 =
        TestRenderer.update(
          rendered3,
          <ButtonWrapperWrapper wrappedText="updatedTextmodified" />
        );
      TestRenderer.convertElement(rendered4)
      |> Alcotest.check(
           renderedElement,
           "Switching Component Types from: ChangeCounter to ButtonWrapperWrapper",
           [
             {
               id: 2,
               component: Component(ButtonWrapperWrapper.component),
               state: "",
               subtree: [
                 {
                   id: 3,
                   component: Component(Div.component),
                   state: "",
                   subtree: [
                     {
                       id: 4,
                       component: Component(ReasonReact.Text.component),
                       state: "buttonWrapperWrapperState",
                       subtree: []
                     },
                     {
                       id: 5,
                       component: Component(ReasonReact.Text.component),
                       state: "wrappedText:updatedTextmodified",
                       subtree: []
                     },
                     {
                       id: 6,
                       component: Component(ButtonWrapper.component),
                       state: "",
                       subtree: [
                         {
                           id: 7,
                           component: Component(StatelessButton.component),
                           state: "",
                           subtree: [
                             {
                               component: Component(Div.component),
                               state: "",
                               id: 8,
                               subtree: []
                             }
                           ]
                         }
                       ]
                     }
                   ]
                 }
               ]
             }
           ]
         );
      Alcotest.check(
        Alcotest.bool,
        "Memoized nested button wrapper",
        true,
        ReasonReact.(
          switch (rendered3, rendered4) {
          | (
              IFlat([
                Instance({
                  instanceSubTree:
                    IFlat([
                      NativeInstance(
                        _,
                        {instanceSubTree: INested(_, [_, _, IFlat([x])])}
                      )
                    ])
                })
              ]),
              IFlat([
                Instance({
                  instanceSubTree:
                    IFlat([
                      NativeInstance(
                        _,
                        {instanceSubTree: INested(_, [_, _, IFlat([y])])}
                      )
                    ])
                })
              ])
            ) =>
            x === y
          | _ => false
          }
        )
      )
    }
  ),
  (
    "Test Lists With Dynamic Keys",
    `Quick,
    () => {
      open ReasonReact;
      GlobalState.reset();
      let rAction = RemoteAction.create();
      let rendered0 =
        RenderedElement.render(<BoxList useDynamicKeys=true rAction />);
      RemoteAction.act(rAction, ~action=BoxList.Create("Hello"));
      let (rendered1, _) = RenderedElement.flushPendingUpdates(rendered0);
      RemoteAction.act(rAction, ~action=BoxList.Create("World"));
      let (rendered2, _) = RenderedElement.flushPendingUpdates(rendered1);
      RemoteAction.act(rAction, ~action=BoxList.Reverse);
      let (rendered3, _) = RenderedElement.flushPendingUpdates(rendered2);
      TestRenderer.convertElement(rendered0)
      |> Alcotest.check(
           renderedElement,
           "Initial BoxList",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: []
             }
           ]
         );
      TestRenderer.convertElement(rendered1)
      |> Alcotest.check(
           renderedElement,
           "Add Hello then Flush",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: [
                 {
                   id: 2,
                   component: Component(BoxWithDynamicKeys.component),
                   state: "Hello",
                   subtree: []
                 }
               ]
             }
           ]
         );
      TestRenderer.convertElement(rendered2)
      |> Alcotest.check(
           renderedElement,
           "Add Hello then Flush",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: [
                 {
                   id: 3,
                   component: Component(BoxWithDynamicKeys.component),
                   state: "World",
                   subtree: []
                 },
                 {
                   id: 2,
                   component: Component(BoxWithDynamicKeys.component),
                   state: "Hello",
                   subtree: []
                 }
               ]
             }
           ]
         );
      TestRenderer.convertElement(rendered3)
      |> Alcotest.check(
           renderedElement,
           "Add Hello then Flush",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: [
                 {
                   id: 2,
                   component: Component(BoxWithDynamicKeys.component),
                   state: "Hello",
                   subtree: []
                 },
                 {
                   id: 3,
                   component: Component(BoxWithDynamicKeys.component),
                   state: "World",
                   subtree: []
                 }
               ]
             }
           ]
         )
    }
  ),
  (
    "Test Lists Without Dynamic Keys",
    `Quick,
    () => {
      open ReasonReact;
      GlobalState.reset();
      let rAction = RemoteAction.create();
      let rendered0 = RenderedElement.render(<BoxList rAction />);
      RemoteAction.act(rAction, ~action=BoxList.Create("Hello"));
      let (rendered1, _) = RenderedElement.flushPendingUpdates(rendered0);
      RemoteAction.act(rAction, ~action=BoxList.Create("World"));
      let (rendered2, _) = RenderedElement.flushPendingUpdates(rendered1);
      RemoteAction.act(rAction, ~action=BoxList.Reverse);
      let (rendered3, _) = RenderedElement.flushPendingUpdates(rendered2);
      TestRenderer.convertElement(rendered0)
      |> Alcotest.check(
           renderedElement,
           "Initial BoxList",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: []
             }
           ]
         );
      TestRenderer.convertElement(rendered1)
      |> Alcotest.check(
           renderedElement,
           "Add Hello then Flush",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: [
                 {
                   id: 2,
                   component: Component(Box.component),
                   state: "Hello",
                   subtree: []
                 }
               ]
             }
           ]
         );
      TestRenderer.convertElement(rendered2)
      |> Alcotest.check(
           renderedElement,
           "Add Hello then Flush",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: [
                 {
                   id: 3,
                   component: Component(Box.component),
                   state: "World",
                   subtree: []
                 },
                 {
                   id: 4,
                   component: Component(Box.component),
                   state: "Hello",
                   subtree: []
                 }
               ]
             }
           ]
         );
      TestRenderer.convertElement(rendered3)
      |> Alcotest.check(
           renderedElement,
           "Add Hello then Flush",
           [
             {
               id: 1,
               component: Component(BoxList.component),
               state: "",
               subtree: [
                 {
                   id: 3,
                   component: Component(Box.component),
                   state: "Hello",
                   subtree: []
                 },
                 {
                   id: 4,
                   component: Component(Box.component),
                   state: "World",
                   subtree: []
                 }
               ]
             }
           ]
         )
    }
  ),
  (
    "Deep Move Box With Dynamic Keys",
    `Quick,
    () => {
      open ReasonReact;
      GlobalState.reset();
      let box_ = <BoxWithDynamicKeys title="box to move" />;
      let rendered0 = RenderedElement.render(box_);
      let (rendered1, _) =
        RenderedElement.update(
          rendered0,
          Nested(
            "div",
            [ReasonReact.stringToElement("before"), Nested("div", [box_])]
          )
        );
      TestRenderer.convertElement(rendered0)
      |> Alcotest.check(
           renderedElement,
           "Initial Box",
           [
             {
               id: 1,
               component: Component(BoxWithDynamicKeys.component),
               state: "box to move",
               subtree: []
             }
           ]
         );
      TestRenderer.convertElement(rendered1)
      |> Alcotest.check(
           renderedElement,
           "After update",
           [
             {
               id: 2,
               component: Component(Text.component),
               state: "before",
               subtree: []
             },
             {
               id: 1,
               component: Component(BoxWithDynamicKeys.component),
               state: "box to move",
               subtree: []
             }
           ]
         )
    }
  ),
  (
    "Test With Static Keys",
    `Quick,
    () => {
      open ReasonReact;
      GlobalState.reset();
      let key1 = Key.create();
      let key2 = Key.create();
      let rendered0 =
        RenderedElement.render(
          ReasonReact.listToElement([
            <Box key=key1 title="Box1unchanged" />,
            <Box key=key2 title="Box2unchanged" />
          ])
        );
      let (rendered1, _) =
        RenderedElement.update(
          rendered0,
          ReasonReact.listToElement([
            <Box key=key2 title="Box2changed" />,
            <Box key=key1 title="Box1changed" />
          ])
        );
      TestRenderer.convertElement(rendered0)
      |> Alcotest.check(
           renderedElement,
           "Initial Boxes",
           [
             {
               id: 1,
               component: Component(Box.component),
               state: "Box1unchanged",
               subtree: []
             },
             {
               id: 2,
               component: Component(Box.component),
               state: "Box2unchanged",
               subtree: []
             }
           ]
         );
      TestRenderer.convertElement(rendered1)
      |> Alcotest.check(
           renderedElement,
           "Swap Boxes",
           [
             {
               id: 2,
               component: Component(Box.component),
               state: "Box2changed",
               subtree: []
             },
             {
               id: 1,
               component: Component(Box.component),
               state: "Box1changed",
               subtree: []
             }
           ]
         )
    }
  ),
  (
    "Test Update on Alternate Clicks",
    `Quick,
    () => {
      open ReasonReact;
      GlobalState.reset();
      let result = (~state, ~text) => [
        {
          TestRenderer.id: 1,
          component: Component(UpdateAlternateClicks.component),
          state,
          subtree: [
            {
              id: 2,
              state: text,
              component: Component(Text.component),
              subtree: []
            }
          ]
        }
      ];
      let rAction = RemoteAction.create();
      let rendered = RenderedElement.render(<UpdateAlternateClicks rAction />);
      TestRenderer.convertElement(rendered)
      |> Alcotest.check(
           renderedElement,
           "Initial",
           result(~state="0", ~text="0")
         );
      RemoteAction.act(rAction, ~action=Click);
      let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
      Alcotest.check(
        renderedElement,
        "First click then flush",
        result(~state="1", ~text="0"),
        TestRenderer.convertElement(rendered)
      );
      RemoteAction.act(rAction, ~action=Click);
      let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
      Alcotest.check(
        renderedElement,
        "Second click then flush",
        result(~state="2", ~text="2"),
        TestRenderer.convertElement(rendered)
      );
      RemoteAction.act(rAction, ~action=Click);
      let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
      Alcotest.check(
        renderedElement,
        "Second click then flush",
        result(~state="3", ~text="2"),
        TestRenderer.convertElement(rendered)
      );
      RemoteAction.act(rAction, ~action=Click);
      let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
      Alcotest.check(
        renderedElement,
        "Second click then flush",
        result(~state="4", ~text="4"),
        TestRenderer.convertElement(rendered)
      )
    }
  )
];

Alcotest.run("Tests", [("BoxWrapper", suite)]);
