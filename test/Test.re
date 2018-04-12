open Assert;

module Option = {
  let withDefault: 'a .('a, option('a)) => 'a =
    x =>
      fun
      | Some(value) => value
      | None => x;
  let map: 'a 'b .('a => 'b, option('a)) => option('b) =
    f =>
      fun
      | Some(value) => Some(f(value))
      | None => None;
};

let getUpdateLog = actual =>
  Option.(
    map(x => x.ReasonReact.RenderedElement.updateLog, snd(actual))
    |> withDefault(ref([]))
  );

let suite =
  Components.[
    (
      "First Render",
      `Quick,
      () => {
        open TestRenderer;
        ReasonReact.GlobalState.reset();
        let rendered = render(<BoxWrapper />);
        let expected =
          TestComponents.[
            <BoxWrapper id=1>
              <Div id=2> <Box id=3 state="ImABox" /> </Div>
            </BoxWrapper>
          ];
        assertElement(expected, rendered);
        let root =
          ReasonReact.Implementation.{name: "root", id: (-1), element: View};
        let forest =
          ReasonReact.OutputTree.fromRenderedElement(root, rendered);
        ReasonReact.OutputTree.mountForest(~forest, ~nearestParentView=root);
        let expectedDiv =
          ReasonReact.Implementation.{name: "Div", id: 2, element: View};
        let expectedBox =
          ReasonReact.Implementation.{
            name: "Box",
            id: 3,
            element: Text("ImABox")
          };
        let actual =
          ReasonReact.RenderedElement.update(
            rendered,
            <BoxWrapper twoBoxes=true />
          );
        ReasonReact.OutputTree.applyUpdateLog(forest, getUpdateLog(actual));
        let twoBoxes =
          TestComponents.(
            <Div id=2>
              <Box id=4 state="ImABox" />
              <Box id=5 state="ImABox" />
            </Div>
          );
        let oneBox = TestComponents.(<Div id=2> <Box id=3 /> </Div>);
        let twoBoxesWrapper =
          TestComponents.(<BoxWrapper id=1> twoBoxes </BoxWrapper>);
        let expected = (
          [twoBoxesWrapper],
          Some({
            subtreeChange: `Nested,
            updateLog:
              TestComponents.[
                UpdateInstance({
                  stateChanged: false,
                  subTreeChanged:
                    `ReplaceElements((
                      [<Box id=3 state="ImABox" />],
                      [
                        <Box id=4 state="ImABox" />,
                        <Box id=5 state="ImABox" />
                      ]
                    )),
                  newInstance: twoBoxes,
                  oldInstance: oneBox
                }),
                UpdateInstance({
                  stateChanged: false,
                  subTreeChanged: `Nested,
                  newInstance: twoBoxesWrapper,
                  oldInstance: <BoxWrapper id=1> oneBox </BoxWrapper>
                })
              ]
          })
        );
        assertUpdate(~label="Top level update", expected, actual);
        let expectedRender =
          ReasonReact.RenderLog.[
            GetInstance(3),
            CreateInstance(expectedBox),
            UpdateInstance("ImABox", expectedBox),
            MemoizeInstance(3, expectedBox),
            GetInstance(2),
            CreateInstance(expectedDiv),
            UpdateInstance("", expectedDiv),
            MemoizeInstance(2, expectedDiv),
            AddSubview(expectedDiv, expectedBox),
            AddSubview(root, expectedDiv),
            /** Update **/
            /** First remove from parent so that it can be reused **/
            RemoveFromParent(expectedDiv, expectedBox),
            FreeInstance(3),
            GetInstance(4),
            CreateInstance(expectedBox),
            UpdateInstance("ImABox", {...expectedBox, id: 4}),
            MemoizeInstance(4, {...expectedBox, id: 4}),
            GetInstance(5),
            CreateInstance(expectedBox),
            UpdateInstance("ImABox", {...expectedBox, id: 5}),
            MemoizeInstance(4, {...expectedBox, id: 5}),
            AddSubview(expectedDiv, {...expectedBox, id: 5}),
            AddSubview(expectedDiv, {...expectedBox, id: 4})
          ];
        assertRenderLog(~label="First render log matches", expectedRender);
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
        let root =
          ReasonReact.Implementation.{name: "root", id: (-1), element: View};
        let forest =
          ReasonReact.OutputTree.fromRenderedElement(root, rendered0);
        ReasonReact.OutputTree.mountForest(~forest, ~nearestParentView=root);
        assertElement(
          [
            TestComponents.(
              <ChangeCounter id=1 label="defaultText" counter=10 />
            )
          ],
          rendered0
        );
        let (rendered1, _) as actual =
          TestRenderer.update(
            rendered0,
            <ChangeCounter label="defaultText" />
          );
        ReasonReact.OutputTree.applyUpdateLog(forest, getUpdateLog(actual));
        assertUpdate(
          (
            [
              TestComponents.(
                <ChangeCounter id=1 label="defaultText" counter=10 />
              )
            ],
            None
          ),
          actual
        );
        let (rendered2, _) as actual2 =
          TestRenderer.update(
            rendered1,
            <ChangeCounter label="updatedText" />
          );
        ReasonReact.OutputTree.applyUpdateLog(forest, getUpdateLog(actual2));
        assertUpdate(
          TestComponents.(
            [<ChangeCounter id=1 label="updatedText" counter=11 />],
            Some(
              TestRenderer.{
                subtreeChange: `Nested,
                updateLog: [
                  UpdateInstance({
                    stateChanged: true,
                    subTreeChanged: `NoChange,
                    oldInstance:
                      <ChangeCounter id=1 label="defaultText" counter=10 />,
                    newInstance:
                      <ChangeCounter id=1 label="updatedText" counter=11 />
                  })
                ]
              }
            )
          ),
          actual2
        );
        let (rendered2f, updateLog) as actual2f =
          ReasonReact.RenderedElement.flushPendingUpdates(rendered2);
        ReasonReact.OutputTree.applyUpdateLog(forest, updateLog);
        assertFlushUpdate(
          TestComponents.(
            [<ChangeCounter id=1 label="updatedText" counter=2011 />],
            [
              UpdateInstance({
                stateChanged: true,
                subTreeChanged: `NoChange,
                oldInstance:
                  <ChangeCounter id=1 label="updatedText" counter=11 />,
                newInstance:
                  <ChangeCounter id=1 label="updatedText" counter=2011 />
              })
            ]
          ),
          actual2f
        );
        let (rendered2f_mem, updateLog) =
          ReasonReact.RenderedElement.flushPendingUpdates(rendered2f);
        ReasonReact.OutputTree.applyUpdateLog(forest, updateLog);
        check(
          Alcotest.bool,
          "it is memoized",
          rendered2f_mem === rendered2f,
          true
        );
        let (rendered2f_mem, updateLog) =
          ReasonReact.RenderedElement.flushPendingUpdates(rendered2f_mem);
        ReasonReact.OutputTree.applyUpdateLog(forest, updateLog);
        check(
          Alcotest.bool,
          "it is memoized",
          rendered2f_mem === rendered2f,
          true
        );
        let (rendered3, _) as actual3 =
          TestRenderer.update(
            rendered2f_mem,
            <ButtonWrapperWrapper wrappedText="updatedText" />
          );
        ReasonReact.OutputTree.applyUpdateLog(forest, getUpdateLog(actual3));
        assertUpdate(
          ~label="Updating components: ChangeCounter to ButtonWrapperWrapper",
          TestComponents.(
            [
              <ButtonWrapperWrapper id=2 nestedText="wrappedText:updatedText" />
            ],
            Some(
              TestRenderer.{
                subtreeChange: `Nested,
                updateLog: [
                  ChangeComponent({
                    oldSubtree: [],
                    newSubtree: [
                      <Div id=3>
                        <Text id=4 title="buttonWrapperWrapperState" />
                        <Text id=5 title="wrappedText:updatedText" />
                        <ButtonWrapper id=6 />
                      </Div>
                    ],
                    oldInstance:
                      <ChangeCounter id=1 label="updatedText" counter=2011 />,
                    newInstance:
                      <ButtonWrapperWrapper
                        id=2
                        nestedText="wrappedText:updatedText"
                      />
                  })
                ]
              }
            )
          ),
          actual3
        );
        let (rendered4, _) as actual4 =
          TestRenderer.update(
            rendered3,
            <ButtonWrapperWrapper wrappedText="updatedTextmodified" />
          );
        ReasonReact.OutputTree.applyUpdateLog(forest, getUpdateLog(actual4));
        assertUpdate(
          ~label="Updating text in the button wrapper",
          TestComponents.(
            [
              <ButtonWrapperWrapper
                id=2
                nestedText="wrappedText:updatedTextmodified"
              />
            ],
            Some(
              TestRenderer.{
                subtreeChange: `Nested,
                updateLog: [
                  UpdateInstance({
                    stateChanged: true,
                    subTreeChanged: `NoChange,
                    oldInstance: <Text id=5 title="wrappedText:updatedText" />,
                    newInstance:
                      <Text id=5 title="wrappedText:updatedTextmodified" />
                  }),
                  UpdateInstance({
                    stateChanged: false,
                    subTreeChanged: `Nested,
                    oldInstance:
                      <Div id=3>
                        <Text id=4 title="buttonWrapperWrapperState" />
                        <Text id=5 title="wrappedText:updatedText" />
                        <ButtonWrapper id=6 />
                      </Div>,
                    newInstance:
                      <Div id=3>
                        <Text id=4 title="buttonWrapperWrapperState" />
                        <Text id=5 title="wrappedText:updatedTextmodified" />
                        <ButtonWrapper id=6 />
                      </Div>
                  }),
                  UpdateInstance({
                    stateChanged: false,
                    subTreeChanged: `Nested,
                    oldInstance:
                      <ButtonWrapperWrapper
                        id=2
                        nestedText="wrappedText:updatedText"
                      />,
                    newInstance:
                      <ButtonWrapperWrapper
                        id=2
                        nestedText="wrappedText:updatedTextmodified"
                      />
                  })
                ]
              }
            )
          ),
          actual4
        );
        check(
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
                        Instance({
                          instanceSubTree: INested(_, [_, _, IFlat([x])])
                        })
                      ])
                  })
                ]),
                IFlat([
                  Instance({
                    instanceSubTree:
                      IFlat([
                        Instance({
                          instanceSubTree: INested(_, [_, _, IFlat([y])])
                        })
                      ])
                  })
                ])
              ) =>
              x === y
            | _ => false
            }
          )
        );
        let div = {
          ReasonReact.Implementation.name: "Div",
          id: 0,
          element: View
        };
        let text = (txt, id) => {
          ReasonReact.Implementation.name: "Text",
          id,
          element: Text(txt)
        };
        let expectedRender =
          ReasonReact.RenderLog.
            /* No render until there's a change from ChangeCounter
             * into ButtonWrapperWrapper
             */
            [
              GetInstance(8),
              CreateInstance({name: "Div", id: 0, element: View}),
              UpdateInstance("", {...div, id: 8}),
              MemoizeInstance(8, {...div, id: 8}),
              GetInstance(5),
              CreateInstance(text("wrappedText:updatedText", 5)),
              UpdateInstance(
                "wrappedText:updatedText",
                text("wrappedText:updatedText", 5)
              ),
              MemoizeInstance(5, text("wrappedText:updatedText", 5)),
              GetInstance(4),
              CreateInstance(text("buttonWrapperWrapperState", 4)),
              UpdateInstance("", text("buttonWrapperWrapperState", 4)),
              MemoizeInstance(4, text("buttonWrapperWrapperState", 4)),
              GetInstance(3),
              CreateInstance(div),
              UpdateInstance("", {...div, id: 3}),
              MemoizeInstance(3, {...div, id: 3}),
              AddSubview({...div, id: 3}, {...div, id: 8}),
              AddSubview({...div, id: 3}, text("", 5)),
              AddSubview({...div, id: 3}, text("", 3)),
              AddSubview(root, {...div, id: 3}),
              /* After last text update */
              UpdateInstance(
                "wrappedText:updatedTextmodified",
                text("wrappedText:updatedText", 5)
              )
            ];
        assertRenderLog(~label="First render log matches", expectedRender);
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
        let (rendered1, _) as actual1 =
          RenderedElement.flushPendingUpdates(rendered0);
        RemoteAction.act(rAction, ~action=BoxList.Create("World"));
        let (rendered2, _) as actual2 =
          RenderedElement.flushPendingUpdates(rendered1);
        RemoteAction.act(rAction, ~action=BoxList.Reverse);
        let (rendered3, _) as actual3 =
          RenderedElement.flushPendingUpdates(rendered2);
        TestRenderer.convertElement(rendered0)
        |> check(
             renderedElement,
             "Initial BoxList",
             [TestComponents.(<BoxList id=1 />)]
           );
        assertFlushUpdate(
          ~label="Add Hello then Flush",
          TestComponents.(
            [
              <BoxList id=1>
                <BoxWithDynamicKeys id=2 state="Hello" />
              </BoxList>
            ],
            [
              UpdateInstance({
                stateChanged: true,
                subTreeChanged:
                  `ReplaceElements((
                    [],
                    [<BoxWithDynamicKeys id=2 state="Hello" />]
                  )),
                oldInstance: <BoxList id=1 />,
                newInstance:
                  <BoxList id=1>
                    <BoxWithDynamicKeys id=2 state="Hello" />
                  </BoxList>
              })
            ]
          ),
          actual1
        );
        assertFlushUpdate(
          ~label="Add Hello then Flush",
          TestComponents.(
            [
              <BoxList id=1>
                <BoxWithDynamicKeys id=3 state="World" />
                <BoxWithDynamicKeys id=2 state="Hello" />
              </BoxList>
            ],
            [
              UpdateInstance({
                stateChanged: true,
                subTreeChanged:
                  `ReplaceElements((
                    [<BoxWithDynamicKeys id=2 state="Hello" />],
                    [
                      <BoxWithDynamicKeys id=3 state="World" />,
                      <BoxWithDynamicKeys id=2 state="Hello" />
                    ]
                  )),
                oldInstance:
                  <BoxList id=1>
                    <BoxWithDynamicKeys id=2 state="Hello" />
                  </BoxList>,
                newInstance:
                  <BoxList id=1>
                    <BoxWithDynamicKeys id=3 state="World" />
                    <BoxWithDynamicKeys id=2 state="Hello" />
                  </BoxList>
              })
            ]
          ),
          actual2
        );
        assertFlushUpdate(
          ~label="Add Hello then Flush",
          TestComponents.(
            [
              <BoxList id=1>
                <BoxWithDynamicKeys id=2 state="Hello" />
                <BoxWithDynamicKeys id=3 state="World" />
              </BoxList>
            ],
            [
              UpdateInstance({
                stateChanged: true,
                subTreeChanged: `Nested,
                oldInstance:
                  <BoxList id=1>
                    <BoxWithDynamicKeys id=3 state="World" />
                    <BoxWithDynamicKeys id=2 state="Hello" />
                  </BoxList>,
                newInstance:
                  <BoxList id=1>
                    <BoxWithDynamicKeys id=2 state="Hello" />
                    <BoxWithDynamicKeys id=3 state="World" />
                  </BoxList>
              })
            ]
          ),
          actual3
        );
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
        let (rendered1, _) as actual1 =
          RenderedElement.flushPendingUpdates(rendered0);
        RemoteAction.act(rAction, ~action=BoxList.Create("World"));
        let (rendered2, _) as actual2 =
          RenderedElement.flushPendingUpdates(rendered1);
        RemoteAction.act(rAction, ~action=BoxList.Reverse);
        let (rendered3, _) as actual3 =
          RenderedElement.flushPendingUpdates(rendered2);
        TestRenderer.convertElement(rendered0)
        |> Assert.check(
             renderedElement,
             "Initial BoxList",
             [TestComponents.(<BoxList id=1 />)]
           );
        assertFlushUpdate(
          ~label="Add Hello then Flush",
          TestComponents.(
            [<BoxList id=1> <Box id=2 state="Hello" /> </BoxList>],
            [
              UpdateInstance({
                stateChanged: true,
                subTreeChanged:
                  `ReplaceElements(([], [<Box id=2 state="Hello" />])),
                oldInstance: <BoxList id=1 />,
                newInstance:
                  <BoxList id=1> <Box id=2 state="Hello" /> </BoxList>
              })
            ]
          ),
          actual1
        );
        assertFlushUpdate(
          ~label="Add Hello then Flush",
          TestComponents.(
            [
              <BoxList id=1>
                <Box id=3 state="World" />
                <Box id=4 state="Hello" />
              </BoxList>
            ],
            [
              UpdateInstance({
                stateChanged: true,
                subTreeChanged:
                  `ReplaceElements((
                    [<Box id=2 state="Hello" />],
                    [<Box id=3 state="World" />, <Box id=4 state="Hello" />]
                  )),
                oldInstance:
                  <BoxList id=1> <Box id=2 state="Hello" /> </BoxList>,
                newInstance:
                  <BoxList id=1>
                    <Box id=3 state="World" />
                    <Box id=4 state="Hello" />
                  </BoxList>
              })
            ]
          ),
          actual2
        );
        assertFlushUpdate(
          ~label="Add Hello then Flush",
          TestComponents.(
            [
              <BoxList id=1>
                <Box id=3 state="Hello" />
                <Box id=4 state="World" />
              </BoxList>
            ],
            [
              UpdateInstance({
                stateChanged: true,
                subTreeChanged: `NoChange,
                oldInstance: <Box id=4 state="Hello" />,
                newInstance: <Box id=4 state="World" />
              }),
              UpdateInstance({
                stateChanged: true,
                subTreeChanged: `NoChange,
                oldInstance: <Box id=3 state="World" />,
                newInstance: <Box id=3 state="Hello" />
              }),
              UpdateInstance({
                stateChanged: true,
                subTreeChanged: `Nested,
                oldInstance:
                  <BoxList id=1>
                    <Box id=3 state="World" />
                    <Box id=4 state="Hello" />
                  </BoxList>,
                newInstance:
                  <BoxList id=1>
                    <Box id=3 state="Hello" />
                    <Box id=4 state="World" />
                  </BoxList>
              })
            ]
          ),
          actual3
        );
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
        TestRenderer.convertElement(rendered0)
        |> check(
             renderedElement,
             "Initial Box",
             [TestComponents.(<BoxWithDynamicKeys id=1 state="box to move" />)]
           );
        let (rendered1, _) as actual1 =
          RenderedElement.update(
            rendered0,
            Nested("div", [stringToElement("before"), Nested("div", [box_])])
          );
        assertUpdate(
          ~label="After update",
          TestComponents.(
            [
              <Text id=2 title="before" />,
              <BoxWithDynamicKeys id=1 state="box to move" />
            ],
            Some(
              TestRenderer.{
                subtreeChange:
                  `ReplaceElements((
                    [<BoxWithDynamicKeys id=1 state="box to move" />],
                    [
                      <Text id=2 title="before" />,
                      <BoxWithDynamicKeys id=1 state="box to move" />
                    ]
                  )),
                updateLog: []
              }
            )
          ),
          actual1
        );
        /* TODO Compare rendered0 and rendered1 */
        /* check(
             Alcotest.bool,
             "Memoized nested box",
             true,
             ReasonReact.(
               switch (rendered0, rendered1) {
               | (
                   IFlat([Instance({instanceSubTree: IFlat([x])})]),
                   IFlat([
                     Instance({instanceSubTree: INested(_, [_, IFlat([y])])})
                   ])
                 ) =>
                 x === y
               | _ => false
               }
             )
           ); */
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
        TestRenderer.convertElement(rendered0)
        |> check(
             renderedElement,
             "Initial Boxes",
             TestComponents.[
               <Box id=1 state="Box1unchanged" />,
               <Box id=2 state="Box2unchanged" />
             ]
           );
        let (rendered1, _) as actual1 =
          RenderedElement.update(
            rendered0,
            ReasonReact.listToElement([
              <Box key=key2 title="Box2changed" />,
              <Box key=key1 title="Box1changed" />
            ])
          );
        assertUpdate(
          ~label="Swap Boxes",
          TestComponents.(
            [
              <Box id=2 state="Box2changed" />,
              <Box id=1 state="Box1changed" />
            ],
            Some(
              TestRenderer.{
                subtreeChange: `Nested,
                updateLog: [
                  UpdateInstance({
                    stateChanged: true,
                    subTreeChanged: `NoChange,
                    oldInstance: <Box id=1 state="Box1unchanged" />,
                    newInstance: <Box id=1 state="Box1changed" />
                  }),
                  UpdateInstance({
                    stateChanged: true,
                    subTreeChanged: `NoChange,
                    oldInstance: <Box id=2 state="Box2unchanged" />,
                    newInstance: <Box id=2 state="Box2changed" />
                  })
                ]
              }
            )
          ),
          actual1
        );
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
            subtree: [TestComponents.(<Text id=2 title=text />)]
          }
        ];
        let rAction = RemoteAction.create();
        let rendered =
          RenderedElement.render(<UpdateAlternateClicks rAction />);
        TestRenderer.convertElement(rendered)
        |> check(renderedElement, "Initial", result(~state="0", ~text="0"));
        RemoteAction.act(rAction, ~action=Click);
        let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
        check(
          renderedElement,
          "First click then flush",
          result(~state="1", ~text="0"),
          TestRenderer.convertElement(rendered)
        );
        RemoteAction.act(rAction, ~action=Click);
        let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
        check(
          renderedElement,
          "Second click then flush",
          result(~state="2", ~text="2"),
          TestRenderer.convertElement(rendered)
        );
        RemoteAction.act(rAction, ~action=Click);
        let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
        check(
          renderedElement,
          "Second click then flush",
          result(~state="3", ~text="2"),
          TestRenderer.convertElement(rendered)
        );
        RemoteAction.act(rAction, ~action=Click);
        let (rendered, _) = RenderedElement.flushPendingUpdates(rendered);
        check(
          renderedElement,
          "Second click then flush",
          result(~state="4", ~text="4"),
          TestRenderer.convertElement(rendered)
        );
      }
    )
  ];

Alcotest.run(~argv=[|"--verbose --color"|], "Tests", [("BoxWrapper", suite)]);
