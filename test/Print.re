open TestRenderer;

let printList = (indent, lst) => {
  let indent = String.make(indent, ' ');
  "[" ++ String.concat(",\n", List.map(s => s, lst)) ++ "\n" ++ indent ++ "]";
};

let rec treeFormatter = () => Fmt.hvbox(Fmt.list(printInstance()))
and printInstance = () =>
  Fmt.hvbox((formatter, instance) =>
    switch instance.subtree {
    | [] =>
      Fmt.pf(
        formatter,
        "<%s id=%s%s/>",
        componentName(instance.component),
        string_of_int(instance.id),
        switch instance.state {
        | "" => ""
        | x => " state=\"" ++ x ++ "\""
        }
      )
    | sub =>
      Fmt.pf(
        formatter,
        "<%s id=%s%s>@ %a@ </%s>",
        componentName(instance.component),
        string_of_int(instance.id),
        switch instance.state {
        | "" => ""
        | x => " state=\"" ++ x ++ "\""
        },
        treeFormatter(),
        sub,
        componentName(instance.component)
      )
    }
  );

let element = formatter => Fmt.pf(formatter, "%a", treeFormatter());

let printSubTreeChange = formatter =>
  Fmt.pf(
    formatter,
    "%a",
    Fmt.hvbox((formatter, change) =>
      switch change {
      | `NoChange => Fmt.pf(formatter, "%s", "`NoChange")
      | `Nested => Fmt.pf(formatter, "%s", "`Nested")
      | `PrependElement(x) =>
        Fmt.pf(formatter, "`PrependElement: %a@,", element, x)
      | `ReplaceElements(oldElems, newElems) =>
        Fmt.pf(
          formatter,
          "`ReplaceElements: %a@, %a@,",
          element,
          oldElems,
          element,
          newElems
        )
      }
    )
  );

let updateLog = formatter => {
  let rec pp = () => Fmt.brackets(Fmt.list(~sep=Fmt.comma, printUpdateLog()))
  and printUpdateLog = ((), formatter, entry) =>
    switch entry {
    | UpdateInstance(update) =>
      Fmt.pf(
        formatter,
        "%s {@[<hov>@,stateChanged: %s,@ subTreeChanged: %a,@ oldInstance: %a,@ newInstance: %a @]}",
        "UpdateInstance",
        string_of_bool(update.stateChanged),
        printSubTreeChange,
        update.subTreeChanged,
        printInstance(),
        update.oldInstance,
        printInstance(),
        update.newInstance
      )
    | ChangeComponent(update) =>
      Fmt.pf(
        formatter,
        "%s {@[<hov>@,oldSubtree: %a,@ newSubtree: %a,@ oldInstance: %a,@ newInstance: %a @]}",
        "ChangeComponent",
        element,
        update.oldSubtree,
        element,
        update.newSubtree,
        printInstance(),
        update.oldInstance,
        printInstance(),
        update.newInstance
      )
    };
  Fmt.pf(formatter, "%a", pp());
};

let topLevelUpdateLog =
  Fmt.hvbox((formatter, topLevelUpdateLog: option(testTopLevelUpdateLog)) =>
    switch topLevelUpdateLog {
    | Some(topLevelUpdate) =>
      Fmt.pf(
        formatter,
        "%s {@[<hov>@,subTreeChanged: %a,@ updateLog: %a @]}",
        "TopLevelUpdate",
        printSubTreeChange,
        topLevelUpdate.subtreeChange,
        updateLog,
        topLevelUpdate.updateLog
      )
    | None => Fmt.pf(formatter, "%s", "NoUpdate")
    }
  );

let hostElement: ReasonReact.Implementation.hostElement => string =
  fun
  | View => "View"
  | Text(title) => "Text(" ++ title ++ ")";

let renderLog = formatter => {
  let rec pp = () => Fmt.brackets(Fmt.list(~sep=Fmt.comma, printRenderLog()))
  and printRenderLog = ((), formatter, entry: ReasonReact.RenderLog.entry) =>
    switch entry {
    | GetInstance(id) =>
      Fmt.pf(formatter, "GetInstance(%s)", string_of_int(id))
    | MemoizeInstance(id, instance) =>
      Fmt.pf(
        formatter,
        "MemoizeInstance(%s, {@[<hov>@,\"%s\", %s, %s}@])",
        string_of_int(id),
        instance.name,
        string_of_int(instance.id),
        hostElement(instance.element)
      )
    | AddSubview(parent, child) =>
      Fmt.pf(
        formatter,
        "AddSubview({@[<hov>@,\"%s\", %s, %s},@ {\"%s\", %s, %s}@])",
        parent.name,
        string_of_int(parent.id),
        hostElement(parent.element),
        child.name,
        string_of_int(child.id),
        hostElement(child.element)
      )
    | CreateInstance(_)
    | FreeInstance(_)
    | RemoveFromParent(_, _)
    | ComponentDidMount(_)
    | ComponentDidUpdate(_)
    | ComponentWillUnmount(_)
    | UpdateInstance(_) => Fmt.pf(formatter, "%s", "[to be printed]")
    };
  Fmt.pf(formatter, "%a", pp());
};
