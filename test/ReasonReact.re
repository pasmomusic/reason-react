open R2n2;

module Implementation = {
  [@deriving (show({with_path: false}), eq, ord)]
  type hostElement =
    | Text(string)
    | View;
  [@deriving (show({with_path: false}), eq, ord)]
  type hostView = {
    name: string,
    element: hostElement,
    mutable id: int
  };
  module RenderLog = {
    [@deriving (show({with_path: false}), eq, ord)]
    type payload = {
      component: string,
      state: string
    };
    [@deriving (show({with_path: false}), eq, ord)]
    type stateUpdatePayload = {
      component: string,
      oldState: string,
      newState: string
    };
    [@deriving (show({with_path: false}), eq, ord)]
    type entry =
      | GetInstance(int)
      | CreateInstance(hostView)
      | MemoizeInstance(int, hostView)
      | FreeInstance(int)
      | AddSubview(hostView, hostView)
      | RemoveFromParent(hostView, hostView)
      | ComponentDidMount(payload)
      | ComponentDidUpdate(stateUpdatePayload)
      | ComponentWillUnmount(payload)
      | UpdateInstance(string, hostView);
    [@deriving (show, eq, ord)]
    type t = list(entry);
    let global: ref(t) = ref([]);
    let add = (action: entry) => global := [action, ...global^];
    let reset = () => global := [];
  };
  let map: Hashtbl.t(int, hostView) = Hashtbl.create(1000);
  let getInstance = id => {
    RenderLog.add(GetInstance(id));
    if (Hashtbl.mem(map, id)) {
      Some(Hashtbl.find(map, id));
    } else {
      None;
    };
  };
  let memoizeInstance = (id, instance) => {
    instance.id = id;
    RenderLog.add(MemoizeInstance(id, instance));
    Hashtbl.add(map, id, instance);
  };
  let freeInstance = id => {
    RenderLog.add(FreeInstance(id));
    Hashtbl.remove(map, id);
  };
  let removeFromParent = (~parent, ~child) =>
    RenderLog.add(
      RemoveFromParent(
        {name: parent.name, element: parent.element, id: parent.id},
        {name: child.name, element: child.element, id: child.id}
      )
    );
  let addSubview = (~parent, ~child) =>
    RenderLog.add(
      AddSubview(
        {name: parent.name, element: parent.element, id: parent.id},
        {name: child.name, element: child.element, id: child.id}
      )
    );
};

module RenderLog = Implementation.RenderLog;

module ReasonReact = ReactCore_Internal.Make(Implementation);

include (
  ReasonReact:
    (module type of ReasonReact) with
      module GlobalState := ReasonReact.GlobalState
);

module GlobalState = {
  include ReasonReact.GlobalState;
  let reset = () => {
    reset();
    RenderLog.reset();
  };
};
