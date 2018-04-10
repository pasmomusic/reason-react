open R2n2;

module Implementation = {
  type hostView =
    | Text(string)
    | View;
  module RenderLog = {
    type payload = {
      component: string,
      state: string
    };
    type stateUpdatePayload = {
      component: string,
      oldState: string,
      newState: string
    };
    type entry =
      | GetInstance(int)
      | MemoizeInstance(int, hostView)
      | FreeInstance(int)
      | AddSubview(int, int)
      | RemoveFromParent(hostView, hostView)
      | ComponentDidMount(payload)
      | ComponentDidUpdate(stateUpdatePayload)
      | ComponentWillUnmount(payload)
      | UpdateInstance(payload, hostView);
    type t = list(entry);
    let global: ref(t) = ref([]);
    let add = (action: entry) => global := [action, ...global^];
    let reset = () => global := [];
  };
  let map: Hashtbl.t(int, hostView) = Hashtbl.create(1000);
  let getInstance = id =>
    if (Hashtbl.mem(map, id)) {
      Some(Hashtbl.find(map, id));
    } else {
      None;
    };
  let memoizeInstance = (id, instance) => Hashtbl.add(map, id, instance);
  let freeInstance = id => Hashtbl.remove(map, id);
  let removeFromParent = (~parent, ~child) => ();
  let addSubview = (~parent, ~child) => RenderLog.add(AddSubview(0, 1));
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
