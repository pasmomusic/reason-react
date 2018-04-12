module NativeView = {
  type t;
  [@noalloc] external getWindow : unit => t = "View_getWindow";
  [@noalloc]
  external makeInstance : unit => t = "View_newView_byte" "View_newView";
  [@noalloc] external addChild : (t, t) => t = "View_addChild";
  [@noalloc] external removeChild : (t, t) => t = "View_removeChild";
  [@noalloc]
  external memoizeInstance : ([@untagged] int, t) => unit =
    "View_memoizeInstance_byte" "View_memoizeInstance";
  [@noalloc]
  external freeInstance : ([@untagged] int) => unit =
    "View_memoizeInstance_byte" "View_freeInstance";
  [@noalloc]
  external setFrame :
    ([@untagged] int, [@untagged] int, [@untagged] int, [@untagged] int, t) =>
    unit =
    "View_setFrame_byte" "View_setFrame";
  [@noalloc]
  external setBackgroundColor :
    (
      [@unboxed] float,
      [@unboxed] float,
      [@unboxed] float,
      [@unboxed] float,
      t
    ) =>
    unit =
    "View_setBackgroundColor_byte" "View_setBackgroundColor";
  [@noalloc]
  external setBorderWidth : ([@unboxed] float, t) => unit =
    "View_setBorderWidth_byte" "View_setBorderWidth";
  [@noalloc]
  external setBorderColor :
    (
      [@unboxed] float,
      [@unboxed] float,
      [@unboxed] float,
      [@unboxed] float,
      t
    ) =>
    unit =
    "View_setBorderColor_byte" "View_setBorderColor";
  [@noalloc]
  external setCornerRadius : ([@unboxed] float, t) => unit =
    "View_setBorderColor_byte" "View_setBorderColor";
};

module Node = {
  type context = unit;
  let nullContext = ();
};

module Layout = Flex.Layout.Create(Node, Flex.FloatEncoding);

module HostImplementation = {
  type hostView = {
    view: NativeView.t,
    layoutNode: Layout.LayoutSupport.LayoutTypes.node
  };
  let instanceMap: Hashtbl.t(int, hostView) = Hashtbl.create(1000);
  let getInstance = id =>
    Hashtbl.(mem(instanceMap, id) ? Some(find(instanceMap, id)) : None);
  let memoizeInstance = (id, instance) => {
    Hashtbl.add(instanceMap, id, instance);
    NativeView.memoizeInstance(id, instance.view);
  };
  let freeInstance = id => {
    Hashtbl.remove(instanceMap, id);
    NativeView.freeInstance(id);
  };
  let cssNodeInsertChild = (node, child, index) => {
    open Layout.LayoutSupport;
    open LayoutTypes;
    assert (child.parent === theNullNode);
    assert (node.measure === None);
    let capacity = Array.length(node.children);
    if (capacity == node.childrenCount) {
      /* TODO:Simply use Array.fill (no need to allocate a separate `fill` array
       * */
      let fill = Array.make(capacity + 1, theNullNode);
      Array.blit(node.children, 0, fill, 0, capacity);
      node.children = fill;
    };
    for (i in node.childrenCount downto index + 1) {
      node.children[i] = node.children[i - 1];
    };
    node.childrenCount = node.childrenCount + 1;
    node.children[index] = child;
    child.parent = node;
    Layout.LayoutSupport.markDirtyInternal(node);
  };
  let addSubview = (~parent, ~child) => {
    cssNodeInsertChild(parent.layoutNode, child.layoutNode, Array.length(parent.layoutNode.children));
    ignore(NativeView.addChild(parent.view, child.view));
  };
  let removeFromParent = (~parent, ~child) => {
    let newChildren =
      Array.of_list(
        List.filter(
          aChild => aChild !== child.layoutNode,
          Array.to_list(parent.layoutNode.children)
        )
      );
    parent.layoutNode.childrenCount = Array.length(newChildren);
    parent.layoutNode.children = newChildren;
    child.layoutNode.parent = Layout.LayoutSupport.theNullNode;
    Layout.LayoutSupport.markDirtyInternal(parent.layoutNode);
    ignore(NativeView.removeChild(parent.view, child.view));
  };
};

include ReactCore.Make(HostImplementation);

let makeLayoutNode = (~layout) =>
  Layout.LayoutSupport.createNode(~withChildren=[||], ~andStyle=layout, ());

module View = {
  type t = NativeView.t;
  type color = {
    red: float,
    green: float,
    blue: float,
    alpha: float
  };
  type style = {
    backgroundColor: color,
    borderWidth: float
  };
  let component = statelessNativeComponent("View");
  let make = (~layout, ~style, ~borderColor, children) => {
    ...component,
    render: (_) => {
      make: () => {
        view: NativeView.makeInstance(),
        layoutNode: makeLayoutNode(~layout)
      },
      updateInstance: ({view}) => {
        let {red, green, blue, alpha} = style.backgroundColor;
        NativeView.setBackgroundColor(red, green, blue, alpha, view);
        NativeView.setBorderWidth(style.borderWidth, view);
        let {red, green, blue, alpha} = borderColor;
        NativeView.setBorderColor(red, green, blue, alpha, view);
      },
      children
    }
  };
};

module Button = {
  type t = NativeView.t;
  external makeInstance : int => NativeView.t = "Button_makeInstance";
  external setText : (string, t) => t = "Button_setText";
  [@noalloc]
  external setCallback : (unit => unit, t) => t = "Button_setCallback";
  let component = statelessNativeComponent("Button");
  let make = (~text, ~style, ~callback=?, children) => {
    ...component,
    render: (_) => {
      make: () => {
        let instance = makeInstance(0) |> setText(text);
        let instance =
          switch callback {
          | Some(callback) => setCallback(callback, instance)
          | None => instance
          };
        {view: instance, layoutNode: makeLayoutNode(~layout=style)};
      },
      children,
      updateInstance: (_) => ()
    }
  };
};
