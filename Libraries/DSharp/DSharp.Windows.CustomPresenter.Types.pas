unit DSharp.Windows.CustomPresenter.Types;

interface

type
  // operations basically allowed during drag and drop
  TDragOperation = (
    doCopy,
    doMove,
    doLink
  );

  // modes to determine drop position further
  TDropMode = (
    dmNowhere,
    dmAbove,
    dmOnNode,
    dmBelow
  );

  TCollapsedEvent = procedure(Sender: TObject; CollapsedItem: TObject) of object;
  TCompareEvent = procedure(Sender: TObject; Item1, Item2: TObject;
    ColumnIndex: Integer; var Result: Integer) of object;
  TDragBeginEvent = procedure(Sender: TObject; Item: TObject;
    var AllowDrag: Boolean) of object;
  TDragOverEvent = procedure(Sender: TObject; Source: TObject; TargetItem: TObject;
    var AllowDrop: Boolean) of object;
  TDragDropEvent = procedure(Sender: TObject; Source: TObject; TargetItem: TObject;
    DragOperation: TDragOperation; var DropMode: TDropMode;
    var Handled: Boolean) of object;
  TExpandedEvent = procedure(Sender: TObject; ExpandedItem: TObject) of object;
  TSelectionChangingEvent = procedure(Sender: TObject; TargetItem: TObject;
    var AllowChange: Boolean) of object;

  TCheckSupport = (csNone, csSimple, csTriState, csRadio);
  TFilterDirection = (fdRootToLeafs, fdLeafsToRoot);
  TSelectionMode = (smSingle, smLevel, smMulti, smNone);

implementation

end.