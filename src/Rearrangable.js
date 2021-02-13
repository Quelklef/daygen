exports.nativeMakeRearrangable = handleSelector => event => () => {
  const $rearrangable = event.target;
  new Sortable($rearrangable, {
    handle: handleSelector,
    animation: 150,
    onSort(event) {
      $rearrangable.dispatchEvent(
        new CustomEvent("rearranged", {
          bubbles: false,
          detail: {
            fromIdx: event.oldIndex,
            toIdx: event.newIndex,
          }
        })
      );
    },
  });
};
