var comboTreeBinding = new Shiny.InputBinding();

$.extend(comboTreeBinding, {
  find: function (scope) {
    return $(scope).find(".comboTree");
  },
  getValue: function (el) {
    var value = el.value.split(", ");
    var empty = value.length === 1 && value[0] === "";
    return empty ? null : value;
  },
  setValue: function(el, value) {
    $(el).setSelection(value);
  },
  subscribe: function (el, callback) {
    $(el).on("change.comboTreeBinding", function (e) {
      callback();
    });
  },
  unsubscribe: function (el) {
    $(el).off(".comboTreeBinding");
  },
  initialize: function(el) {
        var $el = $(el);
        $el.comboTree({
      source: $el.data("choices"),
      isMultiple: $el.data("multiple"),
      cascadeSelect: $el.data("cascaded"),
      selected: $el.data("selected"),
      collapse: true
    });
  }
});

Shiny.inputBindings.register(comboTreeBinding);
