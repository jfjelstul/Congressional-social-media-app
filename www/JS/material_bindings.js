
// switch
$(document).ready(function(){

  var shinyMaterialSwitch = new Shiny.InputBinding();
  $.extend(shinyMaterialSwitch, {
    find: function(scope) {
      return $(scope).find(".my-shiny-switch");
    },
    getValue: function(el) {
      return $(el).val();
    },
    subscribe: function(el, callback) {
      $(el).on("change.my-shiny-switch", function(e) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off(".my-shiny-switch");
    }
  });

  Shiny.inputBindings.register(shinyMaterialSwitch);
});

// button
$(document).ready(function () {
  $(".my-shiny-button").on("click", function () {
    var el = $(this);
    var curVal = parseInt(el.val());
    el.val(curVal + 1);
    el.trigger("change");
  });

  var shinyMaterialButton = new Shiny.InputBinding();
  $.extend(shinyMaterialButton, {
    find: function (scope) {
      return $(scope).find(".my-shiny-button");
    },
    getValue: function (el) {
      return parseInt($(el).val());
    },
    subscribe: function (el, callback) {
      $(el).on("change.my-shiny-button", function (e) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".my-shiny-button");
    }
  });

  Shiny.inputBindings.register(shinyMaterialButton);
});

// slider
$(document).ready(function () {

  var shinyMaterialSlider = new Shiny.InputBinding();

  $.extend(shinyMaterialSlider, {
    find: function (scope) {
      return $(scope).find(".my-shiny-slider");
    },
    getValue: function (el) {
      var classValue = $(el).find(".value").html();
      if (classValue) {
        if (classValue.length === 0) {
          var inputValue = $(el).find('input').val();
          return Number(inputValue);
        } else {
          return Number(classValue);
        }
      } else {
        var inputValue = $(el).find('input').val();
        return Number(inputValue);
      }
    },
    subscribe: function (el, callback) {
      $(el).on("change", function (e) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".my-shiny-slider");
    }
  });

  Shiny.inputBindings.register(shinyMaterialSlider);
});

// radio radio_buttons
$(document).ready(function () {

  var shinyMaterialRadioButton = new Shiny.InputBinding();
  $.extend(shinyMaterialRadioButton, {
    find: function (scope) {
      return $(scope).find(".shiny-material-radio-button");
    },
    getValue: function (el) {
      return $(el).find('input:checked').attr('id').replace(new RegExp("_shinymaterialradioempty_", 'g'), "");
    },
    subscribe: function (el, callback) {
      $(el).on("change.shiny-material-radio-button", function (e) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".shiny-material-radio-button");
    }
  });

  Shiny.inputBindings.register(shinyMaterialRadioButton);

});

// text box
$(document).ready(function () {

  var shinyMaterialTextBox = new Shiny.InputBinding();
  $.extend(shinyMaterialTextBox, {
    find: function (scope) {
      return $(scope).find(".shiny-material-text-box");
    },
    getValue: function (el) {
      return $(el).val();
    },
    subscribe: function (el, callback) {
      $(el).on("change.shiny-material-text-box", function (e) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".shiny-material-text-box");
    }
  });

  Shiny.inputBindings.register(shinyMaterialTextBox);
});

// autocomplete
$(document).ready(function () {

  var shinyMaterialAutocomplete = new Shiny.InputBinding();
  $.extend(shinyMaterialAutocomplete, {
    find: function (scope) {
      return $(scope).find(".autocomplete");
    },
    getValue: function (el) {
      return $(el).val();
    },
    subscribe: function (el, callback) {
      $(el).on("change.autocomplete", function (e) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".autocomplete");
    }
  });

  Shiny.inputBindings.register(shinyMaterialAutocomplete);
});

// date picker
$(document).ready(function () {

  var shinyMaterialSelect = new Shiny.InputBinding();
  $.extend(shinyMaterialAutocomplete, {
    find: function (scope) {
      return $(scope).find(".my-shiny-date");
    },
    getValue: function (el) {
      return $(el).date();
    },
    subscribe: function (el, callback) {
      $(el).on("change.my-shiny-date", function (e) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".my-shiny-date");
    }
  });

  Shiny.inputBindings.register(shinyMaterialSelect);
});

// add options to autocomplete
$(document).ready(function(){
  var x = document.getElementById("my_autocomplete");
  $(x).autocomplete({
    data: {
      "Apple": null,
      "Microsoft": null,
      "Google": null
    },
  });
});

// initialize select
$(document).ready(function(){
  $('select').formSelect();
});

// initialize date picker
$(document).ready(function(){
  $('.datepicker').datepicker();
});

// initialize modal
$(document).ready(function(){
  $('.modal').modal();
});

// initialize tabs
$(document).ready(function(){
  $('.tabs').tabs();
});

// initialize collapsible
$(document).ready(function(){
  $('.collapsible').collapsible();
});

// initialize action button
/*$(document).ready(function(){
  $('.fixed-action-btn').floatingActionButton({
    hoverEnabled: false,
    direction: 'up'
  });
});*/
$(document).ready(function(){
  $('.fixed-action-btn').floatingActionButton();
});

// initialize tool tips
$(document).ready(function(){
  $('.tooltipped').tooltip();
});

import {MDCTextField} from '@material/textfield';
const textField = new MDCTextField(document.querySelector('.mdc-text-field'));
