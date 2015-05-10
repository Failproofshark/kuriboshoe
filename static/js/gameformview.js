GameForm.view = function() {
    var formConfiguration = {
        textAreaDisplay: function() {
            var displayValue = (GameForm.controller.formMode === "search") ? "display:none" : "display:inherit";
            return displayValue;
        },
        actionButtonDisplay: function() {
            var displayValue = (GameForm.controller.isLoading) ? "display:none" : "display:inline";
            return displayValue;
        },
        preloaderDisplay: function() {
            var displayValue = (GameForm.controller.isLoading) ? "display:inherit" : "display:none";
            return displayValue;
        },
        changeFormProperties: function() {
            var styleProperties = "cursor:pointer;";
            if (GameForm.controller.isAdmin) {
                styleProperties += "display:inherit";
            } else {
                styleProperties += "display:none";
            }
            return styleProperties;
        }
    };
    var renderSearchResults = function() {
        var renderedResults = [];
        var displayProperties = (GameForm.controller.searchLoading) ? {results: "display:none", preloader: "display:inherit"} : {results: "display:inherit", preloader: "display:none"};
        var titleCursorProperty = (GameForm.controller.isAdmin) ? "cursor:default" : "cursor:pointer";
        var renderAdminButtons = function(result) {
            var adminButtons = [];
            if (GameForm.controller.isAdmin) {
                adminButtons = m("div.col-xs-3", [
                    m("span.glyphicon.glyphicon-remove.game-search-results-button", {onclick:GameForm.controller.selectDeleteHandler.bind(GameForm.controller, result.id)}),
                    m("span.glyphicon.glyphicon-pencil.game-search-results-button", {onclick:GameForm.controller.selectUpdateHandler.bind(GameForm.controller, result.id)})
                ]);
            };
            return adminButtons;
        };
        if (!_.isEmpty(GameForm.controller.searchResults) || !_.isEmpty(GameForm.controller.noResults)) {
            renderedResults = m("div",
                                {style:displayProperties.results},
                                [m("div", GameForm.controller.noResults),
                                 _.map(GameForm.controller.searchResults, function(result, index) {
                                     var bgColor = "background-color:#CECFE0";
                                     if (index % 2 == 0) {
                                         bgColor = "background-color:#FFF";
                                     }
                                     return m("div.row.result-row", {style:bgColor},
                                              [m("div.col-xs-9",
                                                 {style:bgColor},
                                                 [
                                                     m("span", {style:titleCursorProperty, onclick:GameForm.controller.titleClickHandler.bind(GameForm.controller, result.id)}, (result.name.replace(/\\/g, '') + " [" + result.region + "] (" + result.systemName.replace(/\\/g,'') + ")")),
                                                 ]),
                                               renderAdminButtons(result)
                                              ]);
                                 }),
                                 m("img[src=/images/ajax.gif]", {style:displayProperties.preloader})
                                ]);
        }
        return renderedResults;
    };
    return [m("div.row",[
        m("div.col-xs-12",[
            m("div.text-danger", GameForm.controller.errorMessage),
            m("form", [
                m("input.form-control", {onchange: m.withAttr("value", GameForm.controller.gameForm.fields.name),
                                         value: GameForm.controller.gameForm.fields.name(),
                                         placeholder: "Name"}),
                select2.view({onchange:GameForm.controller.gameForm.fields.region,
                              value: GameForm.controller.gameForm.fields.region(),
                              select2InitializationOptions: {placeholder: "Region", allowClear: true}},
                             ["NTSC", "NTSC-J", "PAL"]),
                m("div", [
                    select2.view({onchange:GameForm.controller.gameForm.fields.systemid,
                                  value: GameForm.controller.gameForm.fields.systemid(),
                                  select2InitializationOptions: {placeholder: "System", allowClear: true}},
                                 GameForm.controller.systems),
                    m("u", {style:formConfiguration.changeFormProperties(), onclick: GameForm.controller.addSystemHandler}, "+Add System")
                ]),
                m("div", [
                    select2.view({onchange:GameForm.controller.gameForm.fields.genres,
                                  value: GameForm.controller.gameForm.fields.genres(),
                                  select2InitializationOptions: {placeholder: "Genres", allowClear: true}},
                                 GameForm.controller.genres,
                                 true),
                    m("u", {style:formConfiguration.changeFormProperties(), onclick: GameForm.controller.addGenreHandler}, "+Add Genre")
                ]),
                m("div", [
                    select2.view({onchange:GameForm.controller.gameForm.fields.companies,
                                  value: GameForm.controller.gameForm.fields.companies(),
                                  select2InitializationOptions: {placeholder: "Companies", allowClear: true}},
                                 GameForm.controller.companies,
                                 true),
                    m("u", {style:formConfiguration.changeFormProperties(), onclick: GameForm.controller.addCompanyHandler}, "+Add Company")
                ]),
                m("input.form-control", {onchange: m.withAttr("value", GameForm.controller.gameForm.fields.quantity),
                                         value: GameForm.controller.gameForm.fields.quantity(),
                                         placeholder: "Quantity"
                                        }),
                m("div", {style:formConfiguration.textAreaDisplay()}, [
                    m("p", "Short Description"),
                    m("textarea", {onchange: m.withAttr("value", GameForm.controller.gameForm.fields.blurb)}, GameForm.controller.gameForm.fields.blurb()),
                ]),
                m("div.checkbox", [
                    m("label", [
                        m("input[type=checkbox]", {onchange: m.withAttr("checked", GameForm.controller.gameForm.fields.hasmanual), checked: GameForm.controller.gameForm.fields.hasmanual()})
                    ]),
                    m("span", "Manual")
                ]),
                m("div.checkbox", [
                    m("label", [
                        m("input[type=checkbox]", {onchange: m.withAttr("checked", GameForm.controller.gameForm.fields.hasbox), checked: GameForm.controller.gameForm.fields.hasbox()})
                    ]),
                    m("span", "Box")
                ]),
                m("div", {style:formConfiguration.textAreaDisplay()}, [
                    m("p", "Notes"),
                    m("textarea", {onchange: m.withAttr("value", GameForm.controller.gameForm.fields.notes)}, GameForm.controller.gameForm.fields.notes()),
                ]),
                m("div", [
                    m("button.btn.btn-success", {style: formConfiguration.actionButtonDisplay(),
                                                 onclick: GameForm.controller.gameForm.submitHandlers[GameForm.controller.formMode]}, "submit"),
                    m("button.btn.btn-danger", {style: formConfiguration.actionButtonDisplay(),
                                                onclick: GameForm.controller.cancelButtonHandler}, "cancel"),
                    m("img[src=/images/ajax.gif]", {style: formConfiguration.preloaderDisplay()})
                ])
            ]),
        ])
    ]),
            renderSearchResults()
           ];
};
