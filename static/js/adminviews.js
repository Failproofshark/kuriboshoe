GameTrackerAdmin.screenHelpers = {};
GameTrackerAdmin.screenHelpers.createButtonDisplayProperties = function(isLoading) {
    var displayProperties = (isLoading) ? {button:"display:none", preloader:"display:inline"} : {button:"display:inline", preloader:"display:none"};
    return displayProperties;
};

GameTrackerAdmin.screenHelpers.createButtonSet = function(isLoading, whichForm) {
    var displayProperties = GameTrackerAdmin.screenHelpers.createButtonDisplayProperties(isLoading);
    return m("div", [
        m("button.btn.btn-success", {style: displayProperties.button,
                                     onclick: GameTrackerAdmin.vm[whichForm].submitHandlers[GameTrackerAdmin.vm.formMode]}, "submit"),
        m("button.btn.btn-danger", {style: displayProperties.button,
                                    onclick: GameTrackerAdmin.vm.returnToMainForm.bind(GameTrackerAdmin.vm, whichForm)}, "cancel"),
        m("img[src=/images/ajax.gif]", {style:displayProperties.preloader})
    ]);
};

GameTrackerAdmin.screenCollection = {};

GameTrackerAdmin.screenCollection.InitialScreen = function() {
    return m("div#initialAdmin", "Welcome to the Kuribo Shoe Admin Panel");
};

GameTrackerAdmin.screenCollection.SelectScreen = function() {
    var displayProperties = GameTrackerAdmin.screenHelpers.createButtonDisplayProperties(GameTrackerAdmin.vm.isLoading);
    var selectDataSet = function() {
        var dataSet = [];
        switch (GameTrackerAdmin.vm.selectScreenState) {
        case "system":
            dataSet = _.pluck(GameTrackerAdmin.vm.systems, "attributes");
            break;
        case "company":
            dataSet = _.pluck(GameTrackerAdmin.vm.companies, "attributes");
            break;
        case "genre":
            dataSet = _.pluck(GameTrackerAdmin.vm.genres, "attributes");
            break;
        };
        return dataSet;
    };
    return m("div.row",[
        m("div.col-xs-12", [
            m("form", [
                m("div", [
                    select2.view({onchange:GameTrackerAdmin.vm.currentSelectEntityId,
                                  value:GameTrackerAdmin.vm.currentSelectEntityId(),
                                  select2InitializationOptions:{placeholder:"Select an item to edit or delete"}},
                                 selectDataSet())
                ]),
                m("div", [
                    m("button.btn.btn-success", {style: displayProperties.button,
                                                 onclick: GameTrackerAdmin.vm.generalInitiateEdit}, "edit"),
                    m("button.btn.btn-danger", {style: displayProperties.button,
                                                onclick: GameTrackerAdmin.vm.generalDelete}, "delete"),
                    m("img[src=/images/ajax.gif]", {style: displayProperties.preloader})
                ])])
        ])
    ]);
};

GameTrackerAdmin.screenCollection.CompanyFormScreen = function() {
    return m("div.row",[
        m("div.col-xs-12", [
            m("form", [m("input.form-control[type=text]", {placeholder:"Company Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.companyForm.fields.name), value: GameTrackerAdmin.vm.companyForm.fields.name()}),
                       m("div.checkbox", [
                           m("label", [
                               m("input[type=checkbox]", {onchange: m.withAttr("checked", GameTrackerAdmin.vm.companyForm.fields.ismanufacturer), checked: GameTrackerAdmin.vm.companyForm.fields.ismanufacturer()})
                           ]),
                           m("span", "Is this company a console manufacuturer?")
                       ]),
                       GameTrackerAdmin.screenHelpers.createButtonSet(GameTrackerAdmin.vm.isLoading, "companyForm")
                      ])
            ])
    ]);
};

GameTrackerAdmin.screenCollection.GenreFormScreen = function() {
    return m("div.row", [
        m("div.col-xs-12", [
            m("form", [ m("input.form-control[type=text]", {placeholder:"Genre Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.genreForm.fields.name), value: GameTrackerAdmin.vm.genreForm.fields.name()}),
                        GameTrackerAdmin.screenHelpers.createButtonSet(GameTrackerAdmin.vm.isLoading, "genreForm")
                      ])
        ])
    ]);
};

GameTrackerAdmin.screenCollection.SystemFormScreen = function() {
    var manufacturerDataSet = function() {
        return _.filter(_.pluck(GameTrackerAdmin.vm.companies, "attributes"), {ismanufacturer:1});
    };
    return m("div.row", [
        m("div.col-xs-12", [
            m("form", [m("input.form-control[type=text]", {placeholder:"System Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.systemForm.fields.name), value: GameTrackerAdmin.vm.systemForm.fields.name()}),
                       m("div", [
                           select2.view({ onchange:GameTrackerAdmin.vm.systemForm.fields.manufacturerid,
                                          value:GameTrackerAdmin.vm.systemForm.fields.manufacturerid(),
                                          select2InitializationOptions:{placeholder:"Manufacturer"}},
                                        manufacturerDataSet()
                                       ),
                           m("u[style=cursor:pointer]", {onclick: GameTrackerAdmin.vm.changeToAddCompany}, "+Add Company")
                       ]),
                       GameTrackerAdmin.screenHelpers.createButtonSet(GameTrackerAdmin.vm.isLoading, "systemForm")
                      ])
        ])
    ]);
};

GameTrackerAdmin.screenCollection.GameFormScreen = GameForm.view;

GameTrackerAdmin.view = function() {
    var renderScreens = function() {
        return _.map(GameTrackerAdmin.screenCollection, function(screenContent, screenName) {
            return m("div", {style:"display:"+GameTrackerAdmin.vm.shouldDisplayScreen(screenName)}, screenContent());
        });
    };
    return [
        m("nav.navbar.navbar-default", [
            m("div.container-fluid", [
                m("div.navbar-header", [
                    m("button.navbar-toggle.collapsed[type=button][data-toggle=collapse][data-target=#main-nav]", [
                        m("span.icon-bar"),
                        m("span.icon-bar"),
                        m("span.icon-bar")
                    ])
                ]),
                m("div#main-nav.collapse.navbar-collapse",[ 
                    m("ul.nav.navbar-nav", [
                        m("li.dropdown", [m("a[href=#].dropdown-toggle[data-toggle=dropdown][role=button]", "Games"),
                                          m("ul.dropdown-menu[role=menu]", [
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "add", "game", "GameFormScreen")}, "Add Game")]),
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "search", "game", "GameFormScreen")}, "Edit/Delete Game")])
                                          ])
                                         ]),
                        m("li.dropdown", [m("a[href=#].dropdown-toggle[data-toggle=dropdown][role=button]", "Systems"),
                                          m("ul.dropdown-menu[role=menu]", [
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "add", "system", "SystemFormScreen")}, "Add System")]),
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "search", "system", "SelectScreen")}, "Edit/Delete System")])
                                          ])
                                         ]),
                        m("li.dropdown", [m("a[href=#].dropdown-toggle[data-toggle=dropdown][role=button]", "Genre"),
                                          m("ul.dropdown-menu[role=menu]", [
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "add", "genre", "GenreFormScreen")}, "Add Genre")]),
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "search", "genre", "SelectScreen")}, "Edit/Delete Genre")])
                                          ])
                                         ]),
                        m("li.dropdown", [m("a[href=#].dropdown-toggle[data-toggle=dropdown][role=button]", "Company"),
                                          m("ul.dropdown-menu[role=menu]", [
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "add", "company", "CompanyFormScreen")}, "Add Company")]),
                                              m("li", [m("a[href=#]", {onclick:GameTrackerAdmin.vm.jumpToScreen.bind(GameTrackerAdmin.vm, "search", "company", "SelectScreen")}, "Edit/Delete Company")])
                                          ])
                                         ]),
                    ])
                ]),
            ])
        ]),
        m("div.container", [
            m("div.text-success", GameTrackerAdmin.vm.successMessage),
            m("div.text-danger", GameTrackerAdmin.vm.errorMessage),
            renderScreens()
        ])
    ];
};
