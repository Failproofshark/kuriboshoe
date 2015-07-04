var GameTrackerShared = {};

GameTrackerShared.TrackerForm = function(fields) {
    this.fields = fields;
    this.populateForm = function(object) {
        var self = this;
        if (object.attributes) {
            //Used for data that's kept in a model (user selects from a drop down)
            _.map(object.attributes, function(attributeValue, attributeKey) {
                if (attributeKey !== "id") {
                    var actualValue = (_.isString(attributeValue)) ? attributeValue.replace(/\\/g,'') : attributeValue;
                    // We need to strip out any slashes recorded in the db entry
                    self.fields[attributeKey](actualValue);
                }
            });
        } else {
            //Mainly used for bootstrapped data and data that comes in from the server (used mainly with games)
            _.map(object, function(value, key) {
                var actualValue = (_.isString(value)) ? value.replace(/\\/g, '') : value;
                if (key !== "id") {
                    self.fields[key](actualValue);
                }
            });
        }
    };
    this.clearForm = _.forEach.bind(this, this.fields, function(input) {
        if (_.isString(input())) {
            input("");
        } else if (_.isArray(input())) {
            input([]);
        } else if (_.isBoolean(input())){
            input(false);
        } else {
            input(null);
        }
    });
    this.returnFields = function() {
        return _.omit(_.mapValues(this.fields, function(field) {
            var returnValue = field();
            if (_.isBoolean(returnValue)) {
                returnValue = Number(returnValue);
            }
            return returnValue;
        }), _.isNull);
    };
    this.submitHandlers = {};
    /* This will probably be refactored out in the future given the only thing that has a search is the game form
     * To keep things from complaining about a missing key we add an empty function here
     */
    this.submitHandlers.search = function() { /*empty*/ };
    this.getSubmitHandler = function(state) {
        return this.submitHandlers[state];
    };

};

var GameForm = {};

GameForm.controller = new function() {
    this.gameForm = new GameTrackerShared.TrackerForm({name: m.prop(""),
                                                       blurb: m.prop(""),
                                                       region: m.prop(""),
                                                       hasmanual: m.prop(false),
                                                       hasbox: m.prop(false),
                                                       notes: m.prop(""),
                                                       quantity: m.prop(""),
                                                       genres: m.prop([]),
                                                       companies: m.prop([]),
                                                       systemid: m.prop("")});

    this.errorMessage = "";

    this.isLoading = false;
    this.searchResults = [];

    this.noResults = "";
    this.formMode = "search";
    this.isAdmin = false;

    this.searchLoading = false;

    this.selectUpdateHandler;
    this.selectDeleteHandler;
    this.cancelButtonHandler;

    this.systems;
    this.genres;
    this.companies;

    this.addCompanyHandler = function() {};
    this.addGenreHandler = function() {};
    this.addSystemHandler = function() {};

    this.populateSelectDataSets = function(systems, genres, companies) {
        this.systems = systems;
        this.genres = genres;
        this.companies = companies;
    };

    this.adminResultButtonHandlers = function(edithandler, deletehandler)  {
        this.selectUpdateHandler = edithandler;
        this.selectDeleteHandler = deletehandler;
    };

    this.bindSubmitFormHandler = function(state, handler) {
        this.gameForm.submitHandlers[state] = handler;
    };

    this.titleClickHandler = function() {};

    this.gameForm.submitHandlers.search = function() {
        GameForm.controller.noResults = "";
        GameForm.controller.isLoading = true;
        var completedSet = _.omit(GameForm.controller.gameForm.returnFields(), function(value, key) {
            var returnValue = true;
            if (_.isBoolean(value)) {
                returnValue = !value;
            } else if (!_.isEmpty(value)) {
                returnValue = false;
            }
            return returnValue;
        });
        if (!_.isEmpty(completedSet)) {
            GameForm.controller.errorMessage = "";
            m.request({method:"post",
                       url: "/search-games-ajax/",
                       data: completedSet})
                .then(function(response) {
                    //Empty results set returns a single item array with null being that object
                    GameForm.controller.searchResults = _.remove(response.results, function(item) { return !_.isNull(item); });
                    if (GameForm.controller.searchResults.length < 1) {
                        GameForm.controller.noResults = "No matches were found";
                    }
                    GameForm.controller.isLoading = false;
                }, function() { GameForm.controller.errorMessage = "Internal Server Error";} );
        } else {
            GameForm.controller.errorMessage = "Please enter at least one search parameter";
            GameForm.controller.isLoading = false;
        }
        return false;
    };
};

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

var GameTrackerAdmin = {};

GameTrackerAdmin.Model = function(defaultEmptySet, backsideUrl) {
    return function (initialValues) {
        if (initialValues) {
            this.attributes = (_.isEmpty(initialValues.id)) ? _.extend({id:null}, _.clone(initialValues,true)) : _.clone(initialValues);
        } else {
            this.attributes = defaultEmptySet;
        }

        this.backsideUrl = backsideUrl;

        this.save = function() {
            var self = this;
            // For backend purposes
            return m.request({method: "POST",
                              url: self.backsideUrl,
                              data: _.omit(self.attributes, "id")
                             })
                .then(function(response) {
                    self.attributes.id = response.newid;
                    return response;
                });
        };

        this.update = function(newAttributes) {
            var self = this;
            _.forIn(newAttributes, function(value, key) {
                self.attributes[key] = value;
            });
            return m.request({method: "PUT",
                              url: self.backsideUrl,
                              data: self.attributes
                             });
        };

        this.remove = function() {
            var self = this;
            return m.request({method: "DELETE",
                              url: self.backsideUrl,
                              data: {id: self.attributes.id}});
        };

    };
};

GameTrackerAdmin.Company = GameTrackerAdmin.Model({id:null,
                                                   name: "",
                                                   ismanufacturer: null},
                                                  "/admin/company/");

GameTrackerAdmin.System = GameTrackerAdmin.Model({ id: null,
                                                   name: "",
                                                   manufacturerid: null },
                                                 "/admin/system/");

GameTrackerAdmin.Genre = GameTrackerAdmin.Model({ id: null,
                                                   name: "",
                                                   manufacturerid: null },
                                                 "/admin/genre/");

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

GameTrackerAdmin.vm = new function() {
    var vm = {};
    vm.init = function() {
        
        vm.formMode = "";
        vm.selectScreenState = "";
        
        //This is used as a stack;
        vm.screenHistory = ["InitialScreen"];

        vm.successMessage = "";
        vm.errorMessage = "";
        vm.reportInternalError = function() {
            vm.errorMessage = "Internal Server Error";
            vm.isLoading = false;
        };
        vm.clearMessages = function() {
            vm.successMessage = "";
            vm.errorMessage = "";
            vm.noResults = "";
        };
        vm.completeReset = function() {
            vm.clearMessages();
            vm.searchResults = [];
            vm.systemForm.clearForm();
            vm.genreForm.clearForm();
            vm.companyForm.clearForm();
            GameForm.controller.gameForm.clearForm();
            vm.currentSelectEntityId("");
        };
        
        vm.jumpToScreen = function(formMode, selectScreenState, screenName) {
            vm.completeReset();
            vm.formMode = formMode;
            GameForm.controller.formMode = formMode;
            vm.selectScreenState = selectScreenState;
            vm.screenHistory = [screenName, "InitialScreen"];
            return false;
        };

        vm.isLoading = false;
        
        //This data is actually bootstraped and the variable it's copying from is in the template
        vm.companies = _.map(companies, function(company) { return new GameTrackerAdmin.Company(company); });
        vm.genres = _.map(genres, function(genre) { return new GameTrackerAdmin.Genre(genre); });
        vm.systems = _.map(systems, function(system) { return new GameTrackerAdmin.System(system); });
        
        vm.shouldDisplayScreen = function(screenName) {
            var displayProperty = "none";
            if (!_.isEmpty(vm.screenHistory)) {
                displayProperty = (screenName === vm.screenHistory[0]) ? "inherit" : "none";
            }
            return displayProperty;
        };
        vm.createBackButton = function(callback) {
            return function() {
                callback();
                vm.screenHistory.shift();
            };
        };

        vm.returnToMainForm = function(whichForm) {
            vm[whichForm].clearForm();
            vm.screenHistory.shift();
            return false;
        };

        //This is slightly different from jumping to a screen because we may want the game form to be different since it's its own entity
        vm.generateChangeHandler = function(newScreen) {
            return function() {
                vm.screenHistory.unshift(newScreen);
                vm.formMode = "add";
            };
        };

        vm.companyForm = new GameTrackerShared.TrackerForm({name: m.prop(""),
                                                            ismanufacturer: m.prop(false)
                                                           });
        /* TODO The add functions are basically the same. There should be a good way of refactoring this either creating a funciton generator
         * or creating a child object
         */
        vm.companyForm.submitHandlers.add = function() {
            vm.isLoading = true;
            vm.clearMessages();
            if (!_.isEmpty(vm.companyForm.fields.name())) {
                var newCompany = new GameTrackerAdmin.Company(vm.companyForm.returnFields());
                newCompany.save()
                    .then(function(response) {
                        if (response.status === "success") {
                            m.startComputation();
                            vm.companies.push(newCompany);
                            GameForm.controller.companies = _.pluck(vm.companies, "attributes");
                            vm.successMessage = "The company has been added";
                            vm.companyForm.clearForm();
                            vm.isLoading = false;
                            m.endComputation();
                        } else {
                            vm.errorMessage = "Could not add the company";
                            vm.isLoading = false;
                        }
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the company";
            }
            return false;
        };

        vm.currentCompanyIndex = null;
        vm.companyForm.submitHandlers.update = function() {
            vm.isLoading = true;
            vm.clearMessages();
            if (!_.isNull(vm.currentCompanyIndex) && !_.isEmpty(vm.companyForm.fields.name())) {
                vm.companies[vm.currentCompanyIndex].update(vm.companyForm.returnFields())
                    .then(function(response) {
                        vm.successMessage = "The company has been updated";
                        vm.isLoading = false;
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the company";
                vm.isLoading = false;
            }
            return false;
        };
        
        vm.genreForm = new GameTrackerShared.TrackerForm({name: m.prop("")});
        vm.genreForm.submitHandlers.add = function() {
            vm.isLoading = true;
            vm.clearMessages();
            if (!_.isEmpty(vm.genreForm.fields.name())) {
                var newGenre = new GameTrackerAdmin.Genre(vm.genreForm.returnFields());
                newGenre.save()
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.genres.push(newGenre);
                            GameForm.controller.genres = _.pluck(vm.genres, "attributes");
                            vm.successMessage = "The genre has been added";
                            vm.genreForm.clearForm();
                            vm.isLoading = false;
                        } else {
                            vm.errorMessage = "Could not add the genre";
                            vm.isLoading = false;
                        }
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the genre";
                vm.isLoading = false;
            }
            return false;
        };
        vm.currentGenreIndex = null;
        vm.genreForm.submitHandlers.update = function() {
            vm.isLoading = true;
            vm.clearMessages();
            if (!_.isNull(vm.currentGenreIndex) && !_.isEmpty(vm.genreForm.fields.name())) {
                vm.genres[vm.currentGenreIndex].update(vm.genreForm.returnFields())
                    .then(function(response) {
                        vm.successMessage = "The genre has been updated";
                        vm.isLoading = false;
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the genre";
                vm.isLoading = false;
            };
            return false;
        };

        vm.changeToAddCompany = vm.generateChangeHandler("CompanyFormScreen");
        vm.systemForm = new GameTrackerShared.TrackerForm({name: m.prop(""),
                                            manufacturerid: m.prop("")});
        vm.systemForm.submitHandlers.add = function() {
            vm.isLoading = true;
            vm.clearMessages();
            if (!_.isEmpty(vm.systemForm.fields.name()) && !_.isEmpty(vm.systemForm.fields.manufacturerid())) {
                var newSystem = new GameTrackerAdmin.System(vm.systemForm.returnFields());
                newSystem.save()
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.systems.push(newSystem);
                            GameForm.controller.systems = _.pluck(vm.systems, "attributes");
                            vm.successMessage = "The system has been added";
                            vm.systemForm.clearForm();
                            vm.isLoading = false;
                        } else {
                            vm.errorMessage = "Could not add the game.";
                            vm.isLoading = false;
                        }
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please fill in all of the fields";
                vm.isLoading = false;
            }
            return false;
        };
        vm.currentSystemIndex = null;
        vm.systemForm.submitHandlers.update = function() {
            vm.isLoading = true;
            vm.clearMessages();
            if (!_.isNull(vm.currentSystemIndex) && !_.isEmpty(vm.systemForm.fields.name()) && !_.isEmpty(vm.systemForm.fields.manufacturerid())) {
                vm.systems[vm.currentSystemIndex].update(vm.systemForm.returnFields())
                    .then(function(response) {
                        vm.successMessage = "The system has been updated";
                        vm.isLoading = false;
                    });
            } else {
                vm.errorMessage = "Please fill in all the fields";
                vm.isLoading = false;
            }
            return false;
        };

        GameForm.controller.isAdmin = true;
        GameForm.controller.addCompanyHandler = vm.generateChangeHandler("CompanyFormScreen");
        GameForm.controller.addGenreHandler = vm.generateChangeHandler("GenreFormScreen");
        GameForm.controller.addSystemHandler = vm.generateChangeHandler("SystemFormScreen");
        GameForm.controller.populateSelectDataSets(_.pluck(vm.systems, "attributes"), _.pluck(vm.genres, "attributes"), _.pluck(vm.companies, "attributes"));
        GameForm.controller.cancelButtonHandler = function() {
            GameForm.controller.gameForm.clearForm();
            vm.screenHistory.shift();
        };
        
        GameForm.controller.bindSubmitFormHandler("add", function() {
            GameForm.controller.isLoading = true;
            if (!_.isEmpty(GameForm.controller.gameForm.fields.name()) &&
                !_.isEmpty(GameForm.controller.gameForm.fields.region()) &&
                _.isFinite(Number(GameForm.controller.gameForm.fields.systemid())) &&
                Number(GameForm.controller.gameForm.fields.systemid()) > 0 &&
                _.isFinite(Number(GameForm.controller.gameForm.fields.quantity())) &&
                Number(GameForm.controller.gameForm.fields.quantity()) > 0) {
                m.request({method: "POST",
                           url: "/admin/game/",
                           data: GameForm.controller.gameForm.returnFields()})
                    .then(function(response) {
                        GameForm.controller.gameForm.clearForm();
                        vm.successMessage = "Successfully added the game";
                        GameForm.controller.isLoading = false;
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please fill in all the fields";
                GameForm.controller.isLoading = false;
            }
            return false;
        });

        vm.currentGameId = 0;
        GameForm.controller.selectUpdateHandler = function(gameId) {
            GameForm.controller.searchLoading = true;
            if (gameId && _.isFinite(Number(gameId))) {
                /* A known limitation with the backend: things we expect to be an array may be a simple object due to the json encoder on the backend
                   not being able to encode single row results correctly
                 */
                var ensureArray = function(item) {
                    var returnValue = _.isArray(item) ? item : [item];
                    return returnValue;
                };
                //We could just use the data we retrieved from the search but let's guarantee the user with the most recent information
                m.request({method: "GET",
                           url: "/game/",
                           data: {id: Number(gameId)}
                          })
                    .then(function(response) {
                        vm.currentGameId = Number(response.id);
                        GameForm.controller.gameForm.fields.companies(_.pluck(ensureArray(response.companies), "companyId"));
                        GameForm.controller.gameForm.fields.genres(_.pluck(ensureArray(response.genres), "genreId"));
                        GameForm.controller.gameForm.populateForm(_.omit(response, ["companies", "genres"]));
                        GameForm.controller.formMode = "update";
                        GameForm.controller.searchResults = [];
                        GameForm.controller.searchLoading = false;
                    }, vm.reportInternalError);
            }
        };

        GameForm.controller.bindSubmitFormHandler("update", function() {
            GameForm.controller.isLoading = true;
            if (!_.isEmpty(GameForm.controller.gameForm.fields.name()) &&
                !_.isEmpty(GameForm.controller.gameForm.fields.region()) &&
                _.isFinite(Number(GameForm.controller.gameForm.fields.systemid())) &&
                Number(GameForm.controller.gameForm.fields.systemid()) > 0 &&
                _.isFinite(Number(GameForm.controller.gameForm.fields.quantity())) &&
                Number(GameForm.controller.gameForm.fields.quantity()) > 0) {            
                var data = _.extend({id: Number(vm.currentGameId)}, GameForm.controller.gameForm.returnFields());

                m.request({method: "PUT",
                           url: "/admin/game/",
                           data: data})
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.successMessage = "Game successfully updated";
                            GameForm.controller.isLoading = false;
                        } 
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please fill in the fields";
                GameForm.controller.isLoading = false;
            }
            return false;
        });

        GameForm.controller.selectDeleteHandler = function(gameId) {
            GameForm.controller.searchLoading = true;
            if (gameId && _.isFinite(Number(gameId))) {
                m.request({method: "DELETE",
                            url: "/admin/game/",
                            data: {id: Number(gameId)}})
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.successMessage = "The game has been deleted";
                            _.remove(GameForm.controller.searchResults, function(game) { return game.id === Number(gameId); });
                            GameForm.controller.searchLoading = false;
                        }
                    }, vm.reportInternalError);
            }
            return false;
        };

        vm.currentSelectEntityId = m.prop("");
        vm.generalInitiateEdit = function() {
            vm.clearMessages();
            if (!_.isEmpty(vm.currentSelectEntityId())) {
                vm.formMode = "update";
                switch (vm.selectScreenState) {
                case "system":
                    vm.currentSystemIndex = _.findIndex(vm.systems, {attributes: {id: Number(vm.currentSelectEntityId())}});
                    vm.systemForm.populateForm(vm.systems[vm.currentSystemIndex]);
                    vm.screenHistory.unshift("SystemFormScreen");
                    break;
                case "company":
                    vm.currentCompanyIndex = _.findIndex(vm.companies, {attributes: {id: Number(vm.currentSelectEntityId())}});
                    vm.companyForm.populateForm(vm.companies[vm.currentCompanyIndex]);
                    vm.screenHistory.unshift("CompanyFormScreen");
                    break;
                case "genre":
                    vm.currentGenreIndex = _.findIndex(vm.genres, {attributes: {id: Number(vm.currentSelectEntityId())}});
                    vm.genreForm.populateForm(vm.genres[vm.currentGenreIndex]);
                    vm.screenHistory.unshift("GenreFormScreen");
                    break;
                }
            } else {
                vm.errorMessage = "Please select an item in the dropdown";
                vm.isLoading = false;
            }
            vm.currentSelectEntityId("");
            return false;
        };
        vm.generalDelete = function() {
            vm.clearMessages();
            if (!_.isEmpty(vm.currentSelectEntityId())) {
                vm.isLoading = true;
                var currentIndex;
                switch (vm.selectScreenState) {
                case "system":
                    currentIndex = _.findIndex(vm.systems, {attributes: {id: Number(vm.currentSelectEntityId())}});
                    vm.systems[currentIndex].remove()
                        .then(function(response) {
                            if (response.status === "success") {
                                _.remove(vm.systems, {attributes: {id: Number(vm.currentSelectEntityId())}});
                                vm.successMessage = "The system has been removed";
                                vm.currentSelectEntityId("");
                                vm.isLoading = false;
                                GameForm.controller.systems = _.pluck(vm.systems, "attributes");
                            }
                        },
                              vm.reportInternalError);
                    break;
                case "company":
                    currentIndex = _.findIndex(vm.companies, {attributes: {id: Number(vm.currentSelectEntityId())}});
                    vm.companies[currentIndex].remove()
                        .then(function(response) {
                            if (response.status === "success") {
                                _.remove(vm.companies, {attributes: {id: Number(vm.currentSelectEntityId())}});
                                vm.successMessage = "The company has been removed";
                                vm.currentSelectEntityId("");
                                vm.isLoading = false;
                                GameForm.controller.companies = _.pluck(vm.companies, "attributes");
                            }
                        },
                              vm.reportInternalError);
                    break;
                case "genre":
                    currentIndex = _.findIndex(vm.genres, {attributes: {id: Number(vm.currentSelectEntityId())}});
                    vm.genres[currentIndex].remove()
                        .then(function(response) {
                            if (response.status === "success") {
                                _.remove(vm.genres, {attributes: {id: Number(vm.currentSelectEntityId())}});
                                vm.successMessage = "The genre has been removed";
                                vm.currentSelectEntityId("");
                                vm.isLoading = false;
                                GameForm.controller.genres = _.pluck(vm.genres, "attributes");
                            }
                        },
                              vm.reportInternalError);
                    break;                
                };
            } else {
                vm.messageError = "Select an item from the dropdown";
            }
            return false;
        };
    };

    return vm;
};

GameTrackerAdmin.controller = function() {
    GameTrackerAdmin.vm.init();
};

//For use with all Views. Code is based on the one found on mithril's site https://lhorie.github.io/mithril/integration.html
var select2= {};

/* This factory function offers a nice closure for anything extra we want to pass in */
select2.config = function(extraArguments) {
    return function(element, isInitialized, controller) {
        var el = $(element);
        if (!isInitialized) {
            if (extraArguments.select2InitializationOptions) {
                el.select2(extraArguments.select2InitializationOptions);
            } else {
                el.select2();
            }
            el.change(function() {
                m.startComputation();
                extraArguments.onchange(el.select2("val"));
                m.endComputation();
            });
        }
        el.select2("val", extraArguments.value);
    };
};

select2.view = function(extraArguments, optionSet, isMultiple) {
    var selector = (isMultiple) ? "select.form-control[multiple=true]" : "select.form-control";
    var sortedOptions = _.sortBy(optionSet, function(value) {
        var returnValue = value;
        if (_.isObject(value)) {
            returnValue = value.name.toLowerCase().replace(/\\/g,'');
        }
        return returnValue;
    });
    var createOptionSet = function() {
        var options = [];
        if (optionSet) {
            options = _.map(sortedOptions, function(value) {
                var returnValue = (_.isObject(value)) ? m("option", {value: value.id}, value.name) : m("option", value);
                return returnValue;
            });
        }
        return options;
    };
    return m(selector, {config:select2.config(extraArguments)},
             [m("option"),createOptionSet()]);
};











m.module(document.body, GameTrackerAdmin);

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImdhbWV0cmFja2Vyc2hhcmVkLmpzIiwiZ2FtZWZvcm0uanMiLCJnYW1lZm9ybXZpZXcuanMiLCJhZG1pbm1vZGVscy5qcyIsImFkbWludmlld3MuanMiLCJhZG1pbnZtY29udHJvbGxlci5qcyIsInNlbGVjdDJtaXRocmlsLmpzIiwiZ2FtZXRyYWNrZXJhZG1pbi5qcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDdkRBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3RGQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3BJQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQzVEQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ2xLQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3JaQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQzdDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0EiLCJmaWxlIjoiYWRtaW4uanMiLCJzb3VyY2VzQ29udGVudCI6WyJ2YXIgR2FtZVRyYWNrZXJTaGFyZWQgPSB7fTtcblxuR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0gPSBmdW5jdGlvbihmaWVsZHMpIHtcbiAgICB0aGlzLmZpZWxkcyA9IGZpZWxkcztcbiAgICB0aGlzLnBvcHVsYXRlRm9ybSA9IGZ1bmN0aW9uKG9iamVjdCkge1xuICAgICAgICB2YXIgc2VsZiA9IHRoaXM7XG4gICAgICAgIGlmIChvYmplY3QuYXR0cmlidXRlcykge1xuICAgICAgICAgICAgLy9Vc2VkIGZvciBkYXRhIHRoYXQncyBrZXB0IGluIGEgbW9kZWwgKHVzZXIgc2VsZWN0cyBmcm9tIGEgZHJvcCBkb3duKVxuICAgICAgICAgICAgXy5tYXAob2JqZWN0LmF0dHJpYnV0ZXMsIGZ1bmN0aW9uKGF0dHJpYnV0ZVZhbHVlLCBhdHRyaWJ1dGVLZXkpIHtcbiAgICAgICAgICAgICAgICBpZiAoYXR0cmlidXRlS2V5ICE9PSBcImlkXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgdmFyIGFjdHVhbFZhbHVlID0gKF8uaXNTdHJpbmcoYXR0cmlidXRlVmFsdWUpKSA/IGF0dHJpYnV0ZVZhbHVlLnJlcGxhY2UoL1xcXFwvZywnJykgOiBhdHRyaWJ1dGVWYWx1ZTtcbiAgICAgICAgICAgICAgICAgICAgLy8gV2UgbmVlZCB0byBzdHJpcCBvdXQgYW55IHNsYXNoZXMgcmVjb3JkZWQgaW4gdGhlIGRiIGVudHJ5XG4gICAgICAgICAgICAgICAgICAgIHNlbGYuZmllbGRzW2F0dHJpYnV0ZUtleV0oYWN0dWFsVmFsdWUpO1xuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIH0pO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgLy9NYWlubHkgdXNlZCBmb3IgYm9vdHN0cmFwcGVkIGRhdGEgYW5kIGRhdGEgdGhhdCBjb21lcyBpbiBmcm9tIHRoZSBzZXJ2ZXIgKHVzZWQgbWFpbmx5IHdpdGggZ2FtZXMpXG4gICAgICAgICAgICBfLm1hcChvYmplY3QsIGZ1bmN0aW9uKHZhbHVlLCBrZXkpIHtcbiAgICAgICAgICAgICAgICB2YXIgYWN0dWFsVmFsdWUgPSAoXy5pc1N0cmluZyh2YWx1ZSkpID8gdmFsdWUucmVwbGFjZSgvXFxcXC9nLCAnJykgOiB2YWx1ZTtcbiAgICAgICAgICAgICAgICBpZiAoa2V5ICE9PSBcImlkXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgc2VsZi5maWVsZHNba2V5XShhY3R1YWxWYWx1ZSk7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICB9O1xuICAgIHRoaXMuY2xlYXJGb3JtID0gXy5mb3JFYWNoLmJpbmQodGhpcywgdGhpcy5maWVsZHMsIGZ1bmN0aW9uKGlucHV0KSB7XG4gICAgICAgIGlmIChfLmlzU3RyaW5nKGlucHV0KCkpKSB7XG4gICAgICAgICAgICBpbnB1dChcIlwiKTtcbiAgICAgICAgfSBlbHNlIGlmIChfLmlzQXJyYXkoaW5wdXQoKSkpIHtcbiAgICAgICAgICAgIGlucHV0KFtdKTtcbiAgICAgICAgfSBlbHNlIGlmIChfLmlzQm9vbGVhbihpbnB1dCgpKSl7XG4gICAgICAgICAgICBpbnB1dChmYWxzZSk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBpbnB1dChudWxsKTtcbiAgICAgICAgfVxuICAgIH0pO1xuICAgIHRoaXMucmV0dXJuRmllbGRzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBfLm9taXQoXy5tYXBWYWx1ZXModGhpcy5maWVsZHMsIGZ1bmN0aW9uKGZpZWxkKSB7XG4gICAgICAgICAgICB2YXIgcmV0dXJuVmFsdWUgPSBmaWVsZCgpO1xuICAgICAgICAgICAgaWYgKF8uaXNCb29sZWFuKHJldHVyblZhbHVlKSkge1xuICAgICAgICAgICAgICAgIHJldHVyblZhbHVlID0gTnVtYmVyKHJldHVyblZhbHVlKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiByZXR1cm5WYWx1ZTtcbiAgICAgICAgfSksIF8uaXNOdWxsKTtcbiAgICB9O1xuICAgIHRoaXMuc3VibWl0SGFuZGxlcnMgPSB7fTtcbiAgICAvKiBUaGlzIHdpbGwgcHJvYmFibHkgYmUgcmVmYWN0b3JlZCBvdXQgaW4gdGhlIGZ1dHVyZSBnaXZlbiB0aGUgb25seSB0aGluZyB0aGF0IGhhcyBhIHNlYXJjaCBpcyB0aGUgZ2FtZSBmb3JtXG4gICAgICogVG8ga2VlcCB0aGluZ3MgZnJvbSBjb21wbGFpbmluZyBhYm91dCBhIG1pc3Npbmcga2V5IHdlIGFkZCBhbiBlbXB0eSBmdW5jdGlvbiBoZXJlXG4gICAgICovXG4gICAgdGhpcy5zdWJtaXRIYW5kbGVycy5zZWFyY2ggPSBmdW5jdGlvbigpIHsgLyplbXB0eSovIH07XG4gICAgdGhpcy5nZXRTdWJtaXRIYW5kbGVyID0gZnVuY3Rpb24oc3RhdGUpIHtcbiAgICAgICAgcmV0dXJuIHRoaXMuc3VibWl0SGFuZGxlcnNbc3RhdGVdO1xuICAgIH07XG5cbn07XG4iLCJ2YXIgR2FtZUZvcm0gPSB7fTtcblxuR2FtZUZvcm0uY29udHJvbGxlciA9IG5ldyBmdW5jdGlvbigpIHtcbiAgICB0aGlzLmdhbWVGb3JtID0gbmV3IEdhbWVUcmFja2VyU2hhcmVkLlRyYWNrZXJGb3JtKHtuYW1lOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmx1cmI6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZWdpb246IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBoYXNtYW51YWw6IG0ucHJvcChmYWxzZSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaGFzYm94OiBtLnByb3AoZmFsc2UpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5vdGVzOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcXVhbnRpdHk6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBnZW5yZXM6IG0ucHJvcChbXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29tcGFuaWVzOiBtLnByb3AoW10pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN5c3RlbWlkOiBtLnByb3AoXCJcIil9KTtcblxuICAgIHRoaXMuZXJyb3JNZXNzYWdlID0gXCJcIjtcblxuICAgIHRoaXMuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgdGhpcy5zZWFyY2hSZXN1bHRzID0gW107XG5cbiAgICB0aGlzLm5vUmVzdWx0cyA9IFwiXCI7XG4gICAgdGhpcy5mb3JtTW9kZSA9IFwic2VhcmNoXCI7XG4gICAgdGhpcy5pc0FkbWluID0gZmFsc2U7XG5cbiAgICB0aGlzLnNlYXJjaExvYWRpbmcgPSBmYWxzZTtcblxuICAgIHRoaXMuc2VsZWN0VXBkYXRlSGFuZGxlcjtcbiAgICB0aGlzLnNlbGVjdERlbGV0ZUhhbmRsZXI7XG4gICAgdGhpcy5jYW5jZWxCdXR0b25IYW5kbGVyO1xuXG4gICAgdGhpcy5zeXN0ZW1zO1xuICAgIHRoaXMuZ2VucmVzO1xuICAgIHRoaXMuY29tcGFuaWVzO1xuXG4gICAgdGhpcy5hZGRDb21wYW55SGFuZGxlciA9IGZ1bmN0aW9uKCkge307XG4gICAgdGhpcy5hZGRHZW5yZUhhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuICAgIHRoaXMuYWRkU3lzdGVtSGFuZGxlciA9IGZ1bmN0aW9uKCkge307XG5cbiAgICB0aGlzLnBvcHVsYXRlU2VsZWN0RGF0YVNldHMgPSBmdW5jdGlvbihzeXN0ZW1zLCBnZW5yZXMsIGNvbXBhbmllcykge1xuICAgICAgICB0aGlzLnN5c3RlbXMgPSBzeXN0ZW1zO1xuICAgICAgICB0aGlzLmdlbnJlcyA9IGdlbnJlcztcbiAgICAgICAgdGhpcy5jb21wYW5pZXMgPSBjb21wYW5pZXM7XG4gICAgfTtcblxuICAgIHRoaXMuYWRtaW5SZXN1bHRCdXR0b25IYW5kbGVycyA9IGZ1bmN0aW9uKGVkaXRoYW5kbGVyLCBkZWxldGVoYW5kbGVyKSAge1xuICAgICAgICB0aGlzLnNlbGVjdFVwZGF0ZUhhbmRsZXIgPSBlZGl0aGFuZGxlcjtcbiAgICAgICAgdGhpcy5zZWxlY3REZWxldGVIYW5kbGVyID0gZGVsZXRlaGFuZGxlcjtcbiAgICB9O1xuXG4gICAgdGhpcy5iaW5kU3VibWl0Rm9ybUhhbmRsZXIgPSBmdW5jdGlvbihzdGF0ZSwgaGFuZGxlcikge1xuICAgICAgICB0aGlzLmdhbWVGb3JtLnN1Ym1pdEhhbmRsZXJzW3N0YXRlXSA9IGhhbmRsZXI7XG4gICAgfTtcblxuICAgIHRoaXMudGl0bGVDbGlja0hhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuXG4gICAgdGhpcy5nYW1lRm9ybS5zdWJtaXRIYW5kbGVycy5zZWFyY2ggPSBmdW5jdGlvbigpIHtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5ub1Jlc3VsdHMgPSBcIlwiO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgIHZhciBjb21wbGV0ZWRTZXQgPSBfLm9taXQoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5yZXR1cm5GaWVsZHMoKSwgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChfLmlzQm9vbGVhbih2YWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9ICF2YWx1ZTtcbiAgICAgICAgICAgIH0gZWxzZSBpZiAoIV8uaXNFbXB0eSh2YWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgICAgICB9KTtcbiAgICAgICAgaWYgKCFfLmlzRW1wdHkoY29tcGxldGVkU2V0KSkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5lcnJvck1lc3NhZ2UgPSBcIlwiO1xuICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6XCJwb3N0XCIsXG4gICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvc2VhcmNoLWdhbWVzLWFqYXgvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IGNvbXBsZXRlZFNldH0pXG4gICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgLy9FbXB0eSByZXN1bHRzIHNldCByZXR1cm5zIGEgc2luZ2xlIGl0ZW0gYXJyYXkgd2l0aCBudWxsIGJlaW5nIHRoYXQgb2JqZWN0XG4gICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cyA9IF8ucmVtb3ZlKHJlc3BvbnNlLnJlc3VsdHMsIGZ1bmN0aW9uKGl0ZW0pIHsgcmV0dXJuICFfLmlzTnVsbChpdGVtKTsgfSk7XG4gICAgICAgICAgICAgICAgICAgIGlmIChHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMubGVuZ3RoIDwgMSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5ub1Jlc3VsdHMgPSBcIk5vIG1hdGNoZXMgd2VyZSBmb3VuZFwiO1xuICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgfSwgZnVuY3Rpb24oKSB7IEdhbWVGb3JtLmNvbnRyb2xsZXIuZXJyb3JNZXNzYWdlID0gXCJJbnRlcm5hbCBTZXJ2ZXIgRXJyb3JcIjt9ICk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIGF0IGxlYXN0IG9uZSBzZWFyY2ggcGFyYW1ldGVyXCI7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9O1xufTtcbiIsIkdhbWVGb3JtLnZpZXcgPSBmdW5jdGlvbigpIHtcbiAgICB2YXIgZm9ybUNvbmZpZ3VyYXRpb24gPSB7XG4gICAgICAgIHRleHRBcmVhRGlzcGxheTogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVZhbHVlID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGUgPT09IFwic2VhcmNoXCIpID8gXCJkaXNwbGF5Om5vbmVcIiA6IFwiZGlzcGxheTppbmhlcml0XCI7XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVZhbHVlO1xuICAgICAgICB9LFxuICAgICAgICBhY3Rpb25CdXR0b25EaXNwbGF5OiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBkaXNwbGF5VmFsdWUgPSAoR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcpID8gXCJkaXNwbGF5Om5vbmVcIiA6IFwiZGlzcGxheTppbmxpbmVcIjtcbiAgICAgICAgICAgIHJldHVybiBkaXNwbGF5VmFsdWU7XG4gICAgICAgIH0sXG4gICAgICAgIHByZWxvYWRlckRpc3BsYXk6IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlWYWx1ZSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZykgPyBcImRpc3BsYXk6aW5oZXJpdFwiIDogXCJkaXNwbGF5Om5vbmVcIjtcbiAgICAgICAgICAgIHJldHVybiBkaXNwbGF5VmFsdWU7XG4gICAgICAgIH0sXG4gICAgICAgIGNoYW5nZUZvcm1Qcm9wZXJ0aWVzOiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBzdHlsZVByb3BlcnRpZXMgPSBcImN1cnNvcjpwb2ludGVyO1wiO1xuICAgICAgICAgICAgaWYgKEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNBZG1pbikge1xuICAgICAgICAgICAgICAgIHN0eWxlUHJvcGVydGllcyArPSBcImRpc3BsYXk6aW5oZXJpdFwiO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICBzdHlsZVByb3BlcnRpZXMgKz0gXCJkaXNwbGF5Om5vbmVcIjtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBzdHlsZVByb3BlcnRpZXM7XG4gICAgICAgIH1cbiAgICB9O1xuICAgIHZhciByZW5kZXJTZWFyY2hSZXN1bHRzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHZhciByZW5kZXJlZFJlc3VsdHMgPSBbXTtcbiAgICAgICAgdmFyIGRpc3BsYXlQcm9wZXJ0aWVzID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoTG9hZGluZykgPyB7cmVzdWx0czogXCJkaXNwbGF5Om5vbmVcIiwgcHJlbG9hZGVyOiBcImRpc3BsYXk6aW5oZXJpdFwifSA6IHtyZXN1bHRzOiBcImRpc3BsYXk6aW5oZXJpdFwiLCBwcmVsb2FkZXI6IFwiZGlzcGxheTpub25lXCJ9O1xuICAgICAgICB2YXIgdGl0bGVDdXJzb3JQcm9wZXJ0eSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmlzQWRtaW4pID8gXCJjdXJzb3I6ZGVmYXVsdFwiIDogXCJjdXJzb3I6cG9pbnRlclwiO1xuICAgICAgICB2YXIgcmVuZGVyQWRtaW5CdXR0b25zID0gZnVuY3Rpb24ocmVzdWx0KSB7XG4gICAgICAgICAgICB2YXIgYWRtaW5CdXR0b25zID0gW107XG4gICAgICAgICAgICBpZiAoR2FtZUZvcm0uY29udHJvbGxlci5pc0FkbWluKSB7XG4gICAgICAgICAgICAgICAgYWRtaW5CdXR0b25zID0gbShcImRpdi5jb2wteHMtM1wiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmdseXBoaWNvbi5nbHlwaGljb24tcmVtb3ZlLmdhbWUtc2VhcmNoLXJlc3VsdHMtYnV0dG9uXCIsIHtvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0RGVsZXRlSGFuZGxlci5iaW5kKEdhbWVGb3JtLmNvbnRyb2xsZXIsIHJlc3VsdC5pZCl9KSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uZ2x5cGhpY29uLmdseXBoaWNvbi1wZW5jaWwuZ2FtZS1zZWFyY2gtcmVzdWx0cy1idXR0b25cIiwge29uY2xpY2s6R2FtZUZvcm0uY29udHJvbGxlci5zZWxlY3RVcGRhdGVIYW5kbGVyLmJpbmQoR2FtZUZvcm0uY29udHJvbGxlciwgcmVzdWx0LmlkKX0pXG4gICAgICAgICAgICAgICAgXSk7XG4gICAgICAgICAgICB9O1xuICAgICAgICAgICAgcmV0dXJuIGFkbWluQnV0dG9ucztcbiAgICAgICAgfTtcbiAgICAgICAgaWYgKCFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzKSB8fCAhXy5pc0VtcHR5KEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSkge1xuICAgICAgICAgICAgcmVuZGVyZWRSZXN1bHRzID0gbShcImRpdlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7c3R5bGU6ZGlzcGxheVByb3BlcnRpZXMucmVzdWx0c30sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFttKFwiZGl2XCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ubWFwKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cywgZnVuY3Rpb24ocmVzdWx0LCBpbmRleCkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhciBiZ0NvbG9yID0gXCJiYWNrZ3JvdW5kLWNvbG9yOiNDRUNGRTBcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAoaW5kZXggJSAyID09IDApIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmdDb2xvciA9IFwiYmFja2dyb3VuZC1jb2xvcjojRkZGXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJldHVybiBtKFwiZGl2LnJvdy5yZXN1bHQtcm93XCIsIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbbShcImRpdi5jb2wteHMtOVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIHtzdHlsZTp0aXRsZUN1cnNvclByb3BlcnR5LCBvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIudGl0bGVDbGlja0hhbmRsZXIuYmluZChHYW1lRm9ybS5jb250cm9sbGVyLCByZXN1bHQuaWQpfSwgKHJlc3VsdC5uYW1lLnJlcGxhY2UoL1xcXFwvZywgJycpICsgXCIgW1wiICsgcmVzdWx0LnJlZ2lvbiArIFwiXSAoXCIgKyByZXN1bHQuc3lzdGVtTmFtZS5yZXBsYWNlKC9cXFxcL2csJycpICsgXCIpXCIpKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcmVuZGVyQWRtaW5CdXR0b25zKHJlc3VsdClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImltZ1tzcmM9L2ltYWdlcy9hamF4LmdpZl1cIiwge3N0eWxlOmRpc3BsYXlQcm9wZXJ0aWVzLnByZWxvYWRlcn0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiByZW5kZXJlZFJlc3VsdHM7XG4gICAgfTtcbiAgICByZXR1cm4gW20oXCJkaXYucm93XCIsW1xuICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLFtcbiAgICAgICAgICAgIG0oXCJkaXYudGV4dC1kYW5nZXJcIiwgR2FtZUZvcm0uY29udHJvbGxlci5lcnJvck1lc3NhZ2UpLFxuICAgICAgICAgICAgbShcImZvcm1cIiwgW1xuICAgICAgICAgICAgICAgIG0oXCJpbnB1dC5mb3JtLWNvbnRyb2xcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubmFtZSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5uYW1lKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHBsYWNlaG9sZGVyOiBcIk5hbWVcIn0pLFxuICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucmVnaW9uLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbigpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9uczoge3BsYWNlaG9sZGVyOiBcIlJlZ2lvblwiLCBhbGxvd0NsZWFyOiB0cnVlfX0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIFtcIk5UU0NcIiwgXCJOVFNDLUpcIiwgXCJQQUxcIl0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJTeXN0ZW1cIiwgYWxsb3dDbGVhcjogdHJ1ZX19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zeXN0ZW1zKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInVcIiwge3N0eWxlOmZvcm1Db25maWd1cmF0aW9uLmNoYW5nZUZvcm1Qcm9wZXJ0aWVzKCksIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuYWRkU3lzdGVtSGFuZGxlcn0sIFwiK0FkZCBTeXN0ZW1cIilcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHtvbmNoYW5nZTpHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmdlbnJlcygpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJHZW5yZXNcIiwgYWxsb3dDbGVhcjogdHJ1ZX19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nZW5yZXMsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0cnVlKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInVcIiwge3N0eWxlOmZvcm1Db25maWd1cmF0aW9uLmNoYW5nZUZvcm1Qcm9wZXJ0aWVzKCksIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuYWRkR2VucmVIYW5kbGVyfSwgXCIrQWRkIEdlbnJlXCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuY29tcGFuaWVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5jb21wYW5pZXMoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiQ29tcGFuaWVzXCIsIGFsbG93Q2xlYXI6IHRydWV9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuY29tcGFuaWVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdHJ1ZSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ1XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi5jaGFuZ2VGb3JtUHJvcGVydGllcygpLCBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmFkZENvbXBhbnlIYW5kbGVyfSwgXCIrQWRkIENvbXBhbnlcIilcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiaW5wdXQuZm9ybS1jb250cm9sXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHBsYWNlaG9sZGVyOiBcIlF1YW50aXR5XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi50ZXh0QXJlYURpc3BsYXkoKX0sIFtcbiAgICAgICAgICAgICAgICAgICAgbShcInBcIiwgXCJTaG9ydCBEZXNjcmlwdGlvblwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInRleHRhcmVhXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmJsdXJiKX0sIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmJsdXJiKCkpLFxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXYuY2hlY2tib3hcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwibGFiZWxcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcImlucHV0W3R5cGU9Y2hlY2tib3hdXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcImNoZWNrZWRcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuaGFzbWFudWFsKSwgY2hlY2tlZDogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuaGFzbWFudWFsKCl9KVxuICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW5cIiwgXCJNYW51YWxcIilcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2LmNoZWNrYm94XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImxhYmVsXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbnB1dFt0eXBlPWNoZWNrYm94XVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJjaGVja2VkXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc2JveCksIGNoZWNrZWQ6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc2JveCgpfSlcbiAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiQm94XCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCB7c3R5bGU6Zm9ybUNvbmZpZ3VyYXRpb24udGV4dEFyZWFEaXNwbGF5KCl9LCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJwXCIsIFwiTm90ZXNcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ0ZXh0YXJlYVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ub3Rlcyl9LCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ub3RlcygpKSxcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLXN1Y2Nlc3NcIiwge3N0eWxlOiBmb3JtQ29uZmlndXJhdGlvbi5hY3Rpb25CdXR0b25EaXNwbGF5KCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5zdWJtaXRIYW5kbGVyc1tHYW1lRm9ybS5jb250cm9sbGVyLmZvcm1Nb2RlXX0sIFwic3VibWl0XCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tZGFuZ2VyXCIsIHtzdHlsZTogZm9ybUNvbmZpZ3VyYXRpb24uYWN0aW9uQnV0dG9uRGlzcGxheSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZUZvcm0uY29udHJvbGxlci5jYW5jZWxCdXR0b25IYW5kbGVyfSwgXCJjYW5jZWxcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTogZm9ybUNvbmZpZ3VyYXRpb24ucHJlbG9hZGVyRGlzcGxheSgpfSlcbiAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgXSksXG4gICAgICAgIF0pXG4gICAgXSksXG4gICAgICAgICAgICByZW5kZXJTZWFyY2hSZXN1bHRzKClcbiAgICAgICAgICAgXTtcbn07XG4iLCJ2YXIgR2FtZVRyYWNrZXJBZG1pbiA9IHt9O1xuXG5HYW1lVHJhY2tlckFkbWluLk1vZGVsID0gZnVuY3Rpb24oZGVmYXVsdEVtcHR5U2V0LCBiYWNrc2lkZVVybCkge1xuICAgIHJldHVybiBmdW5jdGlvbiAoaW5pdGlhbFZhbHVlcykge1xuICAgICAgICBpZiAoaW5pdGlhbFZhbHVlcykge1xuICAgICAgICAgICAgdGhpcy5hdHRyaWJ1dGVzID0gKF8uaXNFbXB0eShpbml0aWFsVmFsdWVzLmlkKSkgPyBfLmV4dGVuZCh7aWQ6bnVsbH0sIF8uY2xvbmUoaW5pdGlhbFZhbHVlcyx0cnVlKSkgOiBfLmNsb25lKGluaXRpYWxWYWx1ZXMpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgdGhpcy5hdHRyaWJ1dGVzID0gZGVmYXVsdEVtcHR5U2V0O1xuICAgICAgICB9XG5cbiAgICAgICAgdGhpcy5iYWNrc2lkZVVybCA9IGJhY2tzaWRlVXJsO1xuXG4gICAgICAgIHRoaXMuc2F2ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIHNlbGYgPSB0aGlzO1xuICAgICAgICAgICAgLy8gRm9yIGJhY2tlbmQgcHVycG9zZXNcbiAgICAgICAgICAgIHJldHVybiBtLnJlcXVlc3Qoe21ldGhvZDogXCJQT1NUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IHNlbGYuYmFja3NpZGVVcmwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBfLm9taXQoc2VsZi5hdHRyaWJ1dGVzLCBcImlkXCIpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pXG4gICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgc2VsZi5hdHRyaWJ1dGVzLmlkID0gcmVzcG9uc2UubmV3aWQ7XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiByZXNwb25zZTtcbiAgICAgICAgICAgICAgICB9KTtcbiAgICAgICAgfTtcblxuICAgICAgICB0aGlzLnVwZGF0ZSA9IGZ1bmN0aW9uKG5ld0F0dHJpYnV0ZXMpIHtcbiAgICAgICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgICAgIF8uZm9ySW4obmV3QXR0cmlidXRlcywgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgICAgIHNlbGYuYXR0cmlidXRlc1trZXldID0gdmFsdWU7XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgICAgIHJldHVybiBtLnJlcXVlc3Qoe21ldGhvZDogXCJQVVRcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogc2VsZi5iYWNrc2lkZVVybCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IHNlbGYuYXR0cmlidXRlc1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KTtcbiAgICAgICAgfTtcblxuICAgICAgICB0aGlzLnJlbW92ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIHNlbGYgPSB0aGlzO1xuICAgICAgICAgICAgcmV0dXJuIG0ucmVxdWVzdCh7bWV0aG9kOiBcIkRFTEVURVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBzZWxmLmJhY2tzaWRlVXJsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YToge2lkOiBzZWxmLmF0dHJpYnV0ZXMuaWR9fSk7XG4gICAgICAgIH07XG5cbiAgICB9O1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5Db21wYW55ID0gR2FtZVRyYWNrZXJBZG1pbi5Nb2RlbCh7aWQ6bnVsbCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5hbWU6IFwiXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpc21hbnVmYWN0dXJlcjogbnVsbH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiL2FkbWluL2NvbXBhbnkvXCIpO1xuXG5HYW1lVHJhY2tlckFkbWluLlN5c3RlbSA9IEdhbWVUcmFja2VyQWRtaW4uTW9kZWwoeyBpZDogbnVsbCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5hbWU6IFwiXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtYW51ZmFjdHVyZXJpZDogbnVsbCB9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiL2FkbWluL3N5c3RlbS9cIik7XG5cbkdhbWVUcmFja2VyQWRtaW4uR2VucmUgPSBHYW1lVHJhY2tlckFkbWluLk1vZGVsKHsgaWQ6IG51bGwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lOiBcIlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbWFudWZhY3R1cmVyaWQ6IG51bGwgfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIi9hZG1pbi9nZW5yZS9cIik7XG4iLCJHYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMgPSB7fTtcbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25EaXNwbGF5UHJvcGVydGllcyA9IGZ1bmN0aW9uKGlzTG9hZGluZykge1xuICAgIHZhciBkaXNwbGF5UHJvcGVydGllcyA9IChpc0xvYWRpbmcpID8ge2J1dHRvbjpcImRpc3BsYXk6bm9uZVwiLCBwcmVsb2FkZXI6XCJkaXNwbGF5OmlubGluZVwifSA6IHtidXR0b246XCJkaXNwbGF5OmlubGluZVwiLCBwcmVsb2FkZXI6XCJkaXNwbGF5Om5vbmVcIn07XG4gICAgcmV0dXJuIGRpc3BsYXlQcm9wZXJ0aWVzO1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvblNldCA9IGZ1bmN0aW9uKGlzTG9hZGluZywgd2hpY2hGb3JtKSB7XG4gICAgdmFyIGRpc3BsYXlQcm9wZXJ0aWVzID0gR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvbkRpc3BsYXlQcm9wZXJ0aWVzKGlzTG9hZGluZyk7XG4gICAgcmV0dXJuIG0oXCJkaXZcIiwgW1xuICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tc3VjY2Vzc1wiLCB7c3R5bGU6IGRpc3BsYXlQcm9wZXJ0aWVzLmJ1dHRvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lVHJhY2tlckFkbWluLnZtW3doaWNoRm9ybV0uc3VibWl0SGFuZGxlcnNbR2FtZVRyYWNrZXJBZG1pbi52bS5mb3JtTW9kZV19LCBcInN1Ym1pdFwiKSxcbiAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLWRhbmdlclwiLCB7c3R5bGU6IGRpc3BsYXlQcm9wZXJ0aWVzLmJ1dHRvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9uY2xpY2s6IEdhbWVUcmFja2VyQWRtaW4udm0ucmV0dXJuVG9NYWluRm9ybS5iaW5kKEdhbWVUcmFja2VyQWRtaW4udm0sIHdoaWNoRm9ybSl9LCBcImNhbmNlbFwiKSxcbiAgICAgICAgbShcImltZ1tzcmM9L2ltYWdlcy9hamF4LmdpZl1cIiwge3N0eWxlOmRpc3BsYXlQcm9wZXJ0aWVzLnByZWxvYWRlcn0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24gPSB7fTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uLkluaXRpYWxTY3JlZW4gPSBmdW5jdGlvbigpIHtcbiAgICByZXR1cm4gbShcImRpdiNpbml0aWFsQWRtaW5cIiwgXCJXZWxjb21lIHRvIHRoZSBLdXJpYm8gU2hvZSBBZG1pbiBQYW5lbFwiKTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5TZWxlY3RTY3JlZW4gPSBmdW5jdGlvbigpIHtcbiAgICB2YXIgZGlzcGxheVByb3BlcnRpZXMgPSBHYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uRGlzcGxheVByb3BlcnRpZXMoR2FtZVRyYWNrZXJBZG1pbi52bS5pc0xvYWRpbmcpO1xuICAgIHZhciBzZWxlY3REYXRhU2V0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHZhciBkYXRhU2V0ID0gW107XG4gICAgICAgIHN3aXRjaCAoR2FtZVRyYWNrZXJBZG1pbi52bS5zZWxlY3RTY3JlZW5TdGF0ZSkge1xuICAgICAgICBjYXNlIFwic3lzdGVtXCI6XG4gICAgICAgICAgICBkYXRhU2V0ID0gXy5wbHVjayhHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbXMsIFwiYXR0cmlidXRlc1wiKTtcbiAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICBjYXNlIFwiY29tcGFueVwiOlxuICAgICAgICAgICAgZGF0YVNldCA9IF8ucGx1Y2soR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW5pZXMsIFwiYXR0cmlidXRlc1wiKTtcbiAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICBjYXNlIFwiZ2VucmVcIjpcbiAgICAgICAgICAgIGRhdGFTZXQgPSBfLnBsdWNrKEdhbWVUcmFja2VyQWRtaW4udm0uZ2VucmVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICBicmVhaztcbiAgICAgICAgfTtcbiAgICAgICAgcmV0dXJuIGRhdGFTZXQ7XG4gICAgfTtcbiAgICByZXR1cm4gbShcImRpdi5yb3dcIixbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFtcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHtvbmNoYW5nZTpHYW1lVHJhY2tlckFkbWluLnZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTpHYW1lVHJhY2tlckFkbWluLnZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6e3BsYWNlaG9sZGVyOlwiU2VsZWN0IGFuIGl0ZW0gdG8gZWRpdCBvciBkZWxldGVcIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0RGF0YVNldCgpKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tc3VjY2Vzc1wiLCB7c3R5bGU6IGRpc3BsYXlQcm9wZXJ0aWVzLmJ1dHRvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lVHJhY2tlckFkbWluLnZtLmdlbmVyYWxJbml0aWF0ZUVkaXR9LCBcImVkaXRcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1kYW5nZXJcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lVHJhY2tlckFkbWluLnZtLmdlbmVyYWxEZWxldGV9LCBcImRlbGV0ZVwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcImltZ1tzcmM9L2ltYWdlcy9hamF4LmdpZl1cIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgICAgICAgICAgICAgIF0pXSlcbiAgICAgICAgXSlcbiAgICBdKTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5Db21wYW55Rm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBtKFwiZGl2LnJvd1wiLFtcbiAgICAgICAgbShcImRpdi5jb2wteHMtMTJcIiwgW1xuICAgICAgICAgICAgbShcImZvcm1cIiwgW20oXCJpbnB1dC5mb3JtLWNvbnRyb2xbdHlwZT10ZXh0XVwiLCB7cGxhY2Vob2xkZXI6XCJDb21wYW55IE5hbWVcIiwgb25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lVHJhY2tlckFkbWluLnZtLmNvbXBhbnlGb3JtLmZpZWxkcy5uYW1lKSwgdmFsdWU6IEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFueUZvcm0uZmllbGRzLm5hbWUoKX0pLFxuICAgICAgICAgICAgICAgICAgICAgICBtKFwiZGl2LmNoZWNrYm94XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsYWJlbFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImlucHV0W3R5cGU9Y2hlY2tib3hdXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcImNoZWNrZWRcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW55Rm9ybS5maWVsZHMuaXNtYW51ZmFjdHVyZXIpLCBjaGVja2VkOiBHYW1lVHJhY2tlckFkbWluLnZtLmNvbXBhbnlGb3JtLmZpZWxkcy5pc21hbnVmYWN0dXJlcigpfSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcInNwYW5cIiwgXCJJcyB0aGlzIGNvbXBhbnkgYSBjb25zb2xlIG1hbnVmYWN1dHVyZXI/XCIpXG4gICAgICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICAgICBHYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uU2V0KEdhbWVUcmFja2VyQWRtaW4udm0uaXNMb2FkaW5nLCBcImNvbXBhbnlGb3JtXCIpXG4gICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uR2VucmVGb3JtU2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgcmV0dXJuIG0oXCJkaXYucm93XCIsIFtcbiAgICAgICAgbShcImRpdi5jb2wteHMtMTJcIiwgW1xuICAgICAgICAgICAgbShcImZvcm1cIiwgWyBtKFwiaW5wdXQuZm9ybS1jb250cm9sW3R5cGU9dGV4dF1cIiwge3BsYWNlaG9sZGVyOlwiR2VucmUgTmFtZVwiLCBvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uZ2VucmVGb3JtLmZpZWxkcy5uYW1lKSwgdmFsdWU6IEdhbWVUcmFja2VyQWRtaW4udm0uZ2VucmVGb3JtLmZpZWxkcy5uYW1lKCl9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25TZXQoR2FtZVRyYWNrZXJBZG1pbi52bS5pc0xvYWRpbmcsIFwiZ2VucmVGb3JtXCIpXG4gICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgXSlcbiAgICBdKTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5TeXN0ZW1Gb3JtU2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgdmFyIG1hbnVmYWN0dXJlckRhdGFTZXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgcmV0dXJuIF8uZmlsdGVyKF8ucGx1Y2soR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW5pZXMsIFwiYXR0cmlidXRlc1wiKSwge2lzbWFudWZhY3R1cmVyOjF9KTtcbiAgICB9O1xuICAgIHJldHVybiBtKFwiZGl2LnJvd1wiLCBbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFttKFwiaW5wdXQuZm9ybS1jb250cm9sW3R5cGU9dGV4dF1cIiwge3BsYWNlaG9sZGVyOlwiU3lzdGVtIE5hbWVcIiwgb25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbUZvcm0uZmllbGRzLm5hbWUpLCB2YWx1ZTogR2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5uYW1lKCl9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoeyBvbmNoYW5nZTpHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6R2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5tYW51ZmFjdHVyZXJpZCgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9uczp7cGxhY2Vob2xkZXI6XCJNYW51ZmFjdHVyZXJcIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1hbnVmYWN0dXJlckRhdGFTZXQoKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJ1W3N0eWxlPWN1cnNvcjpwb2ludGVyXVwiLCB7b25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bS5jaGFuZ2VUb0FkZENvbXBhbnl9LCBcIitBZGQgQ29tcGFueVwiKVxuICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvblNldChHYW1lVHJhY2tlckFkbWluLnZtLmlzTG9hZGluZywgXCJzeXN0ZW1Gb3JtXCIpXG4gICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgXSlcbiAgICBdKTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5HYW1lRm9ybVNjcmVlbiA9IEdhbWVGb3JtLnZpZXc7XG5cbkdhbWVUcmFja2VyQWRtaW4udmlldyA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciByZW5kZXJTY3JlZW5zID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBfLm1hcChHYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24sIGZ1bmN0aW9uKHNjcmVlbkNvbnRlbnQsIHNjcmVlbk5hbWUpIHtcbiAgICAgICAgICAgIHJldHVybiBtKFwiZGl2XCIsIHtzdHlsZTpcImRpc3BsYXk6XCIrR2FtZVRyYWNrZXJBZG1pbi52bS5zaG91bGREaXNwbGF5U2NyZWVuKHNjcmVlbk5hbWUpfSwgc2NyZWVuQ29udGVudCgpKTtcbiAgICAgICAgfSk7XG4gICAgfTtcbiAgICByZXR1cm4gW1xuICAgICAgICBtKFwibmF2Lm5hdmJhci5uYXZiYXItZGVmYXVsdFwiLCBbXG4gICAgICAgICAgICBtKFwiZGl2LmNvbnRhaW5lci1mbHVpZFwiLCBbXG4gICAgICAgICAgICAgICAgbShcImRpdi5uYXZiYXItaGVhZGVyXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5uYXZiYXItdG9nZ2xlLmNvbGxhcHNlZFt0eXBlPWJ1dHRvbl1bZGF0YS10b2dnbGU9Y29sbGFwc2VdW2RhdGEtdGFyZ2V0PSNtYWluLW5hdl1cIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uaWNvbi1iYXJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwic3Bhbi5pY29uLWJhclwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmljb24tYmFyXCIpXG4gICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdiNtYWluLW5hdi5jb2xsYXBzZS5uYXZiYXItY29sbGFwc2VcIixbIFxuICAgICAgICAgICAgICAgICAgICBtKFwidWwubmF2Lm5hdmJhci1uYXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpLmRyb3Bkb3duXCIsIFttKFwiYVtocmVmPSNdLmRyb3Bkb3duLXRvZ2dsZVtkYXRhLXRvZ2dsZT1kcm9wZG93bl1bcm9sZT1idXR0b25dXCIsIFwiR2FtZXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImdhbWVcIiwgXCJHYW1lRm9ybVNjcmVlblwiKX0sIFwiQWRkIEdhbWVcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImdhbWVcIiwgXCJHYW1lRm9ybVNjcmVlblwiKX0sIFwiRWRpdC9EZWxldGUgR2FtZVwiKV0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIlN5c3RlbXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcInN5c3RlbVwiLCBcIlN5c3RlbUZvcm1TY3JlZW5cIil9LCBcIkFkZCBTeXN0ZW1cIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcInN5c3RlbVwiLCBcIlNlbGVjdFNjcmVlblwiKX0sIFwiRWRpdC9EZWxldGUgU3lzdGVtXCIpXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpLmRyb3Bkb3duXCIsIFttKFwiYVtocmVmPSNdLmRyb3Bkb3duLXRvZ2dsZVtkYXRhLXRvZ2dsZT1kcm9wZG93bl1bcm9sZT1idXR0b25dXCIsIFwiR2VucmVcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImdlbnJlXCIsIFwiR2VucmVGb3JtU2NyZWVuXCIpfSwgXCJBZGQgR2VucmVcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImdlbnJlXCIsIFwiU2VsZWN0U2NyZWVuXCIpfSwgXCJFZGl0L0RlbGV0ZSBHZW5yZVwiKV0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIkNvbXBhbnlcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImNvbXBhbnlcIiwgXCJDb21wYW55Rm9ybVNjcmVlblwiKX0sIFwiQWRkIENvbXBhbnlcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImNvbXBhbnlcIiwgXCJTZWxlY3RTY3JlZW5cIil9LCBcIkVkaXQvRGVsZXRlIENvbXBhbnlcIildKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICBdKVxuICAgICAgICBdKSxcbiAgICAgICAgbShcImRpdi5jb250YWluZXJcIiwgW1xuICAgICAgICAgICAgbShcImRpdi50ZXh0LXN1Y2Nlc3NcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5zdWNjZXNzTWVzc2FnZSksXG4gICAgICAgICAgICBtKFwiZGl2LnRleHQtZGFuZ2VyXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uZXJyb3JNZXNzYWdlKSxcbiAgICAgICAgICAgIHJlbmRlclNjcmVlbnMoKVxuICAgICAgICBdKVxuICAgIF07XG59O1xuIiwiR2FtZVRyYWNrZXJBZG1pbi52bSA9IG5ldyBmdW5jdGlvbigpIHtcbiAgICB2YXIgdm0gPSB7fTtcbiAgICB2bS5pbml0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIFxuICAgICAgICB2bS5mb3JtTW9kZSA9IFwiXCI7XG4gICAgICAgIHZtLnNlbGVjdFNjcmVlblN0YXRlID0gXCJcIjtcbiAgICAgICAgXG4gICAgICAgIC8vVGhpcyBpcyB1c2VkIGFzIGEgc3RhY2s7XG4gICAgICAgIHZtLnNjcmVlbkhpc3RvcnkgPSBbXCJJbml0aWFsU2NyZWVuXCJdO1xuXG4gICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvciA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJJbnRlcm5hbCBTZXJ2ZXIgRXJyb3JcIjtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICB2bS5jbGVhck1lc3NhZ2VzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiXCI7XG4gICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlwiO1xuICAgICAgICAgICAgdm0ubm9SZXN1bHRzID0gXCJcIjtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY29tcGxldGVSZXNldCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgdm0uc2VhcmNoUmVzdWx0cyA9IFtdO1xuICAgICAgICAgICAgdm0uc3lzdGVtRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLmdlbnJlRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLmNvbXBhbnlGb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZChcIlwiKTtcbiAgICAgICAgfTtcbiAgICAgICAgXG4gICAgICAgIHZtLmp1bXBUb1NjcmVlbiA9IGZ1bmN0aW9uKGZvcm1Nb2RlLCBzZWxlY3RTY3JlZW5TdGF0ZSwgc2NyZWVuTmFtZSkge1xuICAgICAgICAgICAgdm0uY29tcGxldGVSZXNldCgpO1xuICAgICAgICAgICAgdm0uZm9ybU1vZGUgPSBmb3JtTW9kZTtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGUgPSBmb3JtTW9kZTtcbiAgICAgICAgICAgIHZtLnNlbGVjdFNjcmVlblN0YXRlID0gc2VsZWN0U2NyZWVuU3RhdGU7XG4gICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5ID0gW3NjcmVlbk5hbWUsIFwiSW5pdGlhbFNjcmVlblwiXTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgXG4gICAgICAgIC8vVGhpcyBkYXRhIGlzIGFjdHVhbGx5IGJvb3RzdHJhcGVkIGFuZCB0aGUgdmFyaWFibGUgaXQncyBjb3B5aW5nIGZyb20gaXMgaW4gdGhlIHRlbXBsYXRlXG4gICAgICAgIHZtLmNvbXBhbmllcyA9IF8ubWFwKGNvbXBhbmllcywgZnVuY3Rpb24oY29tcGFueSkgeyByZXR1cm4gbmV3IEdhbWVUcmFja2VyQWRtaW4uQ29tcGFueShjb21wYW55KTsgfSk7XG4gICAgICAgIHZtLmdlbnJlcyA9IF8ubWFwKGdlbnJlcywgZnVuY3Rpb24oZ2VucmUpIHsgcmV0dXJuIG5ldyBHYW1lVHJhY2tlckFkbWluLkdlbnJlKGdlbnJlKTsgfSk7XG4gICAgICAgIHZtLnN5c3RlbXMgPSBfLm1hcChzeXN0ZW1zLCBmdW5jdGlvbihzeXN0ZW0pIHsgcmV0dXJuIG5ldyBHYW1lVHJhY2tlckFkbWluLlN5c3RlbShzeXN0ZW0pOyB9KTtcbiAgICAgICAgXG4gICAgICAgIHZtLnNob3VsZERpc3BsYXlTY3JlZW4gPSBmdW5jdGlvbihzY3JlZW5OYW1lKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVByb3BlcnR5ID0gXCJub25lXCI7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5zY3JlZW5IaXN0b3J5KSkge1xuICAgICAgICAgICAgICAgIGRpc3BsYXlQcm9wZXJ0eSA9IChzY3JlZW5OYW1lID09PSB2bS5zY3JlZW5IaXN0b3J5WzBdKSA/IFwiaW5oZXJpdFwiIDogXCJub25lXCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVByb3BlcnR5O1xuICAgICAgICB9O1xuICAgICAgICB2bS5jcmVhdGVCYWNrQnV0dG9uID0gZnVuY3Rpb24oY2FsbGJhY2spIHtcbiAgICAgICAgICAgIHJldHVybiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgICAgICBjYWxsYmFjaygpO1xuICAgICAgICAgICAgICAgIHZtLnNjcmVlbkhpc3Rvcnkuc2hpZnQoKTtcbiAgICAgICAgICAgIH07XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0ucmV0dXJuVG9NYWluRm9ybSA9IGZ1bmN0aW9uKHdoaWNoRm9ybSkge1xuICAgICAgICAgICAgdm1bd2hpY2hGb3JtXS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLnNjcmVlbkhpc3Rvcnkuc2hpZnQoKTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICAvL1RoaXMgaXMgc2xpZ2h0bHkgZGlmZmVyZW50IGZyb20ganVtcGluZyB0byBhIHNjcmVlbiBiZWNhdXNlIHdlIG1heSB3YW50IHRoZSBnYW1lIGZvcm0gdG8gYmUgZGlmZmVyZW50IHNpbmNlIGl0J3MgaXRzIG93biBlbnRpdHlcbiAgICAgICAgdm0uZ2VuZXJhdGVDaGFuZ2VIYW5kbGVyID0gZnVuY3Rpb24obmV3U2NyZWVuKSB7XG4gICAgICAgICAgICByZXR1cm4gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0KG5ld1NjcmVlbik7XG4gICAgICAgICAgICAgICAgdm0uZm9ybU1vZGUgPSBcImFkZFwiO1xuICAgICAgICAgICAgfTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jb21wYW55Rm9ybSA9IG5ldyBHYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSh7bmFtZTogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaXNtYW51ZmFjdHVyZXI6IG0ucHJvcChmYWxzZSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSk7XG4gICAgICAgIC8qIFRPRE8gVGhlIGFkZCBmdW5jdGlvbnMgYXJlIGJhc2ljYWxseSB0aGUgc2FtZS4gVGhlcmUgc2hvdWxkIGJlIGEgZ29vZCB3YXkgb2YgcmVmYWN0b3JpbmcgdGhpcyBlaXRoZXIgY3JlYXRpbmcgYSBmdW5jaXRvbiBnZW5lcmF0b3JcbiAgICAgICAgICogb3IgY3JlYXRpbmcgYSBjaGlsZCBvYmplY3RcbiAgICAgICAgICovXG4gICAgICAgIHZtLmNvbXBhbnlGb3JtLnN1Ym1pdEhhbmRsZXJzLmFkZCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLmNvbXBhbnlGb3JtLmZpZWxkcy5uYW1lKCkpKSB7XG4gICAgICAgICAgICAgICAgdmFyIG5ld0NvbXBhbnkgPSBuZXcgR2FtZVRyYWNrZXJBZG1pbi5Db21wYW55KHZtLmNvbXBhbnlGb3JtLnJldHVybkZpZWxkcygpKTtcbiAgICAgICAgICAgICAgICBuZXdDb21wYW55LnNhdmUoKVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBtLnN0YXJ0Q29tcHV0YXRpb24oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jb21wYW5pZXMucHVzaChuZXdDb21wYW55KTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNvbXBhbmllcyA9IF8ucGx1Y2sodm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBjb21wYW55IGhhcyBiZWVuIGFkZGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uY29tcGFueUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgbS5lbmRDb21wdXRhdGlvbigpO1xuICAgICAgICAgICAgICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIkNvdWxkIG5vdCBhZGQgdGhlIGNvbXBhbnlcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIHRoZSBuYW1lIG9mIHRoZSBjb21wYW55XCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uY3VycmVudENvbXBhbnlJbmRleCA9IG51bGw7XG4gICAgICAgIHZtLmNvbXBhbnlGb3JtLnN1Ym1pdEhhbmRsZXJzLnVwZGF0ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc051bGwodm0uY3VycmVudENvbXBhbnlJbmRleCkgJiYgIV8uaXNFbXB0eSh2bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZtLmNvbXBhbmllc1t2bS5jdXJyZW50Q29tcGFueUluZGV4XS51cGRhdGUodm0uY29tcGFueUZvcm0ucmV0dXJuRmllbGRzKCkpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGNvbXBhbnkgaGFzIGJlZW4gdXBkYXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBlbnRlciB0aGUgbmFtZSBvZiB0aGUgY29tcGFueVwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICBcbiAgICAgICAgdm0uZ2VucmVGb3JtID0gbmV3IEdhbWVUcmFja2VyU2hhcmVkLlRyYWNrZXJGb3JtKHtuYW1lOiBtLnByb3AoXCJcIil9KTtcbiAgICAgICAgdm0uZ2VucmVGb3JtLnN1Ym1pdEhhbmRsZXJzLmFkZCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZhciBuZXdHZW5yZSA9IG5ldyBHYW1lVHJhY2tlckFkbWluLkdlbnJlKHZtLmdlbnJlRm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgbmV3R2VucmUuc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmdlbnJlcy5wdXNoKG5ld0dlbnJlKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdlbnJlcyA9IF8ucGx1Y2sodm0uZ2VucmVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBnZW5yZSBoYXMgYmVlbiBhZGRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmdlbnJlRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJDb3VsZCBub3QgYWRkIHRoZSBnZW5yZVwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZW50ZXIgdGhlIG5hbWUgb2YgdGhlIGdlbnJlXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgICAgIHZtLmN1cnJlbnRHZW5yZUluZGV4ID0gbnVsbDtcbiAgICAgICAgdm0uZ2VucmVGb3JtLnN1Ym1pdEhhbmRsZXJzLnVwZGF0ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc051bGwodm0uY3VycmVudEdlbnJlSW5kZXgpICYmICFfLmlzRW1wdHkodm0uZ2VucmVGb3JtLmZpZWxkcy5uYW1lKCkpKSB7XG4gICAgICAgICAgICAgICAgdm0uZ2VucmVzW3ZtLmN1cnJlbnRHZW5yZUluZGV4XS51cGRhdGUodm0uZ2VucmVGb3JtLnJldHVybkZpZWxkcygpKVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBnZW5yZSBoYXMgYmVlbiB1cGRhdGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIHRoZSBuYW1lIG9mIHRoZSBnZW5yZVwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jaGFuZ2VUb0FkZENvbXBhbnkgPSB2bS5nZW5lcmF0ZUNoYW5nZUhhbmRsZXIoXCJDb21wYW55Rm9ybVNjcmVlblwiKTtcbiAgICAgICAgdm0uc3lzdGVtRm9ybSA9IG5ldyBHYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSh7bmFtZTogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtYW51ZmFjdHVyZXJpZDogbS5wcm9wKFwiXCIpfSk7XG4gICAgICAgIHZtLnN5c3RlbUZvcm0uc3VibWl0SGFuZGxlcnMuYWRkID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uc3lzdGVtRm9ybS5maWVsZHMubmFtZSgpKSAmJiAhXy5pc0VtcHR5KHZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkKCkpKSB7XG4gICAgICAgICAgICAgICAgdmFyIG5ld1N5c3RlbSA9IG5ldyBHYW1lVHJhY2tlckFkbWluLlN5c3RlbSh2bS5zeXN0ZW1Gb3JtLnJldHVybkZpZWxkcygpKTtcbiAgICAgICAgICAgICAgICBuZXdTeXN0ZW0uc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN5c3RlbXMucHVzaChuZXdTeXN0ZW0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc3lzdGVtcyA9IF8ucGx1Y2sodm0uc3lzdGVtcywgXCJhdHRyaWJ1dGVzXCIpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgc3lzdGVtIGhhcyBiZWVuIGFkZGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3lzdGVtRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJDb3VsZCBub3QgYWRkIHRoZSBnYW1lLlwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIGFsbCBvZiB0aGUgZmllbGRzXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgICAgIHZtLmN1cnJlbnRTeXN0ZW1JbmRleCA9IG51bGw7XG4gICAgICAgIHZtLnN5c3RlbUZvcm0uc3VibWl0SGFuZGxlcnMudXBkYXRlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzTnVsbCh2bS5jdXJyZW50U3lzdGVtSW5kZXgpICYmICFfLmlzRW1wdHkodm0uc3lzdGVtRm9ybS5maWVsZHMubmFtZSgpKSAmJiAhXy5pc0VtcHR5KHZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkKCkpKSB7XG4gICAgICAgICAgICAgICAgdm0uc3lzdGVtc1t2bS5jdXJyZW50U3lzdGVtSW5kZXhdLnVwZGF0ZSh2bS5zeXN0ZW1Gb3JtLnJldHVybkZpZWxkcygpKVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBzeXN0ZW0gaGFzIGJlZW4gdXBkYXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0pO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIGFsbCB0aGUgZmllbGRzXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0FkbWluID0gdHJ1ZTtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5hZGRDb21wYW55SGFuZGxlciA9IHZtLmdlbmVyYXRlQ2hhbmdlSGFuZGxlcihcIkNvbXBhbnlGb3JtU2NyZWVuXCIpO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmFkZEdlbnJlSGFuZGxlciA9IHZtLmdlbmVyYXRlQ2hhbmdlSGFuZGxlcihcIkdlbnJlRm9ybVNjcmVlblwiKTtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5hZGRTeXN0ZW1IYW5kbGVyID0gdm0uZ2VuZXJhdGVDaGFuZ2VIYW5kbGVyKFwiU3lzdGVtRm9ybVNjcmVlblwiKTtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5wb3B1bGF0ZVNlbGVjdERhdGFTZXRzKF8ucGx1Y2sodm0uc3lzdGVtcywgXCJhdHRyaWJ1dGVzXCIpLCBfLnBsdWNrKHZtLmdlbnJlcywgXCJhdHRyaWJ1dGVzXCIpLCBfLnBsdWNrKHZtLmNvbXBhbmllcywgXCJhdHRyaWJ1dGVzXCIpKTtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5jYW5jZWxCdXR0b25IYW5kbGVyID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeS5zaGlmdCgpO1xuICAgICAgICB9O1xuICAgICAgICBcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5iaW5kU3VibWl0Rm9ybUhhbmRsZXIoXCJhZGRcIiwgZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eShHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5uYW1lKCkpICYmXG4gICAgICAgICAgICAgICAgIV8uaXNFbXB0eShHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSkgJiZcbiAgICAgICAgICAgICAgICBfLmlzRmluaXRlKE51bWJlcihHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCgpKSkgJiZcbiAgICAgICAgICAgICAgICBOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSkgPiAwICYmXG4gICAgICAgICAgICAgICAgXy5pc0Zpbml0ZShOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkoKSkpICYmXG4gICAgICAgICAgICAgICAgTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KCkpID4gMCkge1xuICAgICAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOiBcIlBPU1RcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvYWRtaW4vZ2FtZS9cIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0ucmV0dXJuRmllbGRzKCl9KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJTdWNjZXNzZnVsbHkgYWRkZWQgdGhlIGdhbWVcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIGFsbCB0aGUgZmllbGRzXCI7XG4gICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfSk7XG5cbiAgICAgICAgdm0uY3VycmVudEdhbWVJZCA9IDA7XG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0VXBkYXRlSGFuZGxlciA9IGZ1bmN0aW9uKGdhbWVJZCkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChnYW1lSWQgJiYgXy5pc0Zpbml0ZShOdW1iZXIoZ2FtZUlkKSkpIHtcbiAgICAgICAgICAgICAgICAvKiBBIGtub3duIGxpbWl0YXRpb24gd2l0aCB0aGUgYmFja2VuZDogdGhpbmdzIHdlIGV4cGVjdCB0byBiZSBhbiBhcnJheSBtYXkgYmUgYSBzaW1wbGUgb2JqZWN0IGR1ZSB0byB0aGUganNvbiBlbmNvZGVyIG9uIHRoZSBiYWNrZW5kXG4gICAgICAgICAgICAgICAgICAgbm90IGJlaW5nIGFibGUgdG8gZW5jb2RlIHNpbmdsZSByb3cgcmVzdWx0cyBjb3JyZWN0bHlcbiAgICAgICAgICAgICAgICAgKi9cbiAgICAgICAgICAgICAgICB2YXIgZW5zdXJlQXJyYXkgPSBmdW5jdGlvbihpdGVtKSB7XG4gICAgICAgICAgICAgICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IF8uaXNBcnJheShpdGVtKSA/IGl0ZW0gOiBbaXRlbV07XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiByZXR1cm5WYWx1ZTtcbiAgICAgICAgICAgICAgICB9O1xuICAgICAgICAgICAgICAgIC8vV2UgY291bGQganVzdCB1c2UgdGhlIGRhdGEgd2UgcmV0cmlldmVkIGZyb20gdGhlIHNlYXJjaCBidXQgbGV0J3MgZ3VhcmFudGVlIHRoZSB1c2VyIHdpdGggdGhlIG1vc3QgcmVjZW50IGluZm9ybWF0aW9uXG4gICAgICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6IFwiR0VUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiB7aWQ6IE51bWJlcihnYW1lSWQpfVxuICAgICAgICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudEdhbWVJZCA9IE51bWJlcihyZXNwb25zZS5pZCk7XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5jb21wYW5pZXMoXy5wbHVjayhlbnN1cmVBcnJheShyZXNwb25zZS5jb21wYW5pZXMpLCBcImNvbXBhbnlJZFwiKSk7XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMoXy5wbHVjayhlbnN1cmVBcnJheShyZXNwb25zZS5nZW5yZXMpLCBcImdlbnJlSWRcIikpO1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5wb3B1bGF0ZUZvcm0oXy5vbWl0KHJlc3BvbnNlLCBbXCJjb21wYW5pZXNcIiwgXCJnZW5yZXNcIl0pKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGUgPSBcInVwZGF0ZVwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzID0gW107XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaExvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9XG4gICAgICAgIH07XG5cbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5iaW5kU3VibWl0Rm9ybUhhbmRsZXIoXCJ1cGRhdGVcIiwgZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eShHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5uYW1lKCkpICYmXG4gICAgICAgICAgICAgICAgIV8uaXNFbXB0eShHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSkgJiZcbiAgICAgICAgICAgICAgICBfLmlzRmluaXRlKE51bWJlcihHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCgpKSkgJiZcbiAgICAgICAgICAgICAgICBOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSkgPiAwICYmXG4gICAgICAgICAgICAgICAgXy5pc0Zpbml0ZShOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkoKSkpICYmXG4gICAgICAgICAgICAgICAgTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KCkpID4gMCkgeyAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgIHZhciBkYXRhID0gXy5leHRlbmQoe2lkOiBOdW1iZXIodm0uY3VycmVudEdhbWVJZCl9LCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLnJldHVybkZpZWxkcygpKTtcblxuICAgICAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOiBcIlBVVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBcIi9hZG1pbi9nYW1lL1wiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YTogZGF0YX0pXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJHYW1lIHN1Y2Nlc3NmdWxseSB1cGRhdGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH0gXG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIHRoZSBmaWVsZHNcIjtcbiAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9KTtcblxuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlbGVjdERlbGV0ZUhhbmRsZXIgPSBmdW5jdGlvbihnYW1lSWQpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICBpZiAoZ2FtZUlkICYmIF8uaXNGaW5pdGUoTnVtYmVyKGdhbWVJZCkpKSB7XG4gICAgICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6IFwiREVMRVRFXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBcIi9hZG1pbi9nYW1lL1wiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IHtpZDogTnVtYmVyKGdhbWVJZCl9fSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBnYW1lIGhhcyBiZWVuIGRlbGV0ZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnJlbW92ZShHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMsIGZ1bmN0aW9uKGdhbWUpIHsgcmV0dXJuIGdhbWUuaWQgPT09IE51bWJlcihnYW1lSWQpOyB9KTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaExvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uY3VycmVudFNlbGVjdEVudGl0eUlkID0gbS5wcm9wKFwiXCIpO1xuICAgICAgICB2bS5nZW5lcmFsSW5pdGlhdGVFZGl0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSkpIHtcbiAgICAgICAgICAgICAgICB2bS5mb3JtTW9kZSA9IFwidXBkYXRlXCI7XG4gICAgICAgICAgICAgICAgc3dpdGNoICh2bS5zZWxlY3RTY3JlZW5TdGF0ZSkge1xuICAgICAgICAgICAgICAgIGNhc2UgXCJzeXN0ZW1cIjpcbiAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudFN5c3RlbUluZGV4ID0gXy5maW5kSW5kZXgodm0uc3lzdGVtcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc3lzdGVtRm9ybS5wb3B1bGF0ZUZvcm0odm0uc3lzdGVtc1t2bS5jdXJyZW50U3lzdGVtSW5kZXhdKTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0KFwiU3lzdGVtRm9ybVNjcmVlblwiKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgICAgICAgICAgY2FzZSBcImNvbXBhbnlcIjpcbiAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudENvbXBhbnlJbmRleCA9IF8uZmluZEluZGV4KHZtLmNvbXBhbmllcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uY29tcGFueUZvcm0ucG9wdWxhdGVGb3JtKHZtLmNvbXBhbmllc1t2bS5jdXJyZW50Q29tcGFueUluZGV4XSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLnNjcmVlbkhpc3RvcnkudW5zaGlmdChcIkNvbXBhbnlGb3JtU2NyZWVuXCIpO1xuICAgICAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgICAgICBjYXNlIFwiZ2VucmVcIjpcbiAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudEdlbnJlSW5kZXggPSBfLmZpbmRJbmRleCh2bS5nZW5yZXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLmdlbnJlRm9ybS5wb3B1bGF0ZUZvcm0odm0uZ2VucmVzW3ZtLmN1cnJlbnRHZW5yZUluZGV4XSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLnNjcmVlbkhpc3RvcnkudW5zaGlmdChcIkdlbnJlRm9ybVNjcmVlblwiKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBzZWxlY3QgYW4gaXRlbSBpbiB0aGUgZHJvcGRvd25cIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZChcIlwiKTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uZ2VuZXJhbERlbGV0ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpKSB7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgICAgICB2YXIgY3VycmVudEluZGV4O1xuICAgICAgICAgICAgICAgIHN3aXRjaCAodm0uc2VsZWN0U2NyZWVuU3RhdGUpIHtcbiAgICAgICAgICAgICAgICBjYXNlIFwic3lzdGVtXCI6XG4gICAgICAgICAgICAgICAgICAgIGN1cnJlbnRJbmRleCA9IF8uZmluZEluZGV4KHZtLnN5c3RlbXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLnN5c3RlbXNbY3VycmVudEluZGV4XS5yZW1vdmUoKVxuICAgICAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnJlbW92ZSh2bS5zeXN0ZW1zLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIHN5c3RlbSBoYXMgYmVlbiByZW1vdmVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZChcIlwiKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc3lzdGVtcyA9IF8ucGx1Y2sodm0uc3lzdGVtcywgXCJhdHRyaWJ1dGVzXCIpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgICAgICAgICAgY2FzZSBcImNvbXBhbnlcIjpcbiAgICAgICAgICAgICAgICAgICAgY3VycmVudEluZGV4ID0gXy5maW5kSW5kZXgodm0uY29tcGFuaWVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5jb21wYW5pZXNbY3VycmVudEluZGV4XS5yZW1vdmUoKVxuICAgICAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnJlbW92ZSh2bS5jb21wYW5pZXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgY29tcGFueSBoYXMgYmVlbiByZW1vdmVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZChcIlwiKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuY29tcGFuaWVzID0gXy5wbHVjayh2bS5jb21wYW5pZXMsIFwiYXR0cmlidXRlc1wiKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICB9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIGNhc2UgXCJnZW5yZVwiOlxuICAgICAgICAgICAgICAgICAgICBjdXJyZW50SW5kZXggPSBfLmZpbmRJbmRleCh2bS5nZW5yZXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLmdlbnJlc1tjdXJyZW50SW5kZXhdLnJlbW92ZSgpXG4gICAgICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ucmVtb3ZlKHZtLmdlbnJlcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBnZW5yZSBoYXMgYmVlbiByZW1vdmVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZChcIlwiKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2VucmVzID0gXy5wbHVjayh2bS5nZW5yZXMsIFwiYXR0cmlidXRlc1wiKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICB9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrOyAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICB9O1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5tZXNzYWdlRXJyb3IgPSBcIlNlbGVjdCBhbiBpdGVtIGZyb20gdGhlIGRyb3Bkb3duXCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgfTtcblxuICAgIHJldHVybiB2bTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uY29udHJvbGxlciA9IGZ1bmN0aW9uKCkge1xuICAgIEdhbWVUcmFja2VyQWRtaW4udm0uaW5pdCgpO1xufTtcbiIsIi8vRm9yIHVzZSB3aXRoIGFsbCBWaWV3cy4gQ29kZSBpcyBiYXNlZCBvbiB0aGUgb25lIGZvdW5kIG9uIG1pdGhyaWwncyBzaXRlIGh0dHBzOi8vbGhvcmllLmdpdGh1Yi5pby9taXRocmlsL2ludGVncmF0aW9uLmh0bWxcbnZhciBzZWxlY3QyPSB7fTtcblxuLyogVGhpcyBmYWN0b3J5IGZ1bmN0aW9uIG9mZmVycyBhIG5pY2UgY2xvc3VyZSBmb3IgYW55dGhpbmcgZXh0cmEgd2Ugd2FudCB0byBwYXNzIGluICovXG5zZWxlY3QyLmNvbmZpZyA9IGZ1bmN0aW9uKGV4dHJhQXJndW1lbnRzKSB7XG4gICAgcmV0dXJuIGZ1bmN0aW9uKGVsZW1lbnQsIGlzSW5pdGlhbGl6ZWQsIGNvbnRyb2xsZXIpIHtcbiAgICAgICAgdmFyIGVsID0gJChlbGVtZW50KTtcbiAgICAgICAgaWYgKCFpc0luaXRpYWxpemVkKSB7XG4gICAgICAgICAgICBpZiAoZXh0cmFBcmd1bWVudHMuc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9ucykge1xuICAgICAgICAgICAgICAgIGVsLnNlbGVjdDIoZXh0cmFBcmd1bWVudHMuc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9ucyk7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIGVsLnNlbGVjdDIoKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIGVsLmNoYW5nZShmdW5jdGlvbigpIHtcbiAgICAgICAgICAgICAgICBtLnN0YXJ0Q29tcHV0YXRpb24oKTtcbiAgICAgICAgICAgICAgICBleHRyYUFyZ3VtZW50cy5vbmNoYW5nZShlbC5zZWxlY3QyKFwidmFsXCIpKTtcbiAgICAgICAgICAgICAgICBtLmVuZENvbXB1dGF0aW9uKCk7XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgICBlbC5zZWxlY3QyKFwidmFsXCIsIGV4dHJhQXJndW1lbnRzLnZhbHVlKTtcbiAgICB9O1xufTtcblxuc2VsZWN0Mi52aWV3ID0gZnVuY3Rpb24oZXh0cmFBcmd1bWVudHMsIG9wdGlvblNldCwgaXNNdWx0aXBsZSkge1xuICAgIHZhciBzZWxlY3RvciA9IChpc011bHRpcGxlKSA/IFwic2VsZWN0LmZvcm0tY29udHJvbFttdWx0aXBsZT10cnVlXVwiIDogXCJzZWxlY3QuZm9ybS1jb250cm9sXCI7XG4gICAgdmFyIHNvcnRlZE9wdGlvbnMgPSBfLnNvcnRCeShvcHRpb25TZXQsIGZ1bmN0aW9uKHZhbHVlKSB7XG4gICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IHZhbHVlO1xuICAgICAgICBpZiAoXy5pc09iamVjdCh2YWx1ZSkpIHtcbiAgICAgICAgICAgIHJldHVyblZhbHVlID0gdmFsdWUubmFtZS50b0xvd2VyQ2FzZSgpLnJlcGxhY2UoL1xcXFwvZywnJyk7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgIH0pO1xuICAgIHZhciBjcmVhdGVPcHRpb25TZXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgdmFyIG9wdGlvbnMgPSBbXTtcbiAgICAgICAgaWYgKG9wdGlvblNldCkge1xuICAgICAgICAgICAgb3B0aW9ucyA9IF8ubWFwKHNvcnRlZE9wdGlvbnMsIGZ1bmN0aW9uKHZhbHVlKSB7XG4gICAgICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gKF8uaXNPYmplY3QodmFsdWUpKSA/IG0oXCJvcHRpb25cIiwge3ZhbHVlOiB2YWx1ZS5pZH0sIHZhbHVlLm5hbWUpIDogbShcIm9wdGlvblwiLCB2YWx1ZSk7XG4gICAgICAgICAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIG9wdGlvbnM7XG4gICAgfTtcbiAgICByZXR1cm4gbShzZWxlY3Rvciwge2NvbmZpZzpzZWxlY3QyLmNvbmZpZyhleHRyYUFyZ3VtZW50cyl9LFxuICAgICAgICAgICAgIFttKFwib3B0aW9uXCIpLGNyZWF0ZU9wdGlvblNldCgpXSk7XG59O1xuIiwiXG5cblxuXG5cblxuXG5cblxuXG5tLm1vZHVsZShkb2N1bWVudC5ib2R5LCBHYW1lVHJhY2tlckFkbWluKTtcbiJdLCJzb3VyY2VSb290IjoiL3NvdXJjZS8ifQ==
