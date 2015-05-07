var GameTrackerShared = {};

GameTrackerShared.TrackerForm = function(fields) {
    this.fields = fields;
    this.populateForm = function(object) {
        var self = this;
        if (object.attributes) {
            _.map(object.attributes, function(attributeValue, attributeKey) {
                if (attributeKey !== "id") {
                    self.fields[attributeKey](attributeValue);
                }
            });
        } else {
            _.map(object, function(value, key) {
                if (key !== "id") {
                    self.fields[key](value);
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
        return _.mapValues(this.fields, function(field) {
            var returnValue = field();
            if (_.isBoolean(returnValue)) {
                returnValue = Number(returnValue);
            }
            return returnValue;
        });
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
                                                       hasmanual: m.prop(0),
                                                       hasbox: m.prop(0),
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
                                                     m("span", {style:titleCursorProperty, onclick:GameForm.controller.titleClickHandler.bind(GameForm.controller, result.id)}, (result.name + " [" + result.region + "] (" + result.systemName + ")")),
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
                console.log(vm.companyForm.returnFields());
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
                            console.log(response);
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
                           url: "/thosewhodarenotwander/game/",
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
    var createOptionSet = function() {
        var options = [];
        if (optionSet) {
            options = _.map(optionSet, function(value) {
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

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImdhbWV0cmFja2Vyc2hhcmVkLmpzIiwiZ2FtZWZvcm0uanMiLCJnYW1lZm9ybXZpZXcuanMiLCJhZG1pbm1vZGVscy5qcyIsImFkbWludmlld3MuanMiLCJhZG1pbnZtY29udHJvbGxlci5qcyIsInNlbGVjdDJtaXRocmlsLmpzIiwiZ2FtZXRyYWNrZXJhZG1pbi5qcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUNsREE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDdEZBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDcElBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDNURBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDbEtBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUNuWkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDdENBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQSIsImZpbGUiOiJhZG1pbi5qcyIsInNvdXJjZXNDb250ZW50IjpbInZhciBHYW1lVHJhY2tlclNoYXJlZCA9IHt9O1xuXG5HYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSA9IGZ1bmN0aW9uKGZpZWxkcykge1xuICAgIHRoaXMuZmllbGRzID0gZmllbGRzO1xuICAgIHRoaXMucG9wdWxhdGVGb3JtID0gZnVuY3Rpb24ob2JqZWN0KSB7XG4gICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgaWYgKG9iamVjdC5hdHRyaWJ1dGVzKSB7XG4gICAgICAgICAgICBfLm1hcChvYmplY3QuYXR0cmlidXRlcywgZnVuY3Rpb24oYXR0cmlidXRlVmFsdWUsIGF0dHJpYnV0ZUtleSkge1xuICAgICAgICAgICAgICAgIGlmIChhdHRyaWJ1dGVLZXkgIT09IFwiaWRcIikge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmZpZWxkc1thdHRyaWJ1dGVLZXldKGF0dHJpYnV0ZVZhbHVlKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIF8ubWFwKG9iamVjdCwgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgICAgIGlmIChrZXkgIT09IFwiaWRcIikge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmZpZWxkc1trZXldKHZhbHVlKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgIH07XG4gICAgdGhpcy5jbGVhckZvcm0gPSBfLmZvckVhY2guYmluZCh0aGlzLCB0aGlzLmZpZWxkcywgZnVuY3Rpb24oaW5wdXQpIHtcbiAgICAgICAgaWYgKF8uaXNTdHJpbmcoaW5wdXQoKSkpIHtcbiAgICAgICAgICAgIGlucHV0KFwiXCIpO1xuICAgICAgICB9IGVsc2UgaWYgKF8uaXNBcnJheShpbnB1dCgpKSkge1xuICAgICAgICAgICAgaW5wdXQoW10pO1xuICAgICAgICB9IGVsc2UgaWYgKF8uaXNCb29sZWFuKGlucHV0KCkpKXtcbiAgICAgICAgICAgIGlucHV0KGZhbHNlKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIGlucHV0KG51bGwpO1xuICAgICAgICB9XG4gICAgfSk7XG4gICAgdGhpcy5yZXR1cm5GaWVsZHMgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgcmV0dXJuIF8ubWFwVmFsdWVzKHRoaXMuZmllbGRzLCBmdW5jdGlvbihmaWVsZCkge1xuICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gZmllbGQoKTtcbiAgICAgICAgICAgIGlmIChfLmlzQm9vbGVhbihyZXR1cm5WYWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9IE51bWJlcihyZXR1cm5WYWx1ZSk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgIH0pO1xuICAgIH07XG4gICAgdGhpcy5zdWJtaXRIYW5kbGVycyA9IHt9O1xuICAgIC8qIFRoaXMgd2lsbCBwcm9iYWJseSBiZSByZWZhY3RvcmVkIG91dCBpbiB0aGUgZnV0dXJlIGdpdmVuIHRoZSBvbmx5IHRoaW5nIHRoYXQgaGFzIGEgc2VhcmNoIGlzIHRoZSBnYW1lIGZvcm1cbiAgICAgKiBUbyBrZWVwIHRoaW5ncyBmcm9tIGNvbXBsYWluaW5nIGFib3V0IGEgbWlzc2luZyBrZXkgd2UgYWRkIGFuIGVtcHR5IGZ1bmN0aW9uIGhlcmVcbiAgICAgKi9cbiAgICB0aGlzLnN1Ym1pdEhhbmRsZXJzLnNlYXJjaCA9IGZ1bmN0aW9uKCkgeyAvKmVtcHR5Ki8gfTtcbiAgICB0aGlzLmdldFN1Ym1pdEhhbmRsZXIgPSBmdW5jdGlvbihzdGF0ZSkge1xuICAgICAgICByZXR1cm4gdGhpcy5zdWJtaXRIYW5kbGVyc1tzdGF0ZV07XG4gICAgfTtcblxufTtcbiIsInZhciBHYW1lRm9ybSA9IHt9O1xuXG5HYW1lRm9ybS5jb250cm9sbGVyID0gbmV3IGZ1bmN0aW9uKCkge1xuICAgIHRoaXMuZ2FtZUZvcm0gPSBuZXcgR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0oe25hbWU6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBibHVyYjogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlZ2lvbjogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhhc21hbnVhbDogbS5wcm9wKDApLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhhc2JveDogbS5wcm9wKDApLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5vdGVzOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcXVhbnRpdHk6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBnZW5yZXM6IG0ucHJvcChbXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29tcGFuaWVzOiBtLnByb3AoW10pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN5c3RlbWlkOiBtLnByb3AoXCJcIil9KTtcblxuICAgIHRoaXMuZXJyb3JNZXNzYWdlID0gXCJcIjtcblxuICAgIHRoaXMuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgdGhpcy5zZWFyY2hSZXN1bHRzID0gW107XG5cbiAgICB0aGlzLm5vUmVzdWx0cyA9IFwiXCI7XG4gICAgdGhpcy5mb3JtTW9kZSA9IFwic2VhcmNoXCI7XG4gICAgdGhpcy5pc0FkbWluID0gZmFsc2U7XG5cbiAgICB0aGlzLnNlYXJjaExvYWRpbmcgPSBmYWxzZTtcblxuICAgIHRoaXMuc2VsZWN0VXBkYXRlSGFuZGxlcjtcbiAgICB0aGlzLnNlbGVjdERlbGV0ZUhhbmRsZXI7XG4gICAgdGhpcy5jYW5jZWxCdXR0b25IYW5kbGVyO1xuXG4gICAgdGhpcy5zeXN0ZW1zO1xuICAgIHRoaXMuZ2VucmVzO1xuICAgIHRoaXMuY29tcGFuaWVzO1xuXG4gICAgdGhpcy5hZGRDb21wYW55SGFuZGxlciA9IGZ1bmN0aW9uKCkge307XG4gICAgdGhpcy5hZGRHZW5yZUhhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuICAgIHRoaXMuYWRkU3lzdGVtSGFuZGxlciA9IGZ1bmN0aW9uKCkge307XG5cbiAgICB0aGlzLnBvcHVsYXRlU2VsZWN0RGF0YVNldHMgPSBmdW5jdGlvbihzeXN0ZW1zLCBnZW5yZXMsIGNvbXBhbmllcykge1xuICAgICAgICB0aGlzLnN5c3RlbXMgPSBzeXN0ZW1zO1xuICAgICAgICB0aGlzLmdlbnJlcyA9IGdlbnJlcztcbiAgICAgICAgdGhpcy5jb21wYW5pZXMgPSBjb21wYW5pZXM7XG4gICAgfTtcblxuICAgIHRoaXMuYWRtaW5SZXN1bHRCdXR0b25IYW5kbGVycyA9IGZ1bmN0aW9uKGVkaXRoYW5kbGVyLCBkZWxldGVoYW5kbGVyKSAge1xuICAgICAgICB0aGlzLnNlbGVjdFVwZGF0ZUhhbmRsZXIgPSBlZGl0aGFuZGxlcjtcbiAgICAgICAgdGhpcy5zZWxlY3REZWxldGVIYW5kbGVyID0gZGVsZXRlaGFuZGxlcjtcbiAgICB9O1xuXG4gICAgdGhpcy5iaW5kU3VibWl0Rm9ybUhhbmRsZXIgPSBmdW5jdGlvbihzdGF0ZSwgaGFuZGxlcikge1xuICAgICAgICB0aGlzLmdhbWVGb3JtLnN1Ym1pdEhhbmRsZXJzW3N0YXRlXSA9IGhhbmRsZXI7XG4gICAgfTtcblxuICAgIHRoaXMudGl0bGVDbGlja0hhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuXG4gICAgdGhpcy5nYW1lRm9ybS5zdWJtaXRIYW5kbGVycy5zZWFyY2ggPSBmdW5jdGlvbigpIHtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5ub1Jlc3VsdHMgPSBcIlwiO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgIHZhciBjb21wbGV0ZWRTZXQgPSBfLm9taXQoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5yZXR1cm5GaWVsZHMoKSwgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChfLmlzQm9vbGVhbih2YWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9ICF2YWx1ZTtcbiAgICAgICAgICAgIH0gZWxzZSBpZiAoIV8uaXNFbXB0eSh2YWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgICAgICB9KTtcbiAgICAgICAgaWYgKCFfLmlzRW1wdHkoY29tcGxldGVkU2V0KSkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5lcnJvck1lc3NhZ2UgPSBcIlwiO1xuICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6XCJwb3N0XCIsXG4gICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvc2VhcmNoLWdhbWVzLWFqYXgvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IGNvbXBsZXRlZFNldH0pXG4gICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgLy9FbXB0eSByZXN1bHRzIHNldCByZXR1cm5zIGEgc2luZ2xlIGl0ZW0gYXJyYXkgd2l0aCBudWxsIGJlaW5nIHRoYXQgb2JqZWN0XG4gICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cyA9IF8ucmVtb3ZlKHJlc3BvbnNlLnJlc3VsdHMsIGZ1bmN0aW9uKGl0ZW0pIHsgcmV0dXJuICFfLmlzTnVsbChpdGVtKTsgfSk7XG4gICAgICAgICAgICAgICAgICAgIGlmIChHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMubGVuZ3RoIDwgMSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5ub1Jlc3VsdHMgPSBcIk5vIG1hdGNoZXMgd2VyZSBmb3VuZFwiO1xuICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgfSwgZnVuY3Rpb24oKSB7IEdhbWVGb3JtLmNvbnRyb2xsZXIuZXJyb3JNZXNzYWdlID0gXCJJbnRlcm5hbCBTZXJ2ZXIgRXJyb3JcIjt9ICk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIGF0IGxlYXN0IG9uZSBzZWFyY2ggcGFyYW1ldGVyXCI7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9O1xufTtcbiIsIkdhbWVGb3JtLnZpZXcgPSBmdW5jdGlvbigpIHtcbiAgICB2YXIgZm9ybUNvbmZpZ3VyYXRpb24gPSB7XG4gICAgICAgIHRleHRBcmVhRGlzcGxheTogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVZhbHVlID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGUgPT09IFwic2VhcmNoXCIpID8gXCJkaXNwbGF5Om5vbmVcIiA6IFwiZGlzcGxheTppbmhlcml0XCI7XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVZhbHVlO1xuICAgICAgICB9LFxuICAgICAgICBhY3Rpb25CdXR0b25EaXNwbGF5OiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBkaXNwbGF5VmFsdWUgPSAoR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcpID8gXCJkaXNwbGF5Om5vbmVcIiA6IFwiZGlzcGxheTppbmxpbmVcIjtcbiAgICAgICAgICAgIHJldHVybiBkaXNwbGF5VmFsdWU7XG4gICAgICAgIH0sXG4gICAgICAgIHByZWxvYWRlckRpc3BsYXk6IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlWYWx1ZSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZykgPyBcImRpc3BsYXk6aW5oZXJpdFwiIDogXCJkaXNwbGF5Om5vbmVcIjtcbiAgICAgICAgICAgIHJldHVybiBkaXNwbGF5VmFsdWU7XG4gICAgICAgIH0sXG4gICAgICAgIGNoYW5nZUZvcm1Qcm9wZXJ0aWVzOiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBzdHlsZVByb3BlcnRpZXMgPSBcImN1cnNvcjpwb2ludGVyO1wiO1xuICAgICAgICAgICAgaWYgKEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNBZG1pbikge1xuICAgICAgICAgICAgICAgIHN0eWxlUHJvcGVydGllcyArPSBcImRpc3BsYXk6aW5oZXJpdFwiO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICBzdHlsZVByb3BlcnRpZXMgKz0gXCJkaXNwbGF5Om5vbmVcIjtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBzdHlsZVByb3BlcnRpZXM7XG4gICAgICAgIH1cbiAgICB9O1xuICAgIHZhciByZW5kZXJTZWFyY2hSZXN1bHRzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHZhciByZW5kZXJlZFJlc3VsdHMgPSBbXTtcbiAgICAgICAgdmFyIGRpc3BsYXlQcm9wZXJ0aWVzID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoTG9hZGluZykgPyB7cmVzdWx0czogXCJkaXNwbGF5Om5vbmVcIiwgcHJlbG9hZGVyOiBcImRpc3BsYXk6aW5oZXJpdFwifSA6IHtyZXN1bHRzOiBcImRpc3BsYXk6aW5oZXJpdFwiLCBwcmVsb2FkZXI6IFwiZGlzcGxheTpub25lXCJ9O1xuICAgICAgICB2YXIgdGl0bGVDdXJzb3JQcm9wZXJ0eSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmlzQWRtaW4pID8gXCJjdXJzb3I6ZGVmYXVsdFwiIDogXCJjdXJzb3I6cG9pbnRlclwiO1xuICAgICAgICB2YXIgcmVuZGVyQWRtaW5CdXR0b25zID0gZnVuY3Rpb24ocmVzdWx0KSB7XG4gICAgICAgICAgICB2YXIgYWRtaW5CdXR0b25zID0gW107XG4gICAgICAgICAgICBpZiAoR2FtZUZvcm0uY29udHJvbGxlci5pc0FkbWluKSB7XG4gICAgICAgICAgICAgICAgYWRtaW5CdXR0b25zID0gbShcImRpdi5jb2wteHMtM1wiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmdseXBoaWNvbi5nbHlwaGljb24tcmVtb3ZlLmdhbWUtc2VhcmNoLXJlc3VsdHMtYnV0dG9uXCIsIHtvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0RGVsZXRlSGFuZGxlci5iaW5kKEdhbWVGb3JtLmNvbnRyb2xsZXIsIHJlc3VsdC5pZCl9KSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uZ2x5cGhpY29uLmdseXBoaWNvbi1wZW5jaWwuZ2FtZS1zZWFyY2gtcmVzdWx0cy1idXR0b25cIiwge29uY2xpY2s6R2FtZUZvcm0uY29udHJvbGxlci5zZWxlY3RVcGRhdGVIYW5kbGVyLmJpbmQoR2FtZUZvcm0uY29udHJvbGxlciwgcmVzdWx0LmlkKX0pXG4gICAgICAgICAgICAgICAgXSk7XG4gICAgICAgICAgICB9O1xuICAgICAgICAgICAgcmV0dXJuIGFkbWluQnV0dG9ucztcbiAgICAgICAgfTtcbiAgICAgICAgaWYgKCFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzKSB8fCAhXy5pc0VtcHR5KEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSkge1xuICAgICAgICAgICAgcmVuZGVyZWRSZXN1bHRzID0gbShcImRpdlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7c3R5bGU6ZGlzcGxheVByb3BlcnRpZXMucmVzdWx0c30sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFttKFwiZGl2XCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ubWFwKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cywgZnVuY3Rpb24ocmVzdWx0LCBpbmRleCkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhciBiZ0NvbG9yID0gXCJiYWNrZ3JvdW5kLWNvbG9yOiNDRUNGRTBcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAoaW5kZXggJSAyID09IDApIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmdDb2xvciA9IFwiYmFja2dyb3VuZC1jb2xvcjojRkZGXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJldHVybiBtKFwiZGl2LnJvdy5yZXN1bHQtcm93XCIsIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbbShcImRpdi5jb2wteHMtOVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIHtzdHlsZTp0aXRsZUN1cnNvclByb3BlcnR5LCBvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIudGl0bGVDbGlja0hhbmRsZXIuYmluZChHYW1lRm9ybS5jb250cm9sbGVyLCByZXN1bHQuaWQpfSwgKHJlc3VsdC5uYW1lICsgXCIgW1wiICsgcmVzdWx0LnJlZ2lvbiArIFwiXSAoXCIgKyByZXN1bHQuc3lzdGVtTmFtZSArIFwiKVwiKSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlbmRlckFkbWluQnV0dG9ucyhyZXN1bHQpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTpkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gcmVuZGVyZWRSZXN1bHRzO1xuICAgIH07XG4gICAgcmV0dXJuIFttKFwiZGl2LnJvd1wiLFtcbiAgICAgICAgbShcImRpdi5jb2wteHMtMTJcIixbXG4gICAgICAgICAgICBtKFwiZGl2LnRleHQtZGFuZ2VyXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZXJyb3JNZXNzYWdlKSxcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFtcbiAgICAgICAgICAgICAgICBtKFwiaW5wdXQuZm9ybS1jb250cm9sXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLm5hbWUpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubmFtZSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJOYW1lXCJ9KSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJSZWdpb25cIiwgYWxsb3dDbGVhcjogdHJ1ZX19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXCJOVFNDXCIsIFwiTlRTQy1KXCIsIFwiUEFMXCJdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHtvbmNoYW5nZTpHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiU3lzdGVtXCIsIGFsbG93Q2xlYXI6IHRydWV9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc3lzdGVtcyksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ1XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi5jaGFuZ2VGb3JtUHJvcGVydGllcygpLCBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmFkZFN5c3RlbUhhbmRsZXJ9LCBcIitBZGQgU3lzdGVtXCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuZ2VucmVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiR2VucmVzXCIsIGFsbG93Q2xlYXI6IHRydWV9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2VucmVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdHJ1ZSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ1XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi5jaGFuZ2VGb3JtUHJvcGVydGllcygpLCBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmFkZEdlbnJlSGFuZGxlcn0sIFwiK0FkZCBHZW5yZVwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuY29tcGFuaWVzKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9uczoge3BsYWNlaG9sZGVyOiBcIkNvbXBhbmllc1wiLCBhbGxvd0NsZWFyOiB0cnVlfX0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRydWUpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidVwiLCB7c3R5bGU6Zm9ybUNvbmZpZ3VyYXRpb24uY2hhbmdlRm9ybVByb3BlcnRpZXMoKSwgb25jbGljazogR2FtZUZvcm0uY29udHJvbGxlci5hZGRDb21wYW55SGFuZGxlcn0sIFwiK0FkZCBDb21wYW55XCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImlucHV0LmZvcm0tY29udHJvbFwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJRdWFudGl0eVwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCB7c3R5bGU6Zm9ybUNvbmZpZ3VyYXRpb24udGV4dEFyZWFEaXNwbGF5KCl9LCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJwXCIsIFwiU2hvcnQgRGVzY3JpcHRpb25cIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ0ZXh0YXJlYVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ibHVyYil9LCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ibHVyYigpKSxcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2LmNoZWNrYm94XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImxhYmVsXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbnB1dFt0eXBlPWNoZWNrYm94XVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJjaGVja2VkXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc21hbnVhbCksIGNoZWNrZWQ6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc21hbnVhbCgpfSlcbiAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiTWFudWFsXCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJsYWJlbFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwiaW5wdXRbdHlwZT1jaGVja2JveF1cIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwiY2hlY2tlZFwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNib3gpLCBjaGVja2VkOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNib3goKX0pXG4gICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCBcIkJveFwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwge3N0eWxlOmZvcm1Db25maWd1cmF0aW9uLnRleHRBcmVhRGlzcGxheSgpfSwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwicFwiLCBcIk5vdGVzXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidGV4dGFyZWFcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubm90ZXMpfSwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubm90ZXMoKSksXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1zdWNjZXNzXCIsIHtzdHlsZTogZm9ybUNvbmZpZ3VyYXRpb24uYWN0aW9uQnV0dG9uRGlzcGxheSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uc3VibWl0SGFuZGxlcnNbR2FtZUZvcm0uY29udHJvbGxlci5mb3JtTW9kZV19LCBcInN1Ym1pdFwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLWRhbmdlclwiLCB7c3R5bGU6IGZvcm1Db25maWd1cmF0aW9uLmFjdGlvbkJ1dHRvbkRpc3BsYXkoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuY2FuY2VsQnV0dG9uSGFuZGxlcn0sIFwiY2FuY2VsXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwiaW1nW3NyYz0vaW1hZ2VzL2FqYXguZ2lmXVwiLCB7c3R5bGU6IGZvcm1Db25maWd1cmF0aW9uLnByZWxvYWRlckRpc3BsYXkoKX0pXG4gICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgIF0pLFxuICAgICAgICBdKVxuICAgIF0pLFxuICAgICAgICAgICAgcmVuZGVyU2VhcmNoUmVzdWx0cygpXG4gICAgICAgICAgIF07XG59O1xuIiwidmFyIEdhbWVUcmFja2VyQWRtaW4gPSB7fTtcblxuR2FtZVRyYWNrZXJBZG1pbi5Nb2RlbCA9IGZ1bmN0aW9uKGRlZmF1bHRFbXB0eVNldCwgYmFja3NpZGVVcmwpIHtcbiAgICByZXR1cm4gZnVuY3Rpb24gKGluaXRpYWxWYWx1ZXMpIHtcbiAgICAgICAgaWYgKGluaXRpYWxWYWx1ZXMpIHtcbiAgICAgICAgICAgIHRoaXMuYXR0cmlidXRlcyA9IChfLmlzRW1wdHkoaW5pdGlhbFZhbHVlcy5pZCkpID8gXy5leHRlbmQoe2lkOm51bGx9LCBfLmNsb25lKGluaXRpYWxWYWx1ZXMsdHJ1ZSkpIDogXy5jbG9uZShpbml0aWFsVmFsdWVzKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHRoaXMuYXR0cmlidXRlcyA9IGRlZmF1bHRFbXB0eVNldDtcbiAgICAgICAgfVxuXG4gICAgICAgIHRoaXMuYmFja3NpZGVVcmwgPSBiYWNrc2lkZVVybDtcblxuICAgICAgICB0aGlzLnNhdmUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgICAgIC8vIEZvciBiYWNrZW5kIHB1cnBvc2VzXG4gICAgICAgICAgICByZXR1cm4gbS5yZXF1ZXN0KHttZXRob2Q6IFwiUE9TVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBzZWxmLmJhY2tzaWRlVXJsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YTogXy5vbWl0KHNlbGYuYXR0cmlidXRlcywgXCJpZFwiKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgIHNlbGYuYXR0cmlidXRlcy5pZCA9IHJlc3BvbnNlLm5ld2lkO1xuICAgICAgICAgICAgICAgICAgICByZXR1cm4gcmVzcG9uc2U7XG4gICAgICAgICAgICAgICAgfSk7XG4gICAgICAgIH07XG5cbiAgICAgICAgdGhpcy51cGRhdGUgPSBmdW5jdGlvbihuZXdBdHRyaWJ1dGVzKSB7XG4gICAgICAgICAgICB2YXIgc2VsZiA9IHRoaXM7XG4gICAgICAgICAgICBfLmZvckluKG5ld0F0dHJpYnV0ZXMsIGZ1bmN0aW9uKHZhbHVlLCBrZXkpIHtcbiAgICAgICAgICAgICAgICBzZWxmLmF0dHJpYnV0ZXNba2V5XSA9IHZhbHVlO1xuICAgICAgICAgICAgfSk7XG4gICAgICAgICAgICByZXR1cm4gbS5yZXF1ZXN0KHttZXRob2Q6IFwiUFVUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IHNlbGYuYmFja3NpZGVVcmwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBzZWxmLmF0dHJpYnV0ZXNcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSk7XG4gICAgICAgIH07XG5cbiAgICAgICAgdGhpcy5yZW1vdmUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgICAgIHJldHVybiBtLnJlcXVlc3Qoe21ldGhvZDogXCJERUxFVEVcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogc2VsZi5iYWNrc2lkZVVybCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IHtpZDogc2VsZi5hdHRyaWJ1dGVzLmlkfX0pO1xuICAgICAgICB9O1xuXG4gICAgfTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uQ29tcGFueSA9IEdhbWVUcmFja2VyQWRtaW4uTW9kZWwoe2lkOm51bGwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lOiBcIlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaXNtYW51ZmFjdHVyZXI6IG51bGx9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIi9hZG1pbi9jb21wYW55L1wiKTtcblxuR2FtZVRyYWNrZXJBZG1pbi5TeXN0ZW0gPSBHYW1lVHJhY2tlckFkbWluLk1vZGVsKHsgaWQ6IG51bGwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lOiBcIlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbWFudWZhY3R1cmVyaWQ6IG51bGwgfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIi9hZG1pbi9zeXN0ZW0vXCIpO1xuXG5HYW1lVHJhY2tlckFkbWluLkdlbnJlID0gR2FtZVRyYWNrZXJBZG1pbi5Nb2RlbCh7IGlkOiBudWxsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbmFtZTogXCJcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1hbnVmYWN0dXJlcmlkOiBudWxsIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCIvYWRtaW4vZ2VucmUvXCIpO1xuIiwiR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzID0ge307XG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uRGlzcGxheVByb3BlcnRpZXMgPSBmdW5jdGlvbihpc0xvYWRpbmcpIHtcbiAgICB2YXIgZGlzcGxheVByb3BlcnRpZXMgPSAoaXNMb2FkaW5nKSA/IHtidXR0b246XCJkaXNwbGF5Om5vbmVcIiwgcHJlbG9hZGVyOlwiZGlzcGxheTppbmxpbmVcIn0gOiB7YnV0dG9uOlwiZGlzcGxheTppbmxpbmVcIiwgcHJlbG9hZGVyOlwiZGlzcGxheTpub25lXCJ9O1xuICAgIHJldHVybiBkaXNwbGF5UHJvcGVydGllcztcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25TZXQgPSBmdW5jdGlvbihpc0xvYWRpbmcsIHdoaWNoRm9ybSkge1xuICAgIHZhciBkaXNwbGF5UHJvcGVydGllcyA9IEdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25EaXNwbGF5UHJvcGVydGllcyhpc0xvYWRpbmcpO1xuICAgIHJldHVybiBtKFwiZGl2XCIsIFtcbiAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLXN1Y2Nlc3NcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bVt3aGljaEZvcm1dLnN1Ym1pdEhhbmRsZXJzW0dhbWVUcmFja2VyQWRtaW4udm0uZm9ybU1vZGVdfSwgXCJzdWJtaXRcIiksXG4gICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1kYW5nZXJcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lVHJhY2tlckFkbWluLnZtLnJldHVyblRvTWFpbkZvcm0uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCB3aGljaEZvcm0pfSwgXCJjYW5jZWxcIiksXG4gICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTpkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgIF0pO1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uID0ge307XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5Jbml0aWFsU2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgcmV0dXJuIG0oXCJkaXYjaW5pdGlhbEFkbWluXCIsIFwiV2VsY29tZSB0byB0aGUgS3VyaWJvIFNob2UgQWRtaW4gUGFuZWxcIik7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uU2VsZWN0U2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgdmFyIGRpc3BsYXlQcm9wZXJ0aWVzID0gR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvbkRpc3BsYXlQcm9wZXJ0aWVzKEdhbWVUcmFja2VyQWRtaW4udm0uaXNMb2FkaW5nKTtcbiAgICB2YXIgc2VsZWN0RGF0YVNldCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICB2YXIgZGF0YVNldCA9IFtdO1xuICAgICAgICBzd2l0Y2ggKEdhbWVUcmFja2VyQWRtaW4udm0uc2VsZWN0U2NyZWVuU3RhdGUpIHtcbiAgICAgICAgY2FzZSBcInN5c3RlbVwiOlxuICAgICAgICAgICAgZGF0YVNldCA9IF8ucGx1Y2soR2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1zLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICBicmVhaztcbiAgICAgICAgY2FzZSBcImNvbXBhbnlcIjpcbiAgICAgICAgICAgIGRhdGFTZXQgPSBfLnBsdWNrKEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICBicmVhaztcbiAgICAgICAgY2FzZSBcImdlbnJlXCI6XG4gICAgICAgICAgICBkYXRhU2V0ID0gXy5wbHVjayhHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlcywgXCJhdHRyaWJ1dGVzXCIpO1xuICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgIH07XG4gICAgICAgIHJldHVybiBkYXRhU2V0O1xuICAgIH07XG4gICAgcmV0dXJuIG0oXCJkaXYucm93XCIsW1xuICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLCBbXG4gICAgICAgICAgICBtKFwiZm9ybVwiLCBbXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZVRyYWNrZXJBZG1pbi52bS5jdXJyZW50U2VsZWN0RW50aXR5SWQsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6R2FtZVRyYWNrZXJBZG1pbi52bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOntwbGFjZWhvbGRlcjpcIlNlbGVjdCBhbiBpdGVtIHRvIGVkaXQgb3IgZGVsZXRlXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdERhdGFTZXQoKSlcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLXN1Y2Nlc3NcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bS5nZW5lcmFsSW5pdGlhdGVFZGl0fSwgXCJlZGl0XCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tZGFuZ2VyXCIsIHtzdHlsZTogZGlzcGxheVByb3BlcnRpZXMuYnV0dG9uLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bS5nZW5lcmFsRGVsZXRlfSwgXCJkZWxldGVcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTogZGlzcGxheVByb3BlcnRpZXMucHJlbG9hZGVyfSlcbiAgICAgICAgICAgICAgICBdKV0pXG4gICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uQ29tcGFueUZvcm1TY3JlZW4gPSBmdW5jdGlvbigpIHtcbiAgICByZXR1cm4gbShcImRpdi5yb3dcIixbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFttKFwiaW5wdXQuZm9ybS1jb250cm9sW3R5cGU9dGV4dF1cIiwge3BsYWNlaG9sZGVyOlwiQ29tcGFueSBOYW1lXCIsIG9uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSksIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmNvbXBhbnlGb3JtLmZpZWxkcy5uYW1lKCl9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGFiZWxcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbnB1dFt0eXBlPWNoZWNrYm94XVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJjaGVja2VkXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFueUZvcm0uZmllbGRzLmlzbWFudWZhY3R1cmVyKSwgY2hlY2tlZDogR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW55Rm9ybS5maWVsZHMuaXNtYW51ZmFjdHVyZXIoKX0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiSXMgdGhpcyBjb21wYW55IGEgY29uc29sZSBtYW51ZmFjdXR1cmVyP1wiKVxuICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvblNldChHYW1lVHJhY2tlckFkbWluLnZtLmlzTG9hZGluZywgXCJjb21wYW55Rm9ybVwiKVxuICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICBdKVxuICAgIF0pO1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uLkdlbnJlRm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBtKFwiZGl2LnJvd1wiLCBbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFsgbShcImlucHV0LmZvcm0tY29udHJvbFt0eXBlPXRleHRdXCIsIHtwbGFjZWhvbGRlcjpcIkdlbnJlIE5hbWVcIiwgb25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSksIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSgpfSksXG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uU2V0KEdhbWVUcmFja2VyQWRtaW4udm0uaXNMb2FkaW5nLCBcImdlbnJlRm9ybVwiKVxuICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uU3lzdGVtRm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciBtYW51ZmFjdHVyZXJEYXRhU2V0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBfLmZpbHRlcihfLnBsdWNrKEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIiksIHtpc21hbnVmYWN0dXJlcjoxfSk7XG4gICAgfTtcbiAgICByZXR1cm4gbShcImRpdi5yb3dcIiwgW1xuICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLCBbXG4gICAgICAgICAgICBtKFwiZm9ybVwiLCBbbShcImlucHV0LmZvcm0tY29udHJvbFt0eXBlPXRleHRdXCIsIHtwbGFjZWhvbGRlcjpcIlN5c3RlbSBOYW1lXCIsIG9uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5uYW1lKSwgdmFsdWU6IEdhbWVUcmFja2VyQWRtaW4udm0uc3lzdGVtRm9ybS5maWVsZHMubmFtZSgpfSksXG4gICAgICAgICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHsgb25jaGFuZ2U6R2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5tYW51ZmFjdHVyZXJpZCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOkdhbWVUcmFja2VyQWRtaW4udm0uc3lzdGVtRm9ybS5maWVsZHMubWFudWZhY3R1cmVyaWQoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6e3BsYWNlaG9sZGVyOlwiTWFudWZhY3R1cmVyXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtYW51ZmFjdHVyZXJEYXRhU2V0KClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidVtzdHlsZT1jdXJzb3I6cG9pbnRlcl1cIiwge29uY2xpY2s6IEdhbWVUcmFja2VyQWRtaW4udm0uY2hhbmdlVG9BZGRDb21wYW55fSwgXCIrQWRkIENvbXBhbnlcIilcbiAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgICAgIEdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25TZXQoR2FtZVRyYWNrZXJBZG1pbi52bS5pc0xvYWRpbmcsIFwic3lzdGVtRm9ybVwiKVxuICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uR2FtZUZvcm1TY3JlZW4gPSBHYW1lRm9ybS52aWV3O1xuXG5HYW1lVHJhY2tlckFkbWluLnZpZXcgPSBmdW5jdGlvbigpIHtcbiAgICB2YXIgcmVuZGVyU2NyZWVucyA9IGZ1bmN0aW9uKCkge1xuICAgICAgICByZXR1cm4gXy5tYXAoR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uLCBmdW5jdGlvbihzY3JlZW5Db250ZW50LCBzY3JlZW5OYW1lKSB7XG4gICAgICAgICAgICByZXR1cm4gbShcImRpdlwiLCB7c3R5bGU6XCJkaXNwbGF5OlwiK0dhbWVUcmFja2VyQWRtaW4udm0uc2hvdWxkRGlzcGxheVNjcmVlbihzY3JlZW5OYW1lKX0sIHNjcmVlbkNvbnRlbnQoKSk7XG4gICAgICAgIH0pO1xuICAgIH07XG4gICAgcmV0dXJuIFtcbiAgICAgICAgbShcIm5hdi5uYXZiYXIubmF2YmFyLWRlZmF1bHRcIiwgW1xuICAgICAgICAgICAgbShcImRpdi5jb250YWluZXItZmx1aWRcIiwgW1xuICAgICAgICAgICAgICAgIG0oXCJkaXYubmF2YmFyLWhlYWRlclwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJidXR0b24ubmF2YmFyLXRvZ2dsZS5jb2xsYXBzZWRbdHlwZT1idXR0b25dW2RhdGEtdG9nZ2xlPWNvbGxhcHNlXVtkYXRhLXRhcmdldD0jbWFpbi1uYXZdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmljb24tYmFyXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uaWNvbi1iYXJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwic3Bhbi5pY29uLWJhclwiKVxuICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXYjbWFpbi1uYXYuY29sbGFwc2UubmF2YmFyLWNvbGxhcHNlXCIsWyBcbiAgICAgICAgICAgICAgICAgICAgbShcInVsLm5hdi5uYXZiYXItbmF2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIkdhbWVzXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcInVsLmRyb3Bkb3duLW1lbnVbcm9sZT1tZW51XVwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJhZGRcIiwgXCJnYW1lXCIsIFwiR2FtZUZvcm1TY3JlZW5cIil9LCBcIkFkZCBHYW1lXCIpXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJzZWFyY2hcIiwgXCJnYW1lXCIsIFwiR2FtZUZvcm1TY3JlZW5cIil9LCBcIkVkaXQvRGVsZXRlIEdhbWVcIildKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwibGkuZHJvcGRvd25cIiwgW20oXCJhW2hyZWY9I10uZHJvcGRvd24tdG9nZ2xlW2RhdGEtdG9nZ2xlPWRyb3Bkb3duXVtyb2xlPWJ1dHRvbl1cIiwgXCJTeXN0ZW1zXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcInVsLmRyb3Bkb3duLW1lbnVbcm9sZT1tZW51XVwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJhZGRcIiwgXCJzeXN0ZW1cIiwgXCJTeXN0ZW1Gb3JtU2NyZWVuXCIpfSwgXCJBZGQgU3lzdGVtXCIpXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJzZWFyY2hcIiwgXCJzeXN0ZW1cIiwgXCJTZWxlY3RTY3JlZW5cIil9LCBcIkVkaXQvRGVsZXRlIFN5c3RlbVwiKV0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIkdlbnJlXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcInVsLmRyb3Bkb3duLW1lbnVbcm9sZT1tZW51XVwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJhZGRcIiwgXCJnZW5yZVwiLCBcIkdlbnJlRm9ybVNjcmVlblwiKX0sIFwiQWRkIEdlbnJlXCIpXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJzZWFyY2hcIiwgXCJnZW5yZVwiLCBcIlNlbGVjdFNjcmVlblwiKX0sIFwiRWRpdC9EZWxldGUgR2VucmVcIildKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwibGkuZHJvcGRvd25cIiwgW20oXCJhW2hyZWY9I10uZHJvcGRvd24tdG9nZ2xlW2RhdGEtdG9nZ2xlPWRyb3Bkb3duXVtyb2xlPWJ1dHRvbl1cIiwgXCJDb21wYW55XCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcInVsLmRyb3Bkb3duLW1lbnVbcm9sZT1tZW51XVwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJhZGRcIiwgXCJjb21wYW55XCIsIFwiQ29tcGFueUZvcm1TY3JlZW5cIil9LCBcIkFkZCBDb21wYW55XCIpXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpXCIsIFttKFwiYVtocmVmPSNdXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uanVtcFRvU2NyZWVuLmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgXCJzZWFyY2hcIiwgXCJjb21wYW55XCIsIFwiU2VsZWN0U2NyZWVuXCIpfSwgXCJFZGl0L0RlbGV0ZSBDb21wYW55XCIpXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgXSlcbiAgICAgICAgXSksXG4gICAgICAgIG0oXCJkaXYuY29udGFpbmVyXCIsIFtcbiAgICAgICAgICAgIG0oXCJkaXYudGV4dC1zdWNjZXNzXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uc3VjY2Vzc01lc3NhZ2UpLFxuICAgICAgICAgICAgbShcImRpdi50ZXh0LWRhbmdlclwiLCBHYW1lVHJhY2tlckFkbWluLnZtLmVycm9yTWVzc2FnZSksXG4gICAgICAgICAgICByZW5kZXJTY3JlZW5zKClcbiAgICAgICAgXSlcbiAgICBdO1xufTtcbiIsIkdhbWVUcmFja2VyQWRtaW4udm0gPSBuZXcgZnVuY3Rpb24oKSB7XG4gICAgdmFyIHZtID0ge307XG4gICAgdm0uaW5pdCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICBcbiAgICAgICAgdm0uZm9ybU1vZGUgPSBcIlwiO1xuICAgICAgICB2bS5zZWxlY3RTY3JlZW5TdGF0ZSA9IFwiXCI7XG4gICAgICAgIFxuICAgICAgICAvL1RoaXMgaXMgdXNlZCBhcyBhIHN0YWNrO1xuICAgICAgICB2bS5zY3JlZW5IaXN0b3J5ID0gW1wiSW5pdGlhbFNjcmVlblwiXTtcblxuICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiXCI7XG4gICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiXCI7XG4gICAgICAgIHZtLnJlcG9ydEludGVybmFsRXJyb3IgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiSW50ZXJuYWwgU2VydmVyIEVycm9yXCI7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY2xlYXJNZXNzYWdlcyA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlwiO1xuICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgICAgIHZtLm5vUmVzdWx0cyA9IFwiXCI7XG4gICAgICAgIH07XG4gICAgICAgIHZtLmNvbXBsZXRlUmVzZXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIHZtLnNlYXJjaFJlc3VsdHMgPSBbXTtcbiAgICAgICAgICAgIHZtLnN5c3RlbUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5nZW5yZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5jb21wYW55Rm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgIH07XG4gICAgICAgIFxuICAgICAgICB2bS5qdW1wVG9TY3JlZW4gPSBmdW5jdGlvbihmb3JtTW9kZSwgc2VsZWN0U2NyZWVuU3RhdGUsIHNjcmVlbk5hbWUpIHtcbiAgICAgICAgICAgIHZtLmNvbXBsZXRlUmVzZXQoKTtcbiAgICAgICAgICAgIHZtLmZvcm1Nb2RlID0gZm9ybU1vZGU7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmZvcm1Nb2RlID0gZm9ybU1vZGU7XG4gICAgICAgICAgICB2bS5zZWxlY3RTY3JlZW5TdGF0ZSA9IHNlbGVjdFNjcmVlblN0YXRlO1xuICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeSA9IFtzY3JlZW5OYW1lLCBcIkluaXRpYWxTY3JlZW5cIl07XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgIFxuICAgICAgICAvL1RoaXMgZGF0YSBpcyBhY3R1YWxseSBib290c3RyYXBlZCBhbmQgdGhlIHZhcmlhYmxlIGl0J3MgY29weWluZyBmcm9tIGlzIGluIHRoZSB0ZW1wbGF0ZVxuICAgICAgICB2bS5jb21wYW5pZXMgPSBfLm1hcChjb21wYW5pZXMsIGZ1bmN0aW9uKGNvbXBhbnkpIHsgcmV0dXJuIG5ldyBHYW1lVHJhY2tlckFkbWluLkNvbXBhbnkoY29tcGFueSk7IH0pO1xuICAgICAgICB2bS5nZW5yZXMgPSBfLm1hcChnZW5yZXMsIGZ1bmN0aW9uKGdlbnJlKSB7IHJldHVybiBuZXcgR2FtZVRyYWNrZXJBZG1pbi5HZW5yZShnZW5yZSk7IH0pO1xuICAgICAgICB2bS5zeXN0ZW1zID0gXy5tYXAoc3lzdGVtcywgZnVuY3Rpb24oc3lzdGVtKSB7IHJldHVybiBuZXcgR2FtZVRyYWNrZXJBZG1pbi5TeXN0ZW0oc3lzdGVtKTsgfSk7XG4gICAgICAgIFxuICAgICAgICB2bS5zaG91bGREaXNwbGF5U2NyZWVuID0gZnVuY3Rpb24oc2NyZWVuTmFtZSkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlQcm9wZXJ0eSA9IFwibm9uZVwiO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uc2NyZWVuSGlzdG9yeSkpIHtcbiAgICAgICAgICAgICAgICBkaXNwbGF5UHJvcGVydHkgPSAoc2NyZWVuTmFtZSA9PT0gdm0uc2NyZWVuSGlzdG9yeVswXSkgPyBcImluaGVyaXRcIiA6IFwibm9uZVwiO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGRpc3BsYXlQcm9wZXJ0eTtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY3JlYXRlQmFja0J1dHRvbiA9IGZ1bmN0aW9uKGNhbGxiYWNrKSB7XG4gICAgICAgICAgICByZXR1cm4gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICAgICAgY2FsbGJhY2soKTtcbiAgICAgICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnNoaWZ0KCk7XG4gICAgICAgICAgICB9O1xuICAgICAgICB9O1xuXG4gICAgICAgIHZtLnJldHVyblRvTWFpbkZvcm0gPSBmdW5jdGlvbih3aGljaEZvcm0pIHtcbiAgICAgICAgICAgIHZtW3doaWNoRm9ybV0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnNoaWZ0KCk7XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgLy9UaGlzIGlzIHNsaWdodGx5IGRpZmZlcmVudCBmcm9tIGp1bXBpbmcgdG8gYSBzY3JlZW4gYmVjYXVzZSB3ZSBtYXkgd2FudCB0aGUgZ2FtZSBmb3JtIHRvIGJlIGRpZmZlcmVudCBzaW5jZSBpdCdzIGl0cyBvd24gZW50aXR5XG4gICAgICAgIHZtLmdlbmVyYXRlQ2hhbmdlSGFuZGxlciA9IGZ1bmN0aW9uKG5ld1NjcmVlbikge1xuICAgICAgICAgICAgcmV0dXJuIGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgICAgIHZtLnNjcmVlbkhpc3RvcnkudW5zaGlmdChuZXdTY3JlZW4pO1xuICAgICAgICAgICAgICAgIHZtLmZvcm1Nb2RlID0gXCJhZGRcIjtcbiAgICAgICAgICAgIH07XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uY29tcGFueUZvcm0gPSBuZXcgR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0oe25hbWU6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlzbWFudWZhY3R1cmVyOiBtLnByb3AoZmFsc2UpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pO1xuICAgICAgICAvKiBUT0RPIFRoZSBhZGQgZnVuY3Rpb25zIGFyZSBiYXNpY2FsbHkgdGhlIHNhbWUuIFRoZXJlIHNob3VsZCBiZSBhIGdvb2Qgd2F5IG9mIHJlZmFjdG9yaW5nIHRoaXMgZWl0aGVyIGNyZWF0aW5nIGEgZnVuY2l0b24gZ2VuZXJhdG9yXG4gICAgICAgICAqIG9yIGNyZWF0aW5nIGEgY2hpbGQgb2JqZWN0XG4gICAgICAgICAqL1xuICAgICAgICB2bS5jb21wYW55Rm9ybS5zdWJtaXRIYW5kbGVycy5hZGQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZhciBuZXdDb21wYW55ID0gbmV3IEdhbWVUcmFja2VyQWRtaW4uQ29tcGFueSh2bS5jb21wYW55Rm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgY29uc29sZS5sb2codm0uY29tcGFueUZvcm0ucmV0dXJuRmllbGRzKCkpO1xuICAgICAgICAgICAgICAgIG5ld0NvbXBhbnkuc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0uc3RhcnRDb21wdXRhdGlvbigpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmNvbXBhbmllcy5wdXNoKG5ld0NvbXBhbnkpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuY29tcGFuaWVzID0gXy5wbHVjayh2bS5jb21wYW5pZXMsIFwiYXR0cmlidXRlc1wiKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGNvbXBhbnkgaGFzIGJlZW4gYWRkZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jb21wYW55Rm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBtLmVuZENvbXB1dGF0aW9uKCk7XG4gICAgICAgICAgICAgICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbnNvbGUubG9nKHJlc3BvbnNlKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIkNvdWxkIG5vdCBhZGQgdGhlIGNvbXBhbnlcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIHRoZSBuYW1lIG9mIHRoZSBjb21wYW55XCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uY3VycmVudENvbXBhbnlJbmRleCA9IG51bGw7XG4gICAgICAgIHZtLmNvbXBhbnlGb3JtLnN1Ym1pdEhhbmRsZXJzLnVwZGF0ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc051bGwodm0uY3VycmVudENvbXBhbnlJbmRleCkgJiYgIV8uaXNFbXB0eSh2bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZtLmNvbXBhbmllc1t2bS5jdXJyZW50Q29tcGFueUluZGV4XS51cGRhdGUodm0uY29tcGFueUZvcm0ucmV0dXJuRmllbGRzKCkpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGNvbXBhbnkgaGFzIGJlZW4gdXBkYXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBlbnRlciB0aGUgbmFtZSBvZiB0aGUgY29tcGFueVwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICBcbiAgICAgICAgdm0uZ2VucmVGb3JtID0gbmV3IEdhbWVUcmFja2VyU2hhcmVkLlRyYWNrZXJGb3JtKHtuYW1lOiBtLnByb3AoXCJcIil9KTtcbiAgICAgICAgdm0uZ2VucmVGb3JtLnN1Ym1pdEhhbmRsZXJzLmFkZCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZhciBuZXdHZW5yZSA9IG5ldyBHYW1lVHJhY2tlckFkbWluLkdlbnJlKHZtLmdlbnJlRm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgbmV3R2VucmUuc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmdlbnJlcy5wdXNoKG5ld0dlbnJlKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdlbnJlcyA9IF8ucGx1Y2sodm0uZ2VucmVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBnZW5yZSBoYXMgYmVlbiBhZGRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmdlbnJlRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJDb3VsZCBub3QgYWRkIHRoZSBnZW5yZVwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZW50ZXIgdGhlIG5hbWUgb2YgdGhlIGdlbnJlXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgICAgIHZtLmN1cnJlbnRHZW5yZUluZGV4ID0gbnVsbDtcbiAgICAgICAgdm0uZ2VucmVGb3JtLnN1Ym1pdEhhbmRsZXJzLnVwZGF0ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc051bGwodm0uY3VycmVudEdlbnJlSW5kZXgpICYmICFfLmlzRW1wdHkodm0uZ2VucmVGb3JtLmZpZWxkcy5uYW1lKCkpKSB7XG4gICAgICAgICAgICAgICAgdm0uZ2VucmVzW3ZtLmN1cnJlbnRHZW5yZUluZGV4XS51cGRhdGUodm0uZ2VucmVGb3JtLnJldHVybkZpZWxkcygpKVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBnZW5yZSBoYXMgYmVlbiB1cGRhdGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIHRoZSBuYW1lIG9mIHRoZSBnZW5yZVwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jaGFuZ2VUb0FkZENvbXBhbnkgPSB2bS5nZW5lcmF0ZUNoYW5nZUhhbmRsZXIoXCJDb21wYW55Rm9ybVNjcmVlblwiKTtcbiAgICAgICAgdm0uc3lzdGVtRm9ybSA9IG5ldyBHYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSh7bmFtZTogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtYW51ZmFjdHVyZXJpZDogbS5wcm9wKFwiXCIpfSk7XG4gICAgICAgIHZtLnN5c3RlbUZvcm0uc3VibWl0SGFuZGxlcnMuYWRkID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uc3lzdGVtRm9ybS5maWVsZHMubmFtZSgpKSAmJiAhXy5pc0VtcHR5KHZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkKCkpKSB7XG4gICAgICAgICAgICAgICAgdmFyIG5ld1N5c3RlbSA9IG5ldyBHYW1lVHJhY2tlckFkbWluLlN5c3RlbSh2bS5zeXN0ZW1Gb3JtLnJldHVybkZpZWxkcygpKTtcbiAgICAgICAgICAgICAgICBuZXdTeXN0ZW0uc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN5c3RlbXMucHVzaChuZXdTeXN0ZW0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc3lzdGVtcyA9IF8ucGx1Y2sodm0uc3lzdGVtcywgXCJhdHRyaWJ1dGVzXCIpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgc3lzdGVtIGhhcyBiZWVuIGFkZGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3lzdGVtRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGZpbGwgaW4gYWxsIG9mIHRoZSBmaWVsZHNcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY3VycmVudFN5c3RlbUluZGV4ID0gbnVsbDtcbiAgICAgICAgdm0uc3lzdGVtRm9ybS5zdWJtaXRIYW5kbGVycy51cGRhdGUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNOdWxsKHZtLmN1cnJlbnRTeXN0ZW1JbmRleCkgJiYgIV8uaXNFbXB0eSh2bS5zeXN0ZW1Gb3JtLmZpZWxkcy5uYW1lKCkpICYmICFfLmlzRW1wdHkodm0uc3lzdGVtRm9ybS5maWVsZHMubWFudWZhY3R1cmVyaWQoKSkpIHtcbiAgICAgICAgICAgICAgICB2bS5zeXN0ZW1zW3ZtLmN1cnJlbnRTeXN0ZW1JbmRleF0udXBkYXRlKHZtLnN5c3RlbUZvcm0ucmV0dXJuRmllbGRzKCkpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIHN5c3RlbSBoYXMgYmVlbiB1cGRhdGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgfSk7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGZpbGwgaW4gYWxsIHRoZSBmaWVsZHNcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzQWRtaW4gPSB0cnVlO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmFkZENvbXBhbnlIYW5kbGVyID0gdm0uZ2VuZXJhdGVDaGFuZ2VIYW5kbGVyKFwiQ29tcGFueUZvcm1TY3JlZW5cIik7XG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuYWRkR2VucmVIYW5kbGVyID0gdm0uZ2VuZXJhdGVDaGFuZ2VIYW5kbGVyKFwiR2VucmVGb3JtU2NyZWVuXCIpO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmFkZFN5c3RlbUhhbmRsZXIgPSB2bS5nZW5lcmF0ZUNoYW5nZUhhbmRsZXIoXCJTeXN0ZW1Gb3JtU2NyZWVuXCIpO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnBvcHVsYXRlU2VsZWN0RGF0YVNldHMoXy5wbHVjayh2bS5zeXN0ZW1zLCBcImF0dHJpYnV0ZXNcIiksIF8ucGx1Y2sodm0uZ2VucmVzLCBcImF0dHJpYnV0ZXNcIiksIF8ucGx1Y2sodm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIikpO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNhbmNlbEJ1dHRvbkhhbmRsZXIgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnNoaWZ0KCk7XG4gICAgICAgIH07XG4gICAgICAgIFxuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmJpbmRTdWJtaXRGb3JtSGFuZGxlcihcImFkZFwiLCBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLm5hbWUoKSkgJiZcbiAgICAgICAgICAgICAgICAhXy5pc0VtcHR5KEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbigpKSAmJlxuICAgICAgICAgICAgICAgIF8uaXNGaW5pdGUoTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkKCkpKSAmJlxuICAgICAgICAgICAgICAgIE51bWJlcihHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCgpKSA+IDAgJiZcbiAgICAgICAgICAgICAgICBfLmlzRmluaXRlKE51bWJlcihHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpKSkgJiZcbiAgICAgICAgICAgICAgICBOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkoKSkgPiAwKSB7XG4gICAgICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6IFwiUE9TVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBcIi90aG9zZXdob2RhcmVub3R3YW5kZXIvZ2FtZS9cIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0ucmV0dXJuRmllbGRzKCl9KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJTdWNjZXNzZnVsbHkgYWRkZWQgdGhlIGdhbWVcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIGFsbCB0aGUgZmllbGRzXCI7XG4gICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfSk7XG5cbiAgICAgICAgdm0uY3VycmVudEdhbWVJZCA9IDA7XG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0VXBkYXRlSGFuZGxlciA9IGZ1bmN0aW9uKGdhbWVJZCkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChnYW1lSWQgJiYgXy5pc0Zpbml0ZShOdW1iZXIoZ2FtZUlkKSkpIHtcbiAgICAgICAgICAgICAgICAvKiBBIGtub3duIGxpbWl0YXRpb24gd2l0aCB0aGUgYmFja2VuZDogdGhpbmdzIHdlIGV4cGVjdCB0byBiZSBhbiBhcnJheSBtYXkgYmUgYSBzaW1wbGUgb2JqZWN0IGR1ZSB0byB0aGUganNvbiBlbmNvZGVyIG9uIHRoZSBiYWNrZW5kXG4gICAgICAgICAgICAgICAgICAgbm90IGJlaW5nIGFibGUgdG8gZW5jb2RlIHNpbmdsZSByb3cgcmVzdWx0cyBjb3JyZWN0bHlcbiAgICAgICAgICAgICAgICAgKi9cbiAgICAgICAgICAgICAgICB2YXIgZW5zdXJlQXJyYXkgPSBmdW5jdGlvbihpdGVtKSB7XG4gICAgICAgICAgICAgICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IF8uaXNBcnJheShpdGVtKSA/IGl0ZW0gOiBbaXRlbV07XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiByZXR1cm5WYWx1ZTtcbiAgICAgICAgICAgICAgICB9O1xuICAgICAgICAgICAgICAgIC8vV2UgY291bGQganVzdCB1c2UgdGhlIGRhdGEgd2UgcmV0cmlldmVkIGZyb20gdGhlIHNlYXJjaCBidXQgbGV0J3MgZ3VhcmFudGVlIHRoZSB1c2VyIHdpdGggdGhlIG1vc3QgcmVjZW50IGluZm9ybWF0aW9uXG4gICAgICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6IFwiR0VUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiB7aWQ6IE51bWJlcihnYW1lSWQpfVxuICAgICAgICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudEdhbWVJZCA9IE51bWJlcihyZXNwb25zZS5pZCk7XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5jb21wYW5pZXMoXy5wbHVjayhlbnN1cmVBcnJheShyZXNwb25zZS5jb21wYW5pZXMpLCBcImNvbXBhbnlJZFwiKSk7XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMoXy5wbHVjayhlbnN1cmVBcnJheShyZXNwb25zZS5nZW5yZXMpLCBcImdlbnJlSWRcIikpO1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5wb3B1bGF0ZUZvcm0oXy5vbWl0KHJlc3BvbnNlLCBbXCJjb21wYW5pZXNcIiwgXCJnZW5yZXNcIl0pKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGUgPSBcInVwZGF0ZVwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzID0gW107XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaExvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9XG4gICAgICAgIH07XG5cbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5iaW5kU3VibWl0Rm9ybUhhbmRsZXIoXCJ1cGRhdGVcIiwgZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eShHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5uYW1lKCkpICYmXG4gICAgICAgICAgICAgICAgIV8uaXNFbXB0eShHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSkgJiZcbiAgICAgICAgICAgICAgICBfLmlzRmluaXRlKE51bWJlcihHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCgpKSkgJiZcbiAgICAgICAgICAgICAgICBOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSkgPiAwICYmXG4gICAgICAgICAgICAgICAgXy5pc0Zpbml0ZShOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkoKSkpICYmXG4gICAgICAgICAgICAgICAgTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KCkpID4gMCkgeyAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgIHZhciBkYXRhID0gXy5leHRlbmQoe2lkOiBOdW1iZXIodm0uY3VycmVudEdhbWVJZCl9LCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLnJldHVybkZpZWxkcygpKTtcbiAgICAgICAgICAgICAgICBtLnJlcXVlc3Qoe21ldGhvZDogXCJQVVRcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvYWRtaW4vZ2FtZS9cIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IGRhdGF9KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiR2FtZSBzdWNjZXNzZnVsbHkgdXBkYXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICB9IFxuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZmlsbCBpbiB0aGUgZmllbGRzXCI7XG4gICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfSk7XG5cbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWxlY3REZWxldGVIYW5kbGVyID0gZnVuY3Rpb24oZ2FtZUlkKSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaExvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgaWYgKGdhbWVJZCAmJiBfLmlzRmluaXRlKE51bWJlcihnYW1lSWQpKSkge1xuICAgICAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOiBcIkRFTEVURVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvYWRtaW4vZ2FtZS9cIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiB7aWQ6IE51bWJlcihnYW1lSWQpfX0pXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgZ2FtZSBoYXMgYmVlbiBkZWxldGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5yZW1vdmUoR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzLCBmdW5jdGlvbihnYW1lKSB7IHJldHVybiBnYW1lLmlkID09PSBOdW1iZXIoZ2FtZUlkKTsgfSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuXG4gICAgICAgIHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCA9IG0ucHJvcChcIlwiKTtcbiAgICAgICAgdm0uZ2VuZXJhbEluaXRpYXRlRWRpdCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpKSB7XG4gICAgICAgICAgICAgICAgdm0uZm9ybU1vZGUgPSBcInVwZGF0ZVwiO1xuICAgICAgICAgICAgICAgIHN3aXRjaCAodm0uc2VsZWN0U2NyZWVuU3RhdGUpIHtcbiAgICAgICAgICAgICAgICBjYXNlIFwic3lzdGVtXCI6XG4gICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRTeXN0ZW1JbmRleCA9IF8uZmluZEluZGV4KHZtLnN5c3RlbXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLnN5c3RlbUZvcm0ucG9wdWxhdGVGb3JtKHZtLnN5c3RlbXNbdm0uY3VycmVudFN5c3RlbUluZGV4XSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLnNjcmVlbkhpc3RvcnkudW5zaGlmdChcIlN5c3RlbUZvcm1TY3JlZW5cIik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIGNhc2UgXCJjb21wYW55XCI6XG4gICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRDb21wYW55SW5kZXggPSBfLmZpbmRJbmRleCh2bS5jb21wYW5pZXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgIHZtLmNvbXBhbnlGb3JtLnBvcHVsYXRlRm9ybSh2bS5jb21wYW5pZXNbdm0uY3VycmVudENvbXBhbnlJbmRleF0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnVuc2hpZnQoXCJDb21wYW55Rm9ybVNjcmVlblwiKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgICAgICAgICAgY2FzZSBcImdlbnJlXCI6XG4gICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRHZW5yZUluZGV4ID0gXy5maW5kSW5kZXgodm0uZ2VucmVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5nZW5yZUZvcm0ucG9wdWxhdGVGb3JtKHZtLmdlbnJlc1t2bS5jdXJyZW50R2VucmVJbmRleF0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnVuc2hpZnQoXCJHZW5yZUZvcm1TY3JlZW5cIik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2Ugc2VsZWN0IGFuIGl0ZW0gaW4gdGhlIGRyb3Bkb3duXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgICAgIHZtLmdlbmVyYWxEZWxldGUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKSkge1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICAgICAgdmFyIGN1cnJlbnRJbmRleDtcbiAgICAgICAgICAgICAgICBzd2l0Y2ggKHZtLnNlbGVjdFNjcmVlblN0YXRlKSB7XG4gICAgICAgICAgICAgICAgY2FzZSBcInN5c3RlbVwiOlxuICAgICAgICAgICAgICAgICAgICBjdXJyZW50SW5kZXggPSBfLmZpbmRJbmRleCh2bS5zeXN0ZW1zLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5zeXN0ZW1zW2N1cnJlbnRJbmRleF0ucmVtb3ZlKClcbiAgICAgICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5yZW1vdmUodm0uc3lzdGVtcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBzeXN0ZW0gaGFzIGJlZW4gcmVtb3ZlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnN5c3RlbXMgPSBfLnBsdWNrKHZtLnN5c3RlbXMsIFwiYXR0cmlidXRlc1wiKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICB9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIGNhc2UgXCJjb21wYW55XCI6XG4gICAgICAgICAgICAgICAgICAgIGN1cnJlbnRJbmRleCA9IF8uZmluZEluZGV4KHZtLmNvbXBhbmllcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uY29tcGFuaWVzW2N1cnJlbnRJbmRleF0ucmVtb3ZlKClcbiAgICAgICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5yZW1vdmUodm0uY29tcGFuaWVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGNvbXBhbnkgaGFzIGJlZW4gcmVtb3ZlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNvbXBhbmllcyA9IF8ucGx1Y2sodm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgICAgICBjYXNlIFwiZ2VucmVcIjpcbiAgICAgICAgICAgICAgICAgICAgY3VycmVudEluZGV4ID0gXy5maW5kSW5kZXgodm0uZ2VucmVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5nZW5yZXNbY3VycmVudEluZGV4XS5yZW1vdmUoKVxuICAgICAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnJlbW92ZSh2bS5nZW5yZXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgZ2VucmUgaGFzIGJlZW4gcmVtb3ZlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdlbnJlcyA9IF8ucGx1Y2sodm0uZ2VucmVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgICAgICAgICBicmVhazsgICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAgfTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0ubWVzc2FnZUVycm9yID0gXCJTZWxlY3QgYW4gaXRlbSBmcm9tIHRoZSBkcm9wZG93blwiO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgIH07XG5cbiAgICByZXR1cm4gdm07XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLmNvbnRyb2xsZXIgPSBmdW5jdGlvbigpIHtcbiAgICBHYW1lVHJhY2tlckFkbWluLnZtLmluaXQoKTtcbn07XG4iLCIvL0ZvciB1c2Ugd2l0aCBhbGwgVmlld3MuIENvZGUgaXMgYmFzZWQgb24gdGhlIG9uZSBmb3VuZCBvbiBtaXRocmlsJ3Mgc2l0ZSBodHRwczovL2xob3JpZS5naXRodWIuaW8vbWl0aHJpbC9pbnRlZ3JhdGlvbi5odG1sXG52YXIgc2VsZWN0Mj0ge307XG5cbi8qIFRoaXMgZmFjdG9yeSBmdW5jdGlvbiBvZmZlcnMgYSBuaWNlIGNsb3N1cmUgZm9yIGFueXRoaW5nIGV4dHJhIHdlIHdhbnQgdG8gcGFzcyBpbiAqL1xuc2VsZWN0Mi5jb25maWcgPSBmdW5jdGlvbihleHRyYUFyZ3VtZW50cykge1xuICAgIHJldHVybiBmdW5jdGlvbihlbGVtZW50LCBpc0luaXRpYWxpemVkLCBjb250cm9sbGVyKSB7XG4gICAgICAgIHZhciBlbCA9ICQoZWxlbWVudCk7XG4gICAgICAgIGlmICghaXNJbml0aWFsaXplZCkge1xuICAgICAgICAgICAgaWYgKGV4dHJhQXJndW1lbnRzLnNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnMpIHtcbiAgICAgICAgICAgICAgICBlbC5zZWxlY3QyKGV4dHJhQXJndW1lbnRzLnNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnMpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICBlbC5zZWxlY3QyKCk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICBlbC5jaGFuZ2UoZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICAgICAgbS5zdGFydENvbXB1dGF0aW9uKCk7XG4gICAgICAgICAgICAgICAgZXh0cmFBcmd1bWVudHMub25jaGFuZ2UoZWwuc2VsZWN0MihcInZhbFwiKSk7XG4gICAgICAgICAgICAgICAgbS5lbmRDb21wdXRhdGlvbigpO1xuICAgICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgICAgZWwuc2VsZWN0MihcInZhbFwiLCBleHRyYUFyZ3VtZW50cy52YWx1ZSk7XG4gICAgfTtcbn07XG5cbnNlbGVjdDIudmlldyA9IGZ1bmN0aW9uKGV4dHJhQXJndW1lbnRzLCBvcHRpb25TZXQsIGlzTXVsdGlwbGUpIHtcbiAgICB2YXIgc2VsZWN0b3IgPSAoaXNNdWx0aXBsZSkgPyBcInNlbGVjdC5mb3JtLWNvbnRyb2xbbXVsdGlwbGU9dHJ1ZV1cIiA6IFwic2VsZWN0LmZvcm0tY29udHJvbFwiO1xuICAgIHZhciBjcmVhdGVPcHRpb25TZXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgdmFyIG9wdGlvbnMgPSBbXTtcbiAgICAgICAgaWYgKG9wdGlvblNldCkge1xuICAgICAgICAgICAgb3B0aW9ucyA9IF8ubWFwKG9wdGlvblNldCwgZnVuY3Rpb24odmFsdWUpIHtcbiAgICAgICAgICAgICAgICB2YXIgcmV0dXJuVmFsdWUgPSAoXy5pc09iamVjdCh2YWx1ZSkpID8gbShcIm9wdGlvblwiLCB7dmFsdWU6IHZhbHVlLmlkfSwgdmFsdWUubmFtZSkgOiBtKFwib3B0aW9uXCIsIHZhbHVlKTtcbiAgICAgICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gb3B0aW9ucztcbiAgICB9O1xuICAgIHJldHVybiBtKHNlbGVjdG9yLCB7Y29uZmlnOnNlbGVjdDIuY29uZmlnKGV4dHJhQXJndW1lbnRzKX0sXG4gICAgICAgICAgICAgW20oXCJvcHRpb25cIiksY3JlYXRlT3B0aW9uU2V0KCldKTtcbn07XG4iLCJcblxuXG5cblxuXG5cblxuXG5cbm0ubW9kdWxlKGRvY3VtZW50LmJvZHksIEdhbWVUcmFja2VyQWRtaW4pO1xuIl0sInNvdXJjZVJvb3QiOiIvc291cmNlLyJ9