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
            return field();
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
                                                       hasmanual: m.prop(false),
                                                       hasbox: m.prop(false),
                                                       notes: m.prop(""),
                                                       quantity: m.prop(""),
                                                       genres: m.prop([]),
                                                       companies: m.prop([]),
                                                       systemid: m.prop("")});
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

    this.gameForm.submitHandlers.search = function() {
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
            var displayValue = (GameForm.controller.isLoading) ? "display:none" : "display:inherit";
            return displayValue;
        },
        preloaderDisplay: function() {
            var displayValue = (GameForm.controller.isLoading) ? "display:inherit" : "display:none";
            return displayValue;
        }
    };
    var renderSearchResults = function() {
        var renderedResults = [];
        var displayProperties = (GameForm.searchLoading) ? {results: "display:none", preloader: "display:inherit"} : {results: "display:inherit", preloader: "display:none"};
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
                                                 (result.name + " [" + result.region + "] (" + result.systemName + ")")),
                                               renderAdminButtons(result)]);
                                 }),
                                 m("img[src=/images/ajax.gif]", {style:displayProperties.preloader})
                                ]);
        }
        return renderedResults;
    };
    return [m("div.row",[
        m("div.col-xs-12",[
            m("form", [
                m("input.form-control", {onchange: m.withAttr("value", GameForm.controller.gameForm.fields.name),
                                         value: GameForm.controller.gameForm.fields.name(),
                                         placeholder: "Name"}),
                select2.view({onchange:GameForm.controller.gameForm.fields.region,
                              value: GameForm.controller.gameForm.fields.region(),
                              select2InitializationOptions: {placeholder: "Region"}},
                             ["NTSC", "NTSC-J", "PAL"]),
                select2.view({onchange:GameForm.controller.gameForm.fields.systemid,
                              value: GameForm.controller.gameForm.fields.systemid(),
                              select2InitializationOptions: {placeholder: "System"}},
                             _.pluck(GameForm.controller.systems, "attributes")),
                select2.view({onchange:GameForm.controller.gameForm.fields.genres,
                              value: GameForm.controller.gameForm.fields.genres(),
                              select2InitializationOptions: {placeholder: "Genres"}},
                             _.pluck(GameForm.controller.genres, "attributes"),
                             true),
                select2.view({onchange:GameForm.controller.gameForm.fields.companies,
                              value: GameForm.controller.gameForm.fields.companies(),
                              select2InitializationOptions: {placeholder: "Companies"}},
                             _.pluck(GameForm.controller.companies, "attributes"),
                             true),
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
            return m.request({method: "POST",
                              url: self.backsideUrl,
                              data:_.omit(self.attributes, "id")})
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
                              data: self.attributes});
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
    return m("div.row", [
        m("div.col-xs-12", [
            m("form", [m("input.form-control[type=text]", {placeholder:"System Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.systemForm.fields.name), value: GameTrackerAdmin.vm.systemForm.fields.name()}),
                       m("div", [
                           select2.view({ onchange:GameTrackerAdmin.vm.systemForm.fields.manufacturerid,
                                          value:GameTrackerAdmin.vm.systemForm.fields.manufacturerid(),
                                          select2InitializationOptions:{placeholder:"Manufacturer"}},
                                        _.filter(_.pluck(GameTrackerAdmin.vm.companies, "attributes"), {ismanufacturer:1})),
                           m("u[style=cursor:pointer]", {onclick: GameTrackerAdmin.vm.screenHistory.unshift.bind(GameTrackerAdmin.vm.screenHistory, "AddCompanyScreen")}, "+Add Company")
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
                            vm.companies.push(newCompany);
                            vm.successMessage = "The company has been added";
                            vm.companyForm.clearForm();
                            vm.isLoading = false;
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
        GameForm.controller.populateSelectDataSets(vm.systems, vm.genres, vm.companies);
        GameForm.controller.cancelButtonHandler = function() {
            GameForm.controller.gameForm.clearForm();
            vm.screenHistory.shift();
        };
        
        GameForm.controller.bindSubmitFormHandler("add", function() {
            console.log('blorp');
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
                            }
                        },
                              vm.reportInternalError);
                    break;
                case "genre":
                    currentIndex = _.findIndex(vm.genres, {attributes: {id: Number(vm.currentSelectEntityId())}});
                    vm.genres[currentIndex].remove()
                        .then(function(response) {
                            if (response.status === "success") {
                                console.log(vm.currentSelectEntityId());
                                _.remove(vm.genres, {attributes: {id: Number(vm.currentSelectEntityId())}});
                                vm.successMessage = "The genre has been removed";
                                vm.currentSelectEntityId("");
                                vm.isLoading = false;
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

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImdhbWV0cmFja2Vyc2hhcmVkLmpzIiwiZ2FtZWZvcm0uanMiLCJnYW1lZm9ybXZpZXcuanMiLCJhZG1pbm1vZGVscy5qcyIsImFkbWludmlld3MuanMiLCJhZG1pbnZtY29udHJvbGxlci5qcyIsInNlbGVjdDJtaXRocmlsLmpzIiwiZ2FtZXRyYWNrZXJhZG1pbi5qcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDOUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDM0VBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUM3R0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUN6REE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDOUpBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDaFlBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3RDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0EiLCJmaWxlIjoiYWRtaW4uanMiLCJzb3VyY2VzQ29udGVudCI6WyJ2YXIgR2FtZVRyYWNrZXJTaGFyZWQgPSB7fTtcblxuR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0gPSBmdW5jdGlvbihmaWVsZHMpIHtcbiAgICB0aGlzLmZpZWxkcyA9IGZpZWxkcztcbiAgICB0aGlzLnBvcHVsYXRlRm9ybSA9IGZ1bmN0aW9uKG9iamVjdCkge1xuICAgICAgICB2YXIgc2VsZiA9IHRoaXM7XG4gICAgICAgIGlmIChvYmplY3QuYXR0cmlidXRlcykge1xuICAgICAgICAgICAgXy5tYXAob2JqZWN0LmF0dHJpYnV0ZXMsIGZ1bmN0aW9uKGF0dHJpYnV0ZVZhbHVlLCBhdHRyaWJ1dGVLZXkpIHtcbiAgICAgICAgICAgICAgICBpZiAoYXR0cmlidXRlS2V5ICE9PSBcImlkXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgc2VsZi5maWVsZHNbYXR0cmlidXRlS2V5XShhdHRyaWJ1dGVWYWx1ZSk7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgfSk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBfLm1hcChvYmplY3QsIGZ1bmN0aW9uKHZhbHVlLCBrZXkpIHtcbiAgICAgICAgICAgICAgICBpZiAoa2V5ICE9PSBcImlkXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgc2VsZi5maWVsZHNba2V5XSh2YWx1ZSk7XG4gICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICB9O1xuICAgIHRoaXMuY2xlYXJGb3JtID0gXy5mb3JFYWNoLmJpbmQodGhpcywgdGhpcy5maWVsZHMsIGZ1bmN0aW9uKGlucHV0KSB7XG4gICAgICAgIGlmIChfLmlzU3RyaW5nKGlucHV0KCkpKSB7XG4gICAgICAgICAgICBpbnB1dChcIlwiKTtcbiAgICAgICAgfSBlbHNlIGlmIChfLmlzQXJyYXkoaW5wdXQoKSkpIHtcbiAgICAgICAgICAgIGlucHV0KFtdKTtcbiAgICAgICAgfSBlbHNlIGlmIChfLmlzQm9vbGVhbihpbnB1dCgpKSl7XG4gICAgICAgICAgICBpbnB1dChmYWxzZSk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBpbnB1dChudWxsKTtcbiAgICAgICAgfVxuICAgIH0pO1xuICAgIHRoaXMucmV0dXJuRmllbGRzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBfLm1hcFZhbHVlcyh0aGlzLmZpZWxkcywgZnVuY3Rpb24oZmllbGQpIHtcbiAgICAgICAgICAgIHJldHVybiBmaWVsZCgpO1xuICAgICAgICB9KTtcbiAgICB9O1xuICAgIHRoaXMuc3VibWl0SGFuZGxlcnMgPSB7fTtcbiAgICAvKiBUaGlzIHdpbGwgcHJvYmFibHkgYmUgcmVmYWN0b3JlZCBvdXQgaW4gdGhlIGZ1dHVyZSBnaXZlbiB0aGUgb25seSB0aGluZyB0aGF0IGhhcyBhIHNlYXJjaCBpcyB0aGUgZ2FtZSBmb3JtXG4gICAgICogVG8ga2VlcCB0aGluZ3MgZnJvbSBjb21wbGFpbmluZyBhYm91dCBhIG1pc3Npbmcga2V5IHdlIGFkZCBhbiBlbXB0eSBmdW5jdGlvbiBoZXJlXG4gICAgICovXG4gICAgdGhpcy5zdWJtaXRIYW5kbGVycy5zZWFyY2ggPSBmdW5jdGlvbigpIHsgLyplbXB0eSovIH07XG4gICAgdGhpcy5nZXRTdWJtaXRIYW5kbGVyID0gZnVuY3Rpb24oc3RhdGUpIHtcbiAgICAgICAgcmV0dXJuIHRoaXMuc3VibWl0SGFuZGxlcnNbc3RhdGVdO1xuICAgIH07XG5cbn07XG4iLCJ2YXIgR2FtZUZvcm0gPSB7fTtcblxuR2FtZUZvcm0uY29udHJvbGxlciA9IG5ldyBmdW5jdGlvbigpIHtcbiAgICB0aGlzLmdhbWVGb3JtID0gbmV3IEdhbWVUcmFja2VyU2hhcmVkLlRyYWNrZXJGb3JtKHtuYW1lOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmx1cmI6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZWdpb246IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBoYXNtYW51YWw6IG0ucHJvcChmYWxzZSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaGFzYm94OiBtLnByb3AoZmFsc2UpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5vdGVzOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcXVhbnRpdHk6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBnZW5yZXM6IG0ucHJvcChbXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29tcGFuaWVzOiBtLnByb3AoW10pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN5c3RlbWlkOiBtLnByb3AoXCJcIil9KTtcbiAgICB0aGlzLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgIHRoaXMuc2VhcmNoUmVzdWx0cyA9IFtdO1xuXG4gICAgdGhpcy5ub1Jlc3VsdHMgPSBcIlwiO1xuICAgIHRoaXMuZm9ybU1vZGUgPSBcInNlYXJjaFwiO1xuICAgIHRoaXMuaXNBZG1pbiA9IGZhbHNlO1xuXG4gICAgdGhpcy5zZWFyY2hMb2FkaW5nID0gZmFsc2U7XG5cbiAgICB0aGlzLnNlbGVjdFVwZGF0ZUhhbmRsZXI7XG4gICAgdGhpcy5zZWxlY3REZWxldGVIYW5kbGVyO1xuICAgIHRoaXMuY2FuY2VsQnV0dG9uSGFuZGxlcjtcblxuICAgIHRoaXMuc3lzdGVtcztcbiAgICB0aGlzLmdlbnJlcztcbiAgICB0aGlzLmNvbXBhbmllcztcblxuICAgIHRoaXMucG9wdWxhdGVTZWxlY3REYXRhU2V0cyA9IGZ1bmN0aW9uKHN5c3RlbXMsIGdlbnJlcywgY29tcGFuaWVzKSB7XG4gICAgICAgIHRoaXMuc3lzdGVtcyA9IHN5c3RlbXM7XG4gICAgICAgIHRoaXMuZ2VucmVzID0gZ2VucmVzO1xuICAgICAgICB0aGlzLmNvbXBhbmllcyA9IGNvbXBhbmllcztcbiAgICB9O1xuXG4gICAgdGhpcy5hZG1pblJlc3VsdEJ1dHRvbkhhbmRsZXJzID0gZnVuY3Rpb24oZWRpdGhhbmRsZXIsIGRlbGV0ZWhhbmRsZXIpICB7XG4gICAgICAgIHRoaXMuc2VsZWN0VXBkYXRlSGFuZGxlciA9IGVkaXRoYW5kbGVyO1xuICAgICAgICB0aGlzLnNlbGVjdERlbGV0ZUhhbmRsZXIgPSBkZWxldGVoYW5kbGVyO1xuICAgIH07XG5cbiAgICB0aGlzLmJpbmRTdWJtaXRGb3JtSGFuZGxlciA9IGZ1bmN0aW9uKHN0YXRlLCBoYW5kbGVyKSB7XG4gICAgICAgIHRoaXMuZ2FtZUZvcm0uc3VibWl0SGFuZGxlcnNbc3RhdGVdID0gaGFuZGxlcjtcbiAgICB9O1xuXG4gICAgdGhpcy5nYW1lRm9ybS5zdWJtaXRIYW5kbGVycy5zZWFyY2ggPSBmdW5jdGlvbigpIHtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICB2YXIgY29tcGxldGVkU2V0ID0gXy5vbWl0KEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0ucmV0dXJuRmllbGRzKCksIGZ1bmN0aW9uKHZhbHVlLCBrZXkpIHtcbiAgICAgICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IHRydWU7XG4gICAgICAgICAgICBpZiAoXy5pc0Jvb2xlYW4odmFsdWUpKSB7XG4gICAgICAgICAgICAgICAgcmV0dXJuVmFsdWUgPSAhdmFsdWU7XG4gICAgICAgICAgICB9IGVsc2UgaWYgKCFfLmlzRW1wdHkodmFsdWUpKSB7XG4gICAgICAgICAgICAgICAgcmV0dXJuVmFsdWUgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiByZXR1cm5WYWx1ZTtcbiAgICAgICAgfSk7XG4gICAgICAgIGlmICghXy5pc0VtcHR5KGNvbXBsZXRlZFNldCkpIHtcbiAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOlwicG9zdFwiLFxuICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL3NlYXJjaC1nYW1lcy1hamF4L1wiLFxuICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBjb21wbGV0ZWRTZXR9KVxuICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgIC8vRW1wdHkgcmVzdWx0cyBzZXQgcmV0dXJucyBhIHNpbmdsZSBpdGVtIGFycmF5IHdpdGggbnVsbCBiZWluZyB0aGF0IG9iamVjdFxuICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMgPSBfLnJlbW92ZShyZXNwb25zZS5yZXN1bHRzLCBmdW5jdGlvbihpdGVtKSB7IHJldHVybiAhXy5pc051bGwoaXRlbSk7IH0pO1xuICAgICAgICAgICAgICAgICAgICBpZiAoR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzLmxlbmd0aCA8IDEpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzID0gXCJObyBtYXRjaGVzIHdlcmUgZm91bmRcIjtcbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgIH0sIGZ1bmN0aW9uKCkgeyBHYW1lRm9ybS5jb250cm9sbGVyLmVycm9yTWVzc2FnZSA9IFwiSW50ZXJuYWwgU2VydmVyIEVycm9yXCI7fSApO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBlbnRlciBhdCBsZWFzdCBvbmUgc2VhcmNoIHBhcmFtZXRlclwiO1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgfTtcbn07XG4iLCJHYW1lRm9ybS52aWV3ID0gZnVuY3Rpb24oKSB7XG4gICAgdmFyIGZvcm1Db25maWd1cmF0aW9uID0ge1xuICAgICAgICB0ZXh0QXJlYURpc3BsYXk6IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlWYWx1ZSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmZvcm1Nb2RlID09PSBcInNlYXJjaFwiKSA/IFwiZGlzcGxheTpub25lXCIgOiBcImRpc3BsYXk6aW5oZXJpdFwiO1xuICAgICAgICAgICAgcmV0dXJuIGRpc3BsYXlWYWx1ZTtcbiAgICAgICAgfSxcbiAgICAgICAgYWN0aW9uQnV0dG9uRGlzcGxheTogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVZhbHVlID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nKSA/IFwiZGlzcGxheTpub25lXCIgOiBcImRpc3BsYXk6aW5oZXJpdFwiO1xuICAgICAgICAgICAgcmV0dXJuIGRpc3BsYXlWYWx1ZTtcbiAgICAgICAgfSxcbiAgICAgICAgcHJlbG9hZGVyRGlzcGxheTogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVZhbHVlID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nKSA/IFwiZGlzcGxheTppbmhlcml0XCIgOiBcImRpc3BsYXk6bm9uZVwiO1xuICAgICAgICAgICAgcmV0dXJuIGRpc3BsYXlWYWx1ZTtcbiAgICAgICAgfVxuICAgIH07XG4gICAgdmFyIHJlbmRlclNlYXJjaFJlc3VsdHMgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgdmFyIHJlbmRlcmVkUmVzdWx0cyA9IFtdO1xuICAgICAgICB2YXIgZGlzcGxheVByb3BlcnRpZXMgPSAoR2FtZUZvcm0uc2VhcmNoTG9hZGluZykgPyB7cmVzdWx0czogXCJkaXNwbGF5Om5vbmVcIiwgcHJlbG9hZGVyOiBcImRpc3BsYXk6aW5oZXJpdFwifSA6IHtyZXN1bHRzOiBcImRpc3BsYXk6aW5oZXJpdFwiLCBwcmVsb2FkZXI6IFwiZGlzcGxheTpub25lXCJ9O1xuICAgICAgICB2YXIgcmVuZGVyQWRtaW5CdXR0b25zID0gZnVuY3Rpb24ocmVzdWx0KSB7XG4gICAgICAgICAgICB2YXIgYWRtaW5CdXR0b25zID0gW107XG4gICAgICAgICAgICBpZiAoR2FtZUZvcm0uY29udHJvbGxlci5pc0FkbWluKSB7XG4gICAgICAgICAgICAgICAgYWRtaW5CdXR0b25zID0gbShcImRpdi5jb2wteHMtM1wiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmdseXBoaWNvbi5nbHlwaGljb24tcmVtb3ZlLmdhbWUtc2VhcmNoLXJlc3VsdHMtYnV0dG9uXCIsIHtvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0RGVsZXRlSGFuZGxlci5iaW5kKEdhbWVGb3JtLmNvbnRyb2xsZXIsIHJlc3VsdC5pZCl9KSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uZ2x5cGhpY29uLmdseXBoaWNvbi1wZW5jaWwuZ2FtZS1zZWFyY2gtcmVzdWx0cy1idXR0b25cIiwge29uY2xpY2s6R2FtZUZvcm0uY29udHJvbGxlci5zZWxlY3RVcGRhdGVIYW5kbGVyLmJpbmQoR2FtZUZvcm0uY29udHJvbGxlciwgcmVzdWx0LmlkKX0pXG4gICAgICAgICAgICAgICAgXSk7XG4gICAgICAgICAgICB9O1xuICAgICAgICAgICAgcmV0dXJuIGFkbWluQnV0dG9ucztcbiAgICAgICAgfTtcbiAgICAgICAgaWYgKCFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzKSB8fCAhXy5pc0VtcHR5KEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSkge1xuICAgICAgICAgICAgcmVuZGVyZWRSZXN1bHRzID0gbShcImRpdlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7c3R5bGU6ZGlzcGxheVByb3BlcnRpZXMucmVzdWx0c30sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFttKFwiZGl2XCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ubWFwKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cywgZnVuY3Rpb24ocmVzdWx0LCBpbmRleCkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhciBiZ0NvbG9yID0gXCJiYWNrZ3JvdW5kLWNvbG9yOiNDRUNGRTBcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAoaW5kZXggJSAyID09IDApIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmdDb2xvciA9IFwiYmFja2dyb3VuZC1jb2xvcjojRkZGXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJldHVybiBtKFwiZGl2LnJvdy5yZXN1bHQtcm93XCIsIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbbShcImRpdi5jb2wteHMtOVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAocmVzdWx0Lm5hbWUgKyBcIiBbXCIgKyByZXN1bHQucmVnaW9uICsgXCJdIChcIiArIHJlc3VsdC5zeXN0ZW1OYW1lICsgXCIpXCIpKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcmVuZGVyQWRtaW5CdXR0b25zKHJlc3VsdCldKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbShcImltZ1tzcmM9L2ltYWdlcy9hamF4LmdpZl1cIiwge3N0eWxlOmRpc3BsYXlQcm9wZXJ0aWVzLnByZWxvYWRlcn0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiByZW5kZXJlZFJlc3VsdHM7XG4gICAgfTtcbiAgICByZXR1cm4gW20oXCJkaXYucm93XCIsW1xuICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFtcbiAgICAgICAgICAgICAgICBtKFwiaW5wdXQuZm9ybS1jb250cm9sXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLm5hbWUpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubmFtZSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJOYW1lXCJ9KSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJSZWdpb25cIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXCJOVFNDXCIsIFwiTlRTQy1KXCIsIFwiUEFMXCJdKSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiU3lzdGVtXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5wbHVjayhHYW1lRm9ybS5jb250cm9sbGVyLnN5c3RlbXMsIFwiYXR0cmlidXRlc1wiKSksXG4gICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHtvbmNoYW5nZTpHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuZ2VucmVzKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiR2VucmVzXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5wbHVjayhHYW1lRm9ybS5jb250cm9sbGVyLmdlbnJlcywgXCJhdHRyaWJ1dGVzXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0cnVlKSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5jb21wYW5pZXMoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJDb21wYW5pZXNcIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnBsdWNrKEdhbWVGb3JtLmNvbnRyb2xsZXIuY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRydWUpLFxuICAgICAgICAgICAgICAgIG0oXCJpbnB1dC5mb3JtLWNvbnRyb2xcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcGxhY2Vob2xkZXI6IFwiUXVhbnRpdHlcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwge3N0eWxlOmZvcm1Db25maWd1cmF0aW9uLnRleHRBcmVhRGlzcGxheSgpfSwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwicFwiLCBcIlNob3J0IERlc2NyaXB0aW9uXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidGV4dGFyZWFcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuYmx1cmIpfSwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuYmx1cmIoKSksXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJsYWJlbFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwiaW5wdXRbdHlwZT1jaGVja2JveF1cIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwiY2hlY2tlZFwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNtYW51YWwpLCBjaGVja2VkOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNtYW51YWwoKX0pXG4gICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCBcIk1hbnVhbFwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXYuY2hlY2tib3hcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwibGFiZWxcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcImlucHV0W3R5cGU9Y2hlY2tib3hdXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcImNoZWNrZWRcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuaGFzYm94KSwgY2hlY2tlZDogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuaGFzYm94KCl9KVxuICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW5cIiwgXCJCb3hcIilcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi50ZXh0QXJlYURpc3BsYXkoKX0sIFtcbiAgICAgICAgICAgICAgICAgICAgbShcInBcIiwgXCJOb3Rlc1wiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInRleHRhcmVhXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLm5vdGVzKX0sIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLm5vdGVzKCkpLFxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tc3VjY2Vzc1wiLCB7c3R5bGU6IGZvcm1Db25maWd1cmF0aW9uLmFjdGlvbkJ1dHRvbkRpc3BsYXkoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLnN1Ym1pdEhhbmRsZXJzW0dhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGVdfSwgXCJzdWJtaXRcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1kYW5nZXJcIiwge3N0eWxlOiBmb3JtQ29uZmlndXJhdGlvbi5hY3Rpb25CdXR0b25EaXNwbGF5KCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmNhbmNlbEJ1dHRvbkhhbmRsZXJ9LCBcImNhbmNlbFwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcImltZ1tzcmM9L2ltYWdlcy9hamF4LmdpZl1cIiwge3N0eWxlOiBmb3JtQ29uZmlndXJhdGlvbi5wcmVsb2FkZXJEaXNwbGF5KCl9KVxuICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICBdKSxcbiAgICAgICAgXSlcbiAgICBdKSxcbiAgICAgICAgICAgIHJlbmRlclNlYXJjaFJlc3VsdHMoKVxuICAgICAgICAgICBdO1xufTtcbiIsInZhciBHYW1lVHJhY2tlckFkbWluID0ge307XG5cbkdhbWVUcmFja2VyQWRtaW4uTW9kZWwgPSBmdW5jdGlvbihkZWZhdWx0RW1wdHlTZXQsIGJhY2tzaWRlVXJsKSB7XG4gICAgcmV0dXJuIGZ1bmN0aW9uIChpbml0aWFsVmFsdWVzKSB7XG4gICAgICAgIGlmIChpbml0aWFsVmFsdWVzKSB7XG4gICAgICAgICAgICB0aGlzLmF0dHJpYnV0ZXMgPSAoXy5pc0VtcHR5KGluaXRpYWxWYWx1ZXMuaWQpKSA/IF8uZXh0ZW5kKHtpZDpudWxsfSwgXy5jbG9uZShpbml0aWFsVmFsdWVzLHRydWUpKSA6IF8uY2xvbmUoaW5pdGlhbFZhbHVlcyk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICB0aGlzLmF0dHJpYnV0ZXMgPSBkZWZhdWx0RW1wdHlTZXQ7XG4gICAgICAgIH1cblxuICAgICAgICB0aGlzLmJhY2tzaWRlVXJsID0gYmFja3NpZGVVcmw7XG5cbiAgICAgICAgdGhpcy5zYXZlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgc2VsZiA9IHRoaXM7XG4gICAgICAgICAgICByZXR1cm4gbS5yZXF1ZXN0KHttZXRob2Q6IFwiUE9TVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBzZWxmLmJhY2tzaWRlVXJsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YTpfLm9taXQoc2VsZi5hdHRyaWJ1dGVzLCBcImlkXCIpfSlcbiAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmF0dHJpYnV0ZXMuaWQgPSByZXNwb25zZS5uZXdpZDtcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIHJlc3BvbnNlO1xuICAgICAgICAgICAgICAgIH0pO1xuICAgICAgICB9O1xuXG4gICAgICAgIHRoaXMudXBkYXRlID0gZnVuY3Rpb24obmV3QXR0cmlidXRlcykge1xuICAgICAgICAgICAgdmFyIHNlbGYgPSB0aGlzO1xuICAgICAgICAgICAgXy5mb3JJbihuZXdBdHRyaWJ1dGVzLCBmdW5jdGlvbih2YWx1ZSwga2V5KSB7XG4gICAgICAgICAgICAgICAgc2VsZi5hdHRyaWJ1dGVzW2tleV0gPSB2YWx1ZTtcbiAgICAgICAgICAgIH0pO1xuICAgICAgICAgICAgcmV0dXJuIG0ucmVxdWVzdCh7bWV0aG9kOiBcIlBVVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBzZWxmLmJhY2tzaWRlVXJsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YTogc2VsZi5hdHRyaWJ1dGVzfSk7XG4gICAgICAgIH07XG5cbiAgICAgICAgdGhpcy5yZW1vdmUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgICAgIHJldHVybiBtLnJlcXVlc3Qoe21ldGhvZDogXCJERUxFVEVcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogc2VsZi5iYWNrc2lkZVVybCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IHtpZDogc2VsZi5hdHRyaWJ1dGVzLmlkfX0pO1xuICAgICAgICB9O1xuXG4gICAgfTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uQ29tcGFueSA9IEdhbWVUcmFja2VyQWRtaW4uTW9kZWwoe2lkOm51bGwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lOiBcIlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaXNtYW51ZmFjdHVyZXI6IG51bGx9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIi9hZG1pbi9jb21wYW55L1wiKTtcblxuR2FtZVRyYWNrZXJBZG1pbi5TeXN0ZW0gPSBHYW1lVHJhY2tlckFkbWluLk1vZGVsKHsgaWQ6IG51bGwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lOiBcIlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbWFudWZhY3R1cmVyaWQ6IG51bGwgfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIi9hZG1pbi9zeXN0ZW0vXCIpO1xuXG5HYW1lVHJhY2tlckFkbWluLkdlbnJlID0gR2FtZVRyYWNrZXJBZG1pbi5Nb2RlbCh7IGlkOiBudWxsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbmFtZTogXCJcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1hbnVmYWN0dXJlcmlkOiBudWxsIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCIvYWRtaW4vZ2VucmUvXCIpO1xuIiwiR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzID0ge307XG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uRGlzcGxheVByb3BlcnRpZXMgPSBmdW5jdGlvbihpc0xvYWRpbmcpIHtcbiAgICB2YXIgZGlzcGxheVByb3BlcnRpZXMgPSAoaXNMb2FkaW5nKSA/IHtidXR0b246XCJkaXNwbGF5Om5vbmVcIiwgcHJlbG9hZGVyOlwiZGlzcGxheTppbmxpbmVcIn0gOiB7YnV0dG9uOlwiZGlzcGxheTppbmxpbmVcIiwgcHJlbG9hZGVyOlwiZGlzcGxheTpub25lXCJ9O1xuICAgIHJldHVybiBkaXNwbGF5UHJvcGVydGllcztcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25TZXQgPSBmdW5jdGlvbihpc0xvYWRpbmcsIHdoaWNoRm9ybSkge1xuICAgIHZhciBkaXNwbGF5UHJvcGVydGllcyA9IEdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25EaXNwbGF5UHJvcGVydGllcyhpc0xvYWRpbmcpO1xuICAgIHJldHVybiBtKFwiZGl2XCIsIFtcbiAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLXN1Y2Nlc3NcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bVt3aGljaEZvcm1dLnN1Ym1pdEhhbmRsZXJzW0dhbWVUcmFja2VyQWRtaW4udm0uZm9ybU1vZGVdfSwgXCJzdWJtaXRcIiksXG4gICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1kYW5nZXJcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lVHJhY2tlckFkbWluLnZtLnJldHVyblRvTWFpbkZvcm0uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCB3aGljaEZvcm0pfSwgXCJjYW5jZWxcIiksXG4gICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTpkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgIF0pO1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uID0ge307XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5Jbml0aWFsU2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgcmV0dXJuIG0oXCJkaXYjaW5pdGlhbEFkbWluXCIsIFwiV2VsY29tZSB0byB0aGUgS3VyaWJvIFNob2UgQWRtaW4gUGFuZWxcIik7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uU2VsZWN0U2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgdmFyIGRpc3BsYXlQcm9wZXJ0aWVzID0gR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvbkRpc3BsYXlQcm9wZXJ0aWVzKEdhbWVUcmFja2VyQWRtaW4udm0uaXNMb2FkaW5nKTtcbiAgICB2YXIgc2VsZWN0RGF0YVNldCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICB2YXIgZGF0YVNldCA9IFtdO1xuICAgICAgICBzd2l0Y2ggKEdhbWVUcmFja2VyQWRtaW4udm0uc2VsZWN0U2NyZWVuU3RhdGUpIHtcbiAgICAgICAgY2FzZSBcInN5c3RlbVwiOlxuICAgICAgICAgICAgZGF0YVNldCA9IF8ucGx1Y2soR2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1zLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICBicmVhaztcbiAgICAgICAgY2FzZSBcImNvbXBhbnlcIjpcbiAgICAgICAgICAgIGRhdGFTZXQgPSBfLnBsdWNrKEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICBicmVhaztcbiAgICAgICAgY2FzZSBcImdlbnJlXCI6XG4gICAgICAgICAgICBkYXRhU2V0ID0gXy5wbHVjayhHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlcywgXCJhdHRyaWJ1dGVzXCIpO1xuICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgIH07XG4gICAgICAgIHJldHVybiBkYXRhU2V0O1xuICAgIH07XG4gICAgcmV0dXJuIG0oXCJkaXYucm93XCIsW1xuICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLCBbXG4gICAgICAgICAgICBtKFwiZm9ybVwiLCBbXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZVRyYWNrZXJBZG1pbi52bS5jdXJyZW50U2VsZWN0RW50aXR5SWQsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6R2FtZVRyYWNrZXJBZG1pbi52bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOntwbGFjZWhvbGRlcjpcIlNlbGVjdCBhbiBpdGVtIHRvIGVkaXQgb3IgZGVsZXRlXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdERhdGFTZXQoKSlcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLXN1Y2Nlc3NcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bS5nZW5lcmFsSW5pdGlhdGVFZGl0fSwgXCJlZGl0XCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tZGFuZ2VyXCIsIHtzdHlsZTogZGlzcGxheVByb3BlcnRpZXMuYnV0dG9uLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bS5nZW5lcmFsRGVsZXRlfSwgXCJkZWxldGVcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTogZGlzcGxheVByb3BlcnRpZXMucHJlbG9hZGVyfSlcbiAgICAgICAgICAgICAgICBdKV0pXG4gICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uQ29tcGFueUZvcm1TY3JlZW4gPSBmdW5jdGlvbigpIHtcbiAgICByZXR1cm4gbShcImRpdi5yb3dcIixbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFttKFwiaW5wdXQuZm9ybS1jb250cm9sW3R5cGU9dGV4dF1cIiwge3BsYWNlaG9sZGVyOlwiQ29tcGFueSBOYW1lXCIsIG9uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSksIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmNvbXBhbnlGb3JtLmZpZWxkcy5uYW1lKCl9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGFiZWxcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbnB1dFt0eXBlPWNoZWNrYm94XVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJjaGVja2VkXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFueUZvcm0uZmllbGRzLmlzbWFudWZhY3R1cmVyKSwgY2hlY2tlZDogR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW55Rm9ybS5maWVsZHMuaXNtYW51ZmFjdHVyZXIoKX0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiSXMgdGhpcyBjb21wYW55IGEgY29uc29sZSBtYW51ZmFjdXR1cmVyP1wiKVxuICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvblNldChHYW1lVHJhY2tlckFkbWluLnZtLmlzTG9hZGluZywgXCJjb21wYW55Rm9ybVwiKVxuICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICBdKVxuICAgIF0pO1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uLkdlbnJlRm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBtKFwiZGl2LnJvd1wiLCBbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFsgbShcImlucHV0LmZvcm0tY29udHJvbFt0eXBlPXRleHRdXCIsIHtwbGFjZWhvbGRlcjpcIkdlbnJlIE5hbWVcIiwgb25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSksIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSgpfSksXG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uU2V0KEdhbWVUcmFja2VyQWRtaW4udm0uaXNMb2FkaW5nLCBcImdlbnJlRm9ybVwiKVxuICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uU3lzdGVtRm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBtKFwiZGl2LnJvd1wiLCBbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFttKFwiaW5wdXQuZm9ybS1jb250cm9sW3R5cGU9dGV4dF1cIiwge3BsYWNlaG9sZGVyOlwiU3lzdGVtIE5hbWVcIiwgb25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbUZvcm0uZmllbGRzLm5hbWUpLCB2YWx1ZTogR2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5uYW1lKCl9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoeyBvbmNoYW5nZTpHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6R2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5tYW51ZmFjdHVyZXJpZCgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9uczp7cGxhY2Vob2xkZXI6XCJNYW51ZmFjdHVyZXJcIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8uZmlsdGVyKF8ucGx1Y2soR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW5pZXMsIFwiYXR0cmlidXRlc1wiKSwge2lzbWFudWZhY3R1cmVyOjF9KSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidVtzdHlsZT1jdXJzb3I6cG9pbnRlcl1cIiwge29uY2xpY2s6IEdhbWVUcmFja2VyQWRtaW4udm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0LmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bS5zY3JlZW5IaXN0b3J5LCBcIkFkZENvbXBhbnlTY3JlZW5cIil9LCBcIitBZGQgQ29tcGFueVwiKVxuICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvblNldChHYW1lVHJhY2tlckFkbWluLnZtLmlzTG9hZGluZywgXCJzeXN0ZW1Gb3JtXCIpXG4gICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgXSlcbiAgICBdKTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5HYW1lRm9ybVNjcmVlbiA9IEdhbWVGb3JtLnZpZXc7XG5cbkdhbWVUcmFja2VyQWRtaW4udmlldyA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciByZW5kZXJTY3JlZW5zID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBfLm1hcChHYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24sIGZ1bmN0aW9uKHNjcmVlbkNvbnRlbnQsIHNjcmVlbk5hbWUpIHtcbiAgICAgICAgICAgIHJldHVybiBtKFwiZGl2XCIsIHtzdHlsZTpcImRpc3BsYXk6XCIrR2FtZVRyYWNrZXJBZG1pbi52bS5zaG91bGREaXNwbGF5U2NyZWVuKHNjcmVlbk5hbWUpfSwgc2NyZWVuQ29udGVudCgpKTtcbiAgICAgICAgfSk7XG4gICAgfTtcbiAgICByZXR1cm4gW1xuICAgICAgICBtKFwibmF2Lm5hdmJhci5uYXZiYXItZGVmYXVsdFwiLCBbXG4gICAgICAgICAgICBtKFwiZGl2LmNvbnRhaW5lci1mbHVpZFwiLCBbXG4gICAgICAgICAgICAgICAgbShcImRpdi5uYXZiYXItaGVhZGVyXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5uYXZiYXItdG9nZ2xlLmNvbGxhcHNlZFt0eXBlPWJ1dHRvbl1bZGF0YS10b2dnbGU9Y29sbGFwc2VdW2RhdGEtdGFyZ2V0PSNtYWluLW5hdl1cIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uaWNvbi1iYXJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwic3Bhbi5pY29uLWJhclwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmljb24tYmFyXCIpXG4gICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdiNtYWluLW5hdi5jb2xsYXBzZS5uYXZiYXItY29sbGFwc2VcIixbIFxuICAgICAgICAgICAgICAgICAgICBtKFwidWwubmF2Lm5hdmJhci1uYXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpLmRyb3Bkb3duXCIsIFttKFwiYVtocmVmPSNdLmRyb3Bkb3duLXRvZ2dsZVtkYXRhLXRvZ2dsZT1kcm9wZG93bl1bcm9sZT1idXR0b25dXCIsIFwiR2FtZXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImdhbWVcIiwgXCJHYW1lRm9ybVNjcmVlblwiKX0sIFwiQWRkIEdhbWVcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImdhbWVcIiwgXCJHYW1lRm9ybVNjcmVlblwiKX0sIFwiRWRpdC9EZWxldGUgR2FtZVwiKV0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIlN5c3RlbXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcInN5c3RlbVwiLCBcIlN5c3RlbUZvcm1TY3JlZW5cIil9LCBcIkFkZCBTeXN0ZW1cIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcInN5c3RlbVwiLCBcIlNlbGVjdFNjcmVlblwiKX0sIFwiRWRpdC9EZWxldGUgU3lzdGVtXCIpXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpLmRyb3Bkb3duXCIsIFttKFwiYVtocmVmPSNdLmRyb3Bkb3duLXRvZ2dsZVtkYXRhLXRvZ2dsZT1kcm9wZG93bl1bcm9sZT1idXR0b25dXCIsIFwiR2VucmVcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImdlbnJlXCIsIFwiR2VucmVGb3JtU2NyZWVuXCIpfSwgXCJBZGQgR2VucmVcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImdlbnJlXCIsIFwiU2VsZWN0U2NyZWVuXCIpfSwgXCJFZGl0L0RlbGV0ZSBHZW5yZVwiKV0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIkNvbXBhbnlcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImNvbXBhbnlcIiwgXCJDb21wYW55Rm9ybVNjcmVlblwiKX0sIFwiQWRkIENvbXBhbnlcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImNvbXBhbnlcIiwgXCJTZWxlY3RTY3JlZW5cIil9LCBcIkVkaXQvRGVsZXRlIENvbXBhbnlcIildKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICBdKVxuICAgICAgICBdKSxcbiAgICAgICAgbShcImRpdi5jb250YWluZXJcIiwgW1xuICAgICAgICAgICAgbShcImRpdi50ZXh0LXN1Y2Nlc3NcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5zdWNjZXNzTWVzc2FnZSksXG4gICAgICAgICAgICBtKFwiZGl2LnRleHQtZGFuZ2VyXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uZXJyb3JNZXNzYWdlKSxcbiAgICAgICAgICAgIHJlbmRlclNjcmVlbnMoKVxuICAgICAgICBdKVxuICAgIF07XG59O1xuIiwiR2FtZVRyYWNrZXJBZG1pbi52bSA9IG5ldyBmdW5jdGlvbigpIHtcbiAgICB2YXIgdm0gPSB7fTtcbiAgICB2bS5pbml0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIFxuICAgICAgICB2bS5mb3JtTW9kZSA9IFwiXCI7XG4gICAgICAgIHZtLnNlbGVjdFNjcmVlblN0YXRlID0gXCJcIjtcbiAgICAgICAgXG4gICAgICAgIC8vVGhpcyBpcyB1c2VkIGFzIGEgc3RhY2s7XG4gICAgICAgIHZtLnNjcmVlbkhpc3RvcnkgPSBbXCJJbml0aWFsU2NyZWVuXCJdO1xuXG4gICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvciA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJJbnRlcm5hbCBTZXJ2ZXIgRXJyb3JcIjtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICB2bS5jbGVhck1lc3NhZ2VzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiXCI7XG4gICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlwiO1xuICAgICAgICAgICAgdm0ubm9SZXN1bHRzID0gXCJcIjtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY29tcGxldGVSZXNldCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgdm0uc2VhcmNoUmVzdWx0cyA9IFtdO1xuICAgICAgICAgICAgdm0uc3lzdGVtRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLmdlbnJlRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLmNvbXBhbnlGb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZChcIlwiKTtcbiAgICAgICAgfTtcbiAgICAgICAgXG4gICAgICAgIHZtLmp1bXBUb1NjcmVlbiA9IGZ1bmN0aW9uKGZvcm1Nb2RlLCBzZWxlY3RTY3JlZW5TdGF0ZSwgc2NyZWVuTmFtZSkge1xuICAgICAgICAgICAgdm0uY29tcGxldGVSZXNldCgpO1xuICAgICAgICAgICAgdm0uZm9ybU1vZGUgPSBmb3JtTW9kZTtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGUgPSBmb3JtTW9kZTtcbiAgICAgICAgICAgIHZtLnNlbGVjdFNjcmVlblN0YXRlID0gc2VsZWN0U2NyZWVuU3RhdGU7XG4gICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5ID0gW3NjcmVlbk5hbWUsIFwiSW5pdGlhbFNjcmVlblwiXTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgXG4gICAgICAgIC8vVGhpcyBkYXRhIGlzIGFjdHVhbGx5IGJvb3RzdHJhcGVkIGFuZCB0aGUgdmFyaWFibGUgaXQncyBjb3B5aW5nIGZyb20gaXMgaW4gdGhlIHRlbXBsYXRlXG4gICAgICAgIHZtLmNvbXBhbmllcyA9IF8ubWFwKGNvbXBhbmllcywgZnVuY3Rpb24oY29tcGFueSkgeyByZXR1cm4gbmV3IEdhbWVUcmFja2VyQWRtaW4uQ29tcGFueShjb21wYW55KTsgfSk7XG4gICAgICAgIHZtLmdlbnJlcyA9IF8ubWFwKGdlbnJlcywgZnVuY3Rpb24oZ2VucmUpIHsgcmV0dXJuIG5ldyBHYW1lVHJhY2tlckFkbWluLkdlbnJlKGdlbnJlKTsgfSk7XG4gICAgICAgIHZtLnN5c3RlbXMgPSBfLm1hcChzeXN0ZW1zLCBmdW5jdGlvbihzeXN0ZW0pIHsgcmV0dXJuIG5ldyBHYW1lVHJhY2tlckFkbWluLlN5c3RlbShzeXN0ZW0pOyB9KTtcbiAgICAgICAgXG4gICAgICAgIHZtLnNob3VsZERpc3BsYXlTY3JlZW4gPSBmdW5jdGlvbihzY3JlZW5OYW1lKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVByb3BlcnR5ID0gXCJub25lXCI7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5zY3JlZW5IaXN0b3J5KSkge1xuICAgICAgICAgICAgICAgIGRpc3BsYXlQcm9wZXJ0eSA9IChzY3JlZW5OYW1lID09PSB2bS5zY3JlZW5IaXN0b3J5WzBdKSA/IFwiaW5oZXJpdFwiIDogXCJub25lXCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVByb3BlcnR5O1xuICAgICAgICB9O1xuICAgICAgICB2bS5jcmVhdGVCYWNrQnV0dG9uID0gZnVuY3Rpb24oY2FsbGJhY2spIHtcbiAgICAgICAgICAgIHJldHVybiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgICAgICBjYWxsYmFjaygpO1xuICAgICAgICAgICAgICAgIHZtLnNjcmVlbkhpc3Rvcnkuc2hpZnQoKTtcbiAgICAgICAgICAgIH07XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0ucmV0dXJuVG9NYWluRm9ybSA9IGZ1bmN0aW9uKHdoaWNoRm9ybSkge1xuICAgICAgICAgICAgdm1bd2hpY2hGb3JtXS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHZtLnNjcmVlbkhpc3Rvcnkuc2hpZnQoKTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jb21wYW55Rm9ybSA9IG5ldyBHYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSh7bmFtZTogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaXNtYW51ZmFjdHVyZXI6IG0ucHJvcChmYWxzZSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pO1xuICAgICAgICAvKiBUT0RPIFRoZSBhZGQgZnVuY3Rpb25zIGFyZSBiYXNpY2FsbHkgdGhlIHNhbWUuIFRoZXJlIHNob3VsZCBiZSBhIGdvb2Qgd2F5IG9mIHJlZmFjdG9yaW5nIHRoaXMgZWl0aGVyIGNyZWF0aW5nIGEgZnVuY2l0b24gZ2VuZXJhdG9yXG4gICAgICAgICAqIG9yIGNyZWF0aW5nIGEgY2hpbGQgb2JqZWN0XG4gICAgICAgICAqL1xuICAgICAgICB2bS5jb21wYW55Rm9ybS5zdWJtaXRIYW5kbGVycy5hZGQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZhciBuZXdDb21wYW55ID0gbmV3IEdhbWVUcmFja2VyQWRtaW4uQ29tcGFueSh2bS5jb21wYW55Rm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgbmV3Q29tcGFueS5zYXZlKClcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uY29tcGFuaWVzLnB1c2gobmV3Q29tcGFueSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBjb21wYW55IGhhcyBiZWVuIGFkZGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uY29tcGFueUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiQ291bGQgbm90IGFkZCB0aGUgY29tcGFueVwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZW50ZXIgdGhlIG5hbWUgb2YgdGhlIGNvbXBhbnlcIjtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jdXJyZW50Q29tcGFueUluZGV4ID0gbnVsbDtcbiAgICAgICAgdm0uY29tcGFueUZvcm0uc3VibWl0SGFuZGxlcnMudXBkYXRlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzTnVsbCh2bS5jdXJyZW50Q29tcGFueUluZGV4KSAmJiAhXy5pc0VtcHR5KHZtLmNvbXBhbnlGb3JtLmZpZWxkcy5uYW1lKCkpKSB7XG4gICAgICAgICAgICAgICAgdm0uY29tcGFuaWVzW3ZtLmN1cnJlbnRDb21wYW55SW5kZXhdLnVwZGF0ZSh2bS5jb21wYW55Rm9ybS5yZXR1cm5GaWVsZHMoKSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgY29tcGFueSBoYXMgYmVlbiB1cGRhdGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIHRoZSBuYW1lIG9mIHRoZSBjb21wYW55XCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgICAgIFxuICAgICAgICB2bS5nZW5yZUZvcm0gPSBuZXcgR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0oe25hbWU6IG0ucHJvcChcIlwiKX0pO1xuICAgICAgICB2bS5nZW5yZUZvcm0uc3VibWl0SGFuZGxlcnMuYWRkID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uZ2VucmVGb3JtLmZpZWxkcy5uYW1lKCkpKSB7XG4gICAgICAgICAgICAgICAgdmFyIG5ld0dlbnJlID0gbmV3IEdhbWVUcmFja2VyQWRtaW4uR2VucmUodm0uZ2VucmVGb3JtLnJldHVybkZpZWxkcygpKTtcbiAgICAgICAgICAgICAgICBuZXdHZW5yZS5zYXZlKClcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uZ2VucmVzLnB1c2gobmV3R2VucmUpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgZ2VucmUgaGFzIGJlZW4gYWRkZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5nZW5yZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiQ291bGQgbm90IGFkZCB0aGUgZ2VucmVcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIHRoZSBuYW1lIG9mIHRoZSBnZW5yZVwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICB2bS5jdXJyZW50R2VucmVJbmRleCA9IG51bGw7XG4gICAgICAgIHZtLmdlbnJlRm9ybS5zdWJtaXRIYW5kbGVycy51cGRhdGUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNOdWxsKHZtLmN1cnJlbnRHZW5yZUluZGV4KSAmJiAhXy5pc0VtcHR5KHZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZtLmdlbnJlc1t2bS5jdXJyZW50R2VucmVJbmRleF0udXBkYXRlKHZtLmdlbnJlRm9ybS5yZXR1cm5GaWVsZHMoKSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgZ2VucmUgaGFzIGJlZW4gdXBkYXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBlbnRlciB0aGUgbmFtZSBvZiB0aGUgZ2VucmVcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH07XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uc3lzdGVtRm9ybSA9IG5ldyBHYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSh7bmFtZTogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtYW51ZmFjdHVyZXJpZDogbS5wcm9wKFwiXCIpfSk7XG4gICAgICAgIHZtLnN5c3RlbUZvcm0uc3VibWl0SGFuZGxlcnMuYWRkID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uc3lzdGVtRm9ybS5maWVsZHMubmFtZSgpKSAmJiAhXy5pc0VtcHR5KHZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkKCkpKSB7XG4gICAgICAgICAgICAgICAgdmFyIG5ld1N5c3RlbSA9IG5ldyBHYW1lVHJhY2tlckFkbWluLlN5c3RlbSh2bS5zeXN0ZW1Gb3JtLnJldHVybkZpZWxkcygpKTtcbiAgICAgICAgICAgICAgICBuZXdTeXN0ZW0uc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN5c3RlbXMucHVzaChuZXdTeXN0ZW0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgc3lzdGVtIGhhcyBiZWVuIGFkZGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3lzdGVtRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGZpbGwgaW4gYWxsIG9mIHRoZSBmaWVsZHNcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY3VycmVudFN5c3RlbUluZGV4ID0gbnVsbDtcbiAgICAgICAgdm0uc3lzdGVtRm9ybS5zdWJtaXRIYW5kbGVycy51cGRhdGUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNOdWxsKHZtLmN1cnJlbnRTeXN0ZW1JbmRleCkgJiYgIV8uaXNFbXB0eSh2bS5zeXN0ZW1Gb3JtLmZpZWxkcy5uYW1lKCkpICYmICFfLmlzRW1wdHkodm0uc3lzdGVtRm9ybS5maWVsZHMubWFudWZhY3R1cmVyaWQoKSkpIHtcbiAgICAgICAgICAgICAgICB2bS5zeXN0ZW1zW3ZtLmN1cnJlbnRTeXN0ZW1JbmRleF0udXBkYXRlKHZtLnN5c3RlbUZvcm0ucmV0dXJuRmllbGRzKCkpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIHN5c3RlbSBoYXMgYmVlbiB1cGRhdGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgfSk7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGZpbGwgaW4gYWxsIHRoZSBmaWVsZHNcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzQWRtaW4gPSB0cnVlO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnBvcHVsYXRlU2VsZWN0RGF0YVNldHModm0uc3lzdGVtcywgdm0uZ2VucmVzLCB2bS5jb21wYW5pZXMpO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNhbmNlbEJ1dHRvbkhhbmRsZXIgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnNoaWZ0KCk7XG4gICAgICAgIH07XG4gICAgICAgIFxuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmJpbmRTdWJtaXRGb3JtSGFuZGxlcihcImFkZFwiLCBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIGNvbnNvbGUubG9nKCdibG9ycCcpO1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubmFtZSgpKSAmJlxuICAgICAgICAgICAgICAgICFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucmVnaW9uKCkpICYmXG4gICAgICAgICAgICAgICAgXy5pc0Zpbml0ZShOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSkpICYmXG4gICAgICAgICAgICAgICAgTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkKCkpID4gMCAmJlxuICAgICAgICAgICAgICAgIF8uaXNGaW5pdGUoTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KCkpKSAmJlxuICAgICAgICAgICAgICAgIE51bWJlcihHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpKSA+IDApIHtcbiAgICAgICAgICAgICAgICBtLnJlcXVlc3Qoe21ldGhvZDogXCJQT1NUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2FkbWluL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLnJldHVybkZpZWxkcygpfSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiU3VjY2Vzc2Z1bGx5IGFkZGVkIHRoZSBnYW1lXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZmlsbCBpbiBhbGwgdGhlIGZpZWxkc1wiO1xuICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH0pO1xuXG4gICAgICAgIHZtLmN1cnJlbnRHYW1lSWQgPSAwO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlbGVjdFVwZGF0ZUhhbmRsZXIgPSBmdW5jdGlvbihnYW1lSWQpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICBpZiAoZ2FtZUlkICYmIF8uaXNGaW5pdGUoTnVtYmVyKGdhbWVJZCkpKSB7XG4gICAgICAgICAgICAgICAgLyogQSBrbm93biBsaW1pdGF0aW9uIHdpdGggdGhlIGJhY2tlbmQ6IHRoaW5ncyB3ZSBleHBlY3QgdG8gYmUgYW4gYXJyYXkgbWF5IGJlIGEgc2ltcGxlIG9iamVjdCBkdWUgdG8gdGhlIGpzb24gZW5jb2RlciBvbiB0aGUgYmFja2VuZFxuICAgICAgICAgICAgICAgICAgIG5vdCBiZWluZyBhYmxlIHRvIGVuY29kZSBzaW5nbGUgcm93IHJlc3VsdHMgY29ycmVjdGx5XG4gICAgICAgICAgICAgICAgICovXG4gICAgICAgICAgICAgICAgdmFyIGVuc3VyZUFycmF5ID0gZnVuY3Rpb24oaXRlbSkge1xuICAgICAgICAgICAgICAgICAgICB2YXIgcmV0dXJuVmFsdWUgPSBfLmlzQXJyYXkoaXRlbSkgPyBpdGVtIDogW2l0ZW1dO1xuICAgICAgICAgICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgICAgICAgICAgfTtcbiAgICAgICAgICAgICAgICAvL1dlIGNvdWxkIGp1c3QgdXNlIHRoZSBkYXRhIHdlIHJldHJpZXZlZCBmcm9tIHRoZSBzZWFyY2ggYnV0IGxldCdzIGd1YXJhbnRlZSB0aGUgdXNlciB3aXRoIHRoZSBtb3N0IHJlY2VudCBpbmZvcm1hdGlvblxuICAgICAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOiBcIkdFVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBcIi9nYW1lL1wiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YToge2lkOiBOdW1iZXIoZ2FtZUlkKX1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgfSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRHYW1lSWQgPSBOdW1iZXIocmVzcG9uc2UuaWQpO1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuY29tcGFuaWVzKF8ucGx1Y2soZW5zdXJlQXJyYXkocmVzcG9uc2UuY29tcGFuaWVzKSwgXCJjb21wYW55SWRcIikpO1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuZ2VucmVzKF8ucGx1Y2soZW5zdXJlQXJyYXkocmVzcG9uc2UuZ2VucmVzKSwgXCJnZW5yZUlkXCIpKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0ucG9wdWxhdGVGb3JtKF8ub21pdChyZXNwb25zZSwgW1wiY29tcGFuaWVzXCIsIFwiZ2VucmVzXCJdKSk7XG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmZvcm1Nb2RlID0gXCJ1cGRhdGVcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cyA9IFtdO1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfVxuICAgICAgICB9O1xuXG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuYmluZFN1Ym1pdEZvcm1IYW5kbGVyKFwidXBkYXRlXCIsIGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubmFtZSgpKSAmJlxuICAgICAgICAgICAgICAgICFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMucmVnaW9uKCkpICYmXG4gICAgICAgICAgICAgICAgXy5pc0Zpbml0ZShOdW1iZXIoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSkpICYmXG4gICAgICAgICAgICAgICAgTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkKCkpID4gMCAmJlxuICAgICAgICAgICAgICAgIF8uaXNGaW5pdGUoTnVtYmVyKEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KCkpKSAmJlxuICAgICAgICAgICAgICAgIE51bWJlcihHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpKSA+IDApIHsgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICB2YXIgZGF0YSA9IF8uZXh0ZW5kKHtpZDogTnVtYmVyKHZtLmN1cnJlbnRHYW1lSWQpfSwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6IFwiUFVUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2FkbWluL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBkYXRhfSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIkdhbWUgc3VjY2Vzc2Z1bGx5IHVwZGF0ZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfSBcbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGZpbGwgaW4gdGhlIGZpZWxkc1wiO1xuICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH0pO1xuXG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0RGVsZXRlSGFuZGxlciA9IGZ1bmN0aW9uKGdhbWVJZCkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChnYW1lSWQgJiYgXy5pc0Zpbml0ZShOdW1iZXIoZ2FtZUlkKSkpIHtcbiAgICAgICAgICAgICAgICBtLnJlcXVlc3Qoe21ldGhvZDogXCJERUxFVEVcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2FkbWluL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YToge2lkOiBOdW1iZXIoZ2FtZUlkKX19KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGdhbWUgaGFzIGJlZW4gZGVsZXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ucmVtb3ZlKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cywgZnVuY3Rpb24oZ2FtZSkgeyByZXR1cm4gZ2FtZS5pZCA9PT0gTnVtYmVyKGdhbWVJZCk7IH0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQgPSBtLnByb3AoXCJcIik7XG4gICAgICAgIHZtLmdlbmVyYWxJbml0aWF0ZUVkaXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKSkge1xuICAgICAgICAgICAgICAgIHZtLmZvcm1Nb2RlID0gXCJ1cGRhdGVcIjtcbiAgICAgICAgICAgICAgICBzd2l0Y2ggKHZtLnNlbGVjdFNjcmVlblN0YXRlKSB7XG4gICAgICAgICAgICAgICAgY2FzZSBcInN5c3RlbVwiOlxuICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U3lzdGVtSW5kZXggPSBfLmZpbmRJbmRleCh2bS5zeXN0ZW1zLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5zeXN0ZW1Gb3JtLnBvcHVsYXRlRm9ybSh2bS5zeXN0ZW1zW3ZtLmN1cnJlbnRTeXN0ZW1JbmRleF0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnVuc2hpZnQoXCJTeXN0ZW1Gb3JtU2NyZWVuXCIpO1xuICAgICAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgICAgICBjYXNlIFwiY29tcGFueVwiOlxuICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50Q29tcGFueUluZGV4ID0gXy5maW5kSW5kZXgodm0uY29tcGFuaWVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5jb21wYW55Rm9ybS5wb3B1bGF0ZUZvcm0odm0uY29tcGFuaWVzW3ZtLmN1cnJlbnRDb21wYW55SW5kZXhdKTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0KFwiQ29tcGFueUZvcm1TY3JlZW5cIik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIGNhc2UgXCJnZW5yZVwiOlxuICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50R2VucmVJbmRleCA9IF8uZmluZEluZGV4KHZtLmdlbnJlcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uZ2VucmVGb3JtLnBvcHVsYXRlRm9ybSh2bS5nZW5yZXNbdm0uY3VycmVudEdlbnJlSW5kZXhdKTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0KFwiR2VucmVGb3JtU2NyZWVuXCIpO1xuICAgICAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIHNlbGVjdCBhbiBpdGVtIGluIHRoZSBkcm9wZG93blwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgdm0uY3VycmVudFNlbGVjdEVudGl0eUlkKFwiXCIpO1xuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICB2bS5nZW5lcmFsRGVsZXRlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSkpIHtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgICAgIHZhciBjdXJyZW50SW5kZXg7XG4gICAgICAgICAgICAgICAgc3dpdGNoICh2bS5zZWxlY3RTY3JlZW5TdGF0ZSkge1xuICAgICAgICAgICAgICAgIGNhc2UgXCJzeXN0ZW1cIjpcbiAgICAgICAgICAgICAgICAgICAgY3VycmVudEluZGV4ID0gXy5maW5kSW5kZXgodm0uc3lzdGVtcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc3lzdGVtc1tjdXJyZW50SW5kZXhdLnJlbW92ZSgpXG4gICAgICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ucmVtb3ZlKHZtLnN5c3RlbXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgc3lzdGVtIGhhcyBiZWVuIHJlbW92ZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudFNlbGVjdEVudGl0eUlkKFwiXCIpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICB9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIGNhc2UgXCJjb21wYW55XCI6XG4gICAgICAgICAgICAgICAgICAgIGN1cnJlbnRJbmRleCA9IF8uZmluZEluZGV4KHZtLmNvbXBhbmllcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uY29tcGFuaWVzW2N1cnJlbnRJbmRleF0ucmVtb3ZlKClcbiAgICAgICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5yZW1vdmUodm0uY29tcGFuaWVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGNvbXBhbnkgaGFzIGJlZW4gcmVtb3ZlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgICAgICAgICAgY2FzZSBcImdlbnJlXCI6XG4gICAgICAgICAgICAgICAgICAgIGN1cnJlbnRJbmRleCA9IF8uZmluZEluZGV4KHZtLmdlbnJlcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uZ2VucmVzW2N1cnJlbnRJbmRleF0ucmVtb3ZlKClcbiAgICAgICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc29sZS5sb2codm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnJlbW92ZSh2bS5nZW5yZXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgZ2VucmUgaGFzIGJlZW4gcmVtb3ZlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7ICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgIH07XG5cbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0ubWVzc2FnZUVycm9yID0gXCJTZWxlY3QgYW4gaXRlbSBmcm9tIHRoZSBkcm9wZG93blwiO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgIH07XG5cbiAgICByZXR1cm4gdm07XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLmNvbnRyb2xsZXIgPSBmdW5jdGlvbigpIHtcbiAgICBHYW1lVHJhY2tlckFkbWluLnZtLmluaXQoKTtcbn07XG4iLCIvL0ZvciB1c2Ugd2l0aCBhbGwgVmlld3MuIENvZGUgaXMgYmFzZWQgb24gdGhlIG9uZSBmb3VuZCBvbiBtaXRocmlsJ3Mgc2l0ZSBodHRwczovL2xob3JpZS5naXRodWIuaW8vbWl0aHJpbC9pbnRlZ3JhdGlvbi5odG1sXG52YXIgc2VsZWN0Mj0ge307XG5cbi8qIFRoaXMgZmFjdG9yeSBmdW5jdGlvbiBvZmZlcnMgYSBuaWNlIGNsb3N1cmUgZm9yIGFueXRoaW5nIGV4dHJhIHdlIHdhbnQgdG8gcGFzcyBpbiAqL1xuc2VsZWN0Mi5jb25maWcgPSBmdW5jdGlvbihleHRyYUFyZ3VtZW50cykge1xuICAgIHJldHVybiBmdW5jdGlvbihlbGVtZW50LCBpc0luaXRpYWxpemVkLCBjb250cm9sbGVyKSB7XG4gICAgICAgIHZhciBlbCA9ICQoZWxlbWVudCk7XG4gICAgICAgIGlmICghaXNJbml0aWFsaXplZCkge1xuICAgICAgICAgICAgaWYgKGV4dHJhQXJndW1lbnRzLnNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnMpIHtcbiAgICAgICAgICAgICAgICBlbC5zZWxlY3QyKGV4dHJhQXJndW1lbnRzLnNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnMpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICBlbC5zZWxlY3QyKCk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICBlbC5jaGFuZ2UoZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICAgICAgbS5zdGFydENvbXB1dGF0aW9uKCk7XG4gICAgICAgICAgICAgICAgZXh0cmFBcmd1bWVudHMub25jaGFuZ2UoZWwuc2VsZWN0MihcInZhbFwiKSk7XG4gICAgICAgICAgICAgICAgbS5lbmRDb21wdXRhdGlvbigpO1xuICAgICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgICAgZWwuc2VsZWN0MihcInZhbFwiLCBleHRyYUFyZ3VtZW50cy52YWx1ZSk7XG4gICAgfTtcbn07XG5cbnNlbGVjdDIudmlldyA9IGZ1bmN0aW9uKGV4dHJhQXJndW1lbnRzLCBvcHRpb25TZXQsIGlzTXVsdGlwbGUpIHtcbiAgICB2YXIgc2VsZWN0b3IgPSAoaXNNdWx0aXBsZSkgPyBcInNlbGVjdC5mb3JtLWNvbnRyb2xbbXVsdGlwbGU9dHJ1ZV1cIiA6IFwic2VsZWN0LmZvcm0tY29udHJvbFwiO1xuICAgIHZhciBjcmVhdGVPcHRpb25TZXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgdmFyIG9wdGlvbnMgPSBbXTtcbiAgICAgICAgaWYgKG9wdGlvblNldCkge1xuICAgICAgICAgICAgb3B0aW9ucyA9IF8ubWFwKG9wdGlvblNldCwgZnVuY3Rpb24odmFsdWUpIHtcbiAgICAgICAgICAgICAgICB2YXIgcmV0dXJuVmFsdWUgPSAoXy5pc09iamVjdCh2YWx1ZSkpID8gbShcIm9wdGlvblwiLCB7dmFsdWU6IHZhbHVlLmlkfSwgdmFsdWUubmFtZSkgOiBtKFwib3B0aW9uXCIsIHZhbHVlKTtcbiAgICAgICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gb3B0aW9ucztcbiAgICB9O1xuICAgIHJldHVybiBtKHNlbGVjdG9yLCB7Y29uZmlnOnNlbGVjdDIuY29uZmlnKGV4dHJhQXJndW1lbnRzKX0sXG4gICAgICAgICAgICAgW20oXCJvcHRpb25cIiksY3JlYXRlT3B0aW9uU2V0KCldKTtcbn07XG4iLCJcblxuXG5cblxuXG5cblxuXG5cbm0ubW9kdWxlKGRvY3VtZW50LmJvZHksIEdhbWVUcmFja2VyQWRtaW4pO1xuIl0sInNvdXJjZVJvb3QiOiIvc291cmNlLyJ9