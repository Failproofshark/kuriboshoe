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

GameTrackerAdmin.screenCollection.GameFormScreen = function() {
    var formConfiguration = {
        textAreaDisplay: function() {
            var displayValue = (GameTrackerAdmin.vm.formMode === "search") ? "display:none" : "display:inherit";
            return displayValue;
        },
        confirmButtonHandler: function() {
            var handler;
            switch (GameTrackerAdmin.vm.formMode) {
            case "add":
                handler = GameTrackerAdmin.vm.createNewGame;
                break;
            case "search":
                handler = GameTrackerAdmin.vm.searchForGame;
                break;
            case "update":
                handler = GameTrackerAdmin.vm.updateGame;
                break;
            }
            return handler;
        }
    };
    var renderSearchResults = function(isLoading) {
        var renderedResults = [];
        var displayProperties = (isLoading) ? {results: "display:none", preloader: "display:inherit"} : {results: "display:inherit", preloader: "display:none"};
        if (!_.isEmpty(GameTrackerAdmin.vm.searchResults) || !_.isEmpty(GameTrackerAdmin.vm.noResults)) {
            renderedResults = m("div",
                                {style:displayProperties.results},
                                [m("div", GameTrackerAdmin.vm.noResults),
                                 _.map(GameTrackerAdmin.vm.searchResults, function(result, index) {
                                    var bgColor = "background-color:#CECFE0";
                                    if (index % 2 == 0) {
                                        bgColor = "background-color:#FFF";
                                    }
                                    return m("div.row.result-row", {style:bgColor},
                                             [m("div.col-xs-9",
                                                {style:bgColor},
                                                (result.name + " [" + result.region + "] (" + result.systemName + ")")),
                                              m("div.col-xs-3", [
                                                  m("span.glyphicon.glyphicon-remove.game-search-results-button", {onclick:GameTrackerAdmin.vm.deleteGame.bind(GameTrackerAdmin.vm, result.id)}),
                                                  m("span.glyphicon.glyphicon-pencil.game-search-results-button", {onclick:GameTrackerAdmin.vm.initiateEditGameEntry.bind(GameTrackerAdmin.vm, result.id)})
                                              ])]);
                                }),
                                m("img[src=/images/ajax.gif]", {style:displayProperties.preloader})
                               ]);
        }
        return renderedResults;
    };
    return [m("div.row",[
        m("div.col-xs-12",[
            m("form", [
                m("input.form-control", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.fields.name),
                                         value: GameTrackerAdmin.vm.gameForm.fields.name(),
                                         placeholder: "Name"}),
                select2.view({onchange:GameTrackerAdmin.vm.gameForm.fields.region,
                              value: GameTrackerAdmin.vm.gameForm.fields.region(),
                              select2InitializationOptions: {placeholder: "Region"}},
                             ["NTSC", "NTSC-J", "PAL"]),
                select2.view({onchange:GameTrackerAdmin.vm.gameForm.fields.systemid,
                              value: GameTrackerAdmin.vm.gameForm.fields.systemid(),
                              select2InitializationOptions: {placeholder: "System"}},
                             _.pluck(GameTrackerAdmin.vm.systems, "attributes")),
                select2.view({onchange:GameTrackerAdmin.vm.gameForm.fields.genres,
                              value: GameTrackerAdmin.vm.gameForm.fields.genres(),
                              select2InitializationOptions: {placeholder: "Genres"}},
                             _.pluck(GameTrackerAdmin.vm.genres, "attributes"),
                             true),
                select2.view({onchange:GameTrackerAdmin.vm.gameForm.fields.companies,
                              value: GameTrackerAdmin.vm.gameForm.fields.companies(),
                              select2InitializationOptions: {placeholder: "Companies"}},
                             _.pluck(GameTrackerAdmin.vm.companies, "attributes"),
                             true),
                m("input.form-control", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.fields.quantity),
                                         value: GameTrackerAdmin.vm.gameForm.fields.quantity(),
                                         placeholder: "Quantity"
                                        }),
                m("div", {style:formConfiguration.textAreaDisplay()}, [
                    m("p", "Short Description"),
                    m("textarea", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.fields.blurb)}, GameTrackerAdmin.vm.gameForm.fields.blurb()),
                ]),
                m("div.checkbox", [
                    m("label", [
                        m("input[type=checkbox]", {onchange: m.withAttr("checked", GameTrackerAdmin.vm.gameForm.fields.hasmanual), checked: GameTrackerAdmin.vm.gameForm.fields.hasmanual()})
                    ]),
                    m("span", "Manual")
                ]),
                m("div.checkbox", [
                    m("label", [
                        m("input[type=checkbox]", {onchange: m.withAttr("checked", GameTrackerAdmin.vm.gameForm.fields.hasbox), checked: GameTrackerAdmin.vm.gameForm.fields.hasbox()})
                    ]),
                    m("span", "Box")
                ]),
                m("div", {style:formConfiguration.textAreaDisplay()}, [
                    m("p", "Notes"),
                    m("textarea", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.fields.notes)}, GameTrackerAdmin.vm.gameForm.fields.notes()),
                ]),
                GameTrackerAdmin.screenHelpers.createButtonSet(GameTrackerAdmin.vm.isLoading, "gameForm")
            ]),
        ])
    ]),
            renderSearchResults(GameTrackerAdmin.vm.searchIsLoading)
           ];
};

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
            vm.gameForm.clearForm();
            vm.systemForm.clearForm();
            vm.genreForm.clearForm();
            vm.companyForm.clearForm();
            vm.currentSelectEntityId("");
        };
        
        vm.jumpToScreen = function(formMode, selectScreenState, screenName) {
            vm.completeReset();
            vm.formMode = formMode;
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
        
        //The naming convention seems to have changed (not camel case) but this is because we wish
        //To mirror what we have in the table, mainly for back-end convenience
        //TODO have each form have a namespace for their thingies
        vm.gameForm = new GameTrackerShared.TrackerForm({name: m.prop(""),
                                          blurb: m.prop(""),
                                          region: m.prop(""),
                                          hasmanual: m.prop(false),
                                          hasbox: m.prop(false),
                                          notes: m.prop(""),
                                          quantity: m.prop(""),
                                          genres: m.prop([]),
                                          companies: m.prop([]),
                                          systemid: m.prop("")});
        
        vm.gameForm.submitHandlers.add = function() {
            vm.isLoading = true;
            if (!_.isEmpty(vm.gameForm.fields.name()) &&
                !_.isEmpty(vm.gameForm.fields.region()) &&
                _.isFinite(Number(vm.gameForm.fields.systemid())) &&
                Number(vm.gameForm.fields.systemid()) > 0 &&
                _.isFinite(Number(vm.gameForm.fields.quantity())) &&
                Number(vm.gameForm.fields.quantity()) > 0) {
                m.request({method: "POST",
                           url: "/admin/game/",
                           data: vm.gameForm.returnFields()})
                    .then(function(response) {
                        vm.gameForm.clearForm();
                        vm.successMessage = "Successfully added the game";
                        vm.isLoading = false;
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please fill in all the fields";
                vm.isLoading = false;
            }
            return false;
        };

        vm.searchResults = [];
        vm.noResults = "";
        vm.gameForm.submitHandlers.search = function() {
            vm.isLoading = true;
            var completedSet = _.omit(vm.gameForm.returnFields(), function(value, key) {
                var returnValue = true;
                if (_.isBoolean(value)) {
                    returnValue = !value;
                } else if (_.isString(value) && value.length > 0) {
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
                        vm.searchResults = _.remove(response, function(item) { return !_.isNull(item); });
                        if (vm.searchResults.length < 1) {
                            vm.noResults = "No matches were found";
                        }
                        vm.isLoading = false;
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter at least one search parameter";
                vm.isLoading = false;
            }
            return false;
        };

        vm.currentGameId = 0;
        vm.searchIsLoading = false;
        vm.initiateEditGameEntry = function(gameId) {
            vm.searchIsLoading = true;
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
                        vm.gameForm.fields.companies(_.pluck(ensureArray(response.companies), "companyId"));
                        vm.gameForm.fields.genres(_.pluck(ensureArray(response.genres), "genreId"));
                        vm.gameForm.populateForm(_.omit(response, ["companies", "genres"]));
                        vm.formMode = "update";
                        vm.searchResults = [];
                        vm.searchIsLoading = false;
                    }, vm.reportInternalError);
            }
        };

        vm.gameForm.submitHandlers.update = function() {
            vm.isLoading = true;
            if (!_.isEmpty(vm.gameForm.fields.name()) &&
                !_.isEmpty(vm.gameForm.fields.region()) &&
                _.isFinite(Number(vm.gameForm.fields.systemid())) &&
                Number(vm.gameForm.fields.systemid()) > 0 &&
                _.isFinite(Number(vm.gameForm.fields.quantity())) &&
                Number(vm.gameForm.fields.quantity()) > 0) {            
                var data = _.extend({id: Number(vm.currentGameId)}, vm.gameForm.returnFields());
                m.request({method: "PUT",
                           url: "/admin/game/",
                           data: data})
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.successMessage = "Game successfully updated";
                            vm.isLoading = false;
                        } 
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please fill in the fields";
                vm.isLoading = false;
            }
            return false;
        };

        vm.deleteGame = function(gameId) {
            vm.searchIsLoading = true;
            if (gameId && _.isFinite(Number(gameId))) {
                m.request({method: "DELETE",
                            url: "/admin/game/",
                            data: {id: Number(gameId)}})
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.successMessage = "The game has been deleted";
                            _.remove(vm.searchResults, function(game) { return game.id === Number(gameId); });
                            vm.searchIsLoading = false;
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

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImdhbWV0cmFja2Vyc2hhcmVkLmpzIiwiYWRtaW5tb2RlbHMuanMiLCJhZG1pbnZpZXdzLmpzIiwiYWRtaW52bWNvbnRyb2xsZXIuanMiLCJzZWxlY3QybWl0aHJpbC5qcyIsImdhbWV0cmFja2VyYWRtaW4uanMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQzlDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3pEQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUNwUUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUN2YUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDdENBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQSIsImZpbGUiOiJhZG1pbi5qcyIsInNvdXJjZXNDb250ZW50IjpbInZhciBHYW1lVHJhY2tlclNoYXJlZCA9IHt9O1xuXG5HYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSA9IGZ1bmN0aW9uKGZpZWxkcykge1xuICAgIHRoaXMuZmllbGRzID0gZmllbGRzO1xuICAgIHRoaXMucG9wdWxhdGVGb3JtID0gZnVuY3Rpb24ob2JqZWN0KSB7XG4gICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgaWYgKG9iamVjdC5hdHRyaWJ1dGVzKSB7XG4gICAgICAgICAgICBfLm1hcChvYmplY3QuYXR0cmlidXRlcywgZnVuY3Rpb24oYXR0cmlidXRlVmFsdWUsIGF0dHJpYnV0ZUtleSkge1xuICAgICAgICAgICAgICAgIGlmIChhdHRyaWJ1dGVLZXkgIT09IFwiaWRcIikge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmZpZWxkc1thdHRyaWJ1dGVLZXldKGF0dHJpYnV0ZVZhbHVlKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIF8ubWFwKG9iamVjdCwgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgICAgIGlmIChrZXkgIT09IFwiaWRcIikge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmZpZWxkc1trZXldKHZhbHVlKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgIH07XG4gICAgdGhpcy5jbGVhckZvcm0gPSBfLmZvckVhY2guYmluZCh0aGlzLCB0aGlzLmZpZWxkcywgZnVuY3Rpb24oaW5wdXQpIHtcbiAgICAgICAgaWYgKF8uaXNTdHJpbmcoaW5wdXQoKSkpIHtcbiAgICAgICAgICAgIGlucHV0KFwiXCIpO1xuICAgICAgICB9IGVsc2UgaWYgKF8uaXNBcnJheShpbnB1dCgpKSkge1xuICAgICAgICAgICAgaW5wdXQoW10pO1xuICAgICAgICB9IGVsc2UgaWYgKF8uaXNCb29sZWFuKGlucHV0KCkpKXtcbiAgICAgICAgICAgIGlucHV0KGZhbHNlKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIGlucHV0KG51bGwpO1xuICAgICAgICB9XG4gICAgfSk7XG4gICAgdGhpcy5yZXR1cm5GaWVsZHMgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgcmV0dXJuIF8ubWFwVmFsdWVzKHRoaXMuZmllbGRzLCBmdW5jdGlvbihmaWVsZCkge1xuICAgICAgICAgICAgcmV0dXJuIGZpZWxkKCk7XG4gICAgICAgIH0pO1xuICAgIH07XG4gICAgdGhpcy5zdWJtaXRIYW5kbGVycyA9IHt9O1xuICAgIC8qIFRoaXMgd2lsbCBwcm9iYWJseSBiZSByZWZhY3RvcmVkIG91dCBpbiB0aGUgZnV0dXJlIGdpdmVuIHRoZSBvbmx5IHRoaW5nIHRoYXQgaGFzIGEgc2VhcmNoIGlzIHRoZSBnYW1lIGZvcm1cbiAgICAgKiBUbyBrZWVwIHRoaW5ncyBmcm9tIGNvbXBsYWluaW5nIGFib3V0IGEgbWlzc2luZyBrZXkgd2UgYWRkIGFuIGVtcHR5IGZ1bmN0aW9uIGhlcmVcbiAgICAgKi9cbiAgICB0aGlzLnN1Ym1pdEhhbmRsZXJzLnNlYXJjaCA9IGZ1bmN0aW9uKCkgeyAvKmVtcHR5Ki8gfTtcbiAgICB0aGlzLmdldFN1Ym1pdEhhbmRsZXIgPSBmdW5jdGlvbihzdGF0ZSkge1xuICAgICAgICByZXR1cm4gdGhpcy5zdWJtaXRIYW5kbGVyc1tzdGF0ZV07XG4gICAgfTtcblxufTtcbiIsInZhciBHYW1lVHJhY2tlckFkbWluID0ge307XG5cbkdhbWVUcmFja2VyQWRtaW4uTW9kZWwgPSBmdW5jdGlvbihkZWZhdWx0RW1wdHlTZXQsIGJhY2tzaWRlVXJsKSB7XG4gICAgcmV0dXJuIGZ1bmN0aW9uIChpbml0aWFsVmFsdWVzKSB7XG4gICAgICAgIGlmIChpbml0aWFsVmFsdWVzKSB7XG4gICAgICAgICAgICB0aGlzLmF0dHJpYnV0ZXMgPSAoXy5pc0VtcHR5KGluaXRpYWxWYWx1ZXMuaWQpKSA/IF8uZXh0ZW5kKHtpZDpudWxsfSwgXy5jbG9uZShpbml0aWFsVmFsdWVzLHRydWUpKSA6IF8uY2xvbmUoaW5pdGlhbFZhbHVlcyk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICB0aGlzLmF0dHJpYnV0ZXMgPSBkZWZhdWx0RW1wdHlTZXQ7XG4gICAgICAgIH1cblxuICAgICAgICB0aGlzLmJhY2tzaWRlVXJsID0gYmFja3NpZGVVcmw7XG5cbiAgICAgICAgdGhpcy5zYXZlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgc2VsZiA9IHRoaXM7XG4gICAgICAgICAgICByZXR1cm4gbS5yZXF1ZXN0KHttZXRob2Q6IFwiUE9TVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBzZWxmLmJhY2tzaWRlVXJsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YTpfLm9taXQoc2VsZi5hdHRyaWJ1dGVzLCBcImlkXCIpfSlcbiAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmF0dHJpYnV0ZXMuaWQgPSByZXNwb25zZS5uZXdpZDtcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIHJlc3BvbnNlO1xuICAgICAgICAgICAgICAgIH0pO1xuICAgICAgICB9O1xuXG4gICAgICAgIHRoaXMudXBkYXRlID0gZnVuY3Rpb24obmV3QXR0cmlidXRlcykge1xuICAgICAgICAgICAgdmFyIHNlbGYgPSB0aGlzO1xuICAgICAgICAgICAgXy5mb3JJbihuZXdBdHRyaWJ1dGVzLCBmdW5jdGlvbih2YWx1ZSwga2V5KSB7XG4gICAgICAgICAgICAgICAgc2VsZi5hdHRyaWJ1dGVzW2tleV0gPSB2YWx1ZTtcbiAgICAgICAgICAgIH0pO1xuICAgICAgICAgICAgcmV0dXJuIG0ucmVxdWVzdCh7bWV0aG9kOiBcIlBVVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBzZWxmLmJhY2tzaWRlVXJsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YTogc2VsZi5hdHRyaWJ1dGVzfSk7XG4gICAgICAgIH07XG5cbiAgICAgICAgdGhpcy5yZW1vdmUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgICAgIHJldHVybiBtLnJlcXVlc3Qoe21ldGhvZDogXCJERUxFVEVcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogc2VsZi5iYWNrc2lkZVVybCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IHtpZDogc2VsZi5hdHRyaWJ1dGVzLmlkfX0pO1xuICAgICAgICB9O1xuXG4gICAgfTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uQ29tcGFueSA9IEdhbWVUcmFja2VyQWRtaW4uTW9kZWwoe2lkOm51bGwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lOiBcIlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaXNtYW51ZmFjdHVyZXI6IG51bGx9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIi9hZG1pbi9jb21wYW55L1wiKTtcblxuR2FtZVRyYWNrZXJBZG1pbi5TeXN0ZW0gPSBHYW1lVHJhY2tlckFkbWluLk1vZGVsKHsgaWQ6IG51bGwsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBuYW1lOiBcIlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbWFudWZhY3R1cmVyaWQ6IG51bGwgfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIi9hZG1pbi9zeXN0ZW0vXCIpO1xuXG5HYW1lVHJhY2tlckFkbWluLkdlbnJlID0gR2FtZVRyYWNrZXJBZG1pbi5Nb2RlbCh7IGlkOiBudWxsLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbmFtZTogXCJcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1hbnVmYWN0dXJlcmlkOiBudWxsIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCIvYWRtaW4vZ2VucmUvXCIpO1xuIiwiR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzID0ge307XG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uRGlzcGxheVByb3BlcnRpZXMgPSBmdW5jdGlvbihpc0xvYWRpbmcpIHtcbiAgICB2YXIgZGlzcGxheVByb3BlcnRpZXMgPSAoaXNMb2FkaW5nKSA/IHtidXR0b246XCJkaXNwbGF5Om5vbmVcIiwgcHJlbG9hZGVyOlwiZGlzcGxheTppbmxpbmVcIn0gOiB7YnV0dG9uOlwiZGlzcGxheTppbmxpbmVcIiwgcHJlbG9hZGVyOlwiZGlzcGxheTpub25lXCJ9O1xuICAgIHJldHVybiBkaXNwbGF5UHJvcGVydGllcztcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25TZXQgPSBmdW5jdGlvbihpc0xvYWRpbmcsIHdoaWNoRm9ybSkge1xuICAgIHZhciBkaXNwbGF5UHJvcGVydGllcyA9IEdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25EaXNwbGF5UHJvcGVydGllcyhpc0xvYWRpbmcpO1xuICAgIHJldHVybiBtKFwiZGl2XCIsIFtcbiAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLXN1Y2Nlc3NcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bVt3aGljaEZvcm1dLnN1Ym1pdEhhbmRsZXJzW0dhbWVUcmFja2VyQWRtaW4udm0uZm9ybU1vZGVdfSwgXCJzdWJtaXRcIiksXG4gICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1kYW5nZXJcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBvbmNsaWNrOiBHYW1lVHJhY2tlckFkbWluLnZtLnJldHVyblRvTWFpbkZvcm0uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCB3aGljaEZvcm0pfSwgXCJjYW5jZWxcIiksXG4gICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTpkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgIF0pO1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uID0ge307XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5Jbml0aWFsU2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgcmV0dXJuIG0oXCJkaXYjaW5pdGlhbEFkbWluXCIsIFwiV2VsY29tZSB0byB0aGUgS3VyaWJvIFNob2UgQWRtaW4gUGFuZWxcIik7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uU2VsZWN0U2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgdmFyIGRpc3BsYXlQcm9wZXJ0aWVzID0gR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvbkRpc3BsYXlQcm9wZXJ0aWVzKEdhbWVUcmFja2VyQWRtaW4udm0uaXNMb2FkaW5nKTtcbiAgICB2YXIgc2VsZWN0RGF0YVNldCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICB2YXIgZGF0YVNldCA9IFtdO1xuICAgICAgICBzd2l0Y2ggKEdhbWVUcmFja2VyQWRtaW4udm0uc2VsZWN0U2NyZWVuU3RhdGUpIHtcbiAgICAgICAgY2FzZSBcInN5c3RlbVwiOlxuICAgICAgICAgICAgZGF0YVNldCA9IF8ucGx1Y2soR2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1zLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICBicmVhaztcbiAgICAgICAgY2FzZSBcImNvbXBhbnlcIjpcbiAgICAgICAgICAgIGRhdGFTZXQgPSBfLnBsdWNrKEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIik7XG4gICAgICAgICAgICBicmVhaztcbiAgICAgICAgY2FzZSBcImdlbnJlXCI6XG4gICAgICAgICAgICBkYXRhU2V0ID0gXy5wbHVjayhHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlcywgXCJhdHRyaWJ1dGVzXCIpO1xuICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgIH07XG4gICAgICAgIHJldHVybiBkYXRhU2V0O1xuICAgIH07XG4gICAgcmV0dXJuIG0oXCJkaXYucm93XCIsW1xuICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLCBbXG4gICAgICAgICAgICBtKFwiZm9ybVwiLCBbXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZVRyYWNrZXJBZG1pbi52bS5jdXJyZW50U2VsZWN0RW50aXR5SWQsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6R2FtZVRyYWNrZXJBZG1pbi52bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOntwbGFjZWhvbGRlcjpcIlNlbGVjdCBhbiBpdGVtIHRvIGVkaXQgb3IgZGVsZXRlXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdERhdGFTZXQoKSlcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLXN1Y2Nlc3NcIiwge3N0eWxlOiBkaXNwbGF5UHJvcGVydGllcy5idXR0b24sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bS5nZW5lcmFsSW5pdGlhdGVFZGl0fSwgXCJlZGl0XCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tZGFuZ2VyXCIsIHtzdHlsZTogZGlzcGxheVByb3BlcnRpZXMuYnV0dG9uLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgb25jbGljazogR2FtZVRyYWNrZXJBZG1pbi52bS5nZW5lcmFsRGVsZXRlfSwgXCJkZWxldGVcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTogZGlzcGxheVByb3BlcnRpZXMucHJlbG9hZGVyfSlcbiAgICAgICAgICAgICAgICBdKV0pXG4gICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uQ29tcGFueUZvcm1TY3JlZW4gPSBmdW5jdGlvbigpIHtcbiAgICByZXR1cm4gbShcImRpdi5yb3dcIixbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFttKFwiaW5wdXQuZm9ybS1jb250cm9sW3R5cGU9dGV4dF1cIiwge3BsYWNlaG9sZGVyOlwiQ29tcGFueSBOYW1lXCIsIG9uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSksIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmNvbXBhbnlGb3JtLmZpZWxkcy5uYW1lKCl9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGFiZWxcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbnB1dFt0eXBlPWNoZWNrYm94XVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJjaGVja2VkXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFueUZvcm0uZmllbGRzLmlzbWFudWZhY3R1cmVyKSwgY2hlY2tlZDogR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW55Rm9ybS5maWVsZHMuaXNtYW51ZmFjdHVyZXIoKX0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiSXMgdGhpcyBjb21wYW55IGEgY29uc29sZSBtYW51ZmFjdXR1cmVyP1wiKVxuICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvblNldChHYW1lVHJhY2tlckFkbWluLnZtLmlzTG9hZGluZywgXCJjb21wYW55Rm9ybVwiKVxuICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICBdKVxuICAgIF0pO1xufTtcblxuR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5Db2xsZWN0aW9uLkdlbnJlRm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBtKFwiZGl2LnJvd1wiLCBbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFsgbShcImlucHV0LmZvcm0tY29udHJvbFt0eXBlPXRleHRdXCIsIHtwbGFjZWhvbGRlcjpcIkdlbnJlIE5hbWVcIiwgb25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSksIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSgpfSksXG4gICAgICAgICAgICAgICAgICAgICAgICBHYW1lVHJhY2tlckFkbWluLnNjcmVlbkhlbHBlcnMuY3JlYXRlQnV0dG9uU2V0KEdhbWVUcmFja2VyQWRtaW4udm0uaXNMb2FkaW5nLCBcImdlbnJlRm9ybVwiKVxuICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgIF0pXG4gICAgXSk7XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24uU3lzdGVtRm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBtKFwiZGl2LnJvd1wiLCBbXG4gICAgICAgIG0oXCJkaXYuY29sLXhzLTEyXCIsIFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFttKFwiaW5wdXQuZm9ybS1jb250cm9sW3R5cGU9dGV4dF1cIiwge3BsYWNlaG9sZGVyOlwiU3lzdGVtIE5hbWVcIiwgb25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbUZvcm0uZmllbGRzLm5hbWUpLCB2YWx1ZTogR2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5uYW1lKCl9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoeyBvbmNoYW5nZTpHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6R2FtZVRyYWNrZXJBZG1pbi52bS5zeXN0ZW1Gb3JtLmZpZWxkcy5tYW51ZmFjdHVyZXJpZCgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9uczp7cGxhY2Vob2xkZXI6XCJNYW51ZmFjdHVyZXJcIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8uZmlsdGVyKF8ucGx1Y2soR2FtZVRyYWNrZXJBZG1pbi52bS5jb21wYW5pZXMsIFwiYXR0cmlidXRlc1wiKSwge2lzbWFudWZhY3R1cmVyOjF9KSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidVtzdHlsZT1jdXJzb3I6cG9pbnRlcl1cIiwge29uY2xpY2s6IEdhbWVUcmFja2VyQWRtaW4udm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0LmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bS5zY3JlZW5IaXN0b3J5LCBcIkFkZENvbXBhbnlTY3JlZW5cIil9LCBcIitBZGQgQ29tcGFueVwiKVxuICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgR2FtZVRyYWNrZXJBZG1pbi5zY3JlZW5IZWxwZXJzLmNyZWF0ZUJ1dHRvblNldChHYW1lVHJhY2tlckFkbWluLnZtLmlzTG9hZGluZywgXCJzeXN0ZW1Gb3JtXCIpXG4gICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgXSlcbiAgICBdKTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4uc2NyZWVuQ29sbGVjdGlvbi5HYW1lRm9ybVNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciBmb3JtQ29uZmlndXJhdGlvbiA9IHtcbiAgICAgICAgdGV4dEFyZWFEaXNwbGF5OiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBkaXNwbGF5VmFsdWUgPSAoR2FtZVRyYWNrZXJBZG1pbi52bS5mb3JtTW9kZSA9PT0gXCJzZWFyY2hcIikgPyBcImRpc3BsYXk6bm9uZVwiIDogXCJkaXNwbGF5OmluaGVyaXRcIjtcbiAgICAgICAgICAgIHJldHVybiBkaXNwbGF5VmFsdWU7XG4gICAgICAgIH0sXG4gICAgICAgIGNvbmZpcm1CdXR0b25IYW5kbGVyOiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBoYW5kbGVyO1xuICAgICAgICAgICAgc3dpdGNoIChHYW1lVHJhY2tlckFkbWluLnZtLmZvcm1Nb2RlKSB7XG4gICAgICAgICAgICBjYXNlIFwiYWRkXCI6XG4gICAgICAgICAgICAgICAgaGFuZGxlciA9IEdhbWVUcmFja2VyQWRtaW4udm0uY3JlYXRlTmV3R2FtZTtcbiAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgIGNhc2UgXCJzZWFyY2hcIjpcbiAgICAgICAgICAgICAgICBoYW5kbGVyID0gR2FtZVRyYWNrZXJBZG1pbi52bS5zZWFyY2hGb3JHYW1lO1xuICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgY2FzZSBcInVwZGF0ZVwiOlxuICAgICAgICAgICAgICAgIGhhbmRsZXIgPSBHYW1lVHJhY2tlckFkbWluLnZtLnVwZGF0ZUdhbWU7XG4gICAgICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gaGFuZGxlcjtcbiAgICAgICAgfVxuICAgIH07XG4gICAgdmFyIHJlbmRlclNlYXJjaFJlc3VsdHMgPSBmdW5jdGlvbihpc0xvYWRpbmcpIHtcbiAgICAgICAgdmFyIHJlbmRlcmVkUmVzdWx0cyA9IFtdO1xuICAgICAgICB2YXIgZGlzcGxheVByb3BlcnRpZXMgPSAoaXNMb2FkaW5nKSA/IHtyZXN1bHRzOiBcImRpc3BsYXk6bm9uZVwiLCBwcmVsb2FkZXI6IFwiZGlzcGxheTppbmhlcml0XCJ9IDoge3Jlc3VsdHM6IFwiZGlzcGxheTppbmhlcml0XCIsIHByZWxvYWRlcjogXCJkaXNwbGF5Om5vbmVcIn07XG4gICAgICAgIGlmICghXy5pc0VtcHR5KEdhbWVUcmFja2VyQWRtaW4udm0uc2VhcmNoUmVzdWx0cykgfHwgIV8uaXNFbXB0eShHYW1lVHJhY2tlckFkbWluLnZtLm5vUmVzdWx0cykpIHtcbiAgICAgICAgICAgIHJlbmRlcmVkUmVzdWx0cyA9IG0oXCJkaXZcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAge3N0eWxlOmRpc3BsYXlQcm9wZXJ0aWVzLnJlc3VsdHN9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbbShcImRpdlwiLCBHYW1lVHJhY2tlckFkbWluLnZtLm5vUmVzdWx0cyksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLm1hcChHYW1lVHJhY2tlckFkbWluLnZtLnNlYXJjaFJlc3VsdHMsIGZ1bmN0aW9uKHJlc3VsdCwgaW5kZXgpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhciBiZ0NvbG9yID0gXCJiYWNrZ3JvdW5kLWNvbG9yOiNDRUNGRTBcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlmIChpbmRleCAlIDIgPT0gMCkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGJnQ29sb3IgPSBcImJhY2tncm91bmQtY29sb3I6I0ZGRlwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcmV0dXJuIG0oXCJkaXYucm93LnJlc3VsdC1yb3dcIiwge3N0eWxlOmJnQ29sb3J9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgW20oXCJkaXYuY29sLXhzLTlcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIChyZXN1bHQubmFtZSArIFwiIFtcIiArIHJlc3VsdC5yZWdpb24gKyBcIl0gKFwiICsgcmVzdWx0LnN5c3RlbU5hbWUgKyBcIilcIikpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJkaXYuY29sLXhzLTNcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwic3Bhbi5nbHlwaGljb24uZ2x5cGhpY29uLXJlbW92ZS5nYW1lLXNlYXJjaC1yZXN1bHRzLWJ1dHRvblwiLCB7b25jbGljazpHYW1lVHJhY2tlckFkbWluLnZtLmRlbGV0ZUdhbWUuYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCByZXN1bHQuaWQpfSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmdseXBoaWNvbi5nbHlwaGljb24tcGVuY2lsLmdhbWUtc2VhcmNoLXJlc3VsdHMtYnV0dG9uXCIsIHtvbmNsaWNrOkdhbWVUcmFja2VyQWRtaW4udm0uaW5pdGlhdGVFZGl0R2FtZUVudHJ5LmJpbmQoR2FtZVRyYWNrZXJBZG1pbi52bSwgcmVzdWx0LmlkKX0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSldKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTpkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiByZW5kZXJlZFJlc3VsdHM7XG4gICAgfTtcbiAgICByZXR1cm4gW20oXCJkaXYucm93XCIsW1xuICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLFtcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFtcbiAgICAgICAgICAgICAgICBtKFwiaW5wdXQuZm9ybS1jb250cm9sXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uZ2FtZUZvcm0uZmllbGRzLm5hbWUpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMubmFtZSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJOYW1lXCJ9KSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVUcmFja2VyQWRtaW4udm0uZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJSZWdpb25cIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXCJOVFNDXCIsIFwiTlRTQy1KXCIsIFwiUEFMXCJdKSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVUcmFja2VyQWRtaW4udm0uZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWU6IEdhbWVUcmFja2VyQWRtaW4udm0uZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiU3lzdGVtXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5wbHVjayhHYW1lVHJhY2tlckFkbWluLnZtLnN5c3RlbXMsIFwiYXR0cmlidXRlc1wiKSksXG4gICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHtvbmNoYW5nZTpHYW1lVHJhY2tlckFkbWluLnZtLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMuZ2VucmVzKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiR2VucmVzXCJ9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5wbHVjayhHYW1lVHJhY2tlckFkbWluLnZtLmdlbnJlcywgXCJhdHRyaWJ1dGVzXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0cnVlKSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVUcmFja2VyQWRtaW4udm0uZ2FtZUZvcm0uZmllbGRzLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lVHJhY2tlckFkbWluLnZtLmdhbWVGb3JtLmZpZWxkcy5jb21wYW5pZXMoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJDb21wYW5pZXNcIn19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnBsdWNrKEdhbWVUcmFja2VyQWRtaW4udm0uY29tcGFuaWVzLCBcImF0dHJpYnV0ZXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRydWUpLFxuICAgICAgICAgICAgICAgIG0oXCJpbnB1dC5mb3JtLWNvbnRyb2xcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcGxhY2Vob2xkZXI6IFwiUXVhbnRpdHlcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwge3N0eWxlOmZvcm1Db25maWd1cmF0aW9uLnRleHRBcmVhRGlzcGxheSgpfSwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwicFwiLCBcIlNob3J0IERlc2NyaXB0aW9uXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidGV4dGFyZWFcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMuYmx1cmIpfSwgR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMuYmx1cmIoKSksXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJsYWJlbFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwiaW5wdXRbdHlwZT1jaGVja2JveF1cIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwiY2hlY2tlZFwiLCBHYW1lVHJhY2tlckFkbWluLnZtLmdhbWVGb3JtLmZpZWxkcy5oYXNtYW51YWwpLCBjaGVja2VkOiBHYW1lVHJhY2tlckFkbWluLnZtLmdhbWVGb3JtLmZpZWxkcy5oYXNtYW51YWwoKX0pXG4gICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCBcIk1hbnVhbFwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXYuY2hlY2tib3hcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwibGFiZWxcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcImlucHV0W3R5cGU9Y2hlY2tib3hdXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcImNoZWNrZWRcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMuaGFzYm94KSwgY2hlY2tlZDogR2FtZVRyYWNrZXJBZG1pbi52bS5nYW1lRm9ybS5maWVsZHMuaGFzYm94KCl9KVxuICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW5cIiwgXCJCb3hcIilcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi50ZXh0QXJlYURpc3BsYXkoKX0sIFtcbiAgICAgICAgICAgICAgICAgICAgbShcInBcIiwgXCJOb3Rlc1wiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInRleHRhcmVhXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uZ2FtZUZvcm0uZmllbGRzLm5vdGVzKX0sIEdhbWVUcmFja2VyQWRtaW4udm0uZ2FtZUZvcm0uZmllbGRzLm5vdGVzKCkpLFxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIEdhbWVUcmFja2VyQWRtaW4uc2NyZWVuSGVscGVycy5jcmVhdGVCdXR0b25TZXQoR2FtZVRyYWNrZXJBZG1pbi52bS5pc0xvYWRpbmcsIFwiZ2FtZUZvcm1cIilcbiAgICAgICAgICAgIF0pLFxuICAgICAgICBdKVxuICAgIF0pLFxuICAgICAgICAgICAgcmVuZGVyU2VhcmNoUmVzdWx0cyhHYW1lVHJhY2tlckFkbWluLnZtLnNlYXJjaElzTG9hZGluZylcbiAgICAgICAgICAgXTtcbn07XG5cbkdhbWVUcmFja2VyQWRtaW4udmlldyA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciByZW5kZXJTY3JlZW5zID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBfLm1hcChHYW1lVHJhY2tlckFkbWluLnNjcmVlbkNvbGxlY3Rpb24sIGZ1bmN0aW9uKHNjcmVlbkNvbnRlbnQsIHNjcmVlbk5hbWUpIHtcbiAgICAgICAgICAgIHJldHVybiBtKFwiZGl2XCIsIHtzdHlsZTpcImRpc3BsYXk6XCIrR2FtZVRyYWNrZXJBZG1pbi52bS5zaG91bGREaXNwbGF5U2NyZWVuKHNjcmVlbk5hbWUpfSwgc2NyZWVuQ29udGVudCgpKTtcbiAgICAgICAgfSk7XG4gICAgfTtcbiAgICByZXR1cm4gW1xuICAgICAgICBtKFwibmF2Lm5hdmJhci5uYXZiYXItZGVmYXVsdFwiLCBbXG4gICAgICAgICAgICBtKFwiZGl2LmNvbnRhaW5lci1mbHVpZFwiLCBbXG4gICAgICAgICAgICAgICAgbShcImRpdi5uYXZiYXItaGVhZGVyXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5uYXZiYXItdG9nZ2xlLmNvbGxhcHNlZFt0eXBlPWJ1dHRvbl1bZGF0YS10b2dnbGU9Y29sbGFwc2VdW2RhdGEtdGFyZ2V0PSNtYWluLW5hdl1cIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uaWNvbi1iYXJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwic3Bhbi5pY29uLWJhclwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmljb24tYmFyXCIpXG4gICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdiNtYWluLW5hdi5jb2xsYXBzZS5uYXZiYXItY29sbGFwc2VcIixbIFxuICAgICAgICAgICAgICAgICAgICBtKFwidWwubmF2Lm5hdmJhci1uYXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpLmRyb3Bkb3duXCIsIFttKFwiYVtocmVmPSNdLmRyb3Bkb3duLXRvZ2dsZVtkYXRhLXRvZ2dsZT1kcm9wZG93bl1bcm9sZT1idXR0b25dXCIsIFwiR2FtZXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImdhbWVcIiwgXCJHYW1lRm9ybVNjcmVlblwiKX0sIFwiQWRkIEdhbWVcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImdhbWVcIiwgXCJHYW1lRm9ybVNjcmVlblwiKX0sIFwiRWRpdC9EZWxldGUgR2FtZVwiKV0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIlN5c3RlbXNcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcInN5c3RlbVwiLCBcIlN5c3RlbUZvcm1TY3JlZW5cIil9LCBcIkFkZCBTeXN0ZW1cIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcInN5c3RlbVwiLCBcIlNlbGVjdFNjcmVlblwiKX0sIFwiRWRpdC9EZWxldGUgU3lzdGVtXCIpXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICAgICAgbShcImxpLmRyb3Bkb3duXCIsIFttKFwiYVtocmVmPSNdLmRyb3Bkb3duLXRvZ2dsZVtkYXRhLXRvZ2dsZT1kcm9wZG93bl1bcm9sZT1idXR0b25dXCIsIFwiR2VucmVcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImdlbnJlXCIsIFwiR2VucmVGb3JtU2NyZWVuXCIpfSwgXCJBZGQgR2VucmVcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImdlbnJlXCIsIFwiU2VsZWN0U2NyZWVuXCIpfSwgXCJFZGl0L0RlbGV0ZSBHZW5yZVwiKV0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJsaS5kcm9wZG93blwiLCBbbShcImFbaHJlZj0jXS5kcm9wZG93bi10b2dnbGVbZGF0YS10b2dnbGU9ZHJvcGRvd25dW3JvbGU9YnV0dG9uXVwiLCBcIkNvbXBhbnlcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwidWwuZHJvcGRvd24tbWVudVtyb2xlPW1lbnVdXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcImFkZFwiLCBcImNvbXBhbnlcIiwgXCJDb21wYW55Rm9ybVNjcmVlblwiKX0sIFwiQWRkIENvbXBhbnlcIildKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwibGlcIiwgW20oXCJhW2hyZWY9I11cIiwge29uY2xpY2s6R2FtZVRyYWNrZXJBZG1pbi52bS5qdW1wVG9TY3JlZW4uYmluZChHYW1lVHJhY2tlckFkbWluLnZtLCBcInNlYXJjaFwiLCBcImNvbXBhbnlcIiwgXCJTZWxlY3RTY3JlZW5cIil9LCBcIkVkaXQvRGVsZXRlIENvbXBhbnlcIildKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgIF0pXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICBdKVxuICAgICAgICBdKSxcbiAgICAgICAgbShcImRpdi5jb250YWluZXJcIiwgW1xuICAgICAgICAgICAgbShcImRpdi50ZXh0LXN1Y2Nlc3NcIiwgR2FtZVRyYWNrZXJBZG1pbi52bS5zdWNjZXNzTWVzc2FnZSksXG4gICAgICAgICAgICBtKFwiZGl2LnRleHQtZGFuZ2VyXCIsIEdhbWVUcmFja2VyQWRtaW4udm0uZXJyb3JNZXNzYWdlKSxcbiAgICAgICAgICAgIHJlbmRlclNjcmVlbnMoKVxuICAgICAgICBdKVxuICAgIF07XG59O1xuIiwiR2FtZVRyYWNrZXJBZG1pbi52bSA9IG5ldyBmdW5jdGlvbigpIHtcbiAgICB2YXIgdm0gPSB7fTtcbiAgICB2bS5pbml0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIFxuICAgICAgICB2bS5mb3JtTW9kZSA9IFwiXCI7XG4gICAgICAgIHZtLnNlbGVjdFNjcmVlblN0YXRlID0gXCJcIjtcbiAgICAgICAgXG4gICAgICAgIC8vVGhpcyBpcyB1c2VkIGFzIGEgc3RhY2s7XG4gICAgICAgIHZtLnNjcmVlbkhpc3RvcnkgPSBbXCJJbml0aWFsU2NyZWVuXCJdO1xuXG4gICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvciA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJJbnRlcm5hbCBTZXJ2ZXIgRXJyb3JcIjtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICB2bS5jbGVhck1lc3NhZ2VzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiXCI7XG4gICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlwiO1xuICAgICAgICAgICAgdm0ubm9SZXN1bHRzID0gXCJcIjtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY29tcGxldGVSZXNldCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgdm0uc2VhcmNoUmVzdWx0cyA9IFtdO1xuICAgICAgICAgICAgdm0uZ2FtZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5zeXN0ZW1Gb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgdm0uZ2VucmVGb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgdm0uY29tcGFueUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgIH07XG4gICAgICAgIFxuICAgICAgICB2bS5qdW1wVG9TY3JlZW4gPSBmdW5jdGlvbihmb3JtTW9kZSwgc2VsZWN0U2NyZWVuU3RhdGUsIHNjcmVlbk5hbWUpIHtcbiAgICAgICAgICAgIHZtLmNvbXBsZXRlUmVzZXQoKTtcbiAgICAgICAgICAgIHZtLmZvcm1Nb2RlID0gZm9ybU1vZGU7XG4gICAgICAgICAgICB2bS5zZWxlY3RTY3JlZW5TdGF0ZSA9IHNlbGVjdFNjcmVlblN0YXRlO1xuICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeSA9IFtzY3JlZW5OYW1lLCBcIkluaXRpYWxTY3JlZW5cIl07XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgIFxuICAgICAgICAvL1RoaXMgZGF0YSBpcyBhY3R1YWxseSBib290c3RyYXBlZCBhbmQgdGhlIHZhcmlhYmxlIGl0J3MgY29weWluZyBmcm9tIGlzIGluIHRoZSB0ZW1wbGF0ZVxuICAgICAgICB2bS5jb21wYW5pZXMgPSBfLm1hcChjb21wYW5pZXMsIGZ1bmN0aW9uKGNvbXBhbnkpIHsgcmV0dXJuIG5ldyBHYW1lVHJhY2tlckFkbWluLkNvbXBhbnkoY29tcGFueSk7IH0pO1xuICAgICAgICB2bS5nZW5yZXMgPSBfLm1hcChnZW5yZXMsIGZ1bmN0aW9uKGdlbnJlKSB7IHJldHVybiBuZXcgR2FtZVRyYWNrZXJBZG1pbi5HZW5yZShnZW5yZSk7IH0pO1xuICAgICAgICB2bS5zeXN0ZW1zID0gXy5tYXAoc3lzdGVtcywgZnVuY3Rpb24oc3lzdGVtKSB7IHJldHVybiBuZXcgR2FtZVRyYWNrZXJBZG1pbi5TeXN0ZW0oc3lzdGVtKTsgfSk7XG4gICAgICAgIFxuICAgICAgICB2bS5zaG91bGREaXNwbGF5U2NyZWVuID0gZnVuY3Rpb24oc2NyZWVuTmFtZSkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlQcm9wZXJ0eSA9IFwibm9uZVwiO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uc2NyZWVuSGlzdG9yeSkpIHtcbiAgICAgICAgICAgICAgICBkaXNwbGF5UHJvcGVydHkgPSAoc2NyZWVuTmFtZSA9PT0gdm0uc2NyZWVuSGlzdG9yeVswXSkgPyBcImluaGVyaXRcIiA6IFwibm9uZVwiO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGRpc3BsYXlQcm9wZXJ0eTtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY3JlYXRlQmFja0J1dHRvbiA9IGZ1bmN0aW9uKGNhbGxiYWNrKSB7XG4gICAgICAgICAgICByZXR1cm4gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICAgICAgY2FsbGJhY2soKTtcbiAgICAgICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnNoaWZ0KCk7XG4gICAgICAgICAgICB9O1xuICAgICAgICB9O1xuXG4gICAgICAgIHZtLnJldHVyblRvTWFpbkZvcm0gPSBmdW5jdGlvbih3aGljaEZvcm0pIHtcbiAgICAgICAgICAgIHZtW3doaWNoRm9ybV0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnNoaWZ0KCk7XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uY29tcGFueUZvcm0gPSBuZXcgR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0oe25hbWU6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlzbWFudWZhY3R1cmVyOiBtLnByb3AoZmFsc2UpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KTtcbiAgICAgICAgLyogVE9ETyBUaGUgYWRkIGZ1bmN0aW9ucyBhcmUgYmFzaWNhbGx5IHRoZSBzYW1lLiBUaGVyZSBzaG91bGQgYmUgYSBnb29kIHdheSBvZiByZWZhY3RvcmluZyB0aGlzIGVpdGhlciBjcmVhdGluZyBhIGZ1bmNpdG9uIGdlbmVyYXRvclxuICAgICAgICAgKiBvciBjcmVhdGluZyBhIGNoaWxkIG9iamVjdFxuICAgICAgICAgKi9cbiAgICAgICAgdm0uY29tcGFueUZvcm0uc3VibWl0SGFuZGxlcnMuYWRkID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uY29tcGFueUZvcm0uZmllbGRzLm5hbWUoKSkpIHtcbiAgICAgICAgICAgICAgICB2YXIgbmV3Q29tcGFueSA9IG5ldyBHYW1lVHJhY2tlckFkbWluLkNvbXBhbnkodm0uY29tcGFueUZvcm0ucmV0dXJuRmllbGRzKCkpO1xuICAgICAgICAgICAgICAgIG5ld0NvbXBhbnkuc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmNvbXBhbmllcy5wdXNoKG5ld0NvbXBhbnkpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgY29tcGFueSBoYXMgYmVlbiBhZGRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmNvbXBhbnlGb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIkNvdWxkIG5vdCBhZGQgdGhlIGNvbXBhbnlcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIHRoZSBuYW1lIG9mIHRoZSBjb21wYW55XCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uY3VycmVudENvbXBhbnlJbmRleCA9IG51bGw7XG4gICAgICAgIHZtLmNvbXBhbnlGb3JtLnN1Ym1pdEhhbmRsZXJzLnVwZGF0ZSA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc051bGwodm0uY3VycmVudENvbXBhbnlJbmRleCkgJiYgIV8uaXNFbXB0eSh2bS5jb21wYW55Rm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZtLmNvbXBhbmllc1t2bS5jdXJyZW50Q29tcGFueUluZGV4XS51cGRhdGUodm0uY29tcGFueUZvcm0ucmV0dXJuRmllbGRzKCkpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGNvbXBhbnkgaGFzIGJlZW4gdXBkYXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBlbnRlciB0aGUgbmFtZSBvZiB0aGUgY29tcGFueVwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICBcbiAgICAgICAgdm0uZ2VucmVGb3JtID0gbmV3IEdhbWVUcmFja2VyU2hhcmVkLlRyYWNrZXJGb3JtKHtuYW1lOiBtLnByb3AoXCJcIil9KTtcbiAgICAgICAgdm0uZ2VucmVGb3JtLnN1Ym1pdEhhbmRsZXJzLmFkZCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLmdlbnJlRm9ybS5maWVsZHMubmFtZSgpKSkge1xuICAgICAgICAgICAgICAgIHZhciBuZXdHZW5yZSA9IG5ldyBHYW1lVHJhY2tlckFkbWluLkdlbnJlKHZtLmdlbnJlRm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgbmV3R2VucmUuc2F2ZSgpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmdlbnJlcy5wdXNoKG5ld0dlbnJlKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGdlbnJlIGhhcyBiZWVuIGFkZGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uZ2VucmVGb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIkNvdWxkIG5vdCBhZGQgdGhlIGdlbnJlXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBlbnRlciB0aGUgbmFtZSBvZiB0aGUgZ2VucmVcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcbiAgICAgICAgdm0uY3VycmVudEdlbnJlSW5kZXggPSBudWxsO1xuICAgICAgICB2bS5nZW5yZUZvcm0uc3VibWl0SGFuZGxlcnMudXBkYXRlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzTnVsbCh2bS5jdXJyZW50R2VucmVJbmRleCkgJiYgIV8uaXNFbXB0eSh2bS5nZW5yZUZvcm0uZmllbGRzLm5hbWUoKSkpIHtcbiAgICAgICAgICAgICAgICB2bS5nZW5yZXNbdm0uY3VycmVudEdlbnJlSW5kZXhdLnVwZGF0ZSh2bS5nZW5yZUZvcm0ucmV0dXJuRmllbGRzKCkpXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGdlbnJlIGhhcyBiZWVuIHVwZGF0ZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZW50ZXIgdGhlIG5hbWUgb2YgdGhlIGdlbnJlXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9O1xuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuXG4gICAgICAgIHZtLnN5c3RlbUZvcm0gPSBuZXcgR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0oe25hbWU6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgbWFudWZhY3R1cmVyaWQ6IG0ucHJvcChcIlwiKX0pO1xuICAgICAgICB2bS5zeXN0ZW1Gb3JtLnN1Ym1pdEhhbmRsZXJzLmFkZCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLnN5c3RlbUZvcm0uZmllbGRzLm5hbWUoKSkgJiYgIV8uaXNFbXB0eSh2bS5zeXN0ZW1Gb3JtLmZpZWxkcy5tYW51ZmFjdHVyZXJpZCgpKSkge1xuICAgICAgICAgICAgICAgIHZhciBuZXdTeXN0ZW0gPSBuZXcgR2FtZVRyYWNrZXJBZG1pbi5TeXN0ZW0odm0uc3lzdGVtRm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgbmV3U3lzdGVtLnNhdmUoKVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zeXN0ZW1zLnB1c2gobmV3U3lzdGVtKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIHN5c3RlbSBoYXMgYmVlbiBhZGRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN5c3RlbUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIGFsbCBvZiB0aGUgZmllbGRzXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgICAgIHZtLmN1cnJlbnRTeXN0ZW1JbmRleCA9IG51bGw7XG4gICAgICAgIHZtLnN5c3RlbUZvcm0uc3VibWl0SGFuZGxlcnMudXBkYXRlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdm0uY2xlYXJNZXNzYWdlcygpO1xuICAgICAgICAgICAgaWYgKCFfLmlzTnVsbCh2bS5jdXJyZW50U3lzdGVtSW5kZXgpICYmICFfLmlzRW1wdHkodm0uc3lzdGVtRm9ybS5maWVsZHMubmFtZSgpKSAmJiAhXy5pc0VtcHR5KHZtLnN5c3RlbUZvcm0uZmllbGRzLm1hbnVmYWN0dXJlcmlkKCkpKSB7XG4gICAgICAgICAgICAgICAgdm0uc3lzdGVtc1t2bS5jdXJyZW50U3lzdGVtSW5kZXhdLnVwZGF0ZSh2bS5zeXN0ZW1Gb3JtLnJldHVybkZpZWxkcygpKVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIlRoZSBzeXN0ZW0gaGFzIGJlZW4gdXBkYXRlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgIH0pO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIGFsbCB0aGUgZmllbGRzXCI7XG4gICAgICAgICAgICAgICAgdm0uaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG4gICAgICAgIFxuICAgICAgICAvL1RoZSBuYW1pbmcgY29udmVudGlvbiBzZWVtcyB0byBoYXZlIGNoYW5nZWQgKG5vdCBjYW1lbCBjYXNlKSBidXQgdGhpcyBpcyBiZWNhdXNlIHdlIHdpc2hcbiAgICAgICAgLy9UbyBtaXJyb3Igd2hhdCB3ZSBoYXZlIGluIHRoZSB0YWJsZSwgbWFpbmx5IGZvciBiYWNrLWVuZCBjb252ZW5pZW5jZVxuICAgICAgICAvL1RPRE8gaGF2ZSBlYWNoIGZvcm0gaGF2ZSBhIG5hbWVzcGFjZSBmb3IgdGhlaXIgdGhpbmdpZXNcbiAgICAgICAgdm0uZ2FtZUZvcm0gPSBuZXcgR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0oe25hbWU6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGJsdXJiOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZWdpb246IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhhc21hbnVhbDogbS5wcm9wKGZhbHNlKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhhc2JveDogbS5wcm9wKGZhbHNlKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5vdGVzOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBxdWFudGl0eTogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZ2VucmVzOiBtLnByb3AoW10pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29tcGFuaWVzOiBtLnByb3AoW10pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3lzdGVtaWQ6IG0ucHJvcChcIlwiKX0pO1xuICAgICAgICBcbiAgICAgICAgdm0uZ2FtZUZvcm0uc3VibWl0SGFuZGxlcnMuYWRkID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgaWYgKCFfLmlzRW1wdHkodm0uZ2FtZUZvcm0uZmllbGRzLm5hbWUoKSkgJiZcbiAgICAgICAgICAgICAgICAhXy5pc0VtcHR5KHZtLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSkgJiZcbiAgICAgICAgICAgICAgICBfLmlzRmluaXRlKE51bWJlcih2bS5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSkpICYmXG4gICAgICAgICAgICAgICAgTnVtYmVyKHZtLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCgpKSA+IDAgJiZcbiAgICAgICAgICAgICAgICBfLmlzRmluaXRlKE51bWJlcih2bS5nYW1lRm9ybS5maWVsZHMucXVhbnRpdHkoKSkpICYmXG4gICAgICAgICAgICAgICAgTnVtYmVyKHZtLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpKSA+IDApIHtcbiAgICAgICAgICAgICAgICBtLnJlcXVlc3Qoe21ldGhvZDogXCJQT1NUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2FkbWluL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiB2bS5nYW1lRm9ybS5yZXR1cm5GaWVsZHMoKX0pXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5nYW1lRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJTdWNjZXNzZnVsbHkgYWRkZWQgdGhlIGdhbWVcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZmlsbCBpbiBhbGwgdGhlIGZpZWxkc1wiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuXG4gICAgICAgIHZtLnNlYXJjaFJlc3VsdHMgPSBbXTtcbiAgICAgICAgdm0ubm9SZXN1bHRzID0gXCJcIjtcbiAgICAgICAgdm0uZ2FtZUZvcm0uc3VibWl0SGFuZGxlcnMuc2VhcmNoID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgdmFyIGNvbXBsZXRlZFNldCA9IF8ub21pdCh2bS5nYW1lRm9ybS5yZXR1cm5GaWVsZHMoKSwgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IHRydWU7XG4gICAgICAgICAgICAgICAgaWYgKF8uaXNCb29sZWFuKHZhbHVlKSkge1xuICAgICAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9ICF2YWx1ZTtcbiAgICAgICAgICAgICAgICB9IGVsc2UgaWYgKF8uaXNTdHJpbmcodmFsdWUpICYmIHZhbHVlLmxlbmd0aCA+IDApIHtcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuVmFsdWUgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgICAgICAgICAgfSk7XG4gICAgICAgICAgICBcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KGNvbXBsZXRlZFNldCkpIHtcbiAgICAgICAgICAgICAgICBtLnJlcXVlc3Qoe21ldGhvZDpcInBvc3RcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvc2VhcmNoLWdhbWVzLWFqYXgvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBjb21wbGV0ZWRTZXR9KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgLy9FbXB0eSByZXN1bHRzIHNldCByZXR1cm5zIGEgc2luZ2xlIGl0ZW0gYXJyYXkgd2l0aCBudWxsIGJlaW5nIHRoYXQgb2JqZWN0XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5zZWFyY2hSZXN1bHRzID0gXy5yZW1vdmUocmVzcG9uc2UsIGZ1bmN0aW9uKGl0ZW0pIHsgcmV0dXJuICFfLmlzTnVsbChpdGVtKTsgfSk7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAodm0uc2VhcmNoUmVzdWx0cy5sZW5ndGggPCAxKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0ubm9SZXN1bHRzID0gXCJObyBtYXRjaGVzIHdlcmUgZm91bmRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0uZXJyb3JNZXNzYWdlID0gXCJQbGVhc2UgZW50ZXIgYXQgbGVhc3Qgb25lIHNlYXJjaCBwYXJhbWV0ZXJcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jdXJyZW50R2FtZUlkID0gMDtcbiAgICAgICAgdm0uc2VhcmNoSXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgIHZtLmluaXRpYXRlRWRpdEdhbWVFbnRyeSA9IGZ1bmN0aW9uKGdhbWVJZCkge1xuICAgICAgICAgICAgdm0uc2VhcmNoSXNMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChnYW1lSWQgJiYgXy5pc0Zpbml0ZShOdW1iZXIoZ2FtZUlkKSkpIHtcbiAgICAgICAgICAgICAgICAvKiBBIGtub3duIGxpbWl0YXRpb24gd2l0aCB0aGUgYmFja2VuZDogdGhpbmdzIHdlIGV4cGVjdCB0byBiZSBhbiBhcnJheSBtYXkgYmUgYSBzaW1wbGUgb2JqZWN0IGR1ZSB0byB0aGUganNvbiBlbmNvZGVyIG9uIHRoZSBiYWNrZW5kXG4gICAgICAgICAgICAgICAgICAgbm90IGJlaW5nIGFibGUgdG8gZW5jb2RlIHNpbmdsZSByb3cgcmVzdWx0cyBjb3JyZWN0bHlcbiAgICAgICAgICAgICAgICAgKi9cbiAgICAgICAgICAgICAgICB2YXIgZW5zdXJlQXJyYXkgPSBmdW5jdGlvbihpdGVtKSB7XG4gICAgICAgICAgICAgICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IF8uaXNBcnJheShpdGVtKSA/IGl0ZW0gOiBbaXRlbV07XG4gICAgICAgICAgICAgICAgICAgIHJldHVybiByZXR1cm5WYWx1ZTtcbiAgICAgICAgICAgICAgICB9O1xuICAgICAgICAgICAgICAgIC8vV2UgY291bGQganVzdCB1c2UgdGhlIGRhdGEgd2UgcmV0cmlldmVkIGZyb20gdGhlIHNlYXJjaCBidXQgbGV0J3MgZ3VhcmFudGVlIHRoZSB1c2VyIHdpdGggdGhlIG1vc3QgcmVjZW50IGluZm9ybWF0aW9uXG4gICAgICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6IFwiR0VUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiB7aWQ6IE51bWJlcihnYW1lSWQpfVxuICAgICAgICAgICAgICAgICAgICAgICAgICB9KVxuICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudEdhbWVJZCA9IE51bWJlcihyZXNwb25zZS5pZCk7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5nYW1lRm9ybS5maWVsZHMuY29tcGFuaWVzKF8ucGx1Y2soZW5zdXJlQXJyYXkocmVzcG9uc2UuY29tcGFuaWVzKSwgXCJjb21wYW55SWRcIikpO1xuICAgICAgICAgICAgICAgICAgICAgICAgdm0uZ2FtZUZvcm0uZmllbGRzLmdlbnJlcyhfLnBsdWNrKGVuc3VyZUFycmF5KHJlc3BvbnNlLmdlbnJlcyksIFwiZ2VucmVJZFwiKSk7XG4gICAgICAgICAgICAgICAgICAgICAgICB2bS5nYW1lRm9ybS5wb3B1bGF0ZUZvcm0oXy5vbWl0KHJlc3BvbnNlLCBbXCJjb21wYW5pZXNcIiwgXCJnZW5yZXNcIl0pKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLmZvcm1Nb2RlID0gXCJ1cGRhdGVcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLnNlYXJjaFJlc3VsdHMgPSBbXTtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZtLnNlYXJjaElzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgfTtcblxuICAgICAgICB2bS5nYW1lRm9ybS5zdWJtaXRIYW5kbGVycy51cGRhdGUgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5nYW1lRm9ybS5maWVsZHMubmFtZSgpKSAmJlxuICAgICAgICAgICAgICAgICFfLmlzRW1wdHkodm0uZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbigpKSAmJlxuICAgICAgICAgICAgICAgIF8uaXNGaW5pdGUoTnVtYmVyKHZtLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCgpKSkgJiZcbiAgICAgICAgICAgICAgICBOdW1iZXIodm0uZ2FtZUZvcm0uZmllbGRzLnN5c3RlbWlkKCkpID4gMCAmJlxuICAgICAgICAgICAgICAgIF8uaXNGaW5pdGUoTnVtYmVyKHZtLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpKSkgJiZcbiAgICAgICAgICAgICAgICBOdW1iZXIodm0uZ2FtZUZvcm0uZmllbGRzLnF1YW50aXR5KCkpID4gMCkgeyAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgIHZhciBkYXRhID0gXy5leHRlbmQoe2lkOiBOdW1iZXIodm0uY3VycmVudEdhbWVJZCl9LCB2bS5nYW1lRm9ybS5yZXR1cm5GaWVsZHMoKSk7XG4gICAgICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6IFwiUFVUXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL2FkbWluL2dhbWUvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBkYXRhfSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uc3VjY2Vzc01lc3NhZ2UgPSBcIkdhbWUgc3VjY2Vzc2Z1bGx5IHVwZGF0ZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH0gXG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICB2bS5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBmaWxsIGluIHRoZSBmaWVsZHNcIjtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5kZWxldGVHYW1lID0gZnVuY3Rpb24oZ2FtZUlkKSB7XG4gICAgICAgICAgICB2bS5zZWFyY2hJc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgaWYgKGdhbWVJZCAmJiBfLmlzRmluaXRlKE51bWJlcihnYW1lSWQpKSkge1xuICAgICAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOiBcIkRFTEVURVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvYWRtaW4vZ2FtZS9cIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiB7aWQ6IE51bWJlcihnYW1lSWQpfX0pXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2Uuc3RhdHVzID09PSBcInN1Y2Nlc3NcIikge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgZ2FtZSBoYXMgYmVlbiBkZWxldGVkXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5yZW1vdmUodm0uc2VhcmNoUmVzdWx0cywgZnVuY3Rpb24oZ2FtZSkgeyByZXR1cm4gZ2FtZS5pZCA9PT0gTnVtYmVyKGdhbWVJZCk7IH0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnNlYXJjaElzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICB9LCB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQgPSBtLnByb3AoXCJcIik7XG4gICAgICAgIHZtLmdlbmVyYWxJbml0aWF0ZUVkaXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZtLmNsZWFyTWVzc2FnZXMoKTtcbiAgICAgICAgICAgIGlmICghXy5pc0VtcHR5KHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKSkge1xuICAgICAgICAgICAgICAgIHZtLmZvcm1Nb2RlID0gXCJ1cGRhdGVcIjtcbiAgICAgICAgICAgICAgICBzd2l0Y2ggKHZtLnNlbGVjdFNjcmVlblN0YXRlKSB7XG4gICAgICAgICAgICAgICAgY2FzZSBcInN5c3RlbVwiOlxuICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U3lzdGVtSW5kZXggPSBfLmZpbmRJbmRleCh2bS5zeXN0ZW1zLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5zeXN0ZW1Gb3JtLnBvcHVsYXRlRm9ybSh2bS5zeXN0ZW1zW3ZtLmN1cnJlbnRTeXN0ZW1JbmRleF0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5zY3JlZW5IaXN0b3J5LnVuc2hpZnQoXCJTeXN0ZW1Gb3JtU2NyZWVuXCIpO1xuICAgICAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgICAgICBjYXNlIFwiY29tcGFueVwiOlxuICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50Q29tcGFueUluZGV4ID0gXy5maW5kSW5kZXgodm0uY29tcGFuaWVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICB2bS5jb21wYW55Rm9ybS5wb3B1bGF0ZUZvcm0odm0uY29tcGFuaWVzW3ZtLmN1cnJlbnRDb21wYW55SW5kZXhdKTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0KFwiQ29tcGFueUZvcm1TY3JlZW5cIik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIGNhc2UgXCJnZW5yZVwiOlxuICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50R2VucmVJbmRleCA9IF8uZmluZEluZGV4KHZtLmdlbnJlcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uZ2VucmVGb3JtLnBvcHVsYXRlRm9ybSh2bS5nZW5yZXNbdm0uY3VycmVudEdlbnJlSW5kZXhdKTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc2NyZWVuSGlzdG9yeS51bnNoaWZ0KFwiR2VucmVGb3JtU2NyZWVuXCIpO1xuICAgICAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIHZtLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIHNlbGVjdCBhbiBpdGVtIGluIHRoZSBkcm9wZG93blwiO1xuICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgdm0uY3VycmVudFNlbGVjdEVudGl0eUlkKFwiXCIpO1xuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgICAgICB2bS5nZW5lcmFsRGVsZXRlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2bS5jbGVhck1lc3NhZ2VzKCk7XG4gICAgICAgICAgICBpZiAoIV8uaXNFbXB0eSh2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSkpIHtcbiAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICAgICAgICAgIHZhciBjdXJyZW50SW5kZXg7XG4gICAgICAgICAgICAgICAgc3dpdGNoICh2bS5zZWxlY3RTY3JlZW5TdGF0ZSkge1xuICAgICAgICAgICAgICAgIGNhc2UgXCJzeXN0ZW1cIjpcbiAgICAgICAgICAgICAgICAgICAgY3VycmVudEluZGV4ID0gXy5maW5kSW5kZXgodm0uc3lzdGVtcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uc3lzdGVtc1tjdXJyZW50SW5kZXhdLnJlbW92ZSgpXG4gICAgICAgICAgICAgICAgICAgICAgICAudGhlbihmdW5jdGlvbihyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZS5zdGF0dXMgPT09IFwic3VjY2Vzc1wiKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ucmVtb3ZlKHZtLnN5c3RlbXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgc3lzdGVtIGhhcyBiZWVuIHJlbW92ZWRcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0uY3VycmVudFNlbGVjdEVudGl0eUlkKFwiXCIpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICB9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICAgICAgICAgIGJyZWFrO1xuICAgICAgICAgICAgICAgIGNhc2UgXCJjb21wYW55XCI6XG4gICAgICAgICAgICAgICAgICAgIGN1cnJlbnRJbmRleCA9IF8uZmluZEluZGV4KHZtLmNvbXBhbmllcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uY29tcGFuaWVzW2N1cnJlbnRJbmRleF0ucmVtb3ZlKClcbiAgICAgICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXy5yZW1vdmUodm0uY29tcGFuaWVzLCB7YXR0cmlidXRlczoge2lkOiBOdW1iZXIodm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpfX0pO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5zdWNjZXNzTWVzc2FnZSA9IFwiVGhlIGNvbXBhbnkgaGFzIGJlZW4gcmVtb3ZlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7XG4gICAgICAgICAgICAgICAgY2FzZSBcImdlbnJlXCI6XG4gICAgICAgICAgICAgICAgICAgIGN1cnJlbnRJbmRleCA9IF8uZmluZEluZGV4KHZtLmdlbnJlcywge2F0dHJpYnV0ZXM6IHtpZDogTnVtYmVyKHZtLmN1cnJlbnRTZWxlY3RFbnRpdHlJZCgpKX19KTtcbiAgICAgICAgICAgICAgICAgICAgdm0uZ2VucmVzW2N1cnJlbnRJbmRleF0ucmVtb3ZlKClcbiAgICAgICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKHJlc3BvbnNlLnN0YXR1cyA9PT0gXCJzdWNjZXNzXCIpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29uc29sZS5sb2codm0uY3VycmVudFNlbGVjdEVudGl0eUlkKCkpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLnJlbW92ZSh2bS5nZW5yZXMsIHthdHRyaWJ1dGVzOiB7aWQ6IE51bWJlcih2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoKSl9fSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLnN1Y2Nlc3NNZXNzYWdlID0gXCJUaGUgZ2VucmUgaGFzIGJlZW4gcmVtb3ZlZFwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2VsZWN0RW50aXR5SWQoXCJcIik7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIH0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5yZXBvcnRJbnRlcm5hbEVycm9yKTtcbiAgICAgICAgICAgICAgICAgICAgYnJlYWs7ICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgIH07XG5cbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgdm0ubWVzc2FnZUVycm9yID0gXCJTZWxlY3QgYW4gaXRlbSBmcm9tIHRoZSBkcm9wZG93blwiO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGZhbHNlO1xuICAgICAgICB9O1xuICAgIH07XG5cbiAgICByZXR1cm4gdm07XG59O1xuXG5HYW1lVHJhY2tlckFkbWluLmNvbnRyb2xsZXIgPSBmdW5jdGlvbigpIHtcbiAgICBHYW1lVHJhY2tlckFkbWluLnZtLmluaXQoKTtcbn07XG4iLCIvL0ZvciB1c2Ugd2l0aCBhbGwgVmlld3MuIENvZGUgaXMgYmFzZWQgb24gdGhlIG9uZSBmb3VuZCBvbiBtaXRocmlsJ3Mgc2l0ZSBodHRwczovL2xob3JpZS5naXRodWIuaW8vbWl0aHJpbC9pbnRlZ3JhdGlvbi5odG1sXG52YXIgc2VsZWN0Mj0ge307XG5cbi8qIFRoaXMgZmFjdG9yeSBmdW5jdGlvbiBvZmZlcnMgYSBuaWNlIGNsb3N1cmUgZm9yIGFueXRoaW5nIGV4dHJhIHdlIHdhbnQgdG8gcGFzcyBpbiAqL1xuc2VsZWN0Mi5jb25maWcgPSBmdW5jdGlvbihleHRyYUFyZ3VtZW50cykge1xuICAgIHJldHVybiBmdW5jdGlvbihlbGVtZW50LCBpc0luaXRpYWxpemVkLCBjb250cm9sbGVyKSB7XG4gICAgICAgIHZhciBlbCA9ICQoZWxlbWVudCk7XG4gICAgICAgIGlmICghaXNJbml0aWFsaXplZCkge1xuICAgICAgICAgICAgaWYgKGV4dHJhQXJndW1lbnRzLnNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnMpIHtcbiAgICAgICAgICAgICAgICBlbC5zZWxlY3QyKGV4dHJhQXJndW1lbnRzLnNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnMpO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICBlbC5zZWxlY3QyKCk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICBlbC5jaGFuZ2UoZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICAgICAgbS5zdGFydENvbXB1dGF0aW9uKCk7XG4gICAgICAgICAgICAgICAgZXh0cmFBcmd1bWVudHMub25jaGFuZ2UoZWwuc2VsZWN0MihcInZhbFwiKSk7XG4gICAgICAgICAgICAgICAgbS5lbmRDb21wdXRhdGlvbigpO1xuICAgICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgICAgZWwuc2VsZWN0MihcInZhbFwiLCBleHRyYUFyZ3VtZW50cy52YWx1ZSk7XG4gICAgfTtcbn07XG5cbnNlbGVjdDIudmlldyA9IGZ1bmN0aW9uKGV4dHJhQXJndW1lbnRzLCBvcHRpb25TZXQsIGlzTXVsdGlwbGUpIHtcbiAgICB2YXIgc2VsZWN0b3IgPSAoaXNNdWx0aXBsZSkgPyBcInNlbGVjdC5mb3JtLWNvbnRyb2xbbXVsdGlwbGU9dHJ1ZV1cIiA6IFwic2VsZWN0LmZvcm0tY29udHJvbFwiO1xuICAgIHZhciBjcmVhdGVPcHRpb25TZXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgdmFyIG9wdGlvbnMgPSBbXTtcbiAgICAgICAgaWYgKG9wdGlvblNldCkge1xuICAgICAgICAgICAgb3B0aW9ucyA9IF8ubWFwKG9wdGlvblNldCwgZnVuY3Rpb24odmFsdWUpIHtcbiAgICAgICAgICAgICAgICB2YXIgcmV0dXJuVmFsdWUgPSAoXy5pc09iamVjdCh2YWx1ZSkpID8gbShcIm9wdGlvblwiLCB7dmFsdWU6IHZhbHVlLmlkfSwgdmFsdWUubmFtZSkgOiBtKFwib3B0aW9uXCIsIHZhbHVlKTtcbiAgICAgICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gb3B0aW9ucztcbiAgICB9O1xuICAgIHJldHVybiBtKHNlbGVjdG9yLCB7Y29uZmlnOnNlbGVjdDIuY29uZmlnKGV4dHJhQXJndW1lbnRzKX0sXG4gICAgICAgICAgICAgW20oXCJvcHRpb25cIiksY3JlYXRlT3B0aW9uU2V0KCldKTtcbn07XG4iLCJcblxuXG5cblxuXG5cblxuXG5cbm0ubW9kdWxlKGRvY3VtZW50LmJvZHksIEdhbWVUcmFja2VyQWRtaW4pO1xuIl0sInNvdXJjZVJvb3QiOiIvc291cmNlLyJ9