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

        this.delete = function() {
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
                                                  "/company/");

GameTrackerAdmin.System = GameTrackerAdmin.Model({ id: null,
                                                   name: "",
                                                   manufacturerid: null },
                                                 "/system/");

GameTrackerAdmin.Genre = GameTrackerAdmin.Model({ id: null,
                                                   name: "",
                                                   manufacturerid: null },
                                                 "/genre/");

GameTrackerAdmin.vm = new function() {
    var vm = {};
    vm.TrackerForm = function(fields) {
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
    vm.init = function() {
        vm.formMode = "search";
        vm.selectScreenState = "genre";
        
        //This is used as a stack;
        vm.screenHistory = ["GameFormScreen"];

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
        };

        vm.companyForm = new vm.TrackerForm({name: m.prop(""),
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
                        } else {
                            vm.errorMessage = "Could not add the company";
                        }
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the company";
            }
            vm.isLoading = false;
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
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the company";
            }
            vm.isLoading = false;
            return false;
        };
        
        vm.genreForm = new vm.TrackerForm({name: m.prop("")});
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
                        } else {
                            vm.errorMessage = "Could not add the genre";
                        }
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the genre";
            }
            vm.isLoading = false;
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
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter the name of the genre";
            };
            vm.isLoading = false;
            return false;
        };

        vm.systemForm = new vm.TrackerForm({name: m.prop(""),
                                            manufacturerid: m.prop("")});
        vm.systemForm.submitHandlers.add = function() {
            vm.isLoading = true;
            vm.clearMessages();
            if (!_.isEmpty(vm.systemForm.fields.name()) && !_.isEmpty(vm.systemForm.fields.manufacturerid())) {
                var newSystem = new GameTrackerAdmin.System(vm.systemForm.returnFields());
                newSystem.save()
                    .then(function(response) {
                        console.log(response);
                        if (response.status === "success") {
                            vm.systems.push(newSystem);
                            vm.successMessage = "The system has been added";
                            vm.systemForm.clearForm();
                            vm.isLoading = false;
                        } else {
                            vm.errorMessage = "bad crap";
                            vm.isLoading = false;
                        }
                    }, vm.reportInternalError);
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
                    });
            } else {
                vm.errorMessage = "Please fill in all the fields";
            }
            vm.isLoading = false;
            return false;
        };
        
        //The naming convention seems to have changed (not camel case) but this is because we wish
        //To mirror what we have in the table, mainly for back-end convenience
        //TODO have each form have a namespace for their thingies
        vm.gameForm = new vm.TrackerForm({name: m.prop(""),
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
            console.log(vm.gameForm.fields.systemid());
            if (!_.isEmpty(vm.gameForm.fields.name()) &&
                !_.isEmpty(vm.gameForm.fields.region()) &&
                _.isFinite(Number(vm.gameForm.fields.systemid())) &&
                Number(vm.gameForm.fields.systemid()) > 0 &&
                _.isFinite(Number(vm.gameForm.fields.quantity())) &&
                Number(vm.gameForm.fields.quantity()) > 0) {
                m.request({method: "POST",
                           url: "/games/",
                           data: vm.gameForm.returnFields()})
                    .then(function(response) {
                        console.log(response);
                        vm.gameForm.clearForm();
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please fill in all the fields";
            }
            vm.isLoading = false;
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
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please enter at least one search parameter";
            }
            vm.isLoading = false;
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
                           url: "/games/",
                           data: {id: Number(gameId)}
                          })
                    .then(function(response) {
                        vm.currentGameId = Number(response.id);
                        vm.gameForm.fields.companies(_.pluck(ensureArray(response.companies), "companyId"));
                        vm.gameForm.fields.genres(_.pluck(ensureArray(response.genres), "genreId"));
                        vm.gameForm.populateForm(_.omit(response, ["companies", "genres"]));
                        console.log(_.omit(response, ["companies", "genres"]));

                        vm.formMode = "update";
                        vm.searchResults = [];
                    }, vm.reportInternalError);
            }
            vm.searchIsLoading = false;
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
                           url: "/games/",
                           data: data})
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.successMessage = "Game successfully updated";
                        } 
                    }, vm.reportInternalError);
            } else {
                vm.errorMessage = "Please fill in the fields";
            }
            vm.isLoading = false;
            return false;
        };

        vm.deleteGame = function(gameId) {
            vm.searchIsLoading = true;
            if (gameId && _.isFinite(Number(gameId))) {
                m.request({method: "DELETE",
                            url: "/games/",
                            data: {id: Number(gameId)}})
                    .then(function(response) {
                        if (response.status === "success") {
                            vm.successMessage = "The game has been deleted";
                            _.remove(vm.searchResults, function(game) { return game.id === Number(gameId); });
                        }
                    }, vm.reportInternalError);
            }
            vm.searchIsLoading = false;
            return false;
        };

        vm.currentSelectEntityId = m.prop(null);
        vm.generalInitiateEdit = function() {
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
            };
            vm.currentSelectEntityId(null);
            return false;
        };
        vm.generalDelete = function() {
            vm.isLoading = true;
            var currentIndex;
            switch (vm.selectScreenState) {
            case "system":
                currentIndex = _.findIndex(vm.systems, {attributes: {id: Number(vm.currentSelectEntityId())}});
                vm.systems[currentIndex].delete()
                    .then(function(response) {
                        if (response.status === "success") {
                            _.remove(vm.systems, {attributes: {id: Number(vm.currentSelectEntityId())}});
                            vm.successMessage = "The system has been deleted";
                        }
                    },
                          vm.reportInternalError);
                break;
            case "company":
                currentIndex = _.findIndex(vm.companies, {attributes: {id: Number(vm.currentSelectEntityId())}});
                vm.companies[currentIndex].delete()
                    .then(function(response) {
                        if (response.status === "success") {
                            _.remove(vm.companies, {attributes: {id: Number(vm.currentSelectEntityId())}});
                            vm.successMessage = "The company has been deleted";
                        }
                    },
                          vm.reportInternalError);
                break;
            case "genre":
                currentIndex = _.findIndex(vm.genres, {attributes: {id: Number(vm.currentSelectEntityId())}});
                vm.genres[currentIndex].delete()
                    .then(function(response) {
                        if (response.status === "success") {
                            _.remove(vm.genres, {attributes: {id: Number(vm.currentSelectEntityId())}});
                            vm.successMessage = "The genre has been deleted";
                        }
                    },
                          vm.reportInternalError);
                break;                
            };
            vm.currentSelectEntityId(null);
            vm.isLoading =false;
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
    var placeHolder = function() {
        var placeholder = "";
        switch (GameTrackerAdmin.vm.selectScreenState) {
        case "system":
            placeholder = "Select A System";
            break;
        case "company":
            placeholder = "Select A Company";
            break;
        case "genre":
            placeholder = "Select A Genre";
            break;
        };
        return placeholder;
    };
    return m("div.row",[
        m("div.col-xs-12", [
            m("form", [
                m("div", [
                    select2.view({onchange:GameTrackerAdmin.vm.currentSelectEntityId,
                                  value:GameTrackerAdmin.vm.currentSelectEntityId(),
                                  select2InitializationOptions:{placeholder:placeHolder()}},
                                 selectDataSet())
                ]),
                m("div", [
                    m("button.btn.btn-success", {style: displayProperties.button,
                                                 onclick: GameTrackerAdmin.vm.generalInitiateEdit}, "edit"),
                    m("button.btn.btn-danger", {style: displayProperties.button,
                                                onclick: GameTrackerAdmin.vm.generalDelete}, "delete"),
                    m("button.btn.btn-default", {style: displayProperties.button,
                                                 onclick: GameTrackerAdmin.vm.returnToMainForm}, "back"),
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
    return [m("div.text-success", GameTrackerAdmin.vm.successMessage),
            m("div.text-danger", GameTrackerAdmin.vm.errorMessage),
            renderScreens()];
};

m.module(document.getElementById("form-insert"), GameTrackerAdmin);
