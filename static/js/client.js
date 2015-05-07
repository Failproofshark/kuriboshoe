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

var GameTrackerClient = {}

GameTrackerClient.Game = function(initialObject) {
    this.attributes = {
        name : initialObject.name,
        blurb : initialObject.blurb,
        region : initialObject.region,
        hasmanual : initialObject.hasmanual,
        hasbox : initialObject.hasbox,
        notes : initialObject.notes,
        quantity : initialObject.quantity,
        systemname : "",
        genres: [],
        companies: []
    };
    this.attributes.systemname = _.result(_.find(systems, {id: initialObject.systemid}), "name");
    
    var ensureArray = function(item) {
        var returnValue = _.isArray(item) ? item : [item];
        return returnValue;
    };
    /* Bit of a symbolic manipulation trick here ;). Given that the initialObject and global namespace
     * use the same name for genres and companies we simply pass a string, use eval to get the object
     * from global namespace and still use it to reference the attribute we want in the initialObject namespace.
     */
    var getRelatedNames = function(collectionName) {
        var singularName = (collectionName === "genres") ? "genreId" : "companyId";
        return _.pluck(_.filter(eval(collectionName), function(item) {
            return _.contains(_.pluck(ensureArray(initialObject[collectionName]), singularName), item.id);
        }),
                       "name");
    };
    
    this.attributes.genres = getRelatedNames("genres");
    this.attributes.companies = getRelatedNames("companies");

};

GameTrackerClient.vm = new function() {
    var vm = {};
    vm.init = function() {
        GameForm.controller.isLoading = false;
        GameForm.controller.isAdmin = false;

        vm.currentScreen = "SearchScreen";
        vm.shouldDisplayScreen = function(screenName) {
            var displayProperty = (screenName === vm.currentScreen) ? "inherit" : "none";
            return displayProperty;
        };

        //Like admin system, genres, and companies are bootstrapped data
        GameForm.controller.populateSelectDataSets(systems, genres, companies);
        
        GameForm.controller.cancelButtonHandler = function() {
            GameForm.controller.gameForm.clearForm();
            return false;
        };

        vm.currentGame = null;
        //TODO add Should link to gameform object (to determine if the thing should be a link
        GameForm.controller.titleClickHandler = function(gameId) {
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
                        if (response) {
                            vm.currentGame = new GameTrackerClient.Game(response);
                            vm.currentScreen = "InfoScreen";
                            GameForm.controller.searchLoading = false;
                            GameForm.controller.searchResults = [];
                            GameForm.controller.gameForm.clearForm();
                        };
                    }, vm.reportInternalError);
            }            
        };
    };

    vm.returnToSearch = function() {
        vm.currentScreen = "SearchScreen";
        vm.currentGame = null;
    };
    return vm;
};

GameTrackerClient.controller = function() {
    GameTrackerClient.vm.init();
};

GameTrackerClient.screenCollection = {};
GameTrackerClient.screenCollection.SearchScreen = function() {
    return [m("h1", "Game Search"),
            m("div", [
                m("span", "Fill in at least one search parameter below."),
                m("br"),
                m("em", "e.g. To see all Super Famicom games, select Super Famicom in the System select drop down and click submit")
            ]),
            GameForm.view()];
}
GameTrackerClient.screenCollection.InfoScreen = function() {
    var screen = [];
    if (!_.isNull(GameTrackerClient.vm.currentGame)) {
        var theGame = GameTrackerClient.vm.currentGame;
        var completionCondition = function() {
            var condition = "Game only";
            if (theGame.attributes.hasmanual && theGame.attributes.hasbox) {
                condition = "Complete In Box";
            } else if (theGame.attributes.hasmanual) {
                condition = "Game and Manual";
            } else if (theGame.attributes.hasbox) {
                condition = "Game and Box";
            }
            return condition;
        };
        screen = m("div.row", [
            m("div.col-xs-12", [
                m("h1", "Game Data"),
                m("h2", (theGame.attributes.name + " (" + theGame.attributes.systemname + ")")),
                m("div", [
                    m("strong", "Condition: "),
                    m("span", completionCondition()),
                ]),
                m("div", [
                    m("strong", "Region: "),
                    m("span", theGame.attributes.region)
                ]),
                m("div", [
                    m("strong", "Quantity: "),
                    m("span", theGame.attributes.quantity)
                ]),
                m("div", [
                    m("strong", "Genres: "),
                    m("span", theGame.attributes.genres.join(", "))
                ]),
                m("div", [
                    m("strong", "Companies: "),
                    m("span", theGame.attributes.companies.join(", "))
                ]),
                m("div", [
                    m("h3", "Blurb"),
                    m("blockquote", theGame.attributes.blurb)
                ]),
                m("div", [
                    m("h3", "Notes"),
                    m("blockquote", theGame.attributes.notes)
                ]),
                m("button.btn.btn-primary", {onclick: GameTrackerClient.vm.returnToSearch}, "Back")
            ])
        ]);
    };
    return screen;
};

GameTrackerClient.view = function() {
    var renderScreens = function() {
        return _.map(GameTrackerClient.screenCollection, function(screenContent, screenName) {
            return m("div", {style:"display:"+GameTrackerClient.vm.shouldDisplayScreen(screenName)}, screenContent());
        });
    };
    return renderScreens();
};

m.module(document.getElementById("container"), GameTrackerClient);

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNlbGVjdDJtaXRocmlsLmpzIiwiZ2FtZXRyYWNrZXJzaGFyZWQuanMiLCJnYW1lZm9ybS5qcyIsImdhbWVmb3Jtdmlldy5qcyIsImNsaWVudG1vZGVscy5qcyIsImNsaWVudGNvbnRyb2xsZXIuanMiLCJjbGllbnR2aWV3LmpzIiwiZ2FtZXRyYWNrZXJjbGllbnQuanMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FDdENBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ2xEQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUN0RkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUNwSUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3JDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQzVEQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3hFQTtBQUNBIiwiZmlsZSI6ImNsaWVudC5qcyIsInNvdXJjZXNDb250ZW50IjpbIi8vRm9yIHVzZSB3aXRoIGFsbCBWaWV3cy4gQ29kZSBpcyBiYXNlZCBvbiB0aGUgb25lIGZvdW5kIG9uIG1pdGhyaWwncyBzaXRlIGh0dHBzOi8vbGhvcmllLmdpdGh1Yi5pby9taXRocmlsL2ludGVncmF0aW9uLmh0bWxcbnZhciBzZWxlY3QyPSB7fTtcblxuLyogVGhpcyBmYWN0b3J5IGZ1bmN0aW9uIG9mZmVycyBhIG5pY2UgY2xvc3VyZSBmb3IgYW55dGhpbmcgZXh0cmEgd2Ugd2FudCB0byBwYXNzIGluICovXG5zZWxlY3QyLmNvbmZpZyA9IGZ1bmN0aW9uKGV4dHJhQXJndW1lbnRzKSB7XG4gICAgcmV0dXJuIGZ1bmN0aW9uKGVsZW1lbnQsIGlzSW5pdGlhbGl6ZWQsIGNvbnRyb2xsZXIpIHtcbiAgICAgICAgdmFyIGVsID0gJChlbGVtZW50KTtcbiAgICAgICAgaWYgKCFpc0luaXRpYWxpemVkKSB7XG4gICAgICAgICAgICBpZiAoZXh0cmFBcmd1bWVudHMuc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9ucykge1xuICAgICAgICAgICAgICAgIGVsLnNlbGVjdDIoZXh0cmFBcmd1bWVudHMuc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9ucyk7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIGVsLnNlbGVjdDIoKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIGVsLmNoYW5nZShmdW5jdGlvbigpIHtcbiAgICAgICAgICAgICAgICBtLnN0YXJ0Q29tcHV0YXRpb24oKTtcbiAgICAgICAgICAgICAgICBleHRyYUFyZ3VtZW50cy5vbmNoYW5nZShlbC5zZWxlY3QyKFwidmFsXCIpKTtcbiAgICAgICAgICAgICAgICBtLmVuZENvbXB1dGF0aW9uKCk7XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgICBlbC5zZWxlY3QyKFwidmFsXCIsIGV4dHJhQXJndW1lbnRzLnZhbHVlKTtcbiAgICB9O1xufTtcblxuc2VsZWN0Mi52aWV3ID0gZnVuY3Rpb24oZXh0cmFBcmd1bWVudHMsIG9wdGlvblNldCwgaXNNdWx0aXBsZSkge1xuICAgIHZhciBzZWxlY3RvciA9IChpc011bHRpcGxlKSA/IFwic2VsZWN0LmZvcm0tY29udHJvbFttdWx0aXBsZT10cnVlXVwiIDogXCJzZWxlY3QuZm9ybS1jb250cm9sXCI7XG4gICAgdmFyIGNyZWF0ZU9wdGlvblNldCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICB2YXIgb3B0aW9ucyA9IFtdO1xuICAgICAgICBpZiAob3B0aW9uU2V0KSB7XG4gICAgICAgICAgICBvcHRpb25zID0gXy5tYXAob3B0aW9uU2V0LCBmdW5jdGlvbih2YWx1ZSkge1xuICAgICAgICAgICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IChfLmlzT2JqZWN0KHZhbHVlKSkgPyBtKFwib3B0aW9uXCIsIHt2YWx1ZTogdmFsdWUuaWR9LCB2YWx1ZS5uYW1lKSA6IG0oXCJvcHRpb25cIiwgdmFsdWUpO1xuICAgICAgICAgICAgICAgIHJldHVybiByZXR1cm5WYWx1ZTtcbiAgICAgICAgICAgIH0pO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiBvcHRpb25zO1xuICAgIH07XG4gICAgcmV0dXJuIG0oc2VsZWN0b3IsIHtjb25maWc6c2VsZWN0Mi5jb25maWcoZXh0cmFBcmd1bWVudHMpfSxcbiAgICAgICAgICAgICBbbShcIm9wdGlvblwiKSxjcmVhdGVPcHRpb25TZXQoKV0pO1xufTtcbiIsInZhciBHYW1lVHJhY2tlclNoYXJlZCA9IHt9O1xuXG5HYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSA9IGZ1bmN0aW9uKGZpZWxkcykge1xuICAgIHRoaXMuZmllbGRzID0gZmllbGRzO1xuICAgIHRoaXMucG9wdWxhdGVGb3JtID0gZnVuY3Rpb24ob2JqZWN0KSB7XG4gICAgICAgIHZhciBzZWxmID0gdGhpcztcbiAgICAgICAgaWYgKG9iamVjdC5hdHRyaWJ1dGVzKSB7XG4gICAgICAgICAgICBfLm1hcChvYmplY3QuYXR0cmlidXRlcywgZnVuY3Rpb24oYXR0cmlidXRlVmFsdWUsIGF0dHJpYnV0ZUtleSkge1xuICAgICAgICAgICAgICAgIGlmIChhdHRyaWJ1dGVLZXkgIT09IFwiaWRcIikge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmZpZWxkc1thdHRyaWJ1dGVLZXldKGF0dHJpYnV0ZVZhbHVlKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIF8ubWFwKG9iamVjdCwgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgICAgIGlmIChrZXkgIT09IFwiaWRcIikge1xuICAgICAgICAgICAgICAgICAgICBzZWxmLmZpZWxkc1trZXldKHZhbHVlKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgIH07XG4gICAgdGhpcy5jbGVhckZvcm0gPSBfLmZvckVhY2guYmluZCh0aGlzLCB0aGlzLmZpZWxkcywgZnVuY3Rpb24oaW5wdXQpIHtcbiAgICAgICAgaWYgKF8uaXNTdHJpbmcoaW5wdXQoKSkpIHtcbiAgICAgICAgICAgIGlucHV0KFwiXCIpO1xuICAgICAgICB9IGVsc2UgaWYgKF8uaXNBcnJheShpbnB1dCgpKSkge1xuICAgICAgICAgICAgaW5wdXQoW10pO1xuICAgICAgICB9IGVsc2UgaWYgKF8uaXNCb29sZWFuKGlucHV0KCkpKXtcbiAgICAgICAgICAgIGlucHV0KGZhbHNlKTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIGlucHV0KG51bGwpO1xuICAgICAgICB9XG4gICAgfSk7XG4gICAgdGhpcy5yZXR1cm5GaWVsZHMgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgcmV0dXJuIF8ubWFwVmFsdWVzKHRoaXMuZmllbGRzLCBmdW5jdGlvbihmaWVsZCkge1xuICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gZmllbGQoKTtcbiAgICAgICAgICAgIGlmIChfLmlzQm9vbGVhbihyZXR1cm5WYWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9IE51bWJlcihyZXR1cm5WYWx1ZSk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgIH0pO1xuICAgIH07XG4gICAgdGhpcy5zdWJtaXRIYW5kbGVycyA9IHt9O1xuICAgIC8qIFRoaXMgd2lsbCBwcm9iYWJseSBiZSByZWZhY3RvcmVkIG91dCBpbiB0aGUgZnV0dXJlIGdpdmVuIHRoZSBvbmx5IHRoaW5nIHRoYXQgaGFzIGEgc2VhcmNoIGlzIHRoZSBnYW1lIGZvcm1cbiAgICAgKiBUbyBrZWVwIHRoaW5ncyBmcm9tIGNvbXBsYWluaW5nIGFib3V0IGEgbWlzc2luZyBrZXkgd2UgYWRkIGFuIGVtcHR5IGZ1bmN0aW9uIGhlcmVcbiAgICAgKi9cbiAgICB0aGlzLnN1Ym1pdEhhbmRsZXJzLnNlYXJjaCA9IGZ1bmN0aW9uKCkgeyAvKmVtcHR5Ki8gfTtcbiAgICB0aGlzLmdldFN1Ym1pdEhhbmRsZXIgPSBmdW5jdGlvbihzdGF0ZSkge1xuICAgICAgICByZXR1cm4gdGhpcy5zdWJtaXRIYW5kbGVyc1tzdGF0ZV07XG4gICAgfTtcblxufTtcbiIsInZhciBHYW1lRm9ybSA9IHt9O1xuXG5HYW1lRm9ybS5jb250cm9sbGVyID0gbmV3IGZ1bmN0aW9uKCkge1xuICAgIHRoaXMuZ2FtZUZvcm0gPSBuZXcgR2FtZVRyYWNrZXJTaGFyZWQuVHJhY2tlckZvcm0oe25hbWU6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBibHVyYjogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlZ2lvbjogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhhc21hbnVhbDogbS5wcm9wKDApLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhhc2JveDogbS5wcm9wKDApLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG5vdGVzOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcXVhbnRpdHk6IG0ucHJvcChcIlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBnZW5yZXM6IG0ucHJvcChbXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgY29tcGFuaWVzOiBtLnByb3AoW10pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN5c3RlbWlkOiBtLnByb3AoXCJcIil9KTtcblxuICAgIHRoaXMuZXJyb3JNZXNzYWdlID0gXCJcIjtcblxuICAgIHRoaXMuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgdGhpcy5zZWFyY2hSZXN1bHRzID0gW107XG5cbiAgICB0aGlzLm5vUmVzdWx0cyA9IFwiXCI7XG4gICAgdGhpcy5mb3JtTW9kZSA9IFwic2VhcmNoXCI7XG4gICAgdGhpcy5pc0FkbWluID0gZmFsc2U7XG5cbiAgICB0aGlzLnNlYXJjaExvYWRpbmcgPSBmYWxzZTtcblxuICAgIHRoaXMuc2VsZWN0VXBkYXRlSGFuZGxlcjtcbiAgICB0aGlzLnNlbGVjdERlbGV0ZUhhbmRsZXI7XG4gICAgdGhpcy5jYW5jZWxCdXR0b25IYW5kbGVyO1xuXG4gICAgdGhpcy5zeXN0ZW1zO1xuICAgIHRoaXMuZ2VucmVzO1xuICAgIHRoaXMuY29tcGFuaWVzO1xuXG4gICAgdGhpcy5hZGRDb21wYW55SGFuZGxlciA9IGZ1bmN0aW9uKCkge307XG4gICAgdGhpcy5hZGRHZW5yZUhhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuICAgIHRoaXMuYWRkU3lzdGVtSGFuZGxlciA9IGZ1bmN0aW9uKCkge307XG5cbiAgICB0aGlzLnBvcHVsYXRlU2VsZWN0RGF0YVNldHMgPSBmdW5jdGlvbihzeXN0ZW1zLCBnZW5yZXMsIGNvbXBhbmllcykge1xuICAgICAgICB0aGlzLnN5c3RlbXMgPSBzeXN0ZW1zO1xuICAgICAgICB0aGlzLmdlbnJlcyA9IGdlbnJlcztcbiAgICAgICAgdGhpcy5jb21wYW5pZXMgPSBjb21wYW5pZXM7XG4gICAgfTtcblxuICAgIHRoaXMuYWRtaW5SZXN1bHRCdXR0b25IYW5kbGVycyA9IGZ1bmN0aW9uKGVkaXRoYW5kbGVyLCBkZWxldGVoYW5kbGVyKSAge1xuICAgICAgICB0aGlzLnNlbGVjdFVwZGF0ZUhhbmRsZXIgPSBlZGl0aGFuZGxlcjtcbiAgICAgICAgdGhpcy5zZWxlY3REZWxldGVIYW5kbGVyID0gZGVsZXRlaGFuZGxlcjtcbiAgICB9O1xuXG4gICAgdGhpcy5iaW5kU3VibWl0Rm9ybUhhbmRsZXIgPSBmdW5jdGlvbihzdGF0ZSwgaGFuZGxlcikge1xuICAgICAgICB0aGlzLmdhbWVGb3JtLnN1Ym1pdEhhbmRsZXJzW3N0YXRlXSA9IGhhbmRsZXI7XG4gICAgfTtcblxuICAgIHRoaXMudGl0bGVDbGlja0hhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuXG4gICAgdGhpcy5nYW1lRm9ybS5zdWJtaXRIYW5kbGVycy5zZWFyY2ggPSBmdW5jdGlvbigpIHtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5ub1Jlc3VsdHMgPSBcIlwiO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IHRydWU7XG4gICAgICAgIHZhciBjb21wbGV0ZWRTZXQgPSBfLm9taXQoR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5yZXR1cm5GaWVsZHMoKSwgZnVuY3Rpb24odmFsdWUsIGtleSkge1xuICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChfLmlzQm9vbGVhbih2YWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9ICF2YWx1ZTtcbiAgICAgICAgICAgIH0gZWxzZSBpZiAoIV8uaXNFbXB0eSh2YWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9IGZhbHNlO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgICAgICB9KTtcbiAgICAgICAgaWYgKCFfLmlzRW1wdHkoY29tcGxldGVkU2V0KSkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5lcnJvck1lc3NhZ2UgPSBcIlwiO1xuICAgICAgICAgICAgbS5yZXF1ZXN0KHttZXRob2Q6XCJwb3N0XCIsXG4gICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvc2VhcmNoLWdhbWVzLWFqYXgvXCIsXG4gICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IGNvbXBsZXRlZFNldH0pXG4gICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgLy9FbXB0eSByZXN1bHRzIHNldCByZXR1cm5zIGEgc2luZ2xlIGl0ZW0gYXJyYXkgd2l0aCBudWxsIGJlaW5nIHRoYXQgb2JqZWN0XG4gICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cyA9IF8ucmVtb3ZlKHJlc3BvbnNlLnJlc3VsdHMsIGZ1bmN0aW9uKGl0ZW0pIHsgcmV0dXJuICFfLmlzTnVsbChpdGVtKTsgfSk7XG4gICAgICAgICAgICAgICAgICAgIGlmIChHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMubGVuZ3RoIDwgMSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5ub1Jlc3VsdHMgPSBcIk5vIG1hdGNoZXMgd2VyZSBmb3VuZFwiO1xuICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgfSwgZnVuY3Rpb24oKSB7IEdhbWVGb3JtLmNvbnRyb2xsZXIuZXJyb3JNZXNzYWdlID0gXCJJbnRlcm5hbCBTZXJ2ZXIgRXJyb3JcIjt9ICk7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmVycm9yTWVzc2FnZSA9IFwiUGxlYXNlIGVudGVyIGF0IGxlYXN0IG9uZSBzZWFyY2ggcGFyYW1ldGVyXCI7XG4gICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiBmYWxzZTtcbiAgICB9O1xufTtcbiIsIkdhbWVGb3JtLnZpZXcgPSBmdW5jdGlvbigpIHtcbiAgICB2YXIgZm9ybUNvbmZpZ3VyYXRpb24gPSB7XG4gICAgICAgIHRleHRBcmVhRGlzcGxheTogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVZhbHVlID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuZm9ybU1vZGUgPT09IFwic2VhcmNoXCIpID8gXCJkaXNwbGF5Om5vbmVcIiA6IFwiZGlzcGxheTppbmhlcml0XCI7XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVZhbHVlO1xuICAgICAgICB9LFxuICAgICAgICBhY3Rpb25CdXR0b25EaXNwbGF5OiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBkaXNwbGF5VmFsdWUgPSAoR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcpID8gXCJkaXNwbGF5Om5vbmVcIiA6IFwiZGlzcGxheTppbmxpbmVcIjtcbiAgICAgICAgICAgIHJldHVybiBkaXNwbGF5VmFsdWU7XG4gICAgICAgIH0sXG4gICAgICAgIHByZWxvYWRlckRpc3BsYXk6IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlWYWx1ZSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZykgPyBcImRpc3BsYXk6aW5oZXJpdFwiIDogXCJkaXNwbGF5Om5vbmVcIjtcbiAgICAgICAgICAgIHJldHVybiBkaXNwbGF5VmFsdWU7XG4gICAgICAgIH0sXG4gICAgICAgIGNoYW5nZUZvcm1Qcm9wZXJ0aWVzOiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBzdHlsZVByb3BlcnRpZXMgPSBcImN1cnNvcjpwb2ludGVyO1wiO1xuICAgICAgICAgICAgaWYgKEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNBZG1pbikge1xuICAgICAgICAgICAgICAgIHN0eWxlUHJvcGVydGllcyArPSBcImRpc3BsYXk6aW5oZXJpdFwiO1xuICAgICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgICAgICBzdHlsZVByb3BlcnRpZXMgKz0gXCJkaXNwbGF5Om5vbmVcIjtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiBzdHlsZVByb3BlcnRpZXM7XG4gICAgICAgIH1cbiAgICB9O1xuICAgIHZhciByZW5kZXJTZWFyY2hSZXN1bHRzID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHZhciByZW5kZXJlZFJlc3VsdHMgPSBbXTtcbiAgICAgICAgdmFyIGRpc3BsYXlQcm9wZXJ0aWVzID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoTG9hZGluZykgPyB7cmVzdWx0czogXCJkaXNwbGF5Om5vbmVcIiwgcHJlbG9hZGVyOiBcImRpc3BsYXk6aW5oZXJpdFwifSA6IHtyZXN1bHRzOiBcImRpc3BsYXk6aW5oZXJpdFwiLCBwcmVsb2FkZXI6IFwiZGlzcGxheTpub25lXCJ9O1xuICAgICAgICB2YXIgdGl0bGVDdXJzb3JQcm9wZXJ0eSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmlzQWRtaW4pID8gXCJjdXJzb3I6ZGVmYXVsdFwiIDogXCJjdXJzb3I6cG9pbnRlclwiO1xuICAgICAgICB2YXIgcmVuZGVyQWRtaW5CdXR0b25zID0gZnVuY3Rpb24ocmVzdWx0KSB7XG4gICAgICAgICAgICB2YXIgYWRtaW5CdXR0b25zID0gW107XG4gICAgICAgICAgICBpZiAoR2FtZUZvcm0uY29udHJvbGxlci5pc0FkbWluKSB7XG4gICAgICAgICAgICAgICAgYWRtaW5CdXR0b25zID0gbShcImRpdi5jb2wteHMtM1wiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmdseXBoaWNvbi5nbHlwaGljb24tcmVtb3ZlLmdhbWUtc2VhcmNoLXJlc3VsdHMtYnV0dG9uXCIsIHtvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0RGVsZXRlSGFuZGxlci5iaW5kKEdhbWVGb3JtLmNvbnRyb2xsZXIsIHJlc3VsdC5pZCl9KSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW4uZ2x5cGhpY29uLmdseXBoaWNvbi1wZW5jaWwuZ2FtZS1zZWFyY2gtcmVzdWx0cy1idXR0b25cIiwge29uY2xpY2s6R2FtZUZvcm0uY29udHJvbGxlci5zZWxlY3RVcGRhdGVIYW5kbGVyLmJpbmQoR2FtZUZvcm0uY29udHJvbGxlciwgcmVzdWx0LmlkKX0pXG4gICAgICAgICAgICAgICAgXSk7XG4gICAgICAgICAgICB9O1xuICAgICAgICAgICAgcmV0dXJuIGFkbWluQnV0dG9ucztcbiAgICAgICAgfTtcbiAgICAgICAgaWYgKCFfLmlzRW1wdHkoR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzKSB8fCAhXy5pc0VtcHR5KEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSkge1xuICAgICAgICAgICAgcmVuZGVyZWRSZXN1bHRzID0gbShcImRpdlwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7c3R5bGU6ZGlzcGxheVByb3BlcnRpZXMucmVzdWx0c30sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFttKFwiZGl2XCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF8ubWFwKEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cywgZnVuY3Rpb24ocmVzdWx0LCBpbmRleCkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhciBiZ0NvbG9yID0gXCJiYWNrZ3JvdW5kLWNvbG9yOiNDRUNGRTBcIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpZiAoaW5kZXggJSAyID09IDApIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmdDb2xvciA9IFwiYmFja2dyb3VuZC1jb2xvcjojRkZGXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJldHVybiBtKFwiZGl2LnJvdy5yZXN1bHQtcm93XCIsIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbbShcImRpdi5jb2wteHMtOVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHtzdHlsZTpiZ0NvbG9yfSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIHtzdHlsZTp0aXRsZUN1cnNvclByb3BlcnR5LCBvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIudGl0bGVDbGlja0hhbmRsZXIuYmluZChHYW1lRm9ybS5jb250cm9sbGVyLCByZXN1bHQuaWQpfSwgKHJlc3VsdC5uYW1lICsgXCIgW1wiICsgcmVzdWx0LnJlZ2lvbiArIFwiXSAoXCIgKyByZXN1bHQuc3lzdGVtTmFtZSArIFwiKVwiKSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlbmRlckFkbWluQnV0dG9ucyhyZXN1bHQpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTpkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gcmVuZGVyZWRSZXN1bHRzO1xuICAgIH07XG4gICAgcmV0dXJuIFttKFwiZGl2LnJvd1wiLFtcbiAgICAgICAgbShcImRpdi5jb2wteHMtMTJcIixbXG4gICAgICAgICAgICBtKFwiZGl2LnRleHQtZGFuZ2VyXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZXJyb3JNZXNzYWdlKSxcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFtcbiAgICAgICAgICAgICAgICBtKFwiaW5wdXQuZm9ybS1jb250cm9sXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLm5hbWUpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubmFtZSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJOYW1lXCJ9KSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJSZWdpb25cIiwgYWxsb3dDbGVhcjogdHJ1ZX19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXCJOVFNDXCIsIFwiTlRTQy1KXCIsIFwiUEFMXCJdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHtvbmNoYW5nZTpHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiU3lzdGVtXCIsIGFsbG93Q2xlYXI6IHRydWV9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc3lzdGVtcyksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ1XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi5jaGFuZ2VGb3JtUHJvcGVydGllcygpLCBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmFkZFN5c3RlbUhhbmRsZXJ9LCBcIitBZGQgU3lzdGVtXCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuZ2VucmVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiR2VucmVzXCIsIGFsbG93Q2xlYXI6IHRydWV9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2VucmVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdHJ1ZSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ1XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi5jaGFuZ2VGb3JtUHJvcGVydGllcygpLCBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmFkZEdlbnJlSGFuZGxlcn0sIFwiK0FkZCBHZW5yZVwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuY29tcGFuaWVzKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9uczoge3BsYWNlaG9sZGVyOiBcIkNvbXBhbmllc1wiLCBhbGxvd0NsZWFyOiB0cnVlfX0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRydWUpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidVwiLCB7c3R5bGU6Zm9ybUNvbmZpZ3VyYXRpb24uY2hhbmdlRm9ybVByb3BlcnRpZXMoKSwgb25jbGljazogR2FtZUZvcm0uY29udHJvbGxlci5hZGRDb21wYW55SGFuZGxlcn0sIFwiK0FkZCBDb21wYW55XCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImlucHV0LmZvcm0tY29udHJvbFwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJRdWFudGl0eVwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCB7c3R5bGU6Zm9ybUNvbmZpZ3VyYXRpb24udGV4dEFyZWFEaXNwbGF5KCl9LCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJwXCIsIFwiU2hvcnQgRGVzY3JpcHRpb25cIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ0ZXh0YXJlYVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ibHVyYil9LCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ibHVyYigpKSxcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2LmNoZWNrYm94XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImxhYmVsXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbnB1dFt0eXBlPWNoZWNrYm94XVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJjaGVja2VkXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc21hbnVhbCksIGNoZWNrZWQ6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc21hbnVhbCgpfSlcbiAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiTWFudWFsXCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJsYWJlbFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwiaW5wdXRbdHlwZT1jaGVja2JveF1cIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwiY2hlY2tlZFwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNib3gpLCBjaGVja2VkOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNib3goKX0pXG4gICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCBcIkJveFwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwge3N0eWxlOmZvcm1Db25maWd1cmF0aW9uLnRleHRBcmVhRGlzcGxheSgpfSwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwicFwiLCBcIk5vdGVzXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidGV4dGFyZWFcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubm90ZXMpfSwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubm90ZXMoKSksXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1zdWNjZXNzXCIsIHtzdHlsZTogZm9ybUNvbmZpZ3VyYXRpb24uYWN0aW9uQnV0dG9uRGlzcGxheSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uc3VibWl0SGFuZGxlcnNbR2FtZUZvcm0uY29udHJvbGxlci5mb3JtTW9kZV19LCBcInN1Ym1pdFwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLWRhbmdlclwiLCB7c3R5bGU6IGZvcm1Db25maWd1cmF0aW9uLmFjdGlvbkJ1dHRvbkRpc3BsYXkoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuY2FuY2VsQnV0dG9uSGFuZGxlcn0sIFwiY2FuY2VsXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwiaW1nW3NyYz0vaW1hZ2VzL2FqYXguZ2lmXVwiLCB7c3R5bGU6IGZvcm1Db25maWd1cmF0aW9uLnByZWxvYWRlckRpc3BsYXkoKX0pXG4gICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgIF0pLFxuICAgICAgICBdKVxuICAgIF0pLFxuICAgICAgICAgICAgcmVuZGVyU2VhcmNoUmVzdWx0cygpXG4gICAgICAgICAgIF07XG59O1xuIiwidmFyIEdhbWVUcmFja2VyQ2xpZW50ID0ge31cblxuR2FtZVRyYWNrZXJDbGllbnQuR2FtZSA9IGZ1bmN0aW9uKGluaXRpYWxPYmplY3QpIHtcbiAgICB0aGlzLmF0dHJpYnV0ZXMgPSB7XG4gICAgICAgIG5hbWUgOiBpbml0aWFsT2JqZWN0Lm5hbWUsXG4gICAgICAgIGJsdXJiIDogaW5pdGlhbE9iamVjdC5ibHVyYixcbiAgICAgICAgcmVnaW9uIDogaW5pdGlhbE9iamVjdC5yZWdpb24sXG4gICAgICAgIGhhc21hbnVhbCA6IGluaXRpYWxPYmplY3QuaGFzbWFudWFsLFxuICAgICAgICBoYXNib3ggOiBpbml0aWFsT2JqZWN0Lmhhc2JveCxcbiAgICAgICAgbm90ZXMgOiBpbml0aWFsT2JqZWN0Lm5vdGVzLFxuICAgICAgICBxdWFudGl0eSA6IGluaXRpYWxPYmplY3QucXVhbnRpdHksXG4gICAgICAgIHN5c3RlbW5hbWUgOiBcIlwiLFxuICAgICAgICBnZW5yZXM6IFtdLFxuICAgICAgICBjb21wYW5pZXM6IFtdXG4gICAgfTtcbiAgICB0aGlzLmF0dHJpYnV0ZXMuc3lzdGVtbmFtZSA9IF8ucmVzdWx0KF8uZmluZChzeXN0ZW1zLCB7aWQ6IGluaXRpYWxPYmplY3Quc3lzdGVtaWR9KSwgXCJuYW1lXCIpO1xuICAgIFxuICAgIHZhciBlbnN1cmVBcnJheSA9IGZ1bmN0aW9uKGl0ZW0pIHtcbiAgICAgICAgdmFyIHJldHVyblZhbHVlID0gXy5pc0FycmF5KGl0ZW0pID8gaXRlbSA6IFtpdGVtXTtcbiAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgIH07XG4gICAgLyogQml0IG9mIGEgc3ltYm9saWMgbWFuaXB1bGF0aW9uIHRyaWNrIGhlcmUgOykuIEdpdmVuIHRoYXQgdGhlIGluaXRpYWxPYmplY3QgYW5kIGdsb2JhbCBuYW1lc3BhY2VcbiAgICAgKiB1c2UgdGhlIHNhbWUgbmFtZSBmb3IgZ2VucmVzIGFuZCBjb21wYW5pZXMgd2Ugc2ltcGx5IHBhc3MgYSBzdHJpbmcsIHVzZSBldmFsIHRvIGdldCB0aGUgb2JqZWN0XG4gICAgICogZnJvbSBnbG9iYWwgbmFtZXNwYWNlIGFuZCBzdGlsbCB1c2UgaXQgdG8gcmVmZXJlbmNlIHRoZSBhdHRyaWJ1dGUgd2Ugd2FudCBpbiB0aGUgaW5pdGlhbE9iamVjdCBuYW1lc3BhY2UuXG4gICAgICovXG4gICAgdmFyIGdldFJlbGF0ZWROYW1lcyA9IGZ1bmN0aW9uKGNvbGxlY3Rpb25OYW1lKSB7XG4gICAgICAgIHZhciBzaW5ndWxhck5hbWUgPSAoY29sbGVjdGlvbk5hbWUgPT09IFwiZ2VucmVzXCIpID8gXCJnZW5yZUlkXCIgOiBcImNvbXBhbnlJZFwiO1xuICAgICAgICByZXR1cm4gXy5wbHVjayhfLmZpbHRlcihldmFsKGNvbGxlY3Rpb25OYW1lKSwgZnVuY3Rpb24oaXRlbSkge1xuICAgICAgICAgICAgcmV0dXJuIF8uY29udGFpbnMoXy5wbHVjayhlbnN1cmVBcnJheShpbml0aWFsT2JqZWN0W2NvbGxlY3Rpb25OYW1lXSksIHNpbmd1bGFyTmFtZSksIGl0ZW0uaWQpO1xuICAgICAgICB9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgXCJuYW1lXCIpO1xuICAgIH07XG4gICAgXG4gICAgdGhpcy5hdHRyaWJ1dGVzLmdlbnJlcyA9IGdldFJlbGF0ZWROYW1lcyhcImdlbnJlc1wiKTtcbiAgICB0aGlzLmF0dHJpYnV0ZXMuY29tcGFuaWVzID0gZ2V0UmVsYXRlZE5hbWVzKFwiY29tcGFuaWVzXCIpO1xuXG59O1xuIiwiR2FtZVRyYWNrZXJDbGllbnQudm0gPSBuZXcgZnVuY3Rpb24oKSB7XG4gICAgdmFyIHZtID0ge307XG4gICAgdm0uaW5pdCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzQWRtaW4gPSBmYWxzZTtcblxuICAgICAgICB2bS5jdXJyZW50U2NyZWVuID0gXCJTZWFyY2hTY3JlZW5cIjtcbiAgICAgICAgdm0uc2hvdWxkRGlzcGxheVNjcmVlbiA9IGZ1bmN0aW9uKHNjcmVlbk5hbWUpIHtcbiAgICAgICAgICAgIHZhciBkaXNwbGF5UHJvcGVydHkgPSAoc2NyZWVuTmFtZSA9PT0gdm0uY3VycmVudFNjcmVlbikgPyBcImluaGVyaXRcIiA6IFwibm9uZVwiO1xuICAgICAgICAgICAgcmV0dXJuIGRpc3BsYXlQcm9wZXJ0eTtcbiAgICAgICAgfTtcblxuICAgICAgICAvL0xpa2UgYWRtaW4gc3lzdGVtLCBnZW5yZXMsIGFuZCBjb21wYW5pZXMgYXJlIGJvb3RzdHJhcHBlZCBkYXRhXG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIucG9wdWxhdGVTZWxlY3REYXRhU2V0cyhzeXN0ZW1zLCBnZW5yZXMsIGNvbXBhbmllcyk7XG4gICAgICAgIFxuICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNhbmNlbEJ1dHRvbkhhbmRsZXIgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uY2xlYXJGb3JtKCk7XG4gICAgICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgICAgIH07XG5cbiAgICAgICAgdm0uY3VycmVudEdhbWUgPSBudWxsO1xuICAgICAgICAvL1RPRE8gYWRkIFNob3VsZCBsaW5rIHRvIGdhbWVmb3JtIG9iamVjdCAodG8gZGV0ZXJtaW5lIGlmIHRoZSB0aGluZyBzaG91bGQgYmUgYSBsaW5rXG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIudGl0bGVDbGlja0hhbmRsZXIgPSBmdW5jdGlvbihnYW1lSWQpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoTG9hZGluZyA9IHRydWU7XG4gICAgICAgICAgICBpZiAoZ2FtZUlkICYmIF8uaXNGaW5pdGUoTnVtYmVyKGdhbWVJZCkpKSB7XG4gICAgICAgICAgICAgICAgLyogQSBrbm93biBsaW1pdGF0aW9uIHdpdGggdGhlIGJhY2tlbmQ6IHRoaW5ncyB3ZSBleHBlY3QgdG8gYmUgYW4gYXJyYXkgbWF5IGJlIGEgc2ltcGxlIG9iamVjdCBkdWUgdG8gdGhlIGpzb24gZW5jb2RlciBvbiB0aGUgYmFja2VuZFxuICAgICAgICAgICAgICAgICBub3QgYmVpbmcgYWJsZSB0byBlbmNvZGUgc2luZ2xlIHJvdyByZXN1bHRzIGNvcnJlY3RseVxuICAgICAgICAgICAgICAgICAqL1xuICAgICAgICAgICAgICAgIHZhciBlbnN1cmVBcnJheSA9IGZ1bmN0aW9uKGl0ZW0pIHtcbiAgICAgICAgICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gXy5pc0FycmF5KGl0ZW0pID8gaXRlbSA6IFtpdGVtXTtcbiAgICAgICAgICAgICAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgICAgICAgICAgICAgIH07XG4gICAgICAgICAgICAgICAgLy9XZSBjb3VsZCBqdXN0IHVzZSB0aGUgZGF0YSB3ZSByZXRyaWV2ZWQgZnJvbSB0aGUgc2VhcmNoIGJ1dCBsZXQncyBndWFyYW50ZWUgdGhlIHVzZXIgd2l0aCB0aGUgbW9zdCByZWNlbnQgaW5mb3JtYXRpb25cbiAgICAgICAgICAgICAgICBtLnJlcXVlc3Qoe21ldGhvZDogXCJHRVRcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIHVybDogXCIvZ2FtZS9cIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIGRhdGE6IHtpZDogTnVtYmVyKGdhbWVJZCl9XG4gICAgICAgICAgICAgICAgICAgICAgICAgIH0pXG4gICAgICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICBpZiAocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50R2FtZSA9IG5ldyBHYW1lVHJhY2tlckNsaWVudC5HYW1lKHJlc3BvbnNlKTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB2bS5jdXJyZW50U2NyZWVuID0gXCJJbmZvU2NyZWVuXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzID0gW107XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH07XG4gICAgICAgICAgICAgICAgICAgIH0sIHZtLnJlcG9ydEludGVybmFsRXJyb3IpO1xuICAgICAgICAgICAgfSAgICAgICAgICAgIFxuICAgICAgICB9O1xuICAgIH07XG5cbiAgICB2bS5yZXR1cm5Ub1NlYXJjaCA9IGZ1bmN0aW9uKCkge1xuICAgICAgICB2bS5jdXJyZW50U2NyZWVuID0gXCJTZWFyY2hTY3JlZW5cIjtcbiAgICAgICAgdm0uY3VycmVudEdhbWUgPSBudWxsO1xuICAgIH07XG4gICAgcmV0dXJuIHZtO1xufTtcblxuR2FtZVRyYWNrZXJDbGllbnQuY29udHJvbGxlciA9IGZ1bmN0aW9uKCkge1xuICAgIEdhbWVUcmFja2VyQ2xpZW50LnZtLmluaXQoKTtcbn07XG4iLCJHYW1lVHJhY2tlckNsaWVudC5zY3JlZW5Db2xsZWN0aW9uID0ge307XG5HYW1lVHJhY2tlckNsaWVudC5zY3JlZW5Db2xsZWN0aW9uLlNlYXJjaFNjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBbbShcImgxXCIsIFwiR2FtZSBTZWFyY2hcIiksXG4gICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICBtKFwic3BhblwiLCBcIkZpbGwgaW4gYXQgbGVhc3Qgb25lIHNlYXJjaCBwYXJhbWV0ZXIgYmVsb3cuXCIpLFxuICAgICAgICAgICAgICAgIG0oXCJiclwiKSxcbiAgICAgICAgICAgICAgICBtKFwiZW1cIiwgXCJlLmcuIFRvIHNlZSBhbGwgU3VwZXIgRmFtaWNvbSBnYW1lcywgc2VsZWN0IFN1cGVyIEZhbWljb20gaW4gdGhlIFN5c3RlbSBzZWxlY3QgZHJvcCBkb3duIGFuZCBjbGljayBzdWJtaXRcIilcbiAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgR2FtZUZvcm0udmlldygpXTtcbn1cbkdhbWVUcmFja2VyQ2xpZW50LnNjcmVlbkNvbGxlY3Rpb24uSW5mb1NjcmVlbiA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciBzY3JlZW4gPSBbXTtcbiAgICBpZiAoIV8uaXNOdWxsKEdhbWVUcmFja2VyQ2xpZW50LnZtLmN1cnJlbnRHYW1lKSkge1xuICAgICAgICB2YXIgdGhlR2FtZSA9IEdhbWVUcmFja2VyQ2xpZW50LnZtLmN1cnJlbnRHYW1lO1xuICAgICAgICB2YXIgY29tcGxldGlvbkNvbmRpdGlvbiA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIGNvbmRpdGlvbiA9IFwiR2FtZSBvbmx5XCI7XG4gICAgICAgICAgICBpZiAodGhlR2FtZS5hdHRyaWJ1dGVzLmhhc21hbnVhbCAmJiB0aGVHYW1lLmF0dHJpYnV0ZXMuaGFzYm94KSB7XG4gICAgICAgICAgICAgICAgY29uZGl0aW9uID0gXCJDb21wbGV0ZSBJbiBCb3hcIjtcbiAgICAgICAgICAgIH0gZWxzZSBpZiAodGhlR2FtZS5hdHRyaWJ1dGVzLmhhc21hbnVhbCkge1xuICAgICAgICAgICAgICAgIGNvbmRpdGlvbiA9IFwiR2FtZSBhbmQgTWFudWFsXCI7XG4gICAgICAgICAgICB9IGVsc2UgaWYgKHRoZUdhbWUuYXR0cmlidXRlcy5oYXNib3gpIHtcbiAgICAgICAgICAgICAgICBjb25kaXRpb24gPSBcIkdhbWUgYW5kIEJveFwiO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgcmV0dXJuIGNvbmRpdGlvbjtcbiAgICAgICAgfTtcbiAgICAgICAgc2NyZWVuID0gbShcImRpdi5yb3dcIiwgW1xuICAgICAgICAgICAgbShcImRpdi5jb2wteHMtMTJcIiwgW1xuICAgICAgICAgICAgICAgIG0oXCJoMVwiLCBcIkdhbWUgRGF0YVwiKSxcbiAgICAgICAgICAgICAgICBtKFwiaDJcIiwgKHRoZUdhbWUuYXR0cmlidXRlcy5uYW1lICsgXCIgKFwiICsgdGhlR2FtZS5hdHRyaWJ1dGVzLnN5c3RlbW5hbWUgKyBcIilcIikpLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwic3Ryb25nXCIsIFwiQ29uZGl0aW9uOiBcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIGNvbXBsZXRpb25Db25kaXRpb24oKSksXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzdHJvbmdcIiwgXCJSZWdpb246IFwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW5cIiwgdGhlR2FtZS5hdHRyaWJ1dGVzLnJlZ2lvbilcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcInN0cm9uZ1wiLCBcIlF1YW50aXR5OiBcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIHRoZUdhbWUuYXR0cmlidXRlcy5xdWFudGl0eSlcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcInN0cm9uZ1wiLCBcIkdlbnJlczogXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCB0aGVHYW1lLmF0dHJpYnV0ZXMuZ2VucmVzLmpvaW4oXCIsIFwiKSlcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcInN0cm9uZ1wiLCBcIkNvbXBhbmllczogXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCB0aGVHYW1lLmF0dHJpYnV0ZXMuY29tcGFuaWVzLmpvaW4oXCIsIFwiKSlcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImgzXCIsIFwiQmx1cmJcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJibG9ja3F1b3RlXCIsIHRoZUdhbWUuYXR0cmlidXRlcy5ibHVyYilcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImgzXCIsIFwiTm90ZXNcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJibG9ja3F1b3RlXCIsIHRoZUdhbWUuYXR0cmlidXRlcy5ub3RlcylcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiYnV0dG9uLmJ0bi5idG4tcHJpbWFyeVwiLCB7b25jbGljazogR2FtZVRyYWNrZXJDbGllbnQudm0ucmV0dXJuVG9TZWFyY2h9LCBcIkJhY2tcIilcbiAgICAgICAgICAgIF0pXG4gICAgICAgIF0pO1xuICAgIH07XG4gICAgcmV0dXJuIHNjcmVlbjtcbn07XG5cbkdhbWVUcmFja2VyQ2xpZW50LnZpZXcgPSBmdW5jdGlvbigpIHtcbiAgICB2YXIgcmVuZGVyU2NyZWVucyA9IGZ1bmN0aW9uKCkge1xuICAgICAgICByZXR1cm4gXy5tYXAoR2FtZVRyYWNrZXJDbGllbnQuc2NyZWVuQ29sbGVjdGlvbiwgZnVuY3Rpb24oc2NyZWVuQ29udGVudCwgc2NyZWVuTmFtZSkge1xuICAgICAgICAgICAgcmV0dXJuIG0oXCJkaXZcIiwge3N0eWxlOlwiZGlzcGxheTpcIitHYW1lVHJhY2tlckNsaWVudC52bS5zaG91bGREaXNwbGF5U2NyZWVuKHNjcmVlbk5hbWUpfSwgc2NyZWVuQ29udGVudCgpKTtcbiAgICAgICAgfSk7XG4gICAgfTtcbiAgICByZXR1cm4gcmVuZGVyU2NyZWVucygpO1xufTtcbiIsIm0ubW9kdWxlKGRvY3VtZW50LmdldEVsZW1lbnRCeUlkKFwiY29udGFpbmVyXCIpLCBHYW1lVHJhY2tlckNsaWVudCk7XG4iXSwic291cmNlUm9vdCI6Ii9zb3VyY2UvIn0=