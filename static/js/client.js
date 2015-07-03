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

var GameTrackerClient = {}

GameTrackerClient.Game = function(initialObject) {
    this.attributes = {
        name : initialObject.name.replace(/\\/g,''),
        blurb : ((_.isNull(initialObject.blurb)) ? "" : initialObject.blurb.replace(/\\/g,'')),
        region : initialObject.region,
        hasmanual : initialObject.hasmanual,
        hasbox : initialObject.hasbox,
        notes :  ((_.isNull(initialObject.notes)) ? "" : initialObject.notes.replace(/\\/g,'')),
        quantity : initialObject.quantity,
        systemname : "",
        genres: [],
        companies: []
    };
    this.attributes.systemname = _.result(_.find(systems, {id: initialObject.systemid}), "name").replace(/\\/g,'');
    
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
        return _.map(_.pluck(_.filter(eval(collectionName), function(item) {
            return _.contains(_.pluck(ensureArray(initialObject[collectionName]), singularName), item.id);
        }),
                       "name"), function(item) { return item.replace(/\\/g,''); });
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

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbInNlbGVjdDJtaXRocmlsLmpzIiwiZ2FtZXRyYWNrZXJzaGFyZWQuanMiLCJnYW1lZm9ybS5qcyIsImdhbWVmb3Jtdmlldy5qcyIsImNsaWVudG1vZGVscy5qcyIsImNsaWVudGNvbnRyb2xsZXIuanMiLCJjbGllbnR2aWV3LmpzIiwiZ2FtZXRyYWNrZXJjbGllbnQuanMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUM3Q0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3ZEQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUN0RkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUNwSUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3JDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQzVEQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQ3hFQTtBQUNBIiwiZmlsZSI6ImNsaWVudC5qcyIsInNvdXJjZXNDb250ZW50IjpbIi8vRm9yIHVzZSB3aXRoIGFsbCBWaWV3cy4gQ29kZSBpcyBiYXNlZCBvbiB0aGUgb25lIGZvdW5kIG9uIG1pdGhyaWwncyBzaXRlIGh0dHBzOi8vbGhvcmllLmdpdGh1Yi5pby9taXRocmlsL2ludGVncmF0aW9uLmh0bWxcbnZhciBzZWxlY3QyPSB7fTtcblxuLyogVGhpcyBmYWN0b3J5IGZ1bmN0aW9uIG9mZmVycyBhIG5pY2UgY2xvc3VyZSBmb3IgYW55dGhpbmcgZXh0cmEgd2Ugd2FudCB0byBwYXNzIGluICovXG5zZWxlY3QyLmNvbmZpZyA9IGZ1bmN0aW9uKGV4dHJhQXJndW1lbnRzKSB7XG4gICAgcmV0dXJuIGZ1bmN0aW9uKGVsZW1lbnQsIGlzSW5pdGlhbGl6ZWQsIGNvbnRyb2xsZXIpIHtcbiAgICAgICAgdmFyIGVsID0gJChlbGVtZW50KTtcbiAgICAgICAgaWYgKCFpc0luaXRpYWxpemVkKSB7XG4gICAgICAgICAgICBpZiAoZXh0cmFBcmd1bWVudHMuc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9ucykge1xuICAgICAgICAgICAgICAgIGVsLnNlbGVjdDIoZXh0cmFBcmd1bWVudHMuc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9ucyk7XG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIGVsLnNlbGVjdDIoKTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIGVsLmNoYW5nZShmdW5jdGlvbigpIHtcbiAgICAgICAgICAgICAgICBtLnN0YXJ0Q29tcHV0YXRpb24oKTtcbiAgICAgICAgICAgICAgICBleHRyYUFyZ3VtZW50cy5vbmNoYW5nZShlbC5zZWxlY3QyKFwidmFsXCIpKTtcbiAgICAgICAgICAgICAgICBtLmVuZENvbXB1dGF0aW9uKCk7XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfVxuICAgICAgICBlbC5zZWxlY3QyKFwidmFsXCIsIGV4dHJhQXJndW1lbnRzLnZhbHVlKTtcbiAgICB9O1xufTtcblxuc2VsZWN0Mi52aWV3ID0gZnVuY3Rpb24oZXh0cmFBcmd1bWVudHMsIG9wdGlvblNldCwgaXNNdWx0aXBsZSkge1xuICAgIHZhciBzZWxlY3RvciA9IChpc011bHRpcGxlKSA/IFwic2VsZWN0LmZvcm0tY29udHJvbFttdWx0aXBsZT10cnVlXVwiIDogXCJzZWxlY3QuZm9ybS1jb250cm9sXCI7XG4gICAgdmFyIHNvcnRlZE9wdGlvbnMgPSBfLnNvcnRCeShvcHRpb25TZXQsIGZ1bmN0aW9uKHZhbHVlKSB7XG4gICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IHZhbHVlO1xuICAgICAgICBpZiAoXy5pc09iamVjdCh2YWx1ZSkpIHtcbiAgICAgICAgICAgIHJldHVyblZhbHVlID0gdmFsdWUubmFtZS50b0xvd2VyQ2FzZSgpLnJlcGxhY2UoL1xcXFwvZywnJyk7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgIH0pO1xuICAgIHZhciBjcmVhdGVPcHRpb25TZXQgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgdmFyIG9wdGlvbnMgPSBbXTtcbiAgICAgICAgaWYgKG9wdGlvblNldCkge1xuICAgICAgICAgICAgb3B0aW9ucyA9IF8ubWFwKHNvcnRlZE9wdGlvbnMsIGZ1bmN0aW9uKHZhbHVlKSB7XG4gICAgICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gKF8uaXNPYmplY3QodmFsdWUpKSA/IG0oXCJvcHRpb25cIiwge3ZhbHVlOiB2YWx1ZS5pZH0sIHZhbHVlLm5hbWUpIDogbShcIm9wdGlvblwiLCB2YWx1ZSk7XG4gICAgICAgICAgICAgICAgcmV0dXJuIHJldHVyblZhbHVlO1xuICAgICAgICAgICAgfSk7XG4gICAgICAgIH1cbiAgICAgICAgcmV0dXJuIG9wdGlvbnM7XG4gICAgfTtcbiAgICByZXR1cm4gbShzZWxlY3Rvciwge2NvbmZpZzpzZWxlY3QyLmNvbmZpZyhleHRyYUFyZ3VtZW50cyl9LFxuICAgICAgICAgICAgIFttKFwib3B0aW9uXCIpLGNyZWF0ZU9wdGlvblNldCgpXSk7XG59O1xuIiwidmFyIEdhbWVUcmFja2VyU2hhcmVkID0ge307XG5cbkdhbWVUcmFja2VyU2hhcmVkLlRyYWNrZXJGb3JtID0gZnVuY3Rpb24oZmllbGRzKSB7XG4gICAgdGhpcy5maWVsZHMgPSBmaWVsZHM7XG4gICAgdGhpcy5wb3B1bGF0ZUZvcm0gPSBmdW5jdGlvbihvYmplY3QpIHtcbiAgICAgICAgdmFyIHNlbGYgPSB0aGlzO1xuICAgICAgICBpZiAob2JqZWN0LmF0dHJpYnV0ZXMpIHtcbiAgICAgICAgICAgIC8vVXNlZCBmb3IgZGF0YSB0aGF0J3Mga2VwdCBpbiBhIG1vZGVsICh1c2VyIHNlbGVjdHMgZnJvbSBhIGRyb3AgZG93bilcbiAgICAgICAgICAgIF8ubWFwKG9iamVjdC5hdHRyaWJ1dGVzLCBmdW5jdGlvbihhdHRyaWJ1dGVWYWx1ZSwgYXR0cmlidXRlS2V5KSB7XG4gICAgICAgICAgICAgICAgaWYgKGF0dHJpYnV0ZUtleSAhPT0gXCJpZFwiKSB7XG4gICAgICAgICAgICAgICAgICAgIHZhciBhY3R1YWxWYWx1ZSA9IChfLmlzU3RyaW5nKGF0dHJpYnV0ZVZhbHVlKSkgPyBhdHRyaWJ1dGVWYWx1ZS5yZXBsYWNlKC9cXFxcL2csJycpIDogYXR0cmlidXRlVmFsdWU7XG4gICAgICAgICAgICAgICAgICAgIC8vIFdlIG5lZWQgdG8gc3RyaXAgb3V0IGFueSBzbGFzaGVzIHJlY29yZGVkIGluIHRoZSBkYiBlbnRyeVxuICAgICAgICAgICAgICAgICAgICBzZWxmLmZpZWxkc1thdHRyaWJ1dGVLZXldKGFjdHVhbFZhbHVlKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9KTtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIC8vTWFpbmx5IHVzZWQgZm9yIGJvb3RzdHJhcHBlZCBkYXRhIGFuZCBkYXRhIHRoYXQgY29tZXMgaW4gZnJvbSB0aGUgc2VydmVyICh1c2VkIG1haW5seSB3aXRoIGdhbWVzKVxuICAgICAgICAgICAgXy5tYXAob2JqZWN0LCBmdW5jdGlvbih2YWx1ZSwga2V5KSB7XG4gICAgICAgICAgICAgICAgdmFyIGFjdHVhbFZhbHVlID0gKF8uaXNTdHJpbmcodmFsdWUpKSA/IHZhbHVlLnJlcGxhY2UoL1xcXFwvZywgJycpIDogdmFsdWU7XG4gICAgICAgICAgICAgICAgaWYgKGtleSAhPT0gXCJpZFwiKSB7XG4gICAgICAgICAgICAgICAgICAgIHNlbGYuZmllbGRzW2tleV0oYWN0dWFsVmFsdWUpO1xuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIH0pO1xuICAgICAgICB9XG4gICAgfTtcbiAgICB0aGlzLmNsZWFyRm9ybSA9IF8uZm9yRWFjaC5iaW5kKHRoaXMsIHRoaXMuZmllbGRzLCBmdW5jdGlvbihpbnB1dCkge1xuICAgICAgICBpZiAoXy5pc1N0cmluZyhpbnB1dCgpKSkge1xuICAgICAgICAgICAgaW5wdXQoXCJcIik7XG4gICAgICAgIH0gZWxzZSBpZiAoXy5pc0FycmF5KGlucHV0KCkpKSB7XG4gICAgICAgICAgICBpbnB1dChbXSk7XG4gICAgICAgIH0gZWxzZSBpZiAoXy5pc0Jvb2xlYW4oaW5wdXQoKSkpe1xuICAgICAgICAgICAgaW5wdXQoZmFsc2UpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgaW5wdXQobnVsbCk7XG4gICAgICAgIH1cbiAgICB9KTtcbiAgICB0aGlzLnJldHVybkZpZWxkcyA9IGZ1bmN0aW9uKCkge1xuICAgICAgICByZXR1cm4gXy5vbWl0KF8ubWFwVmFsdWVzKHRoaXMuZmllbGRzLCBmdW5jdGlvbihmaWVsZCkge1xuICAgICAgICAgICAgdmFyIHJldHVyblZhbHVlID0gZmllbGQoKTtcbiAgICAgICAgICAgIGlmIChfLmlzQm9vbGVhbihyZXR1cm5WYWx1ZSkpIHtcbiAgICAgICAgICAgICAgICByZXR1cm5WYWx1ZSA9IE51bWJlcihyZXR1cm5WYWx1ZSk7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgIH0pLCBfLmlzTnVsbCk7XG4gICAgfTtcbiAgICB0aGlzLnN1Ym1pdEhhbmRsZXJzID0ge307XG4gICAgLyogVGhpcyB3aWxsIHByb2JhYmx5IGJlIHJlZmFjdG9yZWQgb3V0IGluIHRoZSBmdXR1cmUgZ2l2ZW4gdGhlIG9ubHkgdGhpbmcgdGhhdCBoYXMgYSBzZWFyY2ggaXMgdGhlIGdhbWUgZm9ybVxuICAgICAqIFRvIGtlZXAgdGhpbmdzIGZyb20gY29tcGxhaW5pbmcgYWJvdXQgYSBtaXNzaW5nIGtleSB3ZSBhZGQgYW4gZW1wdHkgZnVuY3Rpb24gaGVyZVxuICAgICAqL1xuICAgIHRoaXMuc3VibWl0SGFuZGxlcnMuc2VhcmNoID0gZnVuY3Rpb24oKSB7IC8qZW1wdHkqLyB9O1xuICAgIHRoaXMuZ2V0U3VibWl0SGFuZGxlciA9IGZ1bmN0aW9uKHN0YXRlKSB7XG4gICAgICAgIHJldHVybiB0aGlzLnN1Ym1pdEhhbmRsZXJzW3N0YXRlXTtcbiAgICB9O1xuXG59O1xuIiwidmFyIEdhbWVGb3JtID0ge307XG5cbkdhbWVGb3JtLmNvbnRyb2xsZXIgPSBuZXcgZnVuY3Rpb24oKSB7XG4gICAgdGhpcy5nYW1lRm9ybSA9IG5ldyBHYW1lVHJhY2tlclNoYXJlZC5UcmFja2VyRm9ybSh7bmFtZTogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGJsdXJiOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgcmVnaW9uOiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaGFzbWFudWFsOiBtLnByb3AoZmFsc2UpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhhc2JveDogbS5wcm9wKGZhbHNlKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBub3RlczogbS5wcm9wKFwiXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHF1YW50aXR5OiBtLnByb3AoXCJcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgZ2VucmVzOiBtLnByb3AoW10pLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNvbXBhbmllczogbS5wcm9wKFtdKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzeXN0ZW1pZDogbS5wcm9wKFwiXCIpfSk7XG5cbiAgICB0aGlzLmVycm9yTWVzc2FnZSA9IFwiXCI7XG5cbiAgICB0aGlzLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgIHRoaXMuc2VhcmNoUmVzdWx0cyA9IFtdO1xuXG4gICAgdGhpcy5ub1Jlc3VsdHMgPSBcIlwiO1xuICAgIHRoaXMuZm9ybU1vZGUgPSBcInNlYXJjaFwiO1xuICAgIHRoaXMuaXNBZG1pbiA9IGZhbHNlO1xuXG4gICAgdGhpcy5zZWFyY2hMb2FkaW5nID0gZmFsc2U7XG5cbiAgICB0aGlzLnNlbGVjdFVwZGF0ZUhhbmRsZXI7XG4gICAgdGhpcy5zZWxlY3REZWxldGVIYW5kbGVyO1xuICAgIHRoaXMuY2FuY2VsQnV0dG9uSGFuZGxlcjtcblxuICAgIHRoaXMuc3lzdGVtcztcbiAgICB0aGlzLmdlbnJlcztcbiAgICB0aGlzLmNvbXBhbmllcztcblxuICAgIHRoaXMuYWRkQ29tcGFueUhhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuICAgIHRoaXMuYWRkR2VucmVIYW5kbGVyID0gZnVuY3Rpb24oKSB7fTtcbiAgICB0aGlzLmFkZFN5c3RlbUhhbmRsZXIgPSBmdW5jdGlvbigpIHt9O1xuXG4gICAgdGhpcy5wb3B1bGF0ZVNlbGVjdERhdGFTZXRzID0gZnVuY3Rpb24oc3lzdGVtcywgZ2VucmVzLCBjb21wYW5pZXMpIHtcbiAgICAgICAgdGhpcy5zeXN0ZW1zID0gc3lzdGVtcztcbiAgICAgICAgdGhpcy5nZW5yZXMgPSBnZW5yZXM7XG4gICAgICAgIHRoaXMuY29tcGFuaWVzID0gY29tcGFuaWVzO1xuICAgIH07XG5cbiAgICB0aGlzLmFkbWluUmVzdWx0QnV0dG9uSGFuZGxlcnMgPSBmdW5jdGlvbihlZGl0aGFuZGxlciwgZGVsZXRlaGFuZGxlcikgIHtcbiAgICAgICAgdGhpcy5zZWxlY3RVcGRhdGVIYW5kbGVyID0gZWRpdGhhbmRsZXI7XG4gICAgICAgIHRoaXMuc2VsZWN0RGVsZXRlSGFuZGxlciA9IGRlbGV0ZWhhbmRsZXI7XG4gICAgfTtcblxuICAgIHRoaXMuYmluZFN1Ym1pdEZvcm1IYW5kbGVyID0gZnVuY3Rpb24oc3RhdGUsIGhhbmRsZXIpIHtcbiAgICAgICAgdGhpcy5nYW1lRm9ybS5zdWJtaXRIYW5kbGVyc1tzdGF0ZV0gPSBoYW5kbGVyO1xuICAgIH07XG5cbiAgICB0aGlzLnRpdGxlQ2xpY2tIYW5kbGVyID0gZnVuY3Rpb24oKSB7fTtcblxuICAgIHRoaXMuZ2FtZUZvcm0uc3VibWl0SGFuZGxlcnMuc2VhcmNoID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzID0gXCJcIjtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSB0cnVlO1xuICAgICAgICB2YXIgY29tcGxldGVkU2V0ID0gXy5vbWl0KEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0ucmV0dXJuRmllbGRzKCksIGZ1bmN0aW9uKHZhbHVlLCBrZXkpIHtcbiAgICAgICAgICAgIHZhciByZXR1cm5WYWx1ZSA9IHRydWU7XG4gICAgICAgICAgICBpZiAoXy5pc0Jvb2xlYW4odmFsdWUpKSB7XG4gICAgICAgICAgICAgICAgcmV0dXJuVmFsdWUgPSAhdmFsdWU7XG4gICAgICAgICAgICB9IGVsc2UgaWYgKCFfLmlzRW1wdHkodmFsdWUpKSB7XG4gICAgICAgICAgICAgICAgcmV0dXJuVmFsdWUgPSBmYWxzZTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiByZXR1cm5WYWx1ZTtcbiAgICAgICAgfSk7XG4gICAgICAgIGlmICghXy5pc0VtcHR5KGNvbXBsZXRlZFNldCkpIHtcbiAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZXJyb3JNZXNzYWdlID0gXCJcIjtcbiAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOlwicG9zdFwiLFxuICAgICAgICAgICAgICAgICAgICAgICB1cmw6IFwiL3NlYXJjaC1nYW1lcy1hamF4L1wiLFxuICAgICAgICAgICAgICAgICAgICAgICBkYXRhOiBjb21wbGV0ZWRTZXR9KVxuICAgICAgICAgICAgICAgIC50aGVuKGZ1bmN0aW9uKHJlc3BvbnNlKSB7XG4gICAgICAgICAgICAgICAgICAgIC8vRW1wdHkgcmVzdWx0cyBzZXQgcmV0dXJucyBhIHNpbmdsZSBpdGVtIGFycmF5IHdpdGggbnVsbCBiZWluZyB0aGF0IG9iamVjdFxuICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMgPSBfLnJlbW92ZShyZXNwb25zZS5yZXN1bHRzLCBmdW5jdGlvbihpdGVtKSB7IHJldHVybiAhXy5pc051bGwoaXRlbSk7IH0pO1xuICAgICAgICAgICAgICAgICAgICBpZiAoR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hSZXN1bHRzLmxlbmd0aCA8IDEpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIubm9SZXN1bHRzID0gXCJObyBtYXRjaGVzIHdlcmUgZm91bmRcIjtcbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmlzTG9hZGluZyA9IGZhbHNlO1xuICAgICAgICAgICAgICAgIH0sIGZ1bmN0aW9uKCkgeyBHYW1lRm9ybS5jb250cm9sbGVyLmVycm9yTWVzc2FnZSA9IFwiSW50ZXJuYWwgU2VydmVyIEVycm9yXCI7fSApO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5lcnJvck1lc3NhZ2UgPSBcIlBsZWFzZSBlbnRlciBhdCBsZWFzdCBvbmUgc2VhcmNoIHBhcmFtZXRlclwiO1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gZmFsc2U7XG4gICAgfTtcbn07XG4iLCJHYW1lRm9ybS52aWV3ID0gZnVuY3Rpb24oKSB7XG4gICAgdmFyIGZvcm1Db25maWd1cmF0aW9uID0ge1xuICAgICAgICB0ZXh0QXJlYURpc3BsYXk6IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlWYWx1ZSA9IChHYW1lRm9ybS5jb250cm9sbGVyLmZvcm1Nb2RlID09PSBcInNlYXJjaFwiKSA/IFwiZGlzcGxheTpub25lXCIgOiBcImRpc3BsYXk6aW5oZXJpdFwiO1xuICAgICAgICAgICAgcmV0dXJuIGRpc3BsYXlWYWx1ZTtcbiAgICAgICAgfSxcbiAgICAgICAgYWN0aW9uQnV0dG9uRGlzcGxheTogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgZGlzcGxheVZhbHVlID0gKEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nKSA/IFwiZGlzcGxheTpub25lXCIgOiBcImRpc3BsYXk6aW5saW5lXCI7XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVZhbHVlO1xuICAgICAgICB9LFxuICAgICAgICBwcmVsb2FkZXJEaXNwbGF5OiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIHZhciBkaXNwbGF5VmFsdWUgPSAoR2FtZUZvcm0uY29udHJvbGxlci5pc0xvYWRpbmcpID8gXCJkaXNwbGF5OmluaGVyaXRcIiA6IFwiZGlzcGxheTpub25lXCI7XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVZhbHVlO1xuICAgICAgICB9LFxuICAgICAgICBjaGFuZ2VGb3JtUHJvcGVydGllczogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgc3R5bGVQcm9wZXJ0aWVzID0gXCJjdXJzb3I6cG9pbnRlcjtcIjtcbiAgICAgICAgICAgIGlmIChHYW1lRm9ybS5jb250cm9sbGVyLmlzQWRtaW4pIHtcbiAgICAgICAgICAgICAgICBzdHlsZVByb3BlcnRpZXMgKz0gXCJkaXNwbGF5OmluaGVyaXRcIjtcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgc3R5bGVQcm9wZXJ0aWVzICs9IFwiZGlzcGxheTpub25lXCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gc3R5bGVQcm9wZXJ0aWVzO1xuICAgICAgICB9XG4gICAgfTtcbiAgICB2YXIgcmVuZGVyU2VhcmNoUmVzdWx0cyA9IGZ1bmN0aW9uKCkge1xuICAgICAgICB2YXIgcmVuZGVyZWRSZXN1bHRzID0gW107XG4gICAgICAgIHZhciBkaXNwbGF5UHJvcGVydGllcyA9IChHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaExvYWRpbmcpID8ge3Jlc3VsdHM6IFwiZGlzcGxheTpub25lXCIsIHByZWxvYWRlcjogXCJkaXNwbGF5OmluaGVyaXRcIn0gOiB7cmVzdWx0czogXCJkaXNwbGF5OmluaGVyaXRcIiwgcHJlbG9hZGVyOiBcImRpc3BsYXk6bm9uZVwifTtcbiAgICAgICAgdmFyIHRpdGxlQ3Vyc29yUHJvcGVydHkgPSAoR2FtZUZvcm0uY29udHJvbGxlci5pc0FkbWluKSA/IFwiY3Vyc29yOmRlZmF1bHRcIiA6IFwiY3Vyc29yOnBvaW50ZXJcIjtcbiAgICAgICAgdmFyIHJlbmRlckFkbWluQnV0dG9ucyA9IGZ1bmN0aW9uKHJlc3VsdCkge1xuICAgICAgICAgICAgdmFyIGFkbWluQnV0dG9ucyA9IFtdO1xuICAgICAgICAgICAgaWYgKEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNBZG1pbikge1xuICAgICAgICAgICAgICAgIGFkbWluQnV0dG9ucyA9IG0oXCJkaXYuY29sLXhzLTNcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwic3Bhbi5nbHlwaGljb24uZ2x5cGhpY29uLXJlbW92ZS5nYW1lLXNlYXJjaC1yZXN1bHRzLWJ1dHRvblwiLCB7b25jbGljazpHYW1lRm9ybS5jb250cm9sbGVyLnNlbGVjdERlbGV0ZUhhbmRsZXIuYmluZChHYW1lRm9ybS5jb250cm9sbGVyLCByZXN1bHQuaWQpfSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuLmdseXBoaWNvbi5nbHlwaGljb24tcGVuY2lsLmdhbWUtc2VhcmNoLXJlc3VsdHMtYnV0dG9uXCIsIHtvbmNsaWNrOkdhbWVGb3JtLmNvbnRyb2xsZXIuc2VsZWN0VXBkYXRlSGFuZGxlci5iaW5kKEdhbWVGb3JtLmNvbnRyb2xsZXIsIHJlc3VsdC5pZCl9KVxuICAgICAgICAgICAgICAgIF0pO1xuICAgICAgICAgICAgfTtcbiAgICAgICAgICAgIHJldHVybiBhZG1pbkJ1dHRvbnM7XG4gICAgICAgIH07XG4gICAgICAgIGlmICghXy5pc0VtcHR5KEdhbWVGb3JtLmNvbnRyb2xsZXIuc2VhcmNoUmVzdWx0cykgfHwgIV8uaXNFbXB0eShHYW1lRm9ybS5jb250cm9sbGVyLm5vUmVzdWx0cykpIHtcbiAgICAgICAgICAgIHJlbmRlcmVkUmVzdWx0cyA9IG0oXCJkaXZcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAge3N0eWxlOmRpc3BsYXlQcm9wZXJ0aWVzLnJlc3VsdHN9LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbbShcImRpdlwiLCBHYW1lRm9ybS5jb250cm9sbGVyLm5vUmVzdWx0cyksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBfLm1hcChHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMsIGZ1bmN0aW9uKHJlc3VsdCwgaW5kZXgpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YXIgYmdDb2xvciA9IFwiYmFja2dyb3VuZC1jb2xvcjojQ0VDRkUwXCI7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaWYgKGluZGV4ICUgMiA9PSAwKSB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGJnQ29sb3IgPSBcImJhY2tncm91bmQtY29sb3I6I0ZGRlwiO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZXR1cm4gbShcImRpdi5yb3cucmVzdWx0LXJvd1wiLCB7c3R5bGU6YmdDb2xvcn0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgW20oXCJkaXYuY29sLXhzLTlcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7c3R5bGU6YmdDb2xvcn0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgW1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCB7c3R5bGU6dGl0bGVDdXJzb3JQcm9wZXJ0eSwgb25jbGljazpHYW1lRm9ybS5jb250cm9sbGVyLnRpdGxlQ2xpY2tIYW5kbGVyLmJpbmQoR2FtZUZvcm0uY29udHJvbGxlciwgcmVzdWx0LmlkKX0sIChyZXN1bHQubmFtZS5yZXBsYWNlKC9cXFxcL2csICcnKSArIFwiIFtcIiArIHJlc3VsdC5yZWdpb24gKyBcIl0gKFwiICsgcmVzdWx0LnN5c3RlbU5hbWUucmVwbGFjZSgvXFxcXC9nLCcnKSArIFwiKVwiKSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlbmRlckFkbWluQnV0dG9ucyhyZXN1bHQpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXSk7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9KSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbWdbc3JjPS9pbWFnZXMvYWpheC5naWZdXCIsIHtzdHlsZTpkaXNwbGF5UHJvcGVydGllcy5wcmVsb2FkZXJ9KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBdKTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gcmVuZGVyZWRSZXN1bHRzO1xuICAgIH07XG4gICAgcmV0dXJuIFttKFwiZGl2LnJvd1wiLFtcbiAgICAgICAgbShcImRpdi5jb2wteHMtMTJcIixbXG4gICAgICAgICAgICBtKFwiZGl2LnRleHQtZGFuZ2VyXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZXJyb3JNZXNzYWdlKSxcbiAgICAgICAgICAgIG0oXCJmb3JtXCIsIFtcbiAgICAgICAgICAgICAgICBtKFwiaW5wdXQuZm9ybS1jb250cm9sXCIsIHtvbmNoYW5nZTogbS53aXRoQXR0cihcInZhbHVlXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLm5hbWUpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubmFtZSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJOYW1lXCJ9KSxcbiAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLnJlZ2lvbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5yZWdpb24oKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHNlbGVjdDJJbml0aWFsaXphdGlvbk9wdGlvbnM6IHtwbGFjZWhvbGRlcjogXCJSZWdpb25cIiwgYWxsb3dDbGVhcjogdHJ1ZX19LFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXCJOVFNDXCIsIFwiTlRTQy1KXCIsIFwiUEFMXCJdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgc2VsZWN0Mi52aWV3KHtvbmNoYW5nZTpHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5zeXN0ZW1pZCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuc3lzdGVtaWQoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiU3lzdGVtXCIsIGFsbG93Q2xlYXI6IHRydWV9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuc3lzdGVtcyksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ1XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi5jaGFuZ2VGb3JtUHJvcGVydGllcygpLCBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmFkZFN5c3RlbUhhbmRsZXJ9LCBcIitBZGQgU3lzdGVtXCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIHNlbGVjdDIudmlldyh7b25jaGFuZ2U6R2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuZ2VucmVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5nZW5yZXMoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZWxlY3QySW5pdGlhbGl6YXRpb25PcHRpb25zOiB7cGxhY2Vob2xkZXI6IFwiR2VucmVzXCIsIGFsbG93Q2xlYXI6IHRydWV9fSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2VucmVzLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdHJ1ZSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ1XCIsIHtzdHlsZTpmb3JtQ29uZmlndXJhdGlvbi5jaGFuZ2VGb3JtUHJvcGVydGllcygpLCBvbmNsaWNrOiBHYW1lRm9ybS5jb250cm9sbGVyLmFkZEdlbnJlSGFuZGxlcn0sIFwiK0FkZCBHZW5yZVwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBzZWxlY3QyLnZpZXcoe29uY2hhbmdlOkdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB2YWx1ZTogR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMuY29tcGFuaWVzKCksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0MkluaXRpYWxpemF0aW9uT3B0aW9uczoge3BsYWNlaG9sZGVyOiBcIkNvbXBhbmllc1wiLCBhbGxvd0NsZWFyOiB0cnVlfX0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmNvbXBhbmllcyxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRydWUpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidVwiLCB7c3R5bGU6Zm9ybUNvbmZpZ3VyYXRpb24uY2hhbmdlRm9ybVByb3BlcnRpZXMoKSwgb25jbGljazogR2FtZUZvcm0uY29udHJvbGxlci5hZGRDb21wYW55SGFuZGxlcn0sIFwiK0FkZCBDb21wYW55XCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImlucHV0LmZvcm0tY29udHJvbFwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhbHVlOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5xdWFudGl0eSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBwbGFjZWhvbGRlcjogXCJRdWFudGl0eVwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCB7c3R5bGU6Zm9ybUNvbmZpZ3VyYXRpb24udGV4dEFyZWFEaXNwbGF5KCl9LCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJwXCIsIFwiU2hvcnQgRGVzY3JpcHRpb25cIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJ0ZXh0YXJlYVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJ2YWx1ZVwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ibHVyYil9LCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5ibHVyYigpKSxcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2LmNoZWNrYm94XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcImxhYmVsXCIsIFtcbiAgICAgICAgICAgICAgICAgICAgICAgIG0oXCJpbnB1dFt0eXBlPWNoZWNrYm94XVwiLCB7b25jaGFuZ2U6IG0ud2l0aEF0dHIoXCJjaGVja2VkXCIsIEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc21hbnVhbCksIGNoZWNrZWQ6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uZmllbGRzLmhhc21hbnVhbCgpfSlcbiAgICAgICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiTWFudWFsXCIpXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdi5jaGVja2JveFwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJsYWJlbFwiLCBbXG4gICAgICAgICAgICAgICAgICAgICAgICBtKFwiaW5wdXRbdHlwZT1jaGVja2JveF1cIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwiY2hlY2tlZFwiLCBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNib3gpLCBjaGVja2VkOiBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmZpZWxkcy5oYXNib3goKX0pXG4gICAgICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCBcIkJveFwiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwge3N0eWxlOmZvcm1Db25maWd1cmF0aW9uLnRleHRBcmVhRGlzcGxheSgpfSwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwicFwiLCBcIk5vdGVzXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwidGV4dGFyZWFcIiwge29uY2hhbmdlOiBtLndpdGhBdHRyKFwidmFsdWVcIiwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubm90ZXMpfSwgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5maWVsZHMubm90ZXMoKSksXG4gICAgICAgICAgICAgICAgXSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1zdWNjZXNzXCIsIHtzdHlsZTogZm9ybUNvbmZpZ3VyYXRpb24uYWN0aW9uQnV0dG9uRGlzcGxheSgpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuZ2FtZUZvcm0uc3VibWl0SGFuZGxlcnNbR2FtZUZvcm0uY29udHJvbGxlci5mb3JtTW9kZV19LCBcInN1Ym1pdFwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcImJ1dHRvbi5idG4uYnRuLWRhbmdlclwiLCB7c3R5bGU6IGZvcm1Db25maWd1cmF0aW9uLmFjdGlvbkJ1dHRvbkRpc3BsYXkoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIG9uY2xpY2s6IEdhbWVGb3JtLmNvbnRyb2xsZXIuY2FuY2VsQnV0dG9uSGFuZGxlcn0sIFwiY2FuY2VsXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwiaW1nW3NyYz0vaW1hZ2VzL2FqYXguZ2lmXVwiLCB7c3R5bGU6IGZvcm1Db25maWd1cmF0aW9uLnByZWxvYWRlckRpc3BsYXkoKX0pXG4gICAgICAgICAgICAgICAgXSlcbiAgICAgICAgICAgIF0pLFxuICAgICAgICBdKVxuICAgIF0pLFxuICAgICAgICAgICAgcmVuZGVyU2VhcmNoUmVzdWx0cygpXG4gICAgICAgICAgIF07XG59O1xuIiwidmFyIEdhbWVUcmFja2VyQ2xpZW50ID0ge31cblxuR2FtZVRyYWNrZXJDbGllbnQuR2FtZSA9IGZ1bmN0aW9uKGluaXRpYWxPYmplY3QpIHtcbiAgICB0aGlzLmF0dHJpYnV0ZXMgPSB7XG4gICAgICAgIG5hbWUgOiBpbml0aWFsT2JqZWN0Lm5hbWUucmVwbGFjZSgvXFxcXC9nLCcnKSxcbiAgICAgICAgYmx1cmIgOiAoKF8uaXNOdWxsKGluaXRpYWxPYmplY3QuYmx1cmIpKSA/IFwiXCIgOiBpbml0aWFsT2JqZWN0LmJsdXJiLnJlcGxhY2UoL1xcXFwvZywnJykpLFxuICAgICAgICByZWdpb24gOiBpbml0aWFsT2JqZWN0LnJlZ2lvbixcbiAgICAgICAgaGFzbWFudWFsIDogaW5pdGlhbE9iamVjdC5oYXNtYW51YWwsXG4gICAgICAgIGhhc2JveCA6IGluaXRpYWxPYmplY3QuaGFzYm94LFxuICAgICAgICBub3RlcyA6ICAoKF8uaXNOdWxsKGluaXRpYWxPYmplY3Qubm90ZXMpKSA/IFwiXCIgOiBpbml0aWFsT2JqZWN0Lm5vdGVzLnJlcGxhY2UoL1xcXFwvZywnJykpLFxuICAgICAgICBxdWFudGl0eSA6IGluaXRpYWxPYmplY3QucXVhbnRpdHksXG4gICAgICAgIHN5c3RlbW5hbWUgOiBcIlwiLFxuICAgICAgICBnZW5yZXM6IFtdLFxuICAgICAgICBjb21wYW5pZXM6IFtdXG4gICAgfTtcbiAgICB0aGlzLmF0dHJpYnV0ZXMuc3lzdGVtbmFtZSA9IF8ucmVzdWx0KF8uZmluZChzeXN0ZW1zLCB7aWQ6IGluaXRpYWxPYmplY3Quc3lzdGVtaWR9KSwgXCJuYW1lXCIpLnJlcGxhY2UoL1xcXFwvZywnJyk7XG4gICAgXG4gICAgdmFyIGVuc3VyZUFycmF5ID0gZnVuY3Rpb24oaXRlbSkge1xuICAgICAgICB2YXIgcmV0dXJuVmFsdWUgPSBfLmlzQXJyYXkoaXRlbSkgPyBpdGVtIDogW2l0ZW1dO1xuICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgfTtcbiAgICAvKiBCaXQgb2YgYSBzeW1ib2xpYyBtYW5pcHVsYXRpb24gdHJpY2sgaGVyZSA7KS4gR2l2ZW4gdGhhdCB0aGUgaW5pdGlhbE9iamVjdCBhbmQgZ2xvYmFsIG5hbWVzcGFjZVxuICAgICAqIHVzZSB0aGUgc2FtZSBuYW1lIGZvciBnZW5yZXMgYW5kIGNvbXBhbmllcyB3ZSBzaW1wbHkgcGFzcyBhIHN0cmluZywgdXNlIGV2YWwgdG8gZ2V0IHRoZSBvYmplY3RcbiAgICAgKiBmcm9tIGdsb2JhbCBuYW1lc3BhY2UgYW5kIHN0aWxsIHVzZSBpdCB0byByZWZlcmVuY2UgdGhlIGF0dHJpYnV0ZSB3ZSB3YW50IGluIHRoZSBpbml0aWFsT2JqZWN0IG5hbWVzcGFjZS5cbiAgICAgKi9cbiAgICB2YXIgZ2V0UmVsYXRlZE5hbWVzID0gZnVuY3Rpb24oY29sbGVjdGlvbk5hbWUpIHtcbiAgICAgICAgdmFyIHNpbmd1bGFyTmFtZSA9IChjb2xsZWN0aW9uTmFtZSA9PT0gXCJnZW5yZXNcIikgPyBcImdlbnJlSWRcIiA6IFwiY29tcGFueUlkXCI7XG4gICAgICAgIHJldHVybiBfLm1hcChfLnBsdWNrKF8uZmlsdGVyKGV2YWwoY29sbGVjdGlvbk5hbWUpLCBmdW5jdGlvbihpdGVtKSB7XG4gICAgICAgICAgICByZXR1cm4gXy5jb250YWlucyhfLnBsdWNrKGVuc3VyZUFycmF5KGluaXRpYWxPYmplY3RbY29sbGVjdGlvbk5hbWVdKSwgc2luZ3VsYXJOYW1lKSwgaXRlbS5pZCk7XG4gICAgICAgIH0pLFxuICAgICAgICAgICAgICAgICAgICAgICBcIm5hbWVcIiksIGZ1bmN0aW9uKGl0ZW0pIHsgcmV0dXJuIGl0ZW0ucmVwbGFjZSgvXFxcXC9nLCcnKTsgfSk7XG4gICAgfTtcbiAgICBcbiAgICB0aGlzLmF0dHJpYnV0ZXMuZ2VucmVzID0gZ2V0UmVsYXRlZE5hbWVzKFwiZ2VucmVzXCIpO1xuICAgIHRoaXMuYXR0cmlidXRlcy5jb21wYW5pZXMgPSBnZXRSZWxhdGVkTmFtZXMoXCJjb21wYW5pZXNcIik7XG5cbn07XG4iLCJHYW1lVHJhY2tlckNsaWVudC52bSA9IG5ldyBmdW5jdGlvbigpIHtcbiAgICB2YXIgdm0gPSB7fTtcbiAgICB2bS5pbml0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNMb2FkaW5nID0gZmFsc2U7XG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuaXNBZG1pbiA9IGZhbHNlO1xuXG4gICAgICAgIHZtLmN1cnJlbnRTY3JlZW4gPSBcIlNlYXJjaFNjcmVlblwiO1xuICAgICAgICB2bS5zaG91bGREaXNwbGF5U2NyZWVuID0gZnVuY3Rpb24oc2NyZWVuTmFtZSkge1xuICAgICAgICAgICAgdmFyIGRpc3BsYXlQcm9wZXJ0eSA9IChzY3JlZW5OYW1lID09PSB2bS5jdXJyZW50U2NyZWVuKSA/IFwiaW5oZXJpdFwiIDogXCJub25lXCI7XG4gICAgICAgICAgICByZXR1cm4gZGlzcGxheVByb3BlcnR5O1xuICAgICAgICB9O1xuXG4gICAgICAgIC8vTGlrZSBhZG1pbiBzeXN0ZW0sIGdlbnJlcywgYW5kIGNvbXBhbmllcyBhcmUgYm9vdHN0cmFwcGVkIGRhdGFcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5wb3B1bGF0ZVNlbGVjdERhdGFTZXRzKHN5c3RlbXMsIGdlbnJlcywgY29tcGFuaWVzKTtcbiAgICAgICAgXG4gICAgICAgIEdhbWVGb3JtLmNvbnRyb2xsZXIuY2FuY2VsQnV0dG9uSGFuZGxlciA9IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5nYW1lRm9ybS5jbGVhckZvcm0oKTtcbiAgICAgICAgICAgIHJldHVybiBmYWxzZTtcbiAgICAgICAgfTtcblxuICAgICAgICB2bS5jdXJyZW50R2FtZSA9IG51bGw7XG4gICAgICAgIC8vVE9ETyBhZGQgU2hvdWxkIGxpbmsgdG8gZ2FtZWZvcm0gb2JqZWN0ICh0byBkZXRlcm1pbmUgaWYgdGhlIHRoaW5nIHNob3VsZCBiZSBhIGxpbmtcbiAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci50aXRsZUNsaWNrSGFuZGxlciA9IGZ1bmN0aW9uKGdhbWVJZCkge1xuICAgICAgICAgICAgR2FtZUZvcm0uY29udHJvbGxlci5zZWFyY2hMb2FkaW5nID0gdHJ1ZTtcbiAgICAgICAgICAgIGlmIChnYW1lSWQgJiYgXy5pc0Zpbml0ZShOdW1iZXIoZ2FtZUlkKSkpIHtcbiAgICAgICAgICAgICAgICAvKiBBIGtub3duIGxpbWl0YXRpb24gd2l0aCB0aGUgYmFja2VuZDogdGhpbmdzIHdlIGV4cGVjdCB0byBiZSBhbiBhcnJheSBtYXkgYmUgYSBzaW1wbGUgb2JqZWN0IGR1ZSB0byB0aGUganNvbiBlbmNvZGVyIG9uIHRoZSBiYWNrZW5kXG4gICAgICAgICAgICAgICAgIG5vdCBiZWluZyBhYmxlIHRvIGVuY29kZSBzaW5nbGUgcm93IHJlc3VsdHMgY29ycmVjdGx5XG4gICAgICAgICAgICAgICAgICovXG4gICAgICAgICAgICAgICAgdmFyIGVuc3VyZUFycmF5ID0gZnVuY3Rpb24oaXRlbSkge1xuICAgICAgICAgICAgICAgICAgICB2YXIgcmV0dXJuVmFsdWUgPSBfLmlzQXJyYXkoaXRlbSkgPyBpdGVtIDogW2l0ZW1dO1xuICAgICAgICAgICAgICAgICAgICByZXR1cm4gcmV0dXJuVmFsdWU7XG4gICAgICAgICAgICAgICAgfTtcbiAgICAgICAgICAgICAgICAvL1dlIGNvdWxkIGp1c3QgdXNlIHRoZSBkYXRhIHdlIHJldHJpZXZlZCBmcm9tIHRoZSBzZWFyY2ggYnV0IGxldCdzIGd1YXJhbnRlZSB0aGUgdXNlciB3aXRoIHRoZSBtb3N0IHJlY2VudCBpbmZvcm1hdGlvblxuICAgICAgICAgICAgICAgIG0ucmVxdWVzdCh7bWV0aG9kOiBcIkdFVFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgdXJsOiBcIi9nYW1lL1wiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgZGF0YToge2lkOiBOdW1iZXIoZ2FtZUlkKX1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgfSlcbiAgICAgICAgICAgICAgICAgICAgLnRoZW4oZnVuY3Rpb24ocmVzcG9uc2UpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIGlmIChyZXNwb25zZSkge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRHYW1lID0gbmV3IEdhbWVUcmFja2VyQ2xpZW50LkdhbWUocmVzcG9uc2UpO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZtLmN1cnJlbnRTY3JlZW4gPSBcIkluZm9TY3JlZW5cIjtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaExvYWRpbmcgPSBmYWxzZTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLnNlYXJjaFJlc3VsdHMgPSBbXTtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBHYW1lRm9ybS5jb250cm9sbGVyLmdhbWVGb3JtLmNsZWFyRm9ybSgpO1xuICAgICAgICAgICAgICAgICAgICAgICAgfTtcbiAgICAgICAgICAgICAgICAgICAgfSwgdm0ucmVwb3J0SW50ZXJuYWxFcnJvcik7XG4gICAgICAgICAgICB9ICAgICAgICAgICAgXG4gICAgICAgIH07XG4gICAgfTtcblxuICAgIHZtLnJldHVyblRvU2VhcmNoID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHZtLmN1cnJlbnRTY3JlZW4gPSBcIlNlYXJjaFNjcmVlblwiO1xuICAgICAgICB2bS5jdXJyZW50R2FtZSA9IG51bGw7XG4gICAgfTtcbiAgICByZXR1cm4gdm07XG59O1xuXG5HYW1lVHJhY2tlckNsaWVudC5jb250cm9sbGVyID0gZnVuY3Rpb24oKSB7XG4gICAgR2FtZVRyYWNrZXJDbGllbnQudm0uaW5pdCgpO1xufTtcbiIsIkdhbWVUcmFja2VyQ2xpZW50LnNjcmVlbkNvbGxlY3Rpb24gPSB7fTtcbkdhbWVUcmFja2VyQ2xpZW50LnNjcmVlbkNvbGxlY3Rpb24uU2VhcmNoU2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgcmV0dXJuIFttKFwiaDFcIiwgXCJHYW1lIFNlYXJjaFwiKSxcbiAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIFwiRmlsbCBpbiBhdCBsZWFzdCBvbmUgc2VhcmNoIHBhcmFtZXRlciBiZWxvdy5cIiksXG4gICAgICAgICAgICAgICAgbShcImJyXCIpLFxuICAgICAgICAgICAgICAgIG0oXCJlbVwiLCBcImUuZy4gVG8gc2VlIGFsbCBTdXBlciBGYW1pY29tIGdhbWVzLCBzZWxlY3QgU3VwZXIgRmFtaWNvbSBpbiB0aGUgU3lzdGVtIHNlbGVjdCBkcm9wIGRvd24gYW5kIGNsaWNrIHN1Ym1pdFwiKVxuICAgICAgICAgICAgXSksXG4gICAgICAgICAgICBHYW1lRm9ybS52aWV3KCldO1xufVxuR2FtZVRyYWNrZXJDbGllbnQuc2NyZWVuQ29sbGVjdGlvbi5JbmZvU2NyZWVuID0gZnVuY3Rpb24oKSB7XG4gICAgdmFyIHNjcmVlbiA9IFtdO1xuICAgIGlmICghXy5pc051bGwoR2FtZVRyYWNrZXJDbGllbnQudm0uY3VycmVudEdhbWUpKSB7XG4gICAgICAgIHZhciB0aGVHYW1lID0gR2FtZVRyYWNrZXJDbGllbnQudm0uY3VycmVudEdhbWU7XG4gICAgICAgIHZhciBjb21wbGV0aW9uQ29uZGl0aW9uID0gZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICB2YXIgY29uZGl0aW9uID0gXCJHYW1lIG9ubHlcIjtcbiAgICAgICAgICAgIGlmICh0aGVHYW1lLmF0dHJpYnV0ZXMuaGFzbWFudWFsICYmIHRoZUdhbWUuYXR0cmlidXRlcy5oYXNib3gpIHtcbiAgICAgICAgICAgICAgICBjb25kaXRpb24gPSBcIkNvbXBsZXRlIEluIEJveFwiO1xuICAgICAgICAgICAgfSBlbHNlIGlmICh0aGVHYW1lLmF0dHJpYnV0ZXMuaGFzbWFudWFsKSB7XG4gICAgICAgICAgICAgICAgY29uZGl0aW9uID0gXCJHYW1lIGFuZCBNYW51YWxcIjtcbiAgICAgICAgICAgIH0gZWxzZSBpZiAodGhlR2FtZS5hdHRyaWJ1dGVzLmhhc2JveCkge1xuICAgICAgICAgICAgICAgIGNvbmRpdGlvbiA9IFwiR2FtZSBhbmQgQm94XCI7XG4gICAgICAgICAgICB9XG4gICAgICAgICAgICByZXR1cm4gY29uZGl0aW9uO1xuICAgICAgICB9O1xuICAgICAgICBzY3JlZW4gPSBtKFwiZGl2LnJvd1wiLCBbXG4gICAgICAgICAgICBtKFwiZGl2LmNvbC14cy0xMlwiLCBbXG4gICAgICAgICAgICAgICAgbShcImgxXCIsIFwiR2FtZSBEYXRhXCIpLFxuICAgICAgICAgICAgICAgIG0oXCJoMlwiLCAodGhlR2FtZS5hdHRyaWJ1dGVzLm5hbWUgKyBcIiAoXCIgKyB0aGVHYW1lLmF0dHJpYnV0ZXMuc3lzdGVtbmFtZSArIFwiKVwiKSksXG4gICAgICAgICAgICAgICAgbShcImRpdlwiLCBbXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzdHJvbmdcIiwgXCJDb25kaXRpb246IFwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW5cIiwgY29tcGxldGlvbkNvbmRpdGlvbigpKSxcbiAgICAgICAgICAgICAgICBdKSxcbiAgICAgICAgICAgICAgICBtKFwiZGl2XCIsIFtcbiAgICAgICAgICAgICAgICAgICAgbShcInN0cm9uZ1wiLCBcIlJlZ2lvbjogXCIpLFxuICAgICAgICAgICAgICAgICAgICBtKFwic3BhblwiLCB0aGVHYW1lLmF0dHJpYnV0ZXMucmVnaW9uKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwic3Ryb25nXCIsIFwiUXVhbnRpdHk6IFwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcInNwYW5cIiwgdGhlR2FtZS5hdHRyaWJ1dGVzLnF1YW50aXR5KVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwic3Ryb25nXCIsIFwiR2VucmVzOiBcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIHRoZUdhbWUuYXR0cmlidXRlcy5nZW5yZXMuam9pbihcIiwgXCIpKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwic3Ryb25nXCIsIFwiQ29tcGFuaWVzOiBcIiksXG4gICAgICAgICAgICAgICAgICAgIG0oXCJzcGFuXCIsIHRoZUdhbWUuYXR0cmlidXRlcy5jb21wYW5pZXMuam9pbihcIiwgXCIpKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwiaDNcIiwgXCJCbHVyYlwiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcImJsb2NrcXVvdGVcIiwgdGhlR2FtZS5hdHRyaWJ1dGVzLmJsdXJiKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJkaXZcIiwgW1xuICAgICAgICAgICAgICAgICAgICBtKFwiaDNcIiwgXCJOb3Rlc1wiKSxcbiAgICAgICAgICAgICAgICAgICAgbShcImJsb2NrcXVvdGVcIiwgdGhlR2FtZS5hdHRyaWJ1dGVzLm5vdGVzKVxuICAgICAgICAgICAgICAgIF0pLFxuICAgICAgICAgICAgICAgIG0oXCJidXR0b24uYnRuLmJ0bi1wcmltYXJ5XCIsIHtvbmNsaWNrOiBHYW1lVHJhY2tlckNsaWVudC52bS5yZXR1cm5Ub1NlYXJjaH0sIFwiQmFja1wiKVxuICAgICAgICAgICAgXSlcbiAgICAgICAgXSk7XG4gICAgfTtcbiAgICByZXR1cm4gc2NyZWVuO1xufTtcblxuR2FtZVRyYWNrZXJDbGllbnQudmlldyA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciByZW5kZXJTY3JlZW5zID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHJldHVybiBfLm1hcChHYW1lVHJhY2tlckNsaWVudC5zY3JlZW5Db2xsZWN0aW9uLCBmdW5jdGlvbihzY3JlZW5Db250ZW50LCBzY3JlZW5OYW1lKSB7XG4gICAgICAgICAgICByZXR1cm4gbShcImRpdlwiLCB7c3R5bGU6XCJkaXNwbGF5OlwiK0dhbWVUcmFja2VyQ2xpZW50LnZtLnNob3VsZERpc3BsYXlTY3JlZW4oc2NyZWVuTmFtZSl9LCBzY3JlZW5Db250ZW50KCkpO1xuICAgICAgICB9KTtcbiAgICB9O1xuICAgIHJldHVybiByZW5kZXJTY3JlZW5zKCk7XG59O1xuIiwibS5tb2R1bGUoZG9jdW1lbnQuZ2V0RWxlbWVudEJ5SWQoXCJjb250YWluZXJcIiksIEdhbWVUcmFja2VyQ2xpZW50KTtcbiJdLCJzb3VyY2VSb290IjoiL3NvdXJjZS8ifQ==