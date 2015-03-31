var GameTrackerAdmin = {};

//Move things like creating an option and what not into the models. Like "value name" pairs and filter, adding item and autosorting etc.
var Company = function(initialValues) {
    var id = m.prop(initialValues.id);
    var name = m.prop(initialValues.name);
};

var Systems = function(initialValues) {
    var id = m.prop(initialValues.id);
    var name = m.prop(initialValues.name);
    var manufacturerId = m.prop(initialValues.manufacturerId);
};

GameTrackerAdmin.vm = new function() {
    var vm = {};
    vm.init = function() {
        vm.formMode = "add";
        
        //This is used as a stack;
        vm.screenHistory = ["GameFormScreen"];
        
        //This data is actually bootstraped and the variable it's copying from is in the template
        vm.companies = companies;
        vm.genres = genres;
        vm.systems = systems;
        
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

        vm.newCompanyName = m.prop("");
        vm.isConsoleManufacturer = m.prop(false);
        vm.createNewCompany = function() {
            if (!_.isEmpty(vm.newCompanyName())) {
                m.request({method: "POST",
                           url: "/company/",
                           data: {name: vm.newCompanyName(),
                                  is_manufacturer: vm.isConsoleManufacturer()}})
                    .then(function(response) {
                        vm.newCompanyName("");
                        vm.isConsoleManufacturer(false);
                    });
            };
            return false;
        };
        vm.cancelCompanyCreation = vm.createBackButton(function() {
            vm.newCompanyName("");
            vm.isConsoleManufacturer(false);
        });

        vm.newGenreName = m.prop("");
        vm.createNewGenre = function() {
            if (!_.isEmpty(vm.newGenreName())) {
                m.request({method: "POST",  url: "/genre/", data: {name: vm.newGenreName()}})
                    .then(function(response) {
                        vm.newGenreName("");
                    });
            }
            return false;
        };
        vm.cancelGenreCreation = vm.createBackButton(function() {
            vm.newGenreName("");
        });

        vm.newSystemName = m.prop("");
        vm.newSystemManufacturer = m.prop(null);
        vm.createNewSystem = function() {
            if (!_.isEmpty(vm.newSystemName()) && !_.isEmpty(vm.newSystemManufacturer())) {
                m.request({method: "POST",
                           url: "/system/",
                           data:{name: vm.newSystemName(),
                                 manufacturer_id: vm.newSystemManufacturer()}})
                .then(function(response) {
                    console.log(response);
                    vm.newSystemName("");
                    vm.newSystemManufacturer(null);
                });
            };
            return false;
        };
        vm.cancelSystemCreation = vm.createBackButton(function() {
            vm.newSystemName = m.prop("");
            vm.newSystemManufacturer = m.prop(null);
        });

        vm.selectedEntity = m.prop("");

        //The naming convention seems to have changed (not camel case) but this is because we wish
        //To mirror what we have in the table, mainly for back-end convenience
        //TODO have each form have a namespace for their thingies
        vm.gameForm = {};
        vm.gameForm.name = m.prop("");
        vm.gameForm.blurb = m.prop("");
        vm.gameForm.region = m.prop("");
        vm.gameForm.has_manual = m.prop(false);
        vm.gameForm.has_box = m.prop(false);
        vm.gameForm.notes = m.prop("");
        vm.gameForm.quantity = m.prop("");
        vm.gameForm.genres = m.prop([]);
        vm.gameForm.companies = m.prop([]);
        vm.gameForm.system_id = m.prop("");
        
        vm.createNewGame = function() {
            console.log("running");
            if (!_.isEmpty(vm.gameForm.name()) &&
                !_.isEmpty(vm.gameForm.region()) &&
                _.isNumber(Number(vm.gameForm.quantity()))
                && Number(vm.gameForm.quantity()) > 0) {
                console.log('requestan');
                m.request({method: "POST",
                           url: "/games/",
                           data: vm.gameForm})
                    .then(function(response) {
                        console.log("readin response");
                        console.log(response);
                        //Reset the form
                        //TODO probably adapt this to be more universal
                        _.forEach(vm.gameForm, function(input) {
                            if (_.isString(input())) {
                                input("");
                            } else if (_.isArray(input())) {
                                input([]);
                            } else {
                                input(false);
                            }
                        });
                    });
                                  
            }
            return false;
        };
        vm.searchForGame = function() {
            
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

GameTrackerAdmin.screenCollection = {};

GameTrackerAdmin.screenCollection.AddCompanyScreen = function() {
    return m("form", [m("input.form-control[type=text]", {placeholder:"Company Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.newCompanyName), value: GameTrackerAdmin.vm.newCompanyName()}),
            m("div.checkbox", [
                m("label", [
                    m("input[type=checkbox]", {onchange: m.withAttr("checked", GameTrackerAdmin.vm.isConsoleManufacturer), checked: GameTrackerAdmin.vm.isConsoleManufacturer()})
                ]),
                m("span", "Is this company a console manufacuturer?")
            ]),
            m("div", [
                m("button.btn.btn-success", {onclick: GameTrackerAdmin.vm.createNewCompany}, "submit"),
                m("button.btn.btn-danger", {onclick: GameTrackerAdmin.vm.returnToMainForm}, "cancel")
            ])]);
};

GameTrackerAdmin.screenCollection.AddGenreScreen = function() {
    return m("form", [ m("input.form-control[type=text]", {placeholder:"Genre Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.newGenreName), value: GameTrackerAdmin.vm.newGenreName()}),
             m("div", [
                 m("button.btn.btn-success", {onclick: GameTrackerAdmin.vm.createNewGenre}, "submit"),
                 m("button.btn.btn-danger", {onclick: GameTrackerAdmin.vm.returnToMainForm}, "cancel")
             ])
           ]);
};

GameTrackerAdmin.screenCollection.AddSystemScreen = function() {
    var createManufacturerSet = function() {
        return _.map(_.filter(GameTrackerAdmin.vm.companies, {'isManufacturer':1}), function(company) {
            return {value: company.id, text: company.name};
        });
    };

    return m("form", [m("input.form-control[type=text]", {placeholder:"System Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.newSystemName), value: GameTrackerAdmin.vm.newSystemName()}),
                      m("div", [
                          select2.view({ onchange:GameTrackerAdmin.vm.newSystemManufacturer,
                                         value:GameTrackerAdmin.vm.newSystemManufacturer(),
                                         select2InitializationOptions:{placeholder:"Manufacturer"}},
                                       createManufacturerSet()),
                          m("u[style=cursor:pointer]", {onclick: GameTrackerAdmin.vm.screenHistory.unshift.bind(GameTrackerAdmin.vm.screenHistory, "AddCompanyScreen")}, "+Add Company")
                      ]),
                      m("div", [
                          m("button.btn.btn-success", {onclick: GameTrackerAdmin.vm.createNewSystem}, "submit"),
                          m("button.btn.btn-danger", {onclick: GameTrackerAdmin.vm.returnToMainForm}, "cancel")
                      ])]);
};

GameTrackerAdmin.screenCollection.GameFormScreen = function() {
    var formConfiguration = {
        textAreaDisplay: function() {
            var displayValue = (GameTrackerAdmin.vm.formMode === "search") ? "display:none" : "display:inherit";
            return displayValue;
        },
        confirmButtonHandler: function() {
            console.log('called');
            var handler;
            switch (GameTrackerAdmin.vm.formMode) {
            case "add":
                console.log('yay');
                handler = GameTrackerAdmin.vm.createNewGame;
                break;
            case "search":
                console.log('wat');
                handler = GameTrackerAdmin.vm.searchForGame;
                break;
            }
            return handler;
        }
    }
    return m("form", [
        m("input.form-control", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.name),
                                 value: GameTrackerAdmin.vm.gameForm.name(),
                                 placeholder: "Name"}),
        select2.view({onchange:GameTrackerAdmin.vm.gameForm.region,
                      value: GameTrackerAdmin.vm.gameForm.region(),
                      select2InitializationOptions: {placeholder: "Region"}},
                     ["NTSC", "NTSC-J", "PAL"]),
        select2.view({onchange:GameTrackerAdmin.vm.gameForm.system_id,
                      value: GameTrackerAdmin.vm.gameForm.system_id(),
                      select2InitializationOptions: {placeholder: "System"}},
                     GameTrackerAdmin.vm.systems),
        select2.view({onchange:GameTrackerAdmin.vm.gameForm.genres,
                      value: GameTrackerAdmin.vm.gameForm.genres(),
                      select2InitializationOptions: {placeholder: "Genres"}},
                     GameTrackerAdmin.vm.genres,
                     true),
        select2.view({onchange:GameTrackerAdmin.vm.gameForm.companies,
                      value: GameTrackerAdmin.vm.gameForm.companies(),
                      select2InitializationOptions: {placeholder: "Companies"}},
                     GameTrackerAdmin.vm.companies,
                     true),
        m("input.form-control", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.quantity),
                                 value: GameTrackerAdmin.vm.gameForm.quantity(),
                                 placeholder: "Quantity"
                                }),
        
        m("div", {style:formConfiguration.textAreaDisplay()}, [
            m("p", "Short Description"),
            m("textarea", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.blurb)}, GameTrackerAdmin.vm.gameForm.blurb()),
        ]),
        m("div.checkbox", [
            m("label", [
                m("input[type=checkbox]", {onchange: m.withAttr("checked", GameTrackerAdmin.vm.gameForm.has_manual), checked: GameTrackerAdmin.vm.gameForm.has_manual()})
            ]),
            m("span", "Manual")
        ]),
        m("div.checkbox", [
            m("label", [
                m("input[type=checkbox]", {onchange: m.withAttr("checked", GameTrackerAdmin.vm.gameForm.has_box), checked: GameTrackerAdmin.vm.gameForm.has_box()})
            ]),
            m("span", "Box")
        ]),
        m("div", {style:formConfiguration.textAreaDisplay()}, [
            m("p", "Notes"),
            m("textarea", {onchange: m.withAttr("value", GameTrackerAdmin.vm.gameForm.notes)}, GameTrackerAdmin.vm.gameForm.notes()),
        ]),        
        m("div", [
            m("button.btn.btn-success", {onclick: formConfiguration.confirmButtonHandler()}, "submit"),
            m("button.btn.btn-danger", {onclick: GameTrackerAdmin.vm.cancelNewGameCreation}, "cancel")
        ])
    ]);
};

GameTrackerAdmin.view = function() {
    var renderScreens = function() {
        return _.map(GameTrackerAdmin.screenCollection, function(screenContent, screenName) {
            return m("div", {style:"display:"+GameTrackerAdmin.vm.shouldDisplayScreen(screenName)}, screenContent());
        });
    };
    return m("div.row", [
        m("div.col-xs-12", renderScreens())
    ]);
};

m.module(document.getElementById("form-insert"), GameTrackerAdmin);
