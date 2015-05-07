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
