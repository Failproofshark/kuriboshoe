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
