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
