var GameTrackerAdmin = {};

var Company = function(initialValues) {
    var name = m.prop(initialValues.name);
    var id = m.prop(initialValues.id);
};

var Systems = function(initialValues) {
    var name = m.prop(initialValues.name);
    var id = m.prop(initialValues.id);
    var manufacturerId = m.prop(initialValues.manufacturerId);
    var regionId = m.prop(initialValues.regionId);
};

GameTrackerAdmin.vm = new function() {
    var vm = {};
    vm.init = function() {
        vm.currentScreen = "add-company";

        vm.shouldDisplayScreen = function(screenName) {
            var displayProperty = (screenName === vm.currentScreen) ? "inherit" : "none";
            return displayProperty;
        };

        vm.newCompanyName = m.prop("");
        vm.isConsoleManufacturer = m.prop(false);
        vm.createNewCompany = function() {
            if (!_.isEmpty(vm.newCompanyName())) {
                m.request({method: "POST", url: "/company/", data: {name: vm.newCompanyName(), is_manufacturer: vm.isConsoleManufacturer()}})
                    .then(function(response) {
                        console.log(response);
                        vm.newCompanyName("");
                        vm.isConsoleManufacturer(false);
                    });
            };
            return false;
        };
    };
    return vm;
};

GameTrackerAdmin.controller = function() {
    GameTrackerAdmin.vm.init();
};

GameTrackerAdmin.SimpleTextInputGenerator = function(screenName, inputPlaceHolderText, changeHandler, submitHandler) {
    return m("form", {display:GameTrackerAdmin.vm.shouldDisplayScreen(screenName)}, [
        
        m("input.form-control[type=text]", {placeholder:inputPlaceHolderText, onchange: m.withAttr("value", changeHandler), value: changeHandler()}),
        m("div", [
            m("button.btn.btn-success", {onclick: submitHandler}, "submit"),
            m("button.btn.btn-danger", {onclick: GameTrackerAdmin.vm.returnToMainForm}, "cancel")
        ])
    ]);
};

GameTrackerAdmin.AddCompanyView = function() {
    return m("form", {display:GameTrackerAdmin.vm.shouldDisplayScreen("add-company")}, [
        m("input.form-control[type=text]", {placeholder:"Name", onchange: m.withAttr("value", GameTrackerAdmin.vm.newCompanyName), value: GameTrackerAdmin.vm.newCompanyName()}),
        m("div.checkbox", [
            m("label", [
                m("input[type=checkbox]", {onchange: m.withAttr("checked", GameTrackerAdmin.vm.isConsoleManufacturer), checked: GameTrackerAdmin.vm.isConsoleManufacturer()})
            ]),
            m("span", "Is this company a console manufacuturer?")
        ]),
        m("div", [
            m("button.btn.btn-success", {onclick: GameTrackerAdmin.vm.createNewCompany}, "submit"),
            m("button.btn.btn-danger", {onclick: GameTrackerAdmin.vm.returnToMainForm}, "cancel")
        ])
    ]);
};

GameTrackerAdmin.view = function() {
    return m("div.row", [
        m("div.col-xs-12", [
            GameTrackerAdmin.AddCompanyView()
        ])
    ]);
};

m.module(document.getElementById("form-insert"), GameTrackerAdmin);
